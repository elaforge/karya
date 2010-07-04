{-# LANGUAGE TypeSynonymInstances #-} -- for instance Typecheck String
{- | The event text in a note track forms a simple language.

    Notes with text are interpreted as function calls.  The function will be
    sought in a function namespace which has global defaults merged with static
    config.  Also, each block defines a function with its name, and may be
    called without the namespace from a block in the same namespace.

    Type checking: functions must be defined with a type signature.  Since this
    is a first-order language, signatures are simply lists of types.

    Type inference: look at the positions of variables in the block and figure
    out what the type of the block is.

    Function arguments:

    - The only automatic coercion is from a number to a constant signal of that
    value.

    - An argument of @_@ is treated as not given, so you can pass positional
    arguments after it.

    Control track: @+, cont@ is the same as @cont | add %cont2@

    call syntax:
    echo delay=%echo-delay,1 feedback=.4 times=1

    echo _ %echo-delay,1

    %note-pitch,*5c
-}
module Derive.TrackLang where
import Control.Monad
import qualified Control.Monad.Error as Error
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<?>))
import Util.Control

import qualified Util.Parse as Parse
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Derive.Score as Score
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal


data Val =
    -- | Goes in a pitch track val field.  Literal: @*5a@
    VNote Pitch.Note
    -- | No special literal yet, but in the track title reuses note literal
    -- syntax.
    | VScale Pitch.Scale
    -- | Sets the instrument in scope for a note.  An empty instrument doesn't
    -- set the instrument, but can be used to mark a track as a note track.
    -- Literal: @>@, @>inst@
    | VInstrument Score.Instrument
    -- | Literal: @42.23@, @-.4@
    | VNum Double
    -- | No literal yet, but is returned from val calls.
    | VDegree Pitch.Degree
    -- | Escape a quote by doubling it.  Literal: @'hello'@, @'quinn''s hat'@
    | VString String
    -- | Relative attribute adjustment.  Literal: @+attr@, @-attr@, @=attr@,
    -- @=-@ (to clear attributes).
    --
    -- This is a bit of a hack, but means I can do attr adjustment without +=
    -- or -= operators.  It's ambiguous with 'p_equal' when spaces are omitted,
    -- e.g. @a=-42@ can be parsed as @[a, =-, 42]@ or @[=, a, -42]@.
    | VRelativeAttr RelativeAttr
    | VAttributes Score.Attributes
    -- | A control name.  An optional value gives a default if the control
    -- isn't present.  Literal: @%control@, @%control,.4@
    | VControl Control
    -- | A call to a function.  There are two kinds: a subderive produces
    -- Events, and a call transforms Events.
    -- Literal: @func@
    | VSymbol Symbol
    -- | An explicit not-given arg for functions so you can use positional args
    -- with defaults.  Literal: @_@
    | VNotGiven
    deriving (Eq, Show)

instance Pretty.Pretty Val where
    pretty val = case val of
        VNote (Pitch.Note n) -> note_literal_prefix ++ n
        VScale scale -> case Pitch.scale_id scale of
            Pitch.ScaleId scale_id -> note_literal_prefix ++ scale_id
        VInstrument (Score.Instrument inst) -> inst_literal_prefix ++ inst
        VNum d -> show_num d
        VDegree (Pitch.Degree d) -> show_num d ++ "d"
        VString s -> "'" ++ Seq.replace "'" "''" s ++ "'"
        VRelativeAttr (RelativeAttr (mode, attr)) -> case mode of
            Add -> '+' : attr
            Remove -> '-' : attr
            Set -> '=' : attr
            Clear -> "=-"
        VAttributes attrs -> Seq.join "+" (Score.attrs_list attrs)
        VControl control -> case control of
            ConstantControl val -> Pretty.pretty val
            DefaultedControl (Score.Control cont) deflt ->
                '%' : cont ++ ',' : Pretty.pretty deflt
            Control (Score.Control cont) -> '%' : cont
        VSymbol sym -> Pretty.pretty sym
        VNotGiven -> "_"

-- | Show a VNum without the bother of converting it to a Val.
show_num :: Double -> String
show_num = Pretty.pretty

data AttrMode = Add | Remove | Set | Clear deriving (Eq, Show)

-- | Symbols used in function call position.
type CallId = Symbol
-- | Symbols to look up a val in the 'ValMap'.
type ValName = Symbol

newtype Symbol = Symbol String deriving (Eq, Ord, Show)
instance Pretty.Pretty Symbol where
    pretty (Symbol "") = "<null>"
    pretty (Symbol s) = s

data Control =
    -- | A constant signal.  This is coerced from a VNum literal.
    ConstantControl Signal.Y
    -- | If the control isn't present, use the constant.
    | DefaultedControl Score.Control Signal.Y
    -- | Throw an exception if the control isn't present.
    | Control Score.Control
    deriving (Eq, Show)
newtype RelativeAttr = RelativeAttr (AttrMode, Score.Attribute)
    deriving (Eq, Show)

set_attr :: RelativeAttr -> Score.Attributes -> Score.Attributes
set_attr (RelativeAttr (mode, attr)) attrs = Score.Attributes $ case mode of
    Add -> Set.insert attr (Score.attrs_set attrs)
    Remove -> Set.delete attr (Score.attrs_set attrs)
    Set -> Set.singleton attr
    Clear -> Set.empty

-- | An empty instrument literal is a no-op, see 'VInstrument'.
is_null_instrument :: Score.Instrument -> Bool
is_null_instrument (Score.Instrument "") = True
is_null_instrument _ = False

-- * constants

c_equal :: CallId
c_equal = Symbol "="

-- | Define a few inhabitants of Environ which are used by the built-in set
-- of calls.
v_instrument, v_attributes, v_scale, v_srate :: ValName
v_instrument = Symbol "inst"
v_attributes = Symbol "attr"
v_scale = Symbol "scale"
v_srate = Symbol "srate"

-- * types

data Type = TNote | TScale | TInstrument | TNum | TDegree | TString
    | TRelativeAttr | TAttributes | TControl | TSymbol | TNotGiven
    | TMaybe Type | TVal
    deriving (Eq, Show)

instance Pretty.Pretty Type where
    pretty (TMaybe typ) = "Maybe " ++ Pretty.pretty typ
    pretty typ = drop 1 (show typ)

type_of :: Val -> Type
type_of val = case val of
    -- Yes, it's duplicated with 'to_type', and I can merge them by adding
    -- a VMaybe and doing @type_of . to_val@, but 'to_val' and 'type_of' both
    -- promising to not evaluate the value seems even more hacky than just
    -- 'to_type' making that promise.
    VNote _ -> TNote
    VScale _ -> TScale
    VInstrument _ -> TInstrument
    VNum _ -> TNum
    VDegree _ -> TDegree
    VString _ -> TString
    VRelativeAttr _ -> TRelativeAttr
    VAttributes _ -> TAttributes
    VControl _ -> TControl
    VSymbol _ -> TSymbol
    VNotGiven -> TNotGiven

class Show a => Typecheck a where
    from_val :: Val -> Maybe a
    to_val :: a -> Val
    -- | This shouldn't evaluate its argument, so you can use
    -- @maybe undefined id@ to get the type of a @Maybe a@.
    -- This is an unsatisfying dangerous hack.
    to_type :: a -> Type

instance Typecheck Val where
    from_val = Just
    to_val = id
    to_type _ = TVal

-- | Putting Maybe in Typecheck means I can have optional arguments with no
-- defaults.  Further docs in 'CallSig.optional'.
instance (Typecheck a) => Typecheck (Maybe a) where
    from_val VNotGiven = Just Nothing
    from_val a = case from_val a of
        Nothing -> Nothing
        Just v -> Just (Just v)
    to_val Nothing = VNotGiven
    to_val (Just a) = to_val a
    to_type val = TMaybe (to_type (maybe undefined id val))

instance Typecheck Pitch.Note where
    from_val (VNote a) = Just a
    from_val _ = Nothing
    to_val = VNote
    to_type _ = TNote

instance Typecheck Pitch.Scale where
    from_val (VScale a) = Just a
    from_val _ = Nothing
    to_val = VScale
    to_type _ = TScale

instance Typecheck Score.Instrument where
    from_val (VInstrument a) = Just a
    from_val _ = Nothing
    to_val = VInstrument
    to_type _ = TInstrument

instance Typecheck Double where
    from_val (VNum a) = Just a
    from_val _ = Nothing
    to_val = VNum
    to_type _ = TNum

instance Typecheck Pitch.Degree where
    from_val (VDegree a) = Just a
    from_val _ = Nothing
    to_val = VDegree
    to_type _ = TDegree

instance Typecheck String where
    from_val (VString s) = Just s
    from_val _ = Nothing
    to_val = VString
    to_type _ = TString

instance Typecheck RelativeAttr where
    from_val (VRelativeAttr a) = Just a
    from_val _ = Nothing
    to_val = VRelativeAttr
    to_type _ = TRelativeAttr

instance Typecheck Score.Attributes where
    from_val (VAttributes a) = Just a
    from_val _ = Nothing
    to_val = VAttributes
    to_type _ = TAttributes

instance Typecheck Control where
    from_val (VControl a) = Just a
    from_val (VNum a) = Just (ConstantControl a)
    from_val _ = Nothing
    to_val = VControl
    to_type _ = TControl

instance Typecheck Symbol where
    from_val (VSymbol a) = Just a
    from_val _ = Nothing
    to_val = VSymbol
    to_type _ = TSymbol

-- * dynamic environment

type Environ = Map.Map ValName Val

-- | Return either the modified environ or the type expected if the type of the
-- argument was wrong.  Once you put a key of a given type into the environ, it
-- can only ever be overwritten by a Val of the same type.
--
-- 'RelativeAttr's are never inserted, they combine with existing Attributes or
-- create new ones.
put_val :: (Typecheck val) => ValName -> val -> Environ -> Either Type Environ
put_val name val environ = case maybe_old of
    Nothing -> case Map.lookup name hardcoded_types of
        Just expected | type_of new_val /= expected -> Left expected
        _ -> Right $ Map.insert name new_val environ
    Just old_val
        | type_of old_val == type_of new_val ->
            Right $ Map.insert name new_val environ
        | otherwise -> Left (type_of old_val)
    where
    maybe_old = Map.lookup name environ
    new_val = case (to_val val) of
        VRelativeAttr rel_attr -> VAttributes $
            case maybe_old of
                Just (VAttributes attrs) -> set_attr rel_attr attrs
                _ -> set_attr rel_attr Score.no_attrs
        _ -> to_val val

-- | If a standard val gets set to the wrong type, it will cause confusing
-- errors later on.
hardcoded_types :: Map.Map ValName Type
hardcoded_types = Map.fromList
    [ (v_instrument, TInstrument)
    , (v_attributes, TAttributes)
    , (v_scale, TScale)
    , (v_srate, TNum)
    ]

data LookupError = NotFound | WrongType Type deriving (Show)

lookup_val :: (Typecheck a) => ValName -> Environ -> Either LookupError a
lookup_val name environ = case Map.lookup name environ of
    Nothing -> Left NotFound
    Just val -> case from_val val of
        Nothing -> Left (WrongType (type_of val))
        Just v -> Right v

-- * extract and call

data TypeError =
    -- | arg number, arg name, expected type, received val
    TypeError Int String Type (Maybe Val)
    -- | Couldn't even call the thing because the name was not found.
    | CallNotFound CallId
    -- | Calling error that doesn't fit into the above categories.
    | ArgError String
    deriving (Eq, Show)

instance Pretty.Pretty TypeError where
    pretty err = case err of
        TypeError argno name expected received ->
            "TypeError: arg " ++ show argno ++ "/" ++ name ++ ": expected "
            ++ Pretty.pretty expected ++ " but got "
            ++ Pretty.pretty (type_of <$> received)
            ++ " " ++ Pretty.pretty received
        ArgError err -> "ArgError: " ++ err
        CallNotFound call_id -> "CallNotFound: " ++ Pretty.pretty call_id

instance Error.Error TypeError where
    strMsg _ = error "strMsg not defined for TypeError"

-- * parsing

-- | The only operator is @|@, so a list of lists suffices for an AST.
type Expr = [Call]
data Call = Call CallId [Term] deriving (Eq, Show)
data Term = ValCall Call | Literal Val deriving (Eq, Show)

-- | Convenient constructor for Call.  Not to be confused with 'call0'--calln.
--
-- TODO I should be able to have different types of vals, but I think I need an
-- existential wrapper for that, or an infix operator.
call :: String -> Call
call sym = Call (Symbol sym) []

val_call :: String -> Term
val_call sym = ValCall (Call (Symbol sym) [])

-- TODO These should remain the same as the ones in Derive.Schema.Default for
-- consistency.  I can't use those directly because of circular imports.
inst_literal_prefix, note_literal_prefix :: String
inst_literal_prefix = ">"
note_literal_prefix = "*"

-- | The returned Expr is never null.
parse :: String -> Either String Expr
parse text = Parse.parse_all p_pipeline (strip_comment text)

-- | Parse a control track title.  The final expression in the composition is
-- parsed simply as a list of values, not a Call.  Control track titles don't
-- follow the normal calling process but pattern match directly on vals.
parse_control_track :: String -> Either String (Expr, [Val])
parse_control_track text = do
    expr <- parse text
    case Seq.break_last expr of
        (calls, Just track_expr) -> do
            combined <- combine track_expr
            Right (calls, combined)
        _ -> Left "not reached because of 'parse' postcondition"
    where
    combine (Call call_id@(Symbol sym) terms)
        | not (null [() | ValCall _ <- terms]) =
            Left "val calls not supported in control track title"
        | null sym = Right [val | Literal val <- terms]
        | otherwise = Right $ VSymbol call_id : [val | Literal val <- terms]

parse_vals :: String -> Either String [Val]
parse_vals text = Parse.parse_all (P.many p_val) (strip_comment text)

parse_val :: String -> Either String Val
parse_val = Parse.parse_all p_val

p_pipeline :: P.Parser Expr
p_pipeline = P.sepBy p_expr (Parse.symbol "|")

p_expr, p_equal :: P.Parser Call
p_expr = P.try p_equal <|> P.try p_call <|> p_null_call
p_equal = do
    a1 <- p_call_symbol
    P.skipMany1 P.space
    P.char '='
    -- This ensures that "a =b" is not interpreted as an equal expression.
    P.skipMany1 P.space
    a2 <- p_term
    return $ Call c_equal [Literal (VSymbol a1), a2]

p_call :: P.Parser Call
p_call = Call <$> Parse.lexeme p_call_symbol <*> P.many p_term

p_null_call :: P.Parser Call
p_null_call = return (Call (Symbol "") []) <?> "null call"

-- | Any word in call position is considered a Symbol.  This means that
-- you can have calls like @4@ and @>@, which are useful names for notes or
-- ornaments.
p_call_symbol :: P.Parser Symbol
p_call_symbol = Symbol <$> p_word

strip_comment :: String -> String
strip_comment = fst . Seq.break_tails ("--" `List.isPrefixOf`)

p_term :: P.Parser Term
p_term = Parse.lexeme $ Literal <$> p_val <|> ValCall <$> p_sub_call

p_sub_call :: P.Parser Call
p_sub_call = P.between (P.char '(') (P.char ')') p_call

p_val :: P.Parser Val
p_val = Parse.lexeme $
    VNote <$> p_note <|> VInstrument <$> p_instrument
    -- RelativeAttr and Num can both start with a '-', but an RelativeAttr has
    -- to have a letter afterwards, while a Num is a '.' or digit, so they're
    -- not ambiguous.
    <|> VRelativeAttr <$> P.try p_rel_attr <|> VNum <$> p_num
    <|> VString <$> p_string <|> VControl <$> p_control
    <|> (P.char '_' >> return VNotGiven)
    <|> VSymbol <$> p_symbol

p_note :: P.Parser Pitch.Note
p_note = P.string note_literal_prefix >> Pitch.Note <$> p_null_word <?> "note"

p_instrument :: P.Parser Score.Instrument
p_instrument = P.string inst_literal_prefix >> Score.Instrument <$> p_null_word
    <?> "instrument"

p_num :: P.Parser Double
p_num = Parse.p_float

p_string :: P.Parser String
p_string = p_single_string <?> "string"

-- There's no particular reason to restrict attrs to idents, but this will
-- force some standardization on the names.
p_rel_attr :: P.Parser RelativeAttr
p_rel_attr = do
    let as m c = const m <$> P.char c
    mode <- P.choice [as Add '+', as Remove '-', as Set '=']
    attr <- case mode of
        Set -> (P.char '-' >> return "") <|> p_ident ""
        _ -> p_ident ""
    return $ RelativeAttr (if null attr then Clear else mode, attr)
    <?> "relative attr"

p_control :: P.Parser Control
p_control = do
    P.char '%'
    control <- Score.Control <$> p_ident ","
    deflt <- Parse.optional (P.char ',' >> Parse.p_float)
    return $ case deflt of
        Nothing -> Control control
        Just val -> DefaultedControl control val
    <?> "control"

-- | Symbols can have anything in them but they have to start with a letter.
-- This means special literals can start with wacky characters and not be
-- ambiguous.
p_symbol :: P.Parser Symbol
p_symbol = do
    c <- P.letter
    rest <- p_null_word
    return (Symbol (c : rest))

-- | Identifiers are somewhat more strict than usual.  They must be lowercase,
-- and the only non-letter allowed is hyphen.  This means words must be
-- separated with hyphens, and leaves me free to give special meanings to
-- underscores or caps if I want.
--
-- @until@ gives additional chars that stop parsing, for idents that are
-- embedded in another lexeme.
p_ident :: String -> P.Parser String
p_ident until = do
    initial <- P.lower
    -- This forces identifiers to be separated with spaces, except with the |
    -- operator.  Otherwise @sym>inst@ is parsed as a call @sym >inst@, which
    -- seems like something I don't want to support.
    rest <- P.many (P.noneOf (" |=" ++ until))
    let ident = initial : rest
    when (not (valid_identifier ident)) $
        P.unexpected $ "chars in identifier; only [a-z0-9-] are accepted: "
            ++ show ident
    return ident

valid_identifier :: String -> Bool
valid_identifier = all $ \c -> Char.isLower c || Char.isDigit c || c == '-'

p_single_string :: P.Parser String
p_single_string = P.between (P.char '\'') (P.char '\'') $
    P.many (P.noneOf "'" <|> (P.try (P.string "''") >> return '\''))

p_word, p_null_word :: P.Parser String
p_word = P.many1 (P.noneOf " ()")
p_null_word = P.many (P.noneOf " ()")
