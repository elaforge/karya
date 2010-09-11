{-# LANGUAGE TypeSynonymInstances #-} -- for instance Typecheck String
{-# LANGUAGE FlexibleInstances #-} -- for instance Typecheck (Control X)
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
    -- | Literal: @42.23@, @-.4@
    VNum Double
    -- | Escape a quote by doubling it.
    --
    -- Literal: @'hello'@, @'quinn''s hat'@
    | VString String

    -- | Relative attribute adjustment.
    --
    -- This is a bit of a hack, but means I can do attr adjustment without +=
    -- or -= operators.
    --
    -- Literal: @+attr@, @-attr@, @=attr@, @=-@ (to clear attributes).
    | VRelativeAttr RelativeAttr
    -- | A set of Attributs for an instrument.  No literal, since you can use
    -- VRelativeAttr.
    | VAttributes Score.Attributes

    -- | A control name.  An optional value gives a default if the control
    -- isn't present.
    --
    -- Literal: @%control@, @%control,.4@
    | VControl Control
    -- | If a control name starts with a *, it denotes a pitch signal and the
    -- scale is taken from the environ.  Unlike a control signal, the empty
    -- string is a valid signal name and means the default pitch signal.
    --
    -- Literal: @#pitch,4c@, @#,4@, @#@
    | VPitchControl PitchControl

    -- | The literal names a ScaleId, and will probably result in an exception
    -- if the lookup fails.  The empty scale is taken to mean the relative
    -- scale.
    --
    -- Literal: @*scale@, @*@.
    | VScaleId Pitch.ScaleId
    -- | Storing the scale in the environ instead of the scale id saves looking
    -- it up again and again.  TODO does it really matter?
    --
    -- It would be nicer to look up the scale in the parser and do away with
    -- VScaleId, but that means mingling Deriver with Deriver with Parser,
    -- which I don't think is a good idea.
    | VScale Pitch.Scale
    -- | No literal yet, but is returned from val calls.
    | VDegree Pitch.Degree
    -- | Sets the instrument in scope for a note.  An empty instrument doesn't
    -- set the instrument, but can be used to mark a track as a note track.
    --
    -- Literal: @>@, @>inst@
    | VInstrument Score.Instrument

    -- | A call to a function.  Symbol parsing is special in that the first
    -- word is always parsed as a symbol.  So you can have symbols of numbers
    -- or other special characters.  This means that a symbol can't be in the
    -- middle of an expression, but if you surround a word with parens like
    -- @(42)@, it will be interpreted as a call.  So the only characters a
    -- symbol can't have are space and parens.
    --
    -- Literal: @func@
    | VSymbol Symbol
    -- | An explicit not-given arg for functions so you can use positional args
    -- with defaults.
    --
    -- Literal: @_@
    | VNotGiven
    deriving (Eq, Show)

instance Pretty.Pretty Val where
    pretty val = case val of
            VNum d -> show_num d
            VString s -> "'" ++ Seq.replace "'" "''" s ++ "'"
            VRelativeAttr (RelativeAttr (mode, attr)) -> case mode of
                Add -> '+' : attr
                Remove -> '-' : attr
                Set -> '=' : attr
                Clear -> "=-"
            VAttributes attrs -> Seq.join "+" (Score.attrs_list attrs)
            VControl control -> show_control '%' Pretty.pretty control
            VPitchControl control -> show_control '#' Pitch.note_text control
            VScaleId (Pitch.ScaleId scale_id) -> '*' : scale_id
            VScale scale -> case Pitch.scale_id scale of
                Pitch.ScaleId scale_id -> "<scale: " ++ scale_id ++ ">"
            VDegree (Pitch.Degree d) -> "<degree: " ++ show_num d ++ ">"
            VInstrument (Score.Instrument inst) -> '>' : inst
            VSymbol sym -> Pretty.pretty sym
            VNotGiven -> "_"
        where
        show_control prefix show_val control = case control of
            ConstantControl val -> show_val val
            DefaultedControl (Score.Control cont) deflt ->
                prefix : cont ++ ',' : show_val deflt
            Control (Score.Control cont) -> prefix : cont

-- | Convert a haskell number into a tracklang number.
show_num :: (RealFloat a) => a -> String
show_num = Pretty.show_float (Just 2)

data AttrMode = Add | Remove | Set | Clear deriving (Eq, Show)

-- | Symbols used in function call position.
type CallId = Symbol
-- | Symbols to look up a val in the 'ValMap'.
type ValName = Symbol

newtype Symbol = Symbol String deriving (Eq, Ord, Show)
instance Pretty.Pretty Symbol where
    pretty (Symbol "") = "<null>"
    pretty (Symbol s) = s

type Control = ControlRef Signal.Y
type PitchControl = ControlRef Pitch.Note

data ControlRef val =
    -- | A constant signal.  For 'Control', this is coerced from a VNum
    -- literal.
    ConstantControl val
    -- | If the control isn't present, use the constant.
    | DefaultedControl Score.Control val
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

data Type = TNum | TString | TRelativeAttr | TAttributes | TControl
    | TPitchControl | TScaleId | TScale | TDegree | TInstrument | TSymbol
    | TNotGiven | TMaybe Type | TVal
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
    VNum {} -> TNum
    VString {} -> TString
    VRelativeAttr {} -> TRelativeAttr
    VAttributes {} -> TAttributes
    VControl {} -> TControl
    VPitchControl {} -> TPitchControl
    VScaleId {} -> TScaleId
    VScale {} -> TScale
    VDegree {} -> TDegree
    VInstrument {} -> TInstrument
    VSymbol {} -> TSymbol
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

instance Typecheck Double where
    from_val (VNum a) = Just a
    from_val _ = Nothing
    to_val = VNum
    to_type _ = TNum

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

instance Typecheck PitchControl where
    from_val (VPitchControl a) = Just a
    from_val _ = Nothing
    to_val = VPitchControl
    to_type _ = TPitchControl

instance Typecheck Pitch.ScaleId where
    from_val (VScaleId a) = Just a
    from_val _ = Nothing
    to_val = VScaleId
    to_type _ = TScaleId

instance Typecheck Pitch.Scale where
    from_val (VScale a) = Just a
    from_val _ = Nothing
    to_val = VScale
    to_type _ = TScale

instance Typecheck Pitch.Degree where
    from_val (VDegree a) = Just a
    from_val _ = Nothing
    to_val = VDegree
    to_type _ = TDegree

instance Typecheck Score.Instrument where
    from_val (VInstrument a) = Just a
    from_val _ = Nothing
    to_val = VInstrument
    to_type _ = TInstrument

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

-- | The returned Expr is never null.
parse :: String -> Either String Expr
parse text = Parse.parse_all p_pipeline (strip_comment text)

-- | Parse a control track title.  The first expression in the composition is
-- parsed simply as a list of values, not a Call.  Control track titles don't
-- follow the normal calling process but pattern match directly on vals.
parse_control_title :: String -> Either String ([Val], Expr)
parse_control_title text = Parse.parse_all p_control_title (strip_comment text)

p_control_title :: P.Parser ([Val], Expr)
p_control_title = do
    vals <- P.many (Parse.lexeme p_val)
    expr <- P.option [] (Parse.symbol "|" >> p_pipeline)
    return (vals, expr)

parse_val :: String -> Either String Val
parse_val = Parse.parse_all (Parse.lexeme p_val)

p_pipeline :: P.Parser Expr
p_pipeline = P.sepBy p_expr (Parse.symbol "|")

p_expr, p_equal :: P.Parser Call
p_expr = P.try p_equal <|> P.try p_call <|> p_null_call
p_equal = do
    a1 <- p_val
    P.skipMany1 P.space
    P.char '='
    -- This ensures that "a =b" is not interpreted as an equal expression.
    P.skipMany1 P.space
    a2 <- p_term
    return $ Call c_equal [Literal a1, a2]

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
p_val =
    VInstrument <$> p_instrument
    -- RelativeAttr and Num can both start with a '-', but an RelativeAttr has
    -- to have a letter afterwards, while a Num is a '.' or digit, so they're
    -- not ambiguous.
    <|> VRelativeAttr <$> P.try p_rel_attr <|> VNum <$> p_num
    <|> VString <$> p_string
    <|> VControl <$> p_control
    <|> VPitchControl <$> p_pitch_control
    <|> VScaleId <$> p_scale_id
    <|> (P.char '_' >> return VNotGiven)
    <|> VSymbol <$> p_symbol

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

p_pitch_control :: P.Parser PitchControl
p_pitch_control = do
    P.char '#'
    control <- Score.Control <$> P.option "" (p_ident ",")
    deflt <- Parse.optional (P.char ',' >> p_word)
    return $ case deflt of
        Nothing -> Control control
        Just val -> DefaultedControl control (Pitch.Note val)
    <?> "pitch control"

p_scale_id :: P.Parser Pitch.ScaleId
p_scale_id = do
    P.char '*'
    Pitch.ScaleId <$> P.option "" (p_ident "")
    <?> "scale id"

p_instrument :: P.Parser Score.Instrument
p_instrument = P.char '>' >> Score.Instrument <$> p_null_word
    <?> "instrument"

-- | Symbols can have anything in them but they have to start with a letter.
-- This means special literals can start with wacky characters and not be
-- ambiguous.
--
-- They can also start with a *.  This is a special hack to support *scale
-- syntax in pitch track titles, but who knows, maybe it'll be useful in other
-- places too.
p_symbol :: P.Parser Symbol
p_symbol = do
    c <- P.letter <|> P.char '*'
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
