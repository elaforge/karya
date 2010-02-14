{-# LANGUAGE FlexibleInstances #-}
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
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<|>), (<?>))

import qualified Util.Parse as Parse
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Derive.Score as Score
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal


data Val =
    -- | Goes in a pitch track val field.  Literal: @*5a@
    VNote Pitch.Note
    -- | Sets the instrument in scope for a note.  An empty instrument doesn't
    -- set the instrument, but can be used to mark a track as a note track.
    -- Literal: @>@, @>inst@
    | VInstrument Score.Instrument
    -- | Goes in a control method field.  Literal: @m'i'@, @m'2e'@
    | VMethod Method
    -- | Goes in a control val field.  Literal: @42.23@
    | VNum Double
    -- | Relative attribute adjustment.  Literal: @+attr@, @-attr@, @=attr@,
    -- @=-@ (to clear attributes).
    --
    -- This is a bit of a hack, but means I can do attr adjustment without +=
    -- or -= operators.  It's ambiguous with 'p_equal' when spaces are omitted,
    -- e.g. @a=-42@ can be parsed as @[a, =-, 42]@ or @[=, a, -42]@.
    | VRelativeAttr RelativeAttr
    | VAttributes Score.Attributes
    -- | A signal name.  An optional value gives a default if the signal isn't
    -- present.  Literal: @%control@, @%control,.4@
    | VSignal Signal
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
        VInstrument (Score.Instrument inst) -> inst_literal_prefix ++ inst
        VMethod (Method m) -> "m'" ++ m ++ "'"
        VNum d -> Pretty.pretty d
        VRelativeAttr (RelativeAttr (mode, attr)) -> case mode of
            Add -> '+' : attr
            Remove -> '-' : attr
            Set -> '=' : attr
            Clear -> "=-"
        VAttributes attrs -> Seq.join "+" (Score.attrs_list attrs)
        VSignal (Signal (deflt, cont)) -> '%' :
            maybe "<no-sig>" (\(Score.Control s) -> s) cont
            ++ maybe "" ((',':) . Pretty.pretty) deflt
        VSymbol sym -> Pretty.pretty sym
        VNotGiven -> "_"

data AttrMode = Add | Remove | Set | Clear deriving (Eq, Show)

-- TODO later this should be Signal.Method
newtype Method = Method String deriving (Eq, Show)
-- | Symbols used in function call position.
type CallId = Symbol
-- | Symbols to look up a val in the 'ValMap'.
type ValName = Symbol

newtype Symbol = Symbol String deriving (Eq, Ord, Show)
instance Pretty.Pretty Symbol where
    pretty (Symbol "") = "<null>"
    pretty (Symbol s) = s

-- | (default, control).  If @default@ is Nothing, the signal must be present
-- or an error will be thrown.  If @control@ is Nothing, always use the default
-- (i.e. it's a constant signal).  There's no syntax for a Nothing Control, but
-- it can be given as a default arg.  Both Nothing doesn't make much sense, and
-- effectively means the arg is required.
newtype Signal = Signal (Maybe Signal.Y, Maybe Score.Control)
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

v_instrument, v_attributes :: ValName
v_instrument = Symbol "inst"
v_attributes = Symbol "attr"

-- * types

data Type = TNote | TInstrument | TMethod | TNum | TRelativeAttr | TAttributes
    | TSignal | TSymbol | TNotGiven
    deriving (Eq, Show)

instance Pretty.Pretty Type where
    pretty typ = "type " ++ drop 1 (show typ)

type_of :: Val -> Type
type_of val = case val of
    VNote _ -> TNote
    VInstrument _ -> TInstrument
    VMethod _ -> TMethod
    VNum _ -> TNum
    VRelativeAttr _ -> TRelativeAttr
    VAttributes _ -> TAttributes
    VSignal _ -> TSignal
    VSymbol _ -> TSymbol
    VNotGiven -> TNotGiven

class Show a => Typecheck a where
    from_val :: Val -> Maybe a
    to_val :: a -> Val

instance Typecheck Val where
    from_val = Just
    to_val = id

instance Typecheck Pitch.Note where
    from_val (VNote a) = Just a
    from_val _ = Nothing
    to_val = VNote

instance Typecheck Score.Instrument where
    from_val (VInstrument a) = Just a
    from_val _ = Nothing
    to_val = VInstrument

instance Typecheck Method where
    from_val (VMethod a) = Just a
    from_val _ = Nothing
    to_val = VMethod

instance Typecheck Double where
    from_val (VNum a) = Just a
    from_val _ = Nothing
    to_val = VNum

instance Typecheck RelativeAttr where
    from_val (VRelativeAttr a) = Just a
    from_val _ = Nothing
    to_val = VRelativeAttr

instance Typecheck Score.Attributes where
    from_val (VAttributes a) = Just a
    from_val _ = Nothing
    to_val = VAttributes

instance Typecheck Signal where
    from_val (VSignal a) = Just a
    from_val (VNum a) = Just (Signal (Just a, Nothing))
    from_val _ = Nothing
    to_val = VSignal

instance Typecheck Symbol where
    from_val (VSymbol a) = Just a
    from_val _ = Nothing
    to_val = VSymbol

-- * val signal

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
    ]

data LookupError = NotFound | WrongType Type deriving (Show)

lookup_val :: (Typecheck a) => ValName -> Environ -> Either LookupError a
lookup_val name environ = case Map.lookup name environ of
    Nothing -> Left NotFound
    Just val -> case from_val val of
        Nothing -> Left (WrongType (type_of val))
        Just v -> Right v

-- * signatures

-- | A single argument in the signature of a call.
data Arg a = Arg {
    -- | An arg that is not explicitly given will be looked for in the dynamic
    -- environment as \<callid>-<argname\>.  Of course signal args get this
    -- already through the control map, but this way non signal args can be
    -- defaulted, or you can default a signal arg to a constant without going
    -- to the bother of making a signal track.
    arg_name :: String
    , arg_default :: Maybe a
    } deriving (Eq, Show)

arg_type :: (Typecheck a) => Arg a -> Type
arg_type arg = type_of_val (maybe undefined id (arg_default arg))

arg_environ_default :: CallId -> String -> ValName
arg_environ_default (Symbol call) arg_name = Symbol $ call ++ "-" ++ arg_name

-- | Passed arguments.  The dynamic environ is also given for defaulting args.
data PassedArgs = PassedArgs {
    passed_vals :: [Val]
    , passed_environ :: Environ
    , passed_call :: CallId
    }

type_of_val :: (Typecheck a) => a -> Type
type_of_val = type_of . to_val

arg_opt :: Arg a -> (Bool, String)
arg_opt arg = (Maybe.isJust (arg_default arg), arg_name arg)

-- Utilities to describe function signatures.

required :: String -> Arg a
required name = Arg name Nothing

optional :: String -> a -> Arg a
optional name deflt = Arg name (Just deflt)

signal :: Signal.Y -> String -> Signal
signal deflt name = Signal (Just deflt, Just (Score.Control name))

required_signal :: String  -> Signal
required_signal name = Signal (Nothing, Just (Score.Control name))

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
            ++ Pretty.pretty expected ++ " but got " ++ Pretty.pretty received
        ArgError err -> "ArgError: " ++ err
        CallNotFound call_id -> "CallNotFound: " ++ Pretty.pretty call_id

instance Error.Error TypeError where
    strMsg _ = error "strMsg not defined for TypeError"

call0 :: PassedArgs -> result -> Either TypeError result
call0 vals f = do
    check_args vals []
    return f

extract1 :: (Typecheck a) => PassedArgs -> Arg a -> Either TypeError a
extract1 vals sig0 = do
    arg0 : _ <- check_args vals [arg_opt sig0]
    extract_arg 0 sig0 arg0

call1 :: (Typecheck a) =>
    PassedArgs -> Arg a -> (a -> result) -> Either TypeError result
call1 vals arg0 f = return . f =<< extract1 vals arg0

extract2 :: (Typecheck a, Typecheck b) =>
    PassedArgs -> (Arg a, Arg b) -> Either TypeError (a, b)
extract2 vals (sig0, sig1) = do
    arg0 : arg1 : _ <- check_args vals [arg_opt sig0, arg_opt sig1]
    liftM2 (,) (extract_arg 0 sig0 arg0) (extract_arg 1 sig1 arg1)

call2 :: (Typecheck a, Typecheck b) =>
    PassedArgs -> (Arg a, Arg b) -> (a -> b -> result)
    -> Either TypeError result
call2 vals (arg0, arg1) f = do
    (val0, val1) <- extract2 vals (arg0, arg1)
    return $ f val0 val1

extract3 :: (Typecheck a, Typecheck b, Typecheck c) =>
    PassedArgs -> (Arg a, Arg b, Arg c) -> Either TypeError (a, b, c)
extract3 vals (sig0, sig1, sig2) = do
    arg0 : arg1 : arg2 : _ <- check_args vals
        [arg_opt sig0, arg_opt sig1, arg_opt sig2]
    liftM3 (,,) (extract_arg 0 sig0 arg0) (extract_arg 1 sig1 arg1)
        (extract_arg 2 sig2 arg2)

call3 :: (Typecheck a, Typecheck b, Typecheck c) =>
    PassedArgs -> (Arg a, Arg b, Arg c) -> (a -> b -> c -> result)
    -> Either TypeError result
call3 vals (arg0, arg1, arg2) f = do
    (val0, val1, val2) <- extract3 vals (arg0, arg1, arg2)
    return $ f val0 val1 val2

extract4 :: (Typecheck a, Typecheck b, Typecheck c, Typecheck d) =>
    PassedArgs -> (Arg a, Arg b, Arg c, Arg d) -> Either TypeError (a, b, c, d)
extract4 vals (sig0, sig1, sig2, sig3) = do
    arg0 : arg1 : arg2 : arg3 : _ <- check_args vals
        [arg_opt sig0, arg_opt sig1, arg_opt sig2, arg_opt sig3]
    liftM4 (,,,) (extract_arg 0 sig0 arg0) (extract_arg 1 sig1 arg1)
        (extract_arg 2 sig2 arg2) (extract_arg 3 sig3 arg3)

call4 :: (Typecheck a, Typecheck b, Typecheck c, Typecheck d) =>
    PassedArgs -> (Arg a, Arg b, Arg c, Arg d) -> (a -> b -> c -> d -> result)
    -> Either TypeError result
call4 vals (arg0, arg1, arg2, arg3) f = do
    (val0, val1, val2, val3) <- extract4 vals (arg0, arg1, arg2, arg3)
    return $ f val0 val1 val2 val3

check_args :: PassedArgs -> [(Bool, String)] -> Either TypeError [Maybe Val]
check_args passed optional_args
    | length defaulted_vals < length required = Left $ ArgError $
        "too few arguments: " ++ expected
    | length vals > length optional_args = Left $ ArgError $
        "too many arguments: " ++ expected
    | not (null bad_required) = Left $ ArgError $
        "required args can't follow an optional one: "
            ++ Seq.join ", " [show i ++ "/" ++ name | (i, name) <- bad_required]
    | otherwise = Right $ map Just defaulted_vals ++ repeat Nothing
    where
    (required, optional) = break (fst . snd) (zip [0..] optional_args)
    vals = passed_vals passed
    defaulted_vals = Maybe.catMaybes $
        zipWith deflt (map Just vals ++ repeat Nothing) optional_args
        where
        deflt (Just val) _ = Just val
        deflt Nothing (_, arg_name) = Map.lookup
            (arg_environ_default (passed_call passed) arg_name)
            (passed_environ passed)
    bad_required = [(i, name) | (i, (False, name)) <- optional]
    from_env = length defaulted_vals - length vals
    arg_range = if length required == length optional_args
        then show (length required)
        else "from " ++ show (length required) ++ " to "
            ++ show (length optional_args)
    expected = "expected " ++ arg_range ++ ", got "
        ++ show (length defaulted_vals)
        ++ if from_env == 0 then ""
            else " (" ++ show from_env ++ " from environ)"

extract_arg :: (Typecheck a) => Int -> Arg a -> Maybe Val -> Either TypeError a
extract_arg argno arg maybe_val = case (arg_default arg, maybe_val2) of
        (Nothing, Nothing) -> err Nothing
        (_, Just val) -> check val
        (Just v, Nothing) -> Right v
    where
    maybe_val2 = case maybe_val of
        Just VNotGiven -> Nothing
        _ -> maybe_val
    check val = case from_val val of
        Nothing -> err (Just val)
        Just v -> Right v
    err val = Left (TypeError argno (arg_name arg) (arg_type arg) val)

-- * parsing

-- | The only operator is @|@, so a list of lists suffices for an AST.  The
-- Expr is in *call* order, which is the reverse of the textual order, since
-- composition associates right to left.
type Expr = [Call]
data Call = Call CallId [Val] deriving (Eq, Show)

-- | Convenient constructor for Call.  Not to be confused with 'call0'--calln.
--
-- TODO I should be able to have different types of vals, but I think I need an
-- existential wrapper for that, or an infix operator.
call :: String -> Call
call sym = Call (Symbol sym) []

-- expr :: (Typecheck a) => [(String, [a])] -> Expr
-- expr = reverse . map (uncurry call)

-- TODO These should remain the same as the ones in Derive.Schema.Default for
-- consistency.  I can't use those directly because of circular imports.
inst_literal_prefix, note_literal_prefix :: String
inst_literal_prefix = ">"
note_literal_prefix = "*"

parse :: String -> Either String Expr
parse text = fmap reverse (Parse.parse_all p_pipeline (strip_comment text))

p_pipeline :: P.Parser Expr
p_pipeline = P.sepBy p_expr (Parse.symbol "|")

p_expr, p_equal :: P.Parser Call
p_expr = P.try p_equal <|> P.try p_call <|> p_null_call
p_equal = do
    a1 <- p_val
    Parse.symbol "="
    a2 <- p_val
    return $ Call c_equal [a1, a2]

p_call, p_null_call :: P.Parser Call
p_call = liftM2 Call p_symbol (P.many p_val)
p_null_call = liftM2 Call (P.option (Symbol "") p_symbol) (P.many p_val)

strip_comment :: String -> String
strip_comment = fst . Seq.break_tails ("--" `List.isPrefixOf`)

p_val :: P.Parser Val
p_val = Parse.lexeme $ fmap VNote p_note <|> fmap VInstrument p_instrument
    <|> fmap VMethod p_method
    -- RelativeAttr and Num can both start with a '-', but an RelativeAttr has
    -- to have a letter afterwards, while a Num is a '.' or digit, so they're
    -- not ambiguous.
    <|> fmap VRelativeAttr (P.try p_rel_attr) <|> fmap VNum p_num
    <|> fmap VSignal p_signal <|> fmap VSymbol p_symbol
    <|> (P.char '_' >> return VNotGiven)

p_note :: P.Parser Pitch.Note
p_note = P.string note_literal_prefix >> fmap Pitch.Note p_word
    <?> "note"

p_instrument :: P.Parser Score.Instrument
p_instrument = do
    P.string inst_literal_prefix
    inst <- p_null_word
    return $ Score.Instrument inst
    <?> "instrument"

p_method :: P.Parser Method
p_method = do
    P.char 'm'
    fmap Method p_single_string
    <?> "method"

p_num :: P.Parser Double
p_num = Parse.p_float

-- There's no particular reason to restrict attrs to idents, but this will
-- force some standardization on the names.
p_rel_attr :: P.Parser RelativeAttr
p_rel_attr = do
    let as m c = fmap (const m) (P.char c)
    mode <- P.choice [as Add '+', as Remove '-', as Set '=']
    attr <- case mode of
        Set -> (P.char '-' >> return "") <|> p_ident ""
        _ -> p_ident ""
    return $ RelativeAttr (if null attr then Clear else mode, attr)
    <?> "relative attr"

p_signal :: P.Parser Signal
p_signal = do
    P.char '%'
    control <- fmap Score.Control (p_ident ",")
    deflt <- Parse.optional (P.char ',' >> Parse.p_float)
    return $ Signal (deflt, Just control)
    <?> "signal"

p_symbol :: P.Parser Symbol
p_symbol = Parse.lexeme (fmap Symbol (p_ident "")) <?> "symbol"

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
p_single_string = P.between (P.char '\'') (P.char '\'') $ P.many (P.noneOf "'")

p_word, p_null_word :: P.Parser String
p_word = P.many1 (P.noneOf " ")
p_null_word = P.many (P.noneOf " ")
