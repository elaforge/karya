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

    Control track: @+, cont@ is the same as @cont | add %cont2@

    Any number can be converted into a signal automatically.

    call syntax:
    echo delay=%echo-delay,1 feedback=%echo-feedback,.4 times=(Nothing, 1)

    echo _ %echo-delay,1

    %note-pitch,*5c
-}
module Derive.TrackLang where
import Control.Monad
import qualified Control.Monad.Error as Error
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<|>), (<?>))

import Util.Control
import qualified Util.Parse as Parse
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
    | VInstrument (Maybe Score.Instrument)
    -- | Goes in a control method field.  Literal: @m'i'@, @m'2e'@
    | VMethod Method
    -- | Goes in a control val field.  Literal: @42.23@
    | VNum Double
    -- | Goes in a note event.  Literal: @+attr@, @-attr@, @=attr@
    | VAttr Attr
    -- | A signal name.  An optional value gives a default if the signal isn't
    -- present.  Literal: @%control@, @%control,.4@
    | VSignal Signal
    -- | A call to a function.  There are two kinds: a subderive produces
    -- Events, and a call transforms Events.
    -- Literal: @func@
    | VCall CallId
    deriving (Eq, Show)

data AttrMode = Add | Remove | Set | Clear deriving (Eq, Show)

-- TODO later this should be Signal.Method
newtype Method = Method String deriving (Eq, Show)
newtype CallId = CallId String deriving (Eq, Ord, Show)
-- | (default, control).  If @control@ is Nothing, always use the default
-- (i.e. it's a constant signal).
newtype Signal = Signal (Signal.Y, Maybe Score.Control) deriving (Eq, Show)
newtype Attr = Attr (AttrMode, Score.Attribute) deriving (Eq, Show)

set_attr :: AttrMode -> Score.Attribute -> Score.Attributes -> Score.Attributes
set_attr mode attr attrs = case mode of
    Add -> Set.insert attr attrs
    Remove -> Set.delete attr attrs
    Set -> Set.singleton attr
    Clear -> Set.empty

-- * type checking

data Arg a = Arg {
    arg_name :: String
    , arg_default :: Maybe a
    } deriving (Eq, Show)

arg_type :: (Typecheck a) => Arg a -> String
arg_type arg = type_name (maybe undefined id (arg_default arg))

arg_opt :: Arg a -> (Bool, String)
arg_opt arg = (Maybe.isJust (arg_default arg), arg_name arg)

data TypeError =
    -- | arg number, arg name, expected type name, received val
    TypeError Int String String (Maybe Val)
    | ArgError String
    deriving (Eq, Show)

show_type_error (TypeError argno name expected received) =
    "type error: arg " ++ show argno ++ "/" ++ name ++ ": expected "
        ++ expected ++ " but got " ++ show received
show_type_error (ArgError err) = "arg error: " ++ err

instance Error.Error TypeError where
    strMsg _ = error "strMsg not defined for TypeError"

extract1 :: (Typecheck a) => [Val] -> Arg a -> Either TypeError a
extract1 vals sig0 = do
    arg0 : _ <- check_args vals [arg_opt sig0]
    extract_arg 0 sig0 arg0

call1 :: (Typecheck a) =>
    [Val] -> Arg a -> (a -> result) -> Either TypeError result
call1 args arg0 f = return . f =<< extract1 args arg0

extract2 :: (Typecheck a, Typecheck b) =>
    [Val] -> (Arg a, Arg b) -> Either TypeError (a, b)
extract2 vals (sig0, sig1) = do
    arg0 : arg1 : _ <- check_args vals [arg_opt sig0, arg_opt sig1]
    liftM2 (,) (extract_arg 0 sig0 arg0) (extract_arg 1 sig1 arg1)

call2 :: (Typecheck a, Typecheck b) =>
    [Val] -> (Arg a, Arg b) -> (a -> b -> result) -> Either TypeError result
call2 args (arg0, arg1) f = do
    (val0, val1) <- extract2 args (arg0, arg1)
    return $ f val0 val1

extract3 :: (Typecheck a, Typecheck b, Typecheck c) =>
    [Val] -> (Arg a, Arg b, Arg c) -> Either TypeError (a, b, c)
extract3 vals (sig0, sig1, sig2) = do
    arg0 : arg1 : arg2 : _ <- check_args vals
        [arg_opt sig0, arg_opt sig1, arg_opt sig2]
    liftM3 (,,) (extract_arg 0 sig0 arg0) (extract_arg 1 sig1 arg1)
        (extract_arg 2 sig2 arg2)

call3 :: (Typecheck a, Typecheck b, Typecheck c) =>
    [Val] -> (Arg a, Arg b, Arg c) -> (a -> b -> c -> result)
    -> Either TypeError result
call3 args (arg0, arg1, arg2) f = do
    (val0, val1, val2) <- extract3 args (arg0, arg1, arg2)
    return $ f val0 val1 val2

extract4 :: (Typecheck a, Typecheck b, Typecheck c, Typecheck d) =>
    [Val] -> (Arg a, Arg b, Arg c, Arg d) -> Either TypeError (a, b, c, d)
extract4 vals (sig0, sig1, sig2, sig3) = do
    arg0 : arg1 : arg2 : arg3 : _ <- check_args vals
        [arg_opt sig0, arg_opt sig1, arg_opt sig2, arg_opt sig3]
    liftM4 (,,,) (extract_arg 0 sig0 arg0) (extract_arg 1 sig1 arg1)
        (extract_arg 2 sig2 arg2) (extract_arg 3 sig3 arg3)

call4 :: (Typecheck a, Typecheck b, Typecheck c, Typecheck d) =>
    [Val] -> (Arg a, Arg b, Arg c, Arg d) -> (a -> b -> c -> d -> result)
    -> Either TypeError result
call4 args (arg0, arg1, arg2, arg3) f = do
    (val0, val1, val2, val3) <- extract4 args (arg0, arg1, arg2, arg3)
    return $ f val0 val1 val2 val3

check_args :: [Val] -> [(Bool, String)] -> Either TypeError [Maybe Val]
check_args vals optional_args
    | not (null bad_required) = Left $ ArgError $
        "required args can't follow an optional one: "
            ++ Seq.join ", " [show i ++ "/" ++ name | (i, name) <- bad_required]
    | length vals < length required = Left $ ArgError $
        "too few arguments, " ++ expected
    | length vals > length optional_args = Left $ ArgError $
        "too many arguments, " ++ expected
    | otherwise = Right $ map Just vals ++ repeat Nothing
    where
    (required, optional) = break (fst . snd) (zip [0..] optional_args)
    bad_required = [(i, name) | (i, (False, name)) <- optional]
    expected = "expected from " ++ show (length required) ++ " to "
        ++ show (length optional_args) ++ ", got " ++ show (length vals)

extract_arg :: (Typecheck a) => Int -> Arg a -> Maybe Val -> Either TypeError a
extract_arg argno arg maybe_val = case (arg_default arg, maybe_val) of
        (Nothing, Nothing) -> err Nothing
        (_, Just val) -> check val
        (Just v, Nothing) -> Right v
    where
    check val = case type_check val of
        Nothing -> err (Just val)
        Just v -> Right v
    err val = Left (TypeError argno (arg_name arg) (arg_type arg) val)

class Typecheck a where
    type_check :: Val -> Maybe a
    type_name :: a -> String

instance Typecheck Pitch.Note where
    type_check (VNote a) = Just a
    type_check _ = Nothing
    type_name _ = "note"

instance Typecheck (Maybe Score.Instrument) where
    type_check (VInstrument a) = Just a
    type_check _ = Nothing
    type_name _ = "instrument"

instance Typecheck Method where
    type_check (VMethod a) = Just a
    type_check _ = Nothing
    type_name _ = "method"

instance Typecheck Double where
    type_check (VNum a) = Just a
    type_check _ = Nothing
    type_name _ = "num"

instance Typecheck Attr where
    type_check (VAttr a) = Just a
    type_check _ = Nothing
    type_name _ = "attribute"

instance Typecheck Signal where
    type_check (VSignal a) = Just a
    type_check _ = Nothing
    type_name _ = "signal"

instance Typecheck CallId where
    type_check (VCall a) = Just a
    type_check _ = Nothing
    type_name _ = "call"

-- ** signature

-- Utilities to describe function signatures.

required :: String -> Arg a
required name = Arg name Nothing

optional :: String -> a -> Arg a
optional name deflt = Arg name (Just deflt)

signal :: Signal.Y -> String -> Signal
signal deflt name = Signal (deflt, Just (Score.Control name))

required_signal :: Signal.Y  -> Signal
required_signal deflt = Signal (deflt, Nothing)

-- * parsing

-- TODO These should remain the same as the ones in Derive.Schema.Default for
-- consistency.  I can't use those directly because of circular imports.
note_track_prefix, pitch_track_prefix :: String
note_track_prefix = ">"
pitch_track_prefix = "*"

-- | The only operator is @|@, so a list of lists suffices for an AST.
-- I return a pair to make it obvious that there is always at least one list.
parse :: String -> Either String ([Val], [[Val]])
parse text = Parse.parse_all p_expr (strip_comment text)

strip_comment :: String -> String
strip_comment = fst . Seq.break_tails ("--" `List.isPrefixOf`)

p_expr :: P.Parser ([Val], [[Val]])
p_expr = do
    P.spaces
    -- The P.many with P.sepBy means I should never get nil.
    first:rest <- P.sepBy (P.many (p_val #>> P.spaces)) (P.char '|' >> P.spaces)
    return (first, rest)

p_val :: P.Parser Val
p_val = fmap VNote p_note <|> fmap VInstrument p_instrument
    <|> fmap VMethod p_method
    -- Attr and Num can both start with a '-', but an Attr has to have a letter
    -- afterwards, while a Num is a '.' or digit, so they're not ambiguous.
    <|> fmap VAttr (P.try p_attr) <|> fmap VNum p_num
    <|> fmap VSignal p_signal <|> fmap VCall p_call

p_note :: P.Parser Pitch.Note
p_note = P.string pitch_track_prefix >> fmap Pitch.Note p_word
    <?> "note"

p_instrument :: P.Parser (Maybe Score.Instrument)
p_instrument = do
    P.string note_track_prefix
    inst <- p_null_word
    return $ if null inst then Nothing else Just (Score.Instrument inst)
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
p_attr :: P.Parser Attr
p_attr = do
    let as m c = fmap (const m) (P.char c)
    mode <- P.choice [as Add '+', as Remove '-', as Set '=']
    attr <- case mode of
        Set -> P.option "" p_ident
        _ -> p_ident
    return $ Attr (if null attr then Clear else mode, attr)
    <?> "attr"

p_signal :: P.Parser Signal
p_signal = do
    P.char '%'
    control <- fmap Score.Control p_ident
    deflt <- P.option 0 $ do
        P.char ','
        Parse.p_float
    return $ Signal (deflt, Just control)

p_call :: P.Parser CallId
p_call = fmap CallId p_ident

-- | Identifiers are somewhat more strict than usual.  They must be lowercase,
-- and the only non-letter allowed is hyphen.  This means words must be
-- separated with hyphens, and leaves me free to give special meanings to
-- underscores or caps if I want.
p_ident :: P.Parser String
p_ident = do
    initial <- P.lower
    rest <- P.many (P.lower <|> P.digit <|> P.char '-')
    return (initial : rest)
    <?> "identifier"

p_single_string :: P.Parser String
p_single_string = P.between (P.char '\'') (P.char '\'') $ P.many (P.noneOf "'")

p_word, p_null_word :: P.Parser String
p_word = P.many1 (P.noneOf " ")
p_null_word = P.many (P.noneOf " ")
