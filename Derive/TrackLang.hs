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
-}
module Derive.TrackLang where
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<|>), (<?>))

import qualified Data.Maybe as Maybe

import Util.Control
import qualified Util.Parse as Parse
import qualified Util.Seq as Seq

import qualified Derive.Score as Score
import qualified Derive.Schema.Default as Default
import qualified Perform.Pitch as Pitch


data Val =
    -- | Goes in a pitch track val field.  Literal: @*5a@
    Note Pitch.Note
    -- | Sets the instrument in scope for a note.  An empty instrument doesn't
    -- set the instrument, but can be used to mark a track as a note track.
    -- Literal: @>@, @>inst@
    | Instrument (Maybe Score.Instrument)
    -- | Goes in a control method field.  Literal: @m'i'@, @m'2e'@
    | Method String -- TODO later this should be Signal.Method
    -- | Goes in a control val field.  Literal: @42.23@
    | Num Double
    -- | Goes in a note event.  Literal: @+attr@, @-attr@, @=attr@
    | Attr AttrMode String
    -- | A call to a function.  There are two kinds: a subderive produces
    -- Events, and a call transforms Events.
    -- Literal: @func@
    | Call String
    deriving (Eq, Show)

data AttrMode = Add | Remove | Set | Clear deriving (Eq, Show)

set_attr :: AttrMode -> Score.Attribute -> Score.Attributes -> Score.Attributes
set_attr mode attr attrs = case mode of
    Add -> Set.insert attr attrs
    Remove -> Set.delete attr attrs
    Set -> Set.singleton attr
    Clear -> Set.empty

-- | Types, used in the signature.
data Type = TNote | TInstrument | TMethod | TNum | TAttr | TCall
    deriving (Eq, Show)

type_equal :: Type -> Val -> Bool
type_equal typ val = case (typ, val) of
    (TNote, Note _) -> True
    (TInstrument, Instrument _) -> True
    (TMethod, Method _) -> True
    (TNum, Num _) -> True
    (TAttr, Attr _ _) -> True
    (TCall, Call _) -> True
    _ -> False

data Arg = Arg {
    arg_type :: Type
    -- | If set, the arg defaults to this value when not given.
    -- TODO if I used a GADT I could make sure it matches Type, right?
    , arg_default :: Maybe Val
    } deriving (Eq, Show)

-- | A signature only includes argument types, because the return type is fixed
-- by the track type.
type Signature = [Arg]

type TypeError = String

typecheck :: Signature -> [Val] -> [TypeError]
typecheck sig vals
    | not (null bad_opt) =
        ["required arg can't follow optional one: " ++ show bad_opt]
    | length vals < length required = ["too few arguments, " ++ expected]
    | length vals > length args = ["too many arguments, " ++ expected]
    | otherwise = Maybe.catMaybes $
        zipWith check args (map Just vals ++ repeat Nothing)
    where
    args = zip [0..] sig
    (required, optional) = break (Maybe.isJust . arg_default . snd) args
    bad_opt = filter (Maybe.isNothing . arg_default . snd) optional
    expected = "expected at least " ++ show (length required)
        ++ ", got " ++ show (length vals)

check :: (Int, Arg) -> Maybe Val -> Maybe TypeError
check (argno, arg) maybe_val = case maybe_val of
    Nothing -> case arg_default arg of
            -- Should have been caught by the checks in 'typecheck'.
        Nothing -> Just $ "required arg not given: " ++ show (argno, arg)
            -- TODO a guarantee that they agree would fix this, GADT?
        Just val
            | not (type_equal (arg_type arg) val) ->
                Just $ "expected type doesn't match default: " ++ type_err val
            | otherwise -> Nothing
    Just val
        | not (type_equal (arg_type arg) val) ->
            Just $ "expected type doesn't match given: " ++ type_err val
        | otherwise -> Nothing
    where
    type_err val = "arg " ++ show argno ++ ": "
        ++ show (arg_type arg) ++ " /= " ++ show val

-- * parsing

-- | The only operator is '|', so a list of lists suffices for an AST.
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
p_val = fmap Note p_note <|> fmap Instrument p_instrument
    <|> fmap Method p_method
    -- Attr and Num can both start with a '-', but an Attr has to have a letter
    -- afterwards, while a Num is a '.' or digit, so they're not ambiguous.
    -- that, so it's not ambiguous with Num.
    <|> fmap (uncurry Attr) (P.try p_attr) <|> fmap Num p_num
    <|> fmap Call p_call

p_note :: P.Parser Pitch.Note
p_note = P.string Default.pitch_track_prefix >> fmap Pitch.Note p_word
    <?> "note"

p_instrument :: P.Parser (Maybe Score.Instrument)
p_instrument = do
    P.string Default.note_track_prefix
    inst <- p_word
    return $ if null inst then Nothing else Just (Score.Instrument inst)
    <?> "instrument"

p_method :: P.Parser String
p_method = do
    P.char 'm'
    p_single_string
    <?> "method"

p_num :: P.Parser Double
p_num = Parse.p_float

-- There's no particular reason to restrict attrs to idents, but this will
-- force some standardization on the names.
p_attr :: P.Parser (AttrMode, String)
p_attr = do
    let as m c = fmap (const m) (P.char c)
    mode <- P.choice [as Add '+', as Remove '-', as Set '=']
    attr <- case mode of
        Set -> P.option "" p_ident
        _ -> p_ident
    return (if null attr then Clear else mode, attr)
    <?> "attr"

p_call :: P.Parser String
p_call = p_ident

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

p_word :: P.Parser String
p_word = P.many (P.noneOf " ")
