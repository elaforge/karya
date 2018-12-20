-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-
    Work done by the parser:
    Separate stanzas, notes and comments.
    Parse barlines and (call, pitch, octaveMark, duration)

    rhythm check:
    Start and duration, based on duration mode.
    Combine ties.
    Verify barlines.

    pitch check:
    Parse pitches, infer octaves.
-}
module Derive.Text.TScore where
import qualified Control.Monad.Combinators as P
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Void as Void

import qualified Numeric
import qualified Text.Megaparsec as P
import           Text.Megaparsec ((<?>))
import qualified Text.Megaparsec.Char as P

import qualified Util.CallStack as CallStack
import qualified Util.Log as Log
import qualified Util.Then as Then

import qualified Derive.LEvent as LEvent
import qualified Ui.Id as Id

import           Global


data Config = Config {
    -- If true, "a" is parsed as "a/", if false, it's parsed as "/a".
    _default_call :: Bool
    } deriving (Eq, Show)

-- * parse

pparse :: Parser a -> Text -> Either String a
pparse p = first P.errorBundlePretty . P.parse (p <* P.eof) "fname"

type Parser a = P.Parsec Void.Void Text a

class Element a where
    parse :: Parser a
    unparse :: a -> Text

newtype Score = Score [Toplevel] deriving (Eq, Show)

instance Element Score where
    parse = p_whitespace False *> (Score <$> P.many (lexeme parse))
    unparse (Score toplevels) = Text.unlines (map unparse toplevels)

data Toplevel = ToplevelDirective !Directive | BlockDefinition !Block
    deriving (Eq, Show)

instance Element Toplevel where
    parse = ToplevelDirective <$> parse <|> BlockDefinition <$> parse
    unparse (ToplevelDirective a) = unparse a
    unparse (BlockDefinition a) = unparse a

data Block = Block {
    block_id :: !Id.BlockId
    , block_directives :: ![Directive]
    , block_title :: !Text
    , block_tracks :: !Tracks
    } deriving (Eq, Show)

-- > ns/block = %directive "block title" [ ... ]
instance Element Block where
    parse = do
        bid <- lexeme parse
        keyword "="
        directives <- P.many (lexeme parse)
        title <- P.option "" (lexeme p_string)
        tracks <- parse
        return $ Block bid directives title tracks
    unparse (Block bid directives title tracks) =
        Text.unwords $ filter (not . Text.null) $ concat
            [ [unparse bid, "="]
            , map unparse directives
            , [if Text.null title then "" else un_string title]
            , [unparse tracks]
            ]

instance Element Id.BlockId where
    parse = do
        a <- P.takeWhile1P Nothing Id.is_id_char
        mb <- P.optional $ P.try $
            P.char '/' *> (P.takeWhile1P Nothing Id.is_id_char)
        let bid = maybe (Id.id default_namespace a)
                (\b -> Id.id (Id.namespace a) b) mb
        maybe (fail $ "invalid BlockId: " <> prettys bid) return (Id.make bid)
        <?> "BlockId"
    unparse = Id.show_short default_namespace . Id.unpack_id

default_namespace :: Id.Namespace
default_namespace = Id.namespace "ns"

newtype Tracks = Tracks [Track]
    deriving (Eq, Show)

instance Element Tracks where
    parse = fmap Tracks $
        keyword "[" *> P.sepBy1 parse (keyword "//") <* keyword "]"
    unparse (Tracks tracks) = Text.unwords $
        "[" : List.intersperse "//" (map unparse tracks) ++ ["]"]

data Track = Track {
    track_title :: !Text
    , track_tokens :: ![Token]
    } deriving (Eq, Show)

instance Element Track where
    parse = Track <$> (P.option "" (lexeme p_string)) <*> P.some (lexeme parse)
    unparse (Track title tokens) =
        (if Text.null title then "" else un_string title <> " ")
        <> Text.unwords (map unparse tokens)

data Directive = Directive !Text !(Maybe Text)
    deriving (Eq, Show)

instance Element Directive where
    parse = do
        P.char '%'
        Directive
            <$> not_in "= \n"
            <*> P.optional (P.char '=' *> not_in " \n")
        where
        not_in :: [Char] -> Parser Text
        not_in cs = P.takeWhile1P Nothing (`notElem` cs)
    unparse (Directive lhs rhs) = "%" <> lhs <> maybe "" ("="<>) rhs

data Token =
    -- | Higher count for larger divisions, e.g. anga vs. avartanam.
    TBarline !Barline
    | TNote !Note
    | TRest !Rest
    deriving (Eq, Show)

type Rank = Int

instance Element Token where
    parse = TBarline <$> parse <|> TNote <$> parse <|> TRest <$> parse
    unparse (TBarline bar) = unparse bar
    unparse (TNote note) = unparse note
    unparse (TRest rest) = unparse rest

-- ** barline

newtype Barline = Barline Int
    deriving (Eq, Show)

instance Element Barline where
    parse = Barline <$>
        (Text.length <$> P.takeWhile1P Nothing (=='|')
            <|> (P.char ';' *> pure 0))
    unparse (Barline 0) = ";"
    unparse (Barline n) = Text.replicate n "|"

-- ** Note

data Note = Note {
    note_call :: !Call
    , note_pitch :: !Pitch
    , note_duration :: !Duration
    } deriving (Eq, Show)

empty_note :: Note
empty_note = Note (Call "") (Pitch (Relative 0) "") (Duration Nothing 0 False)

-- | Parse a note with a letter pitch.
--
-- > a a2 call/a2
-- > a2.
-- > a~ a2~
-- > "call with spaces"/
instance Element Note where
    parse = do
        call <- P.optional $ P.try $ parse <* P.char '/'
        pitch <- parse
        dur <- parse
        let note = Note (fromMaybe (Call "") call) pitch dur
        -- If I allow "" as a note, I can't get P.many of them.
        guard (note /= empty_note)
        return note
    unparse (Note call pitch dur) = mconcat
        [ if call == Call "" then "" else unparse call <> "/"
        , unparse pitch
        , unparse dur
        ]

newtype Call = Call Text
    deriving (Eq, Show)

-- |
-- > word-without-slash
-- > "word with spaces"
-- > "with embedded "() quote"
instance Element Call where
    parse = (<?> "call") $ fmap Call $
        p_string <|> P.takeWhile1P Nothing (`notElem` [' ', '/'])
    unparse (Call call)
        | Text.any (`elem` [' ', '/']) call = "\"" <> call <> "\""
        | otherwise = call

p_string :: Parser Text
p_string = fmap mconcat $ P.between (P.char '"') (P.char '"') $
    P.many (P.try (P.string "\"(") <|> (Text.singleton <$> P.satisfy (/='"')))

un_string :: Text -> Text
un_string str = "\"" <> str <> "\""

newtype Rest = Rest Duration
    deriving (Eq, Show)

instance Element Rest where
    -- TODO I could possibly forbid ~ tie for rests, but I don't see why
    parse = Rest <$> (P.char '_' *> parse)
    unparse (Rest dur) = "_" <> unparse dur

-- ** Pitch

data Pitch = Pitch {
    pitch_octave :: !Octave
    , pitch_call :: !Text
    } deriving (Eq, Show)

instance Element Pitch where
    parse = Pitch <$> parse <*> (P.takeWhileP Nothing Char.isLetter)
        <?> "pitch"
    unparse (Pitch octave call) = unparse octave <> call

data Octave = Absolute !Int | Relative !Int
    deriving (Eq, Show)

instance Element Octave where
    parse = Absolute <$> p_int <|> Relative <$> p_relative <?> "octave"
        where
        p_relative = Text.foldl' (\n c -> n + if c == ',' then -1 else 1) 0 <$>
            P.takeWhileP Nothing (`elem` (",'" :: String))
    unparse (Absolute oct) = showt oct
    unparse (Relative n)
        | n >= 0 = Text.replicate n "'"
        | otherwise = Text.replicate (-n) ","

-- ** Duration

data Duration = Duration {
    dur_duration :: !(Maybe Int)
    , dur_dots :: !Int
    , dur_tie :: !Bool
    } deriving (Eq, Show)

instance Element Duration where
    parse = Duration
        <$> P.optional p_nat
        <*> (Text.length <$> P.takeWhileP Nothing (=='.'))
        <*> P.option False (P.char '~' *> pure True)
        <?> "duration"
    unparse (Duration dur dots tie) = mconcat
        [ maybe "" showt dur
        , Text.replicate dots "."
        , if tie then "~" else ""
        ]

-- ** util

p_whitespace :: Bool -> Parser ()
p_whitespace required = do
    if required then P.eof <|> P.space1 else P.space
    P.option () $ do
        P.string "--"
        P.takeWhileP Nothing (/='\n')
        P.option () (void $ P.char '\n')

p_space :: Parser ()
p_space = void $ P.takeWhile1P Nothing (==' ')

lexeme :: Parser a -> Parser a
lexeme = (<* p_whitespace False)

keyword :: Text -> Parser ()
keyword str = void $ lexeme (P.string str)

-- spaced :: Parser a -> Parser [a]
-- spaced p = P.many (lexeme True p)


p_int :: Parser Int
p_int = do
    sign <- P.option 1 (P.char '-' >> return (-1))
    (*sign) <$> p_nat
    <?> "int"

-- | Natural number including 0.
p_nat :: Parser Int
p_nat = do
    i <- P.takeWhile1P Nothing (\c -> '0' <= c && c <= '9')
    case Numeric.readDec (untxt i) of
        (n, _) : _ -> return n
        _ -> mzero -- this should never happen
    <?> "nat"

-- * rhythm

{-
    Maybe writing, say, 12 for 3 akshara in chatusra gati is too annoying.
    Also, if it turns out I need 8 per akshara then I would have to double
    every single number.  So the duration number should be akshara, with a
    way to divide it.

    Multiplicative doesn't have this problem.

    4, 2, 1, 1/2, 1/4.  I could omit the 1, so /2.  Isn't this effectively
    multiplicative, only with akshara whole notes?  If I allow a numerator,
    then I can write 3/5 etc.

    So for kandam, I would use 1 for akshara, then 3/5 etc. for durations under
    that.  Or I could use a ruler-style subdivision, e.g. 1 is one avartanam,
    .1 is one akshara, ..1 is one matra.  This way I can't say "two notes at
    double speed", I have to know the division, e.g. 'k1 t2 k1 k.2 t o2'.  Of
    course I could also write it as 'k.2 ~ t ~ ~ ~ k ~ k t o ~ ~ ~', or if
    I have a tuplet notation: 'k1 t ~ k t(k t) o ~'.

    In kandam: 'k..2 d..2 p..1'.  It seems like high subdivisions are going to
    be buried in dots, unless I can set a base matra: 'matra=.. k2 d2 p1'

    matra=.. k2 d -/p1 | k2 d -/p1 | k2 k d1 | ~ p k2 -/p1 |
    p = [| k d -/p |]
    sequence = [| 'p | 'p | k2 k d1 | ~ p k2 -/p1 |]

    also what about local assignment:

    '-p' = -/p
    P = matra=.. k2 d -p1
    sequence = matra=.. P | P | k2 k d1 | ~ p k2 -p1 |

    Notes:
        . The matra= setting is local to its expression, e.g. 'P'
        . Calls have implicit duration, they don't need a 5 suffix.
-}

data Meter = Meter {
    -- | Rank, and then time interval to the next rank:
    -- Adi: [(2, 4), (1, 2), (1, 2)] * nadai
    meter_pattern :: [(Rank, Time)]
    -- | How many time units per beat.
    , meter_time_per_beat :: Time
    -- | If true, beats fall at the end of measures.
    , meter_negative :: !Bool
    } deriving (Eq, Show)

-- | Integral time.  This is the smallest time unit expressed.
newtype Time = Time Int
    deriving (Ord, Eq, Num, Enum, Real, Integral)

instance Show Time where
    show (Time t) = show t ++ "t"

data RhythmState = RhythmState {
    state_now :: !Time
    , state_duration :: ! Time
    } deriving (Eq, Show)

additive_rhythm :: Meter -> [Token] -> [LEvent.LEvent (Time, Time, Note)]
additive_rhythm meter =
    Maybe.catMaybes . Then.mapAccumL token initial_state final
    where
    initial_state = RhythmState 0 (meter_time_per_beat meter)
    final state
        | beat == 0 = []
        | otherwise = (:[]) $ Just $
            warn state Nothing $ "extra beats at the end: " <> showt beat
        where beat = state_now state `mod` cycle_dur
    token state t = (state,) $ case t of
        TBarline (Barline rank) -> case Map.lookup beat expected_rank of
             Just r | r == rank -> Nothing
             _ -> Just $ warn state Nothing $ "barline " <> showt rank
        TNote note -> Just $ LEvent.Event (state_now state, 1, note)
            where dur = note_duration note
        where
        beat = state_now state `mod` cycle_dur

    cycle_dur = sum $ map snd pattern
    expected_rank = Map.fromList (zip ts (map fst pattern))
        where ts = scanl (+) 0 (map snd pattern)
    pattern = meter_pattern meter

    warn :: CallStack.Stack => RhythmState -> Maybe Note -> Text
        -> LEvent.LEvent a
    warn state note =
        LEvent.Log . Log.msg Log.Warn Nothing
            . ((showt measure <> "/" <> showt beat <> note_s <> ": ") <>)
        where
        (measure, beat) = state_now state `divMod` cycle_dur
        note_s = maybe "" ((" "<>) . unparse) note



-- * scrap

-- split on lines starting with '# ', then on '## '.  Then parse each section
-- separately.
-- This way is definitely more clunky than just a parser, but otherwise I think
-- I have to make sure terminals end with \n.  But I need the
-- I just don't want to parse a # in the middle of notes as a new track.
-- Maybe I just have to do P.try on the final \n in p_whitespace?
--
-- How about take until \n# and then do a sub-parse?
-- But is that so bad?  Just make notes end with \n.
{-
    # block1
    % block1 directives
    ## track1
    % track1 directives1
    track1 notes1
    % track1 directives2
    track1 notes2

    ## track2
    track2 notes

    # block2
    ## track3
    track3 notes
-}
{-
parse_score :: Text -> Either Text _
parse_score =
    verify . second (map block) . split "# " . zip [1 :: Int ..] . Text.lines
    where
    -- ([(Int, Text)], [([(Int, Text)], [NonEmpty (Int, Text)])])
    verify (pre_block, blocks)
        | not $ null $ filter (Text.null . Text.strip . snd) pre_block =
            Left $ "junk before block definition: " <> pretty pre_block
        | otherwise = mapM verify_block blocks
    -- [[NonEmpty (Int, Text)]]
    verify_block (block_directives, tracks) = Right tracks

    -- ([(Int, Text)], [NonEmpty (Int, Text)])
    -- (pre-block junk, [track])
    block = split "## " . NonEmpty.toList
    -- block = id -- second (map track) . split "## " . NonEmpty.toList
    track = split "% "
    split prefix = Seq.split_before_ne ((prefix `Text.isPrefixOf`) . snd)

parse_lines :: Parser a -> [(Int, Text)] -> Either Text a
parse_lines = undefined

-- Block = [ Block title directives [Track] ]
-- Track = [(Directives, Notes)]
-}
