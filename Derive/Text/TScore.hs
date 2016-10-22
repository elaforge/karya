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
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text

import qualified Numeric
import qualified Text.Parsec as P

import qualified Util.CallStack as CallStack
import qualified Util.Log as Log
import qualified Util.Then as Then

import qualified Derive.LEvent as LEvent
import Global


parse :: Parser a -> Text -> Either P.ParseError a
parse p = P.parse (p <* P.eof) ""

type Parser a = P.Parsec Text () a

data Score = Score [Token]
    deriving (Eq, Show)

data Token =
    -- | Higher count for larger divisions, e.g. anga vs. avartanam.
    TBarline !Rank
    | TNote Note
    deriving (Eq, Show)

type Rank = Int

data Note = Note {
    note_call :: !(Maybe Text)
    , note_pitch :: !Text
    , note_octave :: !Int
    , note_duration :: !Duration
    } deriving (Eq, Show)

data Duration = Duration {
    dur_duration :: !(Maybe Int)
    , dur_dots :: !Int
    , dur_tie :: !Bool
    } deriving (Eq, Show)

-- | This should be the inverse of 'p_note'.
show_note :: Note -> Text
show_note (Note maybe_call pitch octave duration) =
    maybe "" (<>"/") maybe_call <> pitch <> oct <> show_duration duration
    where
    oct
        | octave >= 0 = Text.replicate octave "'"
        | otherwise = Text.replicate (-octave) ","

show_duration :: Duration -> Text
show_duration (Duration dur dots tie) =
    maybe "" showt dur <> Text.replicate dots "." <> (if tie then "~" else "")

p_score :: Parser Score
p_score = Score <$> (p_whitespace False *> P.many (lexeme True p_token))

p_token :: Parser Token
p_token = p_barline <|> TNote <$> p_note

p_barline :: Parser Token
p_barline = TBarline . length <$> P.many1 (P.char '|')


-- | Parse a note with a letter pitch.
--
-- > a a2 call/a2
-- > a2.
-- > a~ a2~
p_note :: Parser Note
p_note = do
    call <- P.optionMaybe $ P.try $ P.many1 (P.satisfy (/='/')) <* P.char '/'
    pitch <- P.many1 P.letter
    octave <- P.many (P.oneOf ",'")
    duration <- p_duration
    return $ Note
        { note_call = txt <$> call
        , note_pitch = txt pitch
        , note_octave = sum $ map (\c -> if c == ',' then -1 else 1) octave
        , note_duration = duration
        }

p_duration :: Parser Duration
p_duration = Duration
    <$> P.optionMaybe p_natural
    <*> (length <$> P.many (P.char '.'))
    <*> P.option False (P.char '~' *> pure True)

p_natural :: Parser Int
p_natural = do
    i <- P.many1 P.digit
    case Numeric.readDec i of
        (n, _) : _ -> return n
        _ -> mzero

p_whitespace :: Bool -> Parser ()
p_whitespace required = do
    when required (void P.space <|> P.eof)
    P.spaces
    P.optional $ P.string "--" >> P.skipMany (P.satisfy (/='\n'))
        >> (void (P.char '\n') <|> P.eof)

lexeme :: Bool -> Parser a -> Parser a
lexeme required = (<* p_whitespace required)


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
        TBarline rank -> case Map.lookup beat expected_rank of
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
        note_s = maybe "" ((" "<>) . show_note) note
