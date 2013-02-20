module Derive.Call.Reyong where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import Util.Control
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Cmd.Meter as Meter
import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.LEvent as LEvent
import qualified Derive.Scale.Symbols as Symbols
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import Derive.Sig (required)
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import Types


-- * types

data Degree = I | O | E | U | A
    deriving (Eq, Ord, Enum, Show)

-- | Octave should range from -2 to 2.
data Pitch = Pitch {
    pitch_octave :: !Pitch.Octave
    , pitch_degree :: !Degree
    }
    deriving (Eq, Ord, Show)

data Note = Note (Set.Set Pitch) Strike
    deriving (Eq, Ord, Show)

data Strike = Open | Cek | Byut | Byong
    deriving (Eq, Ord, Show)

note :: [Pitch] -> Note
note ps = Note (Set.fromList ps) Open

instance Pretty.Pretty Pitch where
    pretty (Pitch oct d) = show oct ++ show d

instance Pretty.Pretty Note where
    pretty = note_to_string

instance Pretty.Pretty Degree where pretty = show
instance Pretty.Pretty Strike where pretty = show

degree_num :: Degree -> Int
degree_num p = case p of
    I -> 1; O -> 2; E -> 3; U -> 5; A -> 6

num_degree :: Int -> Maybe Degree
num_degree n = case n of
    1 -> Just I; 2 -> Just O; 3 -> Just E; 5 -> Just U; 6 -> Just A
    _ -> Nothing

from_pitch :: Pitch -> Pitch.Note
from_pitch (Pitch oct d) = Symbols.dotted_number (degree_num d) oct

read_pitch :: Pitch.Note -> Maybe Pitch
read_pitch n = Map.lookup n to_pitch
    where
    to_pitch = Map.fromList
        [(Symbols.dotted_number (degree_num d) oct, Pitch oct d)
            | oct <- [-2..2], d <- [I .. A]]

show_pitch :: Pitch -> Pitch.Note
show_pitch (Pitch oct d) = Symbols.dotted_number (degree_num d) oct

char_to_note :: Char -> Maybe Note
char_to_note c = case c of
    -- TODO pass in the notes
    '/' -> Just $ Note mempty Cek
    'X' -> Just $ Note mempty Byut
    'O' -> Just $ Note mempty Byong
    '-' -> Just $ note []
    _ -> Nothing

note_to_string :: Note -> String
note_to_string (Note _ Cek) = "/"
note_to_string (Note _ Byut) = "X"
note_to_string (Note _ Byong) = "O"
note_to_string (Note ps _) = case Set.toList ps of
        [] -> "-"
        [p] -> pitch p
        ps -> '[' : concatMap pitch ps ++ "]"
    where
    pitch (Pitch _ d) = show (degree_num d)


-- * realize

note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("realize-kilitan", c_realize)
    ]

c_realize :: Derive.NoteCall
c_realize = Derive.transformer "realize-kilitan" (Tags.idiom <> Tags.postproc)
    "Realize reyong kilitan.  Pitch-less events with +byut, +byong, or\
    \ +cek attrs, and are transformed into +mute, -, or +cek attributed notes\
    \ with the appropriate pitches for each position.  Pitched notes with -\
    \ attrs are expanded into kilitan."
    $ Sig.callt (required "beat" "Duration of the kilitan.  If not given, it\
        \ defaults to `s` timestep.  Normally it should be 4* the pokok\
        \ rhythm.  TODO support more exotic rhythms.") $
    \maybe_beat args deriver -> do
        beat <- maybe (Util.meter_duration (Args.start args) Meter.r_16 1)
            return maybe_beat
        realize (Args.start args) beat deriver

realize :: ScoreTime -> ScoreTime -> Derive.EventDeriver -> Derive.EventDeriver
realize start beat deriver = do
    -- I need a regular beat, that only happens in score time.  So I should
    -- do it all in score time, that way it moves with the tempo.
    -- Derived events are in RealTime, so I have to eval them in flat tempo.
    events <- LEvent.write_logs =<< Internal.in_real_time deriver
    let input = extract_input events
    -- Use the same scale as the pokok.
    scale <- Derive.get_scale . Score.event_scale_id
        =<< Derive.require "no events" (Seq.head events)
    let position voice pos = play_output voice $ realize1 pos start beat input
    Derive.with_scale scale $
        position Attrs.voice1 position1 <> position Attrs.voice2 position2
        <> position Attrs.voice3 position3 <> position Attrs.voice4 position4


-- * input, realize

type Input = (ScoreTime, ScoreTime, Either Degree Strike)
type Output = (ScoreTime, ScoreTime, Note)

extract_input :: [Score.Event] -> [Input]
extract_input = mapMaybe extract
    where
    extract e = (,,)
        (RealTime.to_score (Score.event_start e))
        (RealTime.to_score (Score.event_duration e))
        <$> read_event (Score.event_attributes e) (Score.initial_note e)
    read_event attrs maybe_note
        | has Attrs.cek = Just $ Right Cek
        | has Attrs.byut = Just $ Right Byut
        | has Attrs.byong = Just $ Right Byong
        | otherwise = Left . pitch_degree <$> (read_pitch =<< maybe_note)
        where has = Score.attrs_contain attrs

realize1 :: Position -> ScoreTime -> ScoreTime -> [Input] -> [Output]
realize1 pos start beat all_inputs = go all_inputs
    where
    go [] = []
    go ((start, dur, n) : inputs) = case n of
        Right strike ->
            (start, dur, Note (pos_strike pos) strike) : go inputs
        Left degree -> melody ((start, dur, degree) : degrees) ++ go rest
        where (degrees, rest) = span_while degree_of inputs
    degree_of (start, dur, Left d) = Just (start, dur, d)
    degree_of _ = Nothing
    melody events = output_melody pos beat $ simplify_melody events beats
    -- TODO currently I'm hardcoded to 4 kliitan notes per pokok note.
    beats = Seq.range' start end (beat * 4)
    end = case Seq.last all_inputs of
        Just (start, dur, _) -> start + dur
        Nothing -> start

-- | Take beats to a melody start time and the start time of each
simplify_melody :: [(ScoreTime, ScoreTime, Degree)] -> [ScoreTime]
    -> [(ScoreTime, [Degree])]
simplify_melody events beats =
    group $ zip beats $ snd $ List.mapAccumL extract events beats
    where
    extract events beat = (rest, d)
        where
        rest = dropWhile ((<=beat) . event_end) events
        d = case rest of
            (start, _, d) : _ | start <= beat -> Just d
            _ -> Nothing
    event_end (start, dur, _) = start + dur
    group [] = []
    group events = case dropWhile (Maybe.isNothing . snd) events of
        (start, Just d) : events ->
            let (ds, rest) = span_while snd events
            in (start, d:ds) : group rest
        _ -> group events

-- * output, play

output_melody :: Position -> ScoreTime -> [(ScoreTime, [Degree])] -> [Output]
output_melody pos beat = concatMap play
    where
    play (start, ds) = [(t, beat, pitch) | (t, pitch)
        <- zip (Seq.range_ start beat) (realize_position pos ds)]

play_output :: Score.Attributes -> [Output] -> Derive.EventDeriver
play_output attrs = Derive.d_merge_asc . concatMap go
    where
    go (start, dur, Note ps strike) =
        map (note start dur strike) (Set.toAscList ps)
    note start dur strike pitch = do
        Util.add_attrs (attrs <> strike_attrs strike) $
            Util.with_symbolic_pitch (TrackLang.note sym []) start $
            Derive.d_place start dur Util.note
        where sym = Pitch.note_text $ show_pitch pitch

-- | Convert the strike to lower-level attributes for the actual instrument.
-- These are applied to notes that already have position-appropriate pitches.
strike_attrs :: Strike -> Score.Attributes
strike_attrs strike = case strike of
    Open -> mempty
    Byut -> Attrs.mute
    Byong -> mempty
    Cek -> Attrs.cek

-- | Convert higher-level attributes to the strike.  These attributes are
-- applied to notes that don't have pitches yet.
attrs_strike :: [(Score.Attributes, Strike)]
attrs_strike =
    [ (Attrs.cek, Cek)
    , (Attrs.byut, Byut)
    , (Attrs.byong, Byong)
    , (mempty, Open)
    ]

-- * realize position

t1 = realize_position position1 [I, I, I, A, A, O, O, O, O]
t2 = realize_position position2 [I, I, I, A, A, O, O, O, O]
t3 = Pretty.pprint $ realize_position position1 [I]
t4 = Pretty.pprint $ realize_position position2 [I]
t5 = Pretty.pprint $ realize_position position2 [I, I, O]

realize_position :: Position -> [Degree] -> [Note]
realize_position pos = (rest:) . go
    -- The positions are notated Indonesian style, with the beat at the end, so
    -- add a leading rest to get the beats to line up.
    where
    go [] = []
    go (d:ds) = concat (replicate (length pre) sus ++ [transition]) ++ go post
        where
        (pre, post) = span (==d) ds
        sus = snd (pos_melody pos d)
        transition = maybe sus (fst . pos_melody pos) (Seq.head post)
    rest = Note mempty Open

data Position = Position {
    pos_strike :: Set.Set Pitch
    -- TODO should be [Pitch] not Note
    , pos_melody :: Degree -> ([Note], [Note])
    }

make_position :: [Pitch] -> [(Char, Pitch.Octave)] -> [Char -> Maybe Note]
    -> (Degree -> (String, String)) -> Position
make_position strike octaves parsers melody = Position
    { pos_strike = Set.fromList strike
    , pos_melody = read_both (parse_octaves octaves : parsers) . melody
    }

-- E U A (I) 3 5 6 (1)
position1 :: Position
position1 = make_position [Pitch (-1) E, Pitch (-1) A] octaves [parse] $
    \d -> case d of
        I -> ("556-", "656-")
        O -> ("56:-", ":-:-")
        E -> ("3353", "5353")
        U -> ("5565", "6565")
        A -> ("6636", "3636")
    where
    octaves = [('3', -1), ('5', -1), ('6', -1), ('1', 0)]
    parse ':' = Just $ note [Pitch (-1) E, Pitch 0 I]
    parse _ = Nothing

-- I O E -- 1 2 3
position2 :: Position
position2 = make_position [Pitch 0 I, Pitch 0 E] octaves [] $ \d -> case d of
    I -> ("1121", "2121")
    O -> ("2232", "3232")
    E -> ("3313", "1313")
    U -> ("123-", "323-")
    A -> ("3313", "1313")
    where octaves = [('1', 0), ('2', 0), ('3', 0)]

-- U A -- 5 6
position3 :: Position
position3 = make_position [Pitch 0 U, Pitch 1 I] octaves [] $ \d -> case d of
    I -> ("556-", "656-")
    O -> ("561-", "161-")
    E -> ("565-", "5-5-")
    U -> ("5565", "6565")
    A -> ("66-6", "-6-6")
    where octaves = [('3', 0), ('5', 0), ('6', 0), ('1', 1)]

-- I O E U -- 1 2 3 5
position4 :: Position
position4 = make_position [Pitch 1 O, Pitch 1 E] octaves [] $ \d -> case d of
    I -> ("1121", "2121")
    O -> ("2232", "3232") -- 1 2312
    E -> ("3353", "5353")
    U -> ("123-", "323-")
    A -> ("3313", "1313")
    where octaves = [('6', 0), ('1', 1), ('2', 1), ('3', 1), ('5', 1)]

parse_octaves :: [(Char, Pitch.Octave)] -> Char -> Maybe Note
parse_octaves table c = do
    oct <- lookup c table
    d <- num_degree =<< Num.read_digit c
    return $ note [Pitch oct d]

read_both :: [Char -> Maybe Note] -> ([Char], [Char]) -> ([Note], [Note])
read_both parsers (xs, ys) =
    (map (read_note parsers) xs, map (read_note parsers) ys)

read_note :: [Char -> Maybe Note] -> Char -> Note
read_note parsers c = fromMaybe (error $ "no parse for: " ++ show c) $
    msum (map ($c) parsers) `mplus` char_to_note c

-- * util

span_while :: (a -> Maybe b) -> [a] -> ([b], [a])
span_while f = go
    where
    go [] = ([], [])
    go (x:xs) = case f x of
        Nothing -> ([], x : xs)
        Just y -> let (ys, rest) = go xs in (y:ys, rest)
