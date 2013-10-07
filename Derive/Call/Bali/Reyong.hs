-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Bali.Reyong where
import qualified Data.Char as Char
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
import qualified Derive.Environ as Environ
import qualified Derive.LEvent as LEvent
import qualified Derive.Scale.BaliScales as BaliScales
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
type Pitches = Set.Set Pitch

data Note = Note Pitches Strike
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

-- | TODO this is too hardcoded to the specific representation of the
-- scale.  I need a generic way to parse and unparse symbolic pitches.
read_pitch :: Pitch.Note -> Maybe Pitch
read_pitch n = Map.lookup n to_pitch
    where
    to_pitch = Map.fromList $ zip (take 30 BaliScales.ioeua)
        [Pitch oct d | oct <- [1..], d <- [I .. A]]

show_pitch :: Pitch -> Pitch.Note
show_pitch (Pitch oct d) =
    Pitch.Note $ showt oct <> txt (map Char.toLower (show d))

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

note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.call_maps []
    [ ("realize-kilitan", c_realize)
    ]

c_realize :: Derive.Transformer Derive.Note
c_realize = Derive.transformer "realize-kilitan"
    (Tags.idiom <> Tags.bali <> Tags.postproc)
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

realize :: ScoreTime -> ScoreTime -> Derive.NoteDeriver -> Derive.NoteDeriver
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
        position 0 position1 <> position 1 position2
        <> position 2 position3 <> position 3 position4


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
        Right strike -> (start, dur, strike_note strike) : go inputs
        Left degree -> melody ((start, dur, degree) : degrees) ++ go rest
        where (degrees, rest) = Seq.span_while degree_of inputs
    strike_note Cek = Note (pos_cek pos) Cek
    strike_note strike = Note (pos_byong pos) strike

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
            let (ds, rest) = Seq.span_while snd events
            in (start, d:ds) : group rest
        events -> group events

-- * output, play

output_melody :: Position -> ScoreTime -> [(ScoreTime, [Degree])] -> [Output]
output_melody pos beat = concatMap play
    where
    play (start, ds) = [(t, beat, pitch) | (t, pitch)
        <- zip (Seq.range_ start beat) (realize_melody pos ds)]

play_output :: Int -> [Output] -> Derive.NoteDeriver
play_output voice = Derive.d_merge_asc . concatMap go
    where
    go (start, dur, Note ps strike) =
        map (note start dur strike) (Set.toAscList ps)
    note start dur strike pitch = do
        Derive.with_val Environ.voice voice $
            Util.add_attrs (strike_attrs strike) $
            Util.with_symbolic_pitch (TrackLang.call sym []) start $
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

-- * realize melody

t1 = realize_melody position1 [I, I, I, A, A, O, O, O, O]
t2 = realize_melody position2 [I, I, I, A, A, O, O, O, O]
t3 = Pretty.pprint $ realize_melody position1 [I]
t4 = Pretty.pprint $ realize_melody position2 [I]
t5 = Pretty.pprint $ realize_melody position2 [I, I, O]

realize_melody :: Position -> [Degree] -> [Note]
realize_melody pos = (rest:) . go
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
    pos_cek :: Pitches
    , pos_byong :: Pitches
    , pos_melody :: Degree -> ([Note], [Note])
    }

make_position :: Pitch -> [Pitch] -> (Degree -> ([Note], [Note])) -> Position
make_position cek byong melody = Position
    { pos_cek = Set.singleton cek
    , pos_byong = Set.fromList byong
    , pos_melody = melody
    }

-- E U A (I) 3 5 6 (1)
position1 :: Position
position1 = make $ \d -> case d of
        I -> p ("556-", "656-")
        O -> p ("56:-", ":-:-")
        E -> p ("3353", "5353")
        U -> p ("5565", "6565")
        A -> p ("6636", "3636")
    where
    make = make_position (Pitch 3 U) [Pitch 3 E, Pitch 3 A]
    p = read_both [('3', 3), ('5', 3), ('6', 3), ('1', 4)]

-- I O E -- 1 2 3
position2 :: Position
position2 = make $ \d -> case d of
        I -> p ("1121", "2121")
        O -> p ("2232", "3232")
        E -> p ("3313", "1313")
        U -> p ("123-", "323-")
        A -> p ("3313", "1313")
    where
    make = make_position (Pitch 4 O) [Pitch 4 I, Pitch 4 E]
    p = read_both [('1', 4), ('2', 4), ('3', 4)]

-- U A -- 5 6
position3 :: Position
position3 = make $ \d -> case d of
        I -> p ("556-", "656-")
        O -> p ("561-", "161-")
        E -> p ("565-", "5-5-")
        U -> p ("5565", "6565")
        A -> p ("66-6", "-6-6")
    where
    make = make_position (Pitch 4 A) [Pitch 4 U, Pitch 5 I]
    p = read_both [('3', 4), ('5', 4), ('6', 4), ('1', 5)]

-- I O E U -- 1 2 3 5
position4 :: Position
position4 = make $ \d -> case d of
        I -> p ("1121", "2121")
        O -> p ("2232", "3232") -- 1 2312
        E -> p ("3353", "5353")
        U -> p ("123-", "323-")
        A -> p ("3313", "1313")
    where
    make = make_position (Pitch 5 E) [Pitch 5 O, Pitch 5 U]
    p = read_both [('6', 4), ('1', 5), ('2', 5), ('3', 5), ('5', 5)]

read_both :: [(Char, Pitch.Octave)] -> ([Char], [Char]) -> ([Note], [Note])
read_both octaves (xs, ys) = (map f xs, map f ys)
    where
    f c = fromMaybe (error $ "no parse for: " ++ show c) $
        char_to_note octaves c

char_to_note :: [(Char, Pitch.Octave)] -> Char -> Maybe Note
char_to_note octaves c = case c of
    '-' -> Just $ note []
    ':' -> Just $ note [Pitch 3 E, Pitch 4 I]
    _ -> case lookup c octaves of
        Just oct ->
            note . (:[]) . Pitch oct <$> (num_degree =<< Num.read_digit c)
        Nothing -> Nothing
