module Derive.Call.Reyong where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified Util.ApproxEq as ApproxEq
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
data Pitch = Pitch Pitch.Octave Degree
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
        realize (Args.start args) (Args.end args) beat deriver

realize :: ScoreTime -> ScoreTime -> ScoreTime -> Derive.EventDeriver
    -> Derive.EventDeriver
realize start end beat deriver = do
    -- I need a regular beat, that only happens in score time.  So I should
    -- do it all in score time, that way it moves with the tempo.
    -- Derived events are in RealTime, so I have to eval them in flat tempo.
    events <- LEvent.write_logs =<< Internal.in_real_time deriver
    let beats = Seq.range' (RealTime.score start) (RealTime.score end)
            (RealTime.score beat * 4)
    pokok <- case extract_pokok beats events of
        Nothing : _ -> Derive.throw "missing initial pitch"
        pokok -> return $ Maybe.catMaybes pokok
    let degrees = [d | Pitch _ d <- pokok]
    let position voice pos = play_notes start beat voice
            (realize_position pos degrees)
    position Attrs.voice1 position1 <> position Attrs.voice2 position2
        <> position Attrs.voice3 position3 <> position Attrs.voice4 position4

decimate :: Int -> [a] -> [a]
decimate _ [] = []
decimate n (x:xs) = x : decimate n (drop n xs)

-- | Unrecognized pitches carry the previous pitch forward, unless there was
-- no previous pitch, in which case it becomes Nothing.
extract_pokok :: [RealTime] -> [Score.Event] -> [Maybe Pitch]
extract_pokok _ [] = []
extract_pokok beats events = extract $ group_beats 0.1 beats event_pitches
    where
    event_pitches = [(Score.event_start event, note)
        | (event, Just note) <- zip events (map Score.initial_note events)]
    extract = snd . List.mapAccumL extract1 Nothing
    extract1 prev pitches = maybe (prev, prev) (\p -> (Just p, Just p))
        (read_pitch =<< Seq.last pitches)

-- | Group events by the given beats, with a certain amount of eta.  The beats
-- are Indonesian style, in that they start after the previous beat and end at
-- the current beat.
group_beats :: RealTime -> [RealTime] -> [(RealTime, a)] -> [[a]]
group_beats eta = go
    where
    go [] _ = []
    go (beat:beats) notes = map snd pre : go beats post
        where (pre, post) = span ((`lte` beat) . fst) notes
    lte x y = x <= y || ApproxEq.approx_eq (RealTime.to_seconds eta) x y

play_notes :: ScoreTime -> ScoreTime -> Score.Attributes -> [Note]
    -> Derive.EventDeriver
play_notes start beat voice =
    Derive.d_merge_asc . concatMap go . zip (Seq.range_ start beat)
    where
    go (start, Note ps strike) =
        map (note start (strike_attrs strike)) (Set.toAscList ps)
    note start attrs pitch = do
        Util.add_attrs (voice <> attrs) $
            Util.with_symbolic_pitch (TrackLang.note sym []) start $
            Derive.d_place start beat $ Util.note
        where sym = Pitch.note_text $ show_pitch pitch

strike_attrs :: Strike -> Score.Attributes
strike_attrs strike = case strike of
    Open -> mempty
    Byut -> Attrs.mute
    Byong -> mempty
    Cek -> Attrs.cek

-- * realize position

t1 = realize_position position1 [I, I, I, A, A, O, O, O, O]
t2 = realize_position position2 [I, I, I, A, A, O, O, O, O]
t3 = Pretty.pprint $ realize_position position1 [I]
t4 = Pretty.pprint $ realize_position position2 [I]
t5 = Pretty.pprint $ realize_position position2 [I, I, O]

realize_position :: Position -> [Degree] -> [Note]
realize_position position = (rest:) . go
    -- The positions are notated Indonesian style, with the beat at the end, so
    -- add a leading rest to get the beats to line up.
    where
    go [] = []
    go (d:ds) = concat (replicate (length pre) sus ++ [transition]) ++ go post
        where
        (pre, post) = span (==d) ds
        sus = snd (position d)
        transition = maybe sus (fst . position) (Seq.head post)
    rest = Note mempty Open

type Position = Degree -> ([Note], [Note])

-- E U A (I) 3 5 6 (1)
position1 :: Position
position1 d = read_both [parse_octaves octaves, parse] $ case d of
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
position2 p = read_both [parse_octaves octaves] $ case p of
    I -> ("1121", "2121")
    O -> ("2232", "3232")
    E -> ("3313", "1313")
    U -> ("123-", "323-")
    A -> ("3313", "1313")
    where octaves = [('1', 0), ('2', 0), ('3', 0)]

-- U A -- 5 6
position3 :: Position
position3 p = read_both [parse_octaves octaves] $ case p of
    I -> ("556-", "656-")
    O -> ("561-", "161-")
    E -> ("565-", "5-5-")
    U -> ("5565", "6565")
    A -> ("66-6", "-6-6")
    where octaves = [('3', 0), ('5', 0), ('6', 0), ('1', 1)]

-- I O E U -- 1 2 3 5
position4 :: Position
position4 p = read_both [parse_octaves octaves] $ case p of
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
