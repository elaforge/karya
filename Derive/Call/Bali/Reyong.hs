-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls for reyong and trompong techniques.
module Derive.Call.Bali.Reyong where
import qualified Data.Map.Strict as Map

import Util.Control
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.Call.Bali.Kotekan as Kotekan
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.Environ as Environ
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig

import qualified Perform.Pitch as Pitch
import Types


{-
    Kotekan uses separate calls for the cycle and the \"pickup\" preparation.
    It would be nicer to do it automatically, but sometimes pickups appear in
    irregular places, so I still need a manual option.

    The pickup is actually a transitition, since it may be different depending
    on source and destination pitches.

       3<------2<------1<------1
    +++3<--3+++2<--2+++1<------1
    3353535322323232112121212121
-}

note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.generator_call_map
    [ ("kilit", realize_pattern Kotekan.Repeat norot_patterns)
    , (">kilit", realize_pattern Kotekan.Once pickup_patterns)
    , ("k/_\\", realize_pattern Kotekan.Repeat k_12_1_21)
    , ("k//", realize_pattern Kotekan.Repeat k12_12_12)
    , ("k\\\\", realize_pattern Kotekan.Repeat k21_21_21)
    , ("k_\\", realize_pattern Kotekan.Once k_11_1_21)
    , ("k\\/", realize_pattern Kotekan.Once rejang)
    , ("/", articulation "cek-loose" ((:[]) . pos_cek)
        (Attrs.rim <> Attrs.loose))
    , ("X", articulation "cek" ((:[]) . pos_cek) Attrs.rim)
    , ("O", articulation "byong" pos_byong mempty)
    , ("+", articulation "byut" pos_byong Attrs.mute)
    ]
    where
    articulation = make_articulation reyong_positions

k_12_1_21, k12_12_12, k21_21_21, k_11_1_21, rejang :: Pattern
k_12_1_21 = reyong_pattern "34-343-4" "-12-1-21"
k12_12_12 = reyong_pattern "4-34-34-" "12-12-12"
k21_21_21 = reyong_pattern "-43-43-4" "21-21-21"
k_11_1_21 = reyong_pattern "-44-43-4" "-11-1-21"
rejang = reyong_pattern "44-34-3- 43-434-3" "-12-12-2 1-21-12-"

reyong_patterns :: [Pattern]
reyong_patterns = [k_12_1_21, k12_12_12, k21_21_21, k_11_1_21, rejang]

reyong_pattern :: [Char] -> [Char] -> Pattern
reyong_pattern above below = reyong_kotkean_pattern $ parse_kotekan above below

module_ :: Module.Module
module_ = "bali" <> "reyong"

-- * kilitan

-- | Kilitan is implemented as a set of patterns indexed by an absolute pitch
-- degree.  The patterns are similar to kotekan, except with absolute pitches,
-- and without a polos \/ sangsih division.
realize_pattern :: Kotekan.Repeat -> Pattern -> Derive.Generator Derive.Note
realize_pattern repeat pattern =
    Derive.make_call module_ "reyong" Tags.inst "Emit reyong kilitan."
    $ Sig.call (Kotekan.dur_arg) $ \dur -> Sub.inverting $ \args -> do
        (parse_pitch, show_pitch, _) <- Util.get_pitch_functions
        pitch <- Util.get_parsed_pitch parse_pitch =<< Args.real_start args
        positions <- Derive.require ("no pattern for pitch: " <> pretty pitch)
            (Map.lookup (Pitch.pitch_pc pitch) pattern)
        mconcat $ map (realize show_pitch (Args.range args) dur)
            (zip [1..] positions)
    where
    realize show_pitch (start, end) dur (voice, position) =
        Kotekan.realize_notes start (realize_note show_pitch voice start) $
            Kotekan.realize_pattern repeat start end dur (const position)

-- * articulation

make_articulation :: [Position] -> Text -> (Position -> [Pitch.Pitch])
    -> Score.Attributes -> Derive.Generator Derive.Note
make_articulation positions name get_notes attrs =
    Derive.make_call module_ name Tags.inst "Reyong articulation."
    $ Sig.call0 $ Sub.inverting $ \args -> do
        (_, show_pitch, _) <- Util.get_pitch_functions
        mconcat $ map (realize show_pitch args) (zip [1..] positions)
    where
    realize show_pitch args (voice, position) = mconcat $
        map (Util.place args . realize_note show_pitch voice (Args.start args))
            (map (\p -> (p, attrs)) (get_notes position))

type Voice = Int

realize_note :: (Pitch.Pitch -> Maybe Pitch.Note) -> Voice -> ScoreTime
    -> Note -> Derive.NoteDeriver
realize_note show_pitch voice start (pitch, attrs) =
    Util.add_attrs attrs $
    Derive.with_val Environ.voice voice $ do
        note <- Derive.require ("unshowable pitch: " ++ Pretty.pretty pitch)
            (show_pitch pitch)
        Util.pitched_note =<< Util.eval_note start note


-- * kotekan

data KotekanPattern = KotekanPattern {
    kotekan_above :: !([Maybe Pitch.Step], Pitch.Step)
    , kotekan_below :: !([Maybe Pitch.Step], Pitch.Step)
    } deriving (Show)

{- What about the high ding?

    I still want to do high ding automatically, but it depends on the speed.
    How about leave it out for now, and add it later.

    "44-34-3- 43-434-3" "-12-12-2 1-21-12(3)"
    Says to play the (3) if someone else isn't playing it, e.g. if it's
    being played on the top position.

    Or I could write both and filter out duplicate notes in favor of the
    lower position.  But that makes above and below have the same 'last',
    where the polos part should be considered closer.
-}

type Pattern = Map.Map Pitch.PitchClass [[Chord]]
type Chord = [Note]
type Note = (Pitch.Pitch, Score.Attributes)

reyong_kotkean_pattern :: KotekanPattern -> Pattern
reyong_kotkean_pattern = kotekan_pattern 5 (map pos_cek reyong_positions)

kotekan_pattern :: Pitch.PitchClass -> [Pitch.Pitch] -> KotekanPattern
    -> Pattern
kotekan_pattern per_octave centers pattern =
    Map.fromList $ map (\dest -> (dest, make dest)) [0 .. per_octave - 1]
    where
    make dest = map (map convert) $
        assign_positions pattern per_octave dest centers
    convert = maybe [] (\p -> [(p, mempty)])

assign_positions :: KotekanPattern -> Pitch.PitchClass -> Pitch.PitchClass
    -> [Pitch.Pitch] -> [[Maybe Pitch.Pitch]]
assign_positions (KotekanPattern above below) per_octave destination =
    map extract . assign_closest distance absolute
    where
    absolute = concatMap (\dest -> map (transpose dest) [below, above])
        [Pitch.Pitch oct (Pitch.Degree destination 0) | oct <- [0..]]
    transpose pitch (steps, last) =
        (map (fmap (add pitch)) steps, add pitch last)
    add pitch steps = Pitch.add_pc per_octave steps pitch
    extract ((steps, _), _) = steps
    distance (_, last) center =
        abs $ Pitch.subtract_pitch per_octave last center

-- | Find the value from the first list that is closest to the first element of
-- the first list, and then zip them up.
assign_closest :: Ord key => (a -> b -> key) -> [a] -> [b] -> [(a, b)]
assign_closest distance = go
    where
    go [] _ = []
    go _ [] = []
    go (x1 : xs@(x2:_)) (y:ys)
        | distance x1 y < distance x2 y = (x1, y) : zip xs ys
        | otherwise = go xs (y:ys)
    go [x] (y:_) = [(x, y)]

parse_kotekan :: [Char] -> [Char] -> KotekanPattern
parse_kotekan above below = KotekanPattern
    { kotekan_above = (map (fmap (subtract dest)) abovep, above_last - dest)
    , kotekan_below = (map (fmap (subtract dest)) belowp, below_last - dest)
    }
    where
    (abovep, belowp) = (parse_relative above, parse_relative below)
    Just above_last = msum (reverse abovep)
    Just below_last = msum (reverse belowp)
    Just dest = msum $ mapMaybe Seq.last [belowp, abovep]

parse_relative :: [Char] -> [Maybe Pitch.Step]
parse_relative = map parse1 . filter (/=' ')
    where
    parse1 '-' = Nothing
    parse1 c = Just (digit c)
    digit c = fromMaybe
        (error $ "Reyong.parse_kotekan: not a digit: " <> show c)
        (Num.read_digit c)

-- ** absolute

type NoteTable = Map.Map Char Chord

-- | Pentatonic pitch degree.
data Degree = I | O | E | U | A deriving (Eq, Ord, Enum, Show)
instance Pretty.Pretty Degree where pretty = show

to_pc :: Degree -> Pitch.PitchClass
to_pc = fromEnum

parse_absolute :: NoteTable -> [Char] -> [Chord]
parse_absolute table = map $ \c -> fromMaybe
    (error $ "parse_absolute: not in table: " ++ show c) (Map.lookup c table)

parse_note :: NoteTable -> Char -> Note
parse_note table = extract . head . parse_absolute table . (:[])
    where
    extract [x] = x
    extract xs = error $ "parse_note: expected only one: " <> pretty xs

norot_patterns :: Map.Map Pitch.PitchClass [[Chord]]
norot_patterns = Map.fromList $ zip [0..] $ map parse by_degree
    where
    parse = zipWith parse_absolute (map pos_table reyong_positions)
    by_degree =
        [ ["uau-", "oioi", "aua-", "oioi"] -- i
               -- i oeio
        , [":-:-", "eoeo", "iai-", "eoeo"] -- o
        , ["ueue", "ieie", "u-u-", "ueue"] -- e
        , ["auau", "eoe-", "auau", "eoe-"] -- u
        , ["eaea", "ieie", "-a-a", "ieie"] -- a
        ]

pickup_patterns :: Map.Map Pitch.PitchClass [[Chord]]
pickup_patterns = Map.fromList $ zip [0..] $ map parse by_degree
    where
    parse = zipWith parse_absolute (map pos_table reyong_positions)
    by_degree =
        [ ["uua-", "iioi", "uua-", "iioi"] -- i
        , ["ua:-", "ooeo", "uai-", "ooeo"] -- o
        , ["eeue", "eeie", "uau-", "eeue"] -- e
        , ["uuau", "ioe-", "uuau", "ioe-"] -- u
        , ["aaea", "eeie", "aa-a", "eeie"] -- a
        ]

note_table :: Pitch.Octave -> Degree -> NoteTable
note_table octave start = Map.fromList $
    ('-', []) : take (length notes) (drop (to_pc start) pitches)
    where
    pitches =
        [ (char, [(Pitch.Pitch oct (Pitch.Degree pc 0), mempty)])
        | oct <- [octave..], (pc, char) <- zip [0..] notes
        ]
    notes = "ioeua"

data Position = Position {
    pos_cek :: !Pitch.Pitch
    , pos_byong :: ![Pitch.Pitch]
    , pos_table :: !NoteTable
    } deriving (Eq, Show)

reyong_positions :: [Position]
reyong_positions = [position1, position2, position3, position4]

position1 :: Position
position1 = make_position ptable 'u' "ea"
    where
    ptable = Map.insert ':' (map (parse_note table) "ei") table
        where table = note_table 3 E

position2 :: Position
position2 = make_position (note_table 4 I) 'o' "ie"

position3 :: Position
position3 = make_position (note_table 4 U) 'a' "ui"

position4 :: Position
position4 = make_position (note_table 5 I) 'e' "ou"

make_position :: NoteTable -> Char -> [Char] -> Position
make_position table cek byong = Position
    { pos_cek = parse cek
    , pos_byong = map parse byong
    , pos_table = table
    }
    where parse = fst . parse_note table
