-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls for reyong and trompong techniques.
module Derive.Call.Bali.Reyong where
import qualified Data.List as List
import qualified Data.Map.Strict as Map

import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.Call as Call
import qualified Derive.Call.Bali.Gangsa as Gangsa
import qualified Derive.Call.Bali.Gender as Gender
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import Global
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

module_ :: Module.Module
module_ = "bali" <> "reyong"

note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.call_maps
    [ ("kilit", realize_pattern Gangsa.Repeat norot_patterns)
    , (">kilit", realize_pattern Gangsa.Once pickup_patterns)
    , ("k/_\\", realize_pattern Gangsa.Repeat k_12_1_21)
    , ("k//", realize_pattern Gangsa.Repeat k12_12_12)
    , ("k\\\\", realize_pattern Gangsa.Repeat k21_21_21)
    , ("k_\\", realize_pattern Gangsa.Once k_11_1_21)
    , ("k\\/", realize_pattern Gangsa.Once rejang)
    , ("/", articulation "cek-loose" ((:[]) . pos_cek)
        (Attrs.rim <> Attrs.loose))
    , ("X", articulation "cek" ((:[]) . pos_cek) Attrs.rim)
    , ("O", articulation "byong" pos_byong mempty)
    , ("+", articulation "byut" pos_byong Attrs.mute)
    , ("'", c_ngoret $ pure Nothing)
    , ("'n", c_ngoret $ Just <$> Gender.interval_arg)
    , ("'^", c_ngoret $ pure $ Just $ Pitch.Diatonic (-1))
    , ("'_", c_ngoret $ pure $ Just $ Pitch.Diatonic 1)
    ]
    [ ("reyong-damp", c_reyong_damp)
    , ("cancel-pasang", Derive.set_module module_ Gangsa.c_cancel_pasang)
    ]
    where articulation = make_articulation reyong_positions

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

c_ngoret :: Sig.Parser (Maybe Pitch.Transpose) -> Derive.Generator Derive.Note
c_ngoret = Gender.ngoret module_ False (pure (TrackLang.constant_control 1))

-- * kilitan

-- | Kilitan is implemented as a set of patterns indexed by an absolute pitch
-- degree.  The patterns are similar to kotekan, except with absolute pitches,
-- and without a polos \/ sangsih division.
realize_pattern :: Gangsa.Repeat -> Pattern -> Derive.Generator Derive.Note
realize_pattern repeat pattern =
    Derive.generator module_ "reyong" Tags.inst "Emit reyong kilitan."
    $ Sig.call Gangsa.dur_env $ \dur -> Sub.inverting $ \args -> do
        (parse_pitch, show_pitch, _) <- Call.get_pitch_functions
        pitch <- Call.get_parsed_pitch parse_pitch =<< Args.real_start args
        positions <- Derive.require ("no pattern for pitch: " <> pretty pitch)
            (Map.lookup (Pitch.pitch_pc pitch) pattern)
        mconcatMap (realize show_pitch (Args.range args) dur)
            (zip [1..] positions)
    where
    realize show_pitch (start, end) dur (voice, position) =
        Gangsa.realize_notes (realize_note show_pitch voice start) $
            Gangsa.realize_pattern repeat True start end dur (const position)

-- * articulation

make_articulation :: [Position] -> Text -> (Position -> [Pitch.Pitch])
    -> Score.Attributes -> Derive.Generator Derive.Note
make_articulation positions name get_notes attrs =
    Derive.generator module_ name Tags.inst "Reyong articulation."
    $ Sig.call0 $ Sub.inverting $ \args -> do
        (_, show_pitch, _) <- Call.get_pitch_functions
        mconcatMap (realize show_pitch args) (zip [1..] positions)
    where
    realize show_pitch args (voice, position) = mconcatMap
        (Call.place args . realize_note show_pitch voice (Args.start args))
        (map (\p -> (p, attrs)) (get_notes position))

type Voice = Int

realize_note :: (Pitch.Pitch -> Maybe Pitch.Note) -> Voice -> ScoreTime
    -> Note -> Derive.NoteDeriver
realize_note show_pitch voice start (pitch, attrs) =
    Call.add_attrs attrs $
    Derive.with_val EnvKey.voice voice $ do
        note <- Derive.require ("unshowable pitch: " <> pretty pitch)
            (show_pitch pitch)
        Call.pitched_note =<< Call.eval_note start note


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
instance Pretty.Pretty Degree where pretty = showt

to_pc :: Degree -> Pitch.PitchClass
to_pc = fromEnum

parse_absolute :: NoteTable -> [Char] -> [Chord]
parse_absolute table = map $ \c -> fromMaybe
    (error $ "parse_absolute: not in table: " ++ show c) (Map.lookup c table)

parse_note :: NoteTable -> Char -> Note
parse_note table = extract . head . parse_absolute table . (:[])
    where
    extract [x] = x
    extract xs = error $ "parse_note: expected only one: " <> prettys xs

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


-- * damping

c_reyong_damp :: Derive.Transformer Derive.Note
c_reyong_damp = Derive.transformer module_ "reyong-damp" Tags.postproc
    ("Add damping for reyong parts. The " <> ShowVal.doc_val damped
    <> " attribute will force a damp, while " <> ShowVal.doc_val undamped
    <> " will prevent damping. The latter can cause a previously undamped note\
    \ to become damped because the hand is now freed  up.")
    $ Sig.callt ((,,)
    <$> Sig.defaulted "duration" 0.15
        "This is how fast the player is able to damp. A note is only damped if\
        \ there is a hand available which has this much time to move into\
        \ position for the damp stroke, and then move into position for its\
        \ next note afterwards."
    <*> Sig.defaulted "attr" Attrs.mute
        "A damp stroke is a note of zero duration with this attribute."
    <*> Sig.defaulted "dyn" 0.75 "Scale dyn for damp strokes."
    ) $ \(dur, damp_attr, dyn) _args deriver -> do
        events <- deriver
        return events <> reyong_damp_voices dur damp_attr dyn events

damped :: Score.Attributes
damped = Score.attr "damped"

undamped :: Score.Attributes
undamped = Score.attr "undamped"

data Hand = L | R deriving (Eq, Show)
instance Pretty.Pretty Hand where pretty = showt

other :: Hand -> Hand
other L = R
other R = L

-- | Divide notes into voices.  Assign each note to a hand.  The end of each
-- note needs a free hand to damp.  That can be the same hand if the next note
-- with that hand is sufficiently far, or the opposite hand if it is not too
-- busy.
reyong_damp_voices :: RealTime -> Score.Attributes -> Signal.Y -> Derive.Events
    -> Derive.NoteDeriver
reyong_damp_voices dur damp_attr dyn =
    mconcatMap (reyong_damp damp_attr dyn dur)
        . Seq.group_sort event_voice . LEvent.events_of

-- | To damp, if either hand has enough time before and after then damp,
-- otherwise let it ring.
reyong_damp :: Score.Attributes -> Signal.Y -> RealTime -> [Score.Event]
    -> Derive.NoteDeriver
reyong_damp damp_attr dyn dur =
    mconcatMap (damped_note damp_attr dyn . snd) . filter fst
        . zip_on (can_damp dur)

damped_note :: Score.Attributes -> Signal.Y -> Score.Event -> Derive.NoteDeriver
damped_note attr dyn event =
    case Score.pitch_at (Score.event_start event) event of
        Nothing -> Derive.throw $ "no pitch on " <> pretty event
        Just pitch -> do
            end <- Derive.score (Score.event_end event)
            Call.add_attrs attr $ Call.multiply_dynamic dyn $
                Derive.place end 0 $ Call.pitched_note pitch

can_damp :: RealTime -> [Score.Event] -> [Bool]
can_damp dur = snd . List.mapAccumL damp (0, 0) . zip_next . assign_hands
    where
    damp prev ((hand, event), nexts) = (hands_state, can_damp)
        where
        can_damp = Score.has_attribute damped event
            || (not (Score.has_attribute undamped event)
                && (same_hand_can_damp || other_hand_can_damp))
        -- The same hand can damp if its next strike is sufficiently distant.
        same_hand_can_damp = enough_time (next hand)
        -- The other hand can damp if it has enough time from its previous
        -- strike to move, and the current hand has moved out of the way by
        -- changing pitches.
        -- TODO maybe doesn't need a full dur from prev strike?
        other_hand_can_damp = and
            [ (now - prev_strike (other hand)) >= dur
            , enough_time (next (other hand))
            , maybe True ((/= Score.initial_nn event) . Score.initial_nn . snd)
                (next hand)
            ]
        now = Score.event_end event
        prev_strike L = fst prev
        prev_strike R = snd prev
        hands_state
            | can_damp = case hand of
                L -> (now, snd prev)
                R -> (fst prev, now)
            | otherwise = prev
        next hand = Seq.head $ filter ((==hand) . fst) nexts
        enough_time = maybe True
            ((>=dur) . subtract now . Score.event_start . snd)

zip_next :: [a] -> [(a, [a])]
zip_next xs = zip xs (drop 1 (List.tails xs))

zip_on :: ([a] -> [b]) -> [a] -> [(b, a)]
zip_on f xs = zip (f xs) xs

-- | Assign hands based on the direction of the pitches.  This is a bit
-- simplistic but hopefully works well enough.
assign_hands :: [Score.Event] -> [(Hand, Score.Event)]
assign_hands =
    snd . List.mapAccumL assign (L, 999) . Seq.key_on_maybe Score.initial_nn
    where
    assign (prev_hand, prev_pitch) (pitch, event) =
        ((hand, pitch), (hand, event))
        where
        hand
            | pitch == prev_pitch = prev_hand
            | pitch > prev_pitch = R
            | otherwise = L

event_voice :: Score.Event -> Int
event_voice = fromMaybe 0 . Env.maybe_val EnvKey.voice . Score.event_environ
