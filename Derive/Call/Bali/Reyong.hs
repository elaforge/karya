-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls for reyong and trompong techniques.
--
-- >  /-------\/----\/-------\/----\--\
-- > 3e 3u 3a 4i 4o 4e 4u 4a 5i 5o 5e 5u
-- > 3  5  6  1  2  3  5  6  1  2  3  5
module Derive.Call.Bali.Reyong where
import qualified Data.List as List
import qualified Data.Map.Strict as Map

import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Ui.ScoreTime as ScoreTime
import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.Call as Call
import qualified Derive.Call.Bali.Gangsa as Gangsa
import qualified Derive.Call.Bali.Gender as Gender
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Post.Postproc as Postproc
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
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
    , ("k//", c_kotekan_regular (Just "-12-12-1") (Just Call.Down))
    , ("k\\\\", c_kotekan_regular (Just "-21-21-21") (Just Call.Up))
    , ("k_\\", realize_pattern Gangsa.Once $
        reyong_pattern "-44-43-4" "-11-1-21")
    , ("k//\\\\", realize_pattern Gangsa.Once $
        reyong_pattern "44-34-3- 43-434-3" "-12-12-2 1-21-12-")
    , ("k", c_kotekan_regular Nothing Nothing)
    , ("/", articulation "cek-loose" ((:[]) . pos_cek) (cek <> Attrs.open))
    , ("X", articulation "cek" ((:[]) . pos_cek) cek)
    , ("O", articulation "byong" pos_byong mempty)
    , ("-", articulation "byut-loose" pos_byong (Attrs.mute <> Attrs.open))
    , ("+", articulation "byut" pos_byong Attrs.mute)
    , ("'", c_ngoret $ pure Nothing)
    , ("'n", c_ngoret $ Just <$> Gender.interval_arg)
    , ("'^", c_ngoret $ pure $ Just $ Pitch.Diatonic (-1))
    , ("'_", c_ngoret $ pure $ Just $ Pitch.Diatonic 1)
    ]
    [ ("infer-damp", c_infer_damp)
    , ("cancel-kotekan", Derive.set_module module_ c_cancel_kotekan)
    ]
    where articulation = make_articulation reyong_positions

cek :: Score.Attributes
cek = Score.attr "cek"

reyong_pattern :: [Char] -> [Char] -> Pattern
reyong_pattern above below = make_pattern $ parse_kotekan above below

c_ngoret :: Sig.Parser (Maybe Pitch.Transpose) -> Derive.Generator Derive.Note
c_ngoret = Gender.ngoret module_ False (pure (TrackLang.constant_control 1))

voices_env :: Sig.Parser [Voice]
voices_env = Sig.environ "reyong-voices" Sig.Unprefixed []
    "Only emit notes for these positions, from 1 to 4. Empty means all of them."

-- * cancel

c_cancel_kotekan :: Derive.Transformer Derive.Note
c_cancel_kotekan = Derive.transformer module_ "cancel-kotekan" Tags.postproc
    "This is like the `cancel` call, except it understands flags set by\
    \ kotekan, and cancels based on reyong voice." $
    Postproc.make_cancel cancel Post.voice_key

-- | Same as 'Gangsa.cancel_pasang' except don't handle pasang.
cancel :: [Score.Event] -> Either Text [Score.Event]
cancel es = Postproc.cancel_strong_weak Postproc.infer_duration_merged $
    case Seq.partition2 (has Gangsa.final_flag) (has Gangsa.initial_flag) es of
        (_, _, normals@(_:_)) -> normals
        (finals@(_:_), _, []) -> finals
        ([], initials, []) -> initials
    where has = Score.has_flags

-- * kilitan

-- | Kilitan is implemented as a set of patterns indexed by an absolute pitch
-- degree.  The patterns are similar to kotekan, except with absolute pitches,
-- and without a polos \/ sangsih division.
realize_pattern :: Gangsa.Repeat -> Pattern -> Derive.Generator Derive.Note
realize_pattern repeat pattern =
    Derive.generator module_ "reyong" Tags.inst "Emit reyong kilitan."
    $ Sig.call ((,,)
    <$> Gangsa.dur_env <*> Gangsa.initial_final_env <*> voices_env)
    $ \(dur, initial_final, voices) -> Sub.inverting $ \args -> do
        (parse_pitch, show_pitch, _) <- Call.get_pitch_functions
        pitch <- Call.get_parsed_pitch parse_pitch =<< Args.real_start args
        positions <- Derive.require ("no pattern for pitch: " <> pretty pitch)
            (Map.lookup (Pitch.pitch_pc pitch) pattern)
        mconcatMap
            (realize_notes show_pitch repeat initial_final (Args.range args)
                dur)
            (filter_voices voices (zip [1..] positions))

filter_voices :: [Voice] -> [(Voice, a)] -> [(Voice, a)]
filter_voices voices
    | null voices = id
    | otherwise = filter ((`elem` voices) . fst)

-- * kotekan

c_kotekan_regular :: Maybe Text -> Maybe Call.UpDown
    -> Derive.Generator Derive.Note
c_kotekan_regular maybe_kernel maybe_dir =
    Derive.generator module_ "kotekan" Tags.inst
    ("Render a kotekan pattern from a kernel representing the polos.\
    \ The sangsih is inferred. This can emit notes at both the beginning and\
    \ end of the event, so use `cancel-kotekan` to cancel the extras.")
    $ Sig.call ((,,,,)
    <$> maybe (Sig.required "kernel" kernel_doc) pure maybe_kernel
    <*> maybe
        (Sig.defaulted "dir" Call.Up
            "Inferred part is above or below the explicit one.")
        pure maybe_dir
    <*> Gangsa.dur_env <*> Gangsa.initial_final_env <*> voices_env
    ) $ \(kernel_s, dir, dur, initial_final, voices) -> Sub.inverting $
    \args -> do
        kernel <- Derive.require_right id $ Gangsa.make_kernel (untxt kernel_s)
        (parse_pitch, show_pitch, _) <- Call.get_pitch_functions
        pitch <- Call.get_parsed_pitch parse_pitch =<< Args.real_start args
        pattern <- Derive.require "empty pattern" $ kernel_to_pattern dir kernel
        let positions = kotekan_pattern 5 (map pos_cek reyong_positions)
                pattern (Pitch.pitch_pc pitch)
        mconcatMap
            (realize_notes show_pitch Gangsa.Repeat initial_final
                (Args.range args) dur)
            (filter_voices voices (zip [1..] positions))

kernel_doc :: Text
kernel_doc = "Transposition steps for the part that ends on the destination\
    \ pitch. It should consist of `-`, `1`, and `2`. You can start with `k` to\
    \ avoid needing quotes. Starting with `k` will also require the length to\
    \ be a multiple of 4."

realize_notes :: (Pitch.Pitch -> Maybe Pitch.Note) -> Gangsa.Repeat
    -> (Bool, Bool) -> (ScoreTime, ScoreTime) -> ScoreTime -> (Voice, [[Note]])
    -> Derive.NoteDeriver
realize_notes show_pitch repeat (initial, final) (start, end) dur
        (voice, position) =
    Gangsa.realize_notes (realize_note show_pitch voice start) $
        modify_initial $ Gangsa.realize_pattern repeat final start end
            dur (const position)
    where
    modify_initial ns
        | initial = map (Gangsa.add_flag Gangsa.initial_flag) pre ++ post
        | otherwise = post
        where (pre, post) = span ((ScoreTime.<= start) . Gangsa.note_start) ns

kernel_to_pattern :: Call.UpDown -> Gangsa.Kernel -> Maybe KotekanPattern
kernel_to_pattern direction kernel = do
    -- Reyong doesn't really have polos and sangsih, so here polos is the
    -- one that has the destination.
    let polos = map to_steps kernel
        sangsih = map infer_sangsih kernel
    sangsih_last <- msum (reverse sangsih)
    polos_last <- msum (reverse polos)
    return $ case direction of
        Call.Up -> KotekanPattern
            { kotekan_above = (sangsih, sangsih_last)
            , kotekan_below = (polos, polos_last)
            }
        Call.Down -> KotekanPattern
            { kotekan_above = (polos, polos_last)
            , kotekan_below = (sangsih, sangsih_last)
            }
    where
    to_steps a = case a of
        Gangsa.Low -> Just 0
        Gangsa.High -> Just 1
        Gangsa.Rest -> Nothing
    infer_sangsih a = case direction of
        Call.Up -> case a of
            Gangsa.Low -> Just 3
            Gangsa.High -> Nothing
            Gangsa.Rest -> Just 2
        Call.Down -> case a of
            Gangsa.Low -> Nothing
            Gangsa.High -> Just (-2)
            Gangsa.Rest -> Just (-1)

-- * articulation

make_articulation :: [Position] -> Text -> (Position -> [Pitch.Pitch])
    -> Score.Attributes -> Derive.Generator Derive.Note
make_articulation positions name get_notes attrs =
    Derive.generator module_ name Tags.inst "Reyong articulation." $
    Sig.call voices_env $ \voices -> Sub.inverting $ \args -> do
        (_, show_pitch, _) <- Call.get_pitch_functions
        mconcatMap (realize show_pitch args)
            (filter_voices voices (zip [1..] positions))
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
    -- | Pair relative steps with the final note's distance from the
    -- destination pitch.  This is used to find this line's distance from
    -- a given position.
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

-- | Figure out a pattern for each note of the scale.
make_pattern :: KotekanPattern -> Pattern
make_pattern pattern = Map.fromList
    [ (pc, kotekan_pattern per_octave (map pos_cek reyong_positions) pattern pc)
    | pc <- pcs
    ]
    where
    per_octave = 5
    pcs = [0 .. per_octave - 1]

kotekan_pattern :: Pitch.PitchClass -> [Pitch.Pitch] -> KotekanPattern
    -> Pitch.PitchClass -> [[Chord]]
kotekan_pattern per_octave centers pattern pc =
    map (map convert) $ assign_positions pattern per_octave pc centers
    where convert = maybe [] (\p -> [(p, mempty)])

assign_positions :: KotekanPattern -> Pitch.PitchClass -> Pitch.PitchClass
    -> [Pitch.Pitch] -> [[Maybe Pitch.Pitch]]
assign_positions (KotekanPattern above below) per_octave destination =
    map extract . assign_closest distance absolute
    where
    -- Kotekan at theoretical positions in every octave.
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
        (Num.readDigit c)

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

c_infer_damp :: Derive.Transformer Derive.Note
c_infer_damp = Derive.transformer module_ "infer-damp" Tags.postproc
    ("Add damping for reyong parts. The " <> ShowVal.doc damped
    <> " attribute will force a damp, while " <> ShowVal.doc undamped
    <> " will prevent damping. The latter can cause a previously undamped note\
    \ to become damped because the hand is now freed  up.")
    $ Sig.callt ((,)
    <$> Sig.required "inst" "Apply damping to this instrument."
    <*> Sig.defaulted "dur" (Sig.control "damp-dur" 0.15)
        "This is how fast the player is able to damp. A note is only damped if\
        \ there is a hand available which has this much time to move into\
        \ position for the damp stroke, and then move into position for its\
        \ next note afterwards."
    ) $ \(inst, dur) _args deriver -> do
        dur <- Call.to_function dur
        -- TODO infer_damp can only add a %damp control, so it preserves order.
        -- Is there a way to express this statically?
        Post.apply (infer_damp_voices inst (RealTime.seconds . dur)) <$> deriver

damp_control :: Score.Control
damp_control = "damp"

-- | Divide notes into voices.  Assign each note to a hand.  The end of each
-- note needs a free hand to damp.  That can be the same hand if the next note
-- with that hand is sufficiently far, or the opposite hand if it is not too
-- busy.
infer_damp_voices :: Score.Instrument -> (RealTime -> RealTime) -> [Score.Event]
    -> [Score.Event]
infer_damp_voices reyong_inst dur_at =
    Seq.merge_lists Score.event_start . map infer1
        . Seq.keyed_group_sort Post.voice_key
    where
    infer1 ((inst, _voice), events)
        | inst /= reyong_inst = events
        | otherwise = zipWith set_damp damps events
        where damps = infer_damp dur_at events

set_damp :: Signal.Y -> Score.Event -> Score.Event
set_damp damp = Score.merge_control damp_control merge
    where
    merge old
        | old == Signal.empty = new
        | otherwise = Signal.sig_multiply old new
    new = Signal.constant damp

infer_damp :: (RealTime -> RealTime) -> [Score.Event] -> [Signal.Y]
infer_damp dur_at = snd . List.mapAccumL infer (0, 0) . zip_next . assign_hands
    where
    -- TODO also infer damp level
    infer prev ((hand, event), nexts) = (hands_state, if can_damp then 1 else 0)
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
        dur = dur_at (Score.event_start event)

damped :: Score.Attributes
damped = Score.attr "damped"

undamped :: Score.Attributes
undamped = Score.attr "undamped"

data Hand = L | R deriving (Eq, Show)
instance Pretty.Pretty Hand where pretty = showt

other :: Hand -> Hand
other L = R
other R = L

zip_next :: [a] -> [(a, [a])]
zip_next xs = zip xs (drop 1 (List.tails xs))

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
