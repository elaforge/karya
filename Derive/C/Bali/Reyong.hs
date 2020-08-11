-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls for reyong and trompong techniques.
--
-- >  /-------\/----\/-------\/----\--\
-- > 4e 4u 4a 5i 5o 5e 5u 5a 6i 6o 6e 6u
-- > 3  5  6  1  2  3  5  6  1  2  3  5
module Derive.C.Bali.Reyong where
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified Util.Doc as Doc
import qualified Util.Log as Log
import qualified Util.Num as Num
import qualified Util.Seq as Seq

import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.C.Bali.Gangsa as Gangsa
import qualified Derive.C.Bali.Gender as Gender
import qualified Derive.C.Post.Postproc as Postproc
import qualified Derive.Call as Call
import qualified Derive.Call.GraceUtil as GraceUtil
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.StaticMacro as StaticMacro
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Flags as Flags
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import qualified Ui.Types as Types

import           Global
import           Types


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

library :: Library.Library
library = mconcat
    [ Library.generators
        [ ("kilit", realize_pattern norot_patterns)
        , (">kilit", realize_pattern norot_prepare_patterns)
        -- kilit is probably redundant now that I have norot.
        , ("norot", c_norot Nothing)
        , ("nt", c_norot Nothing)
        , ("nt>", c_norot (Just True))
        , ("nt-", c_norot (Just False))
        , ("k//", c_kotekan_regular (Just "-12-12-1") (Just Call.Down))
        , ("k\\\\", c_kotekan_regular (Just "-21-21-21") (Just Call.Up))
        , ("k_\\", realize_pattern $ reyong_pattern "-44-43-4" "-11-1-21")
        , ("k//\\\\", realize_pattern $
            reyong_pattern "-4-34-3-43-434-3" "-12-12-21-21-12-")
        , ("k", c_kotekan_regular Nothing Nothing)
        , ("t", c_tumpuk)
        , ("a", c_tumpuk_auto)
        , ("o", c_byong)
        , (":", c_pitches [Pitch.pitch 4 2, Pitch.pitch 5 0])
        , ("/", articulation False "cek-loose"
            ((:[]) . pos_cek) (cek <> Attrs.open))
        , ("//", articulation True "cek-loose"
            ((:[]) . pos_cek) (cek <> Attrs.open))
        , ("X", articulation False "cek" ((:[]) . pos_cek) cek)
        , ("XX", articulation True "cek" ((:[]) . pos_cek) cek)
        , ("O", articulation False "byong" pos_byong mempty)
        , ("-", articulation False "byut-loose"
            pos_byong (Attrs.mute <> Attrs.open))
        , ("+", articulation False "byut" pos_byong Attrs.mute)

        , ("n1", c_solkattu_note [0])
        , ("n2", c_solkattu_note [1])
        , ("n3", c_solkattu_note [2])
        , ("n4", c_solkattu_note [3])
        , ("n14", c_solkattu_note [0, 3])
        ]
    , Library.generators $ Gender.ngoret_variations c_ngoret
    , Library.transformers
        [ ("infer-damp", c_infer_damp)
        , ("hand-damp", c_hand_damp)
        , ("cancel-kotekan", c_cancel_kotekan)
        , ("realize-ngoret", Derive.set_module module_ Gender.c_realize_ngoret)
        , ("realize-reyong", c_realize_reyong)
        , ("realize-trompong", c_realize_trompong)
        , ("vv", c_lower_octave_note)
        ]
    , Library.both
        [ ("lv", Make.attributed_note module_ undamped)
        , ("upper", c_upper)
        ]
    ]
    where articulation = make_articulation reyong_positions

cek :: Attrs.Attributes
cek = Attrs.attr "cek"

reyong_pattern :: [Char] -> [Char] -> Pattern
reyong_pattern above below = make_pattern $ parse_kotekan above below

c_ngoret :: Sig.Parser (Maybe Pitch.Transpose) -> Derive.Generator Derive.Note
c_ngoret = Gender.ngoret module_ False (pure (DeriveT.constant_control 0))

voices_env :: Sig.Parser [Voice]
voices_env = Sig.environ "voices" Sig.Both []
    "Only emit notes for these positions, from 1 to 4. Empty means all of them."

-- * tumpuk

c_tumpuk :: Derive.Generator Derive.Note
c_tumpuk = Derive.generator module_ "tumpuk" Tags.inst "Pile up notes together."
    $ Sig.call ((,,)
    <$> Sig.required "notes"
        ("Articulations, from " <> Doc.literal (txt (Map.keys articulations))
        <> ", pitches from `edcba0123456789`, or a space for a rest.")
    <*> Sig.defaulted "dur" 0.1 "Duration of each note."
    <*> place_env
    ) $ \(notes, dur, place) -> Sub.inverting $ \args -> do
        notes <- Derive.require_right id $ parse_tumpuk (untxt notes)
        tumpuk args place dur notes

tumpuk :: Derive.PassedArgs Score.Event -> DeriveT.ControlRef -> RealTime
    -> [TumpukNote] -> Derive.NoteDeriver
tumpuk args place dur notes = do
    (start, end) <- Args.real_range args
    prev <- traverse Derive.real $ Args.prev_start args
    place <- Call.control_at place start
    pitch <- Call.get_pitch start
    realize_tumpuk prev start end place (Args.prev_event_pitch args) pitch dur
        notes

place_env :: Sig.Parser DeriveT.ControlRef
place_env = Sig.environ "place" Sig.Both (Sig.control "place" 1)
    "At 0, grace notes fall before their base note.  At 1, grace notes fall on\
    \ the base note, and the base note is delayed."

-- | Dyn 0 means a rest.
type TumpukNote = (TumpukPitch, Attrs.Attributes, Dyn)
data TumpukPitch = Transpose Pitch.Step | Prev deriving (Eq, Show)
type Dyn = Signal.Y

realize_tumpuk :: Maybe RealTime -> RealTime -> RealTime -> Double
    -> Maybe PSignal.Pitch -> PSignal.Pitch -> RealTime -> [TumpukNote]
    -> Derive.NoteDeriver
realize_tumpuk prev event_start event_end place prev_pitch event_pitch dur
        notes =
    mconcatMap realize $ filter ((>0) . note_dyn . fst) $ zip notes extents
    where
    extents = GraceUtil.fit_grace_durs (RealTime.seconds place)
        prev event_start event_end (length notes) dur
    note_dyn (_, _, dyn) = dyn
    realize ((tpitch, attrs, dyn), (real_start, dur)) = do
        start <- Derive.score real_start
        end <- Derive.score (real_start + dur)
        pitch <- case tpitch of
            Transpose steps -> return $ Pitches.transpose_d steps event_pitch
            Prev -> Derive.require "no prev pitch" prev_pitch
        Derive.place start (end-start) $ Call.multiply_dynamic dyn $
            Call.add_attributes attrs $ Call.pitched_note pitch

parse_tumpuk :: [Char] -> Either Text [TumpukNote]
parse_tumpuk = fmap (Maybe.catMaybes . snd) . Seq.mapAccumLM parse (Transpose 0)
    where
    parse pitch c = case Map.lookup c articulations of
        Just (attrs, dyn) -> Right (pitch, Just (pitch, attrs, dyn))
        Nothing -> case Num.readDigit c <|> lookup c negative of
            Just steps -> Right (Transpose steps, Nothing)
            Nothing
                | c == 'p' -> Right (Prev, Nothing)
                | otherwise -> Left $ "unknown articulation: " <> showt c
    negative = [('a', -1), ('b', -2), ('c', -3), ('d', -4), ('e', -5)]

-- | These more or less correspond to the group articulations:
--
-- > /   X   -   +   o
-- >         m-  nx  .o
articulations :: Map Char (Attrs.Attributes, Dyn)
articulations = Map.fromList
    [ (' ', (mempty, 0))
    , ('.', (mempty, 0.75))
    , ('o', (mempty, 1))
    , ('m', (Attrs.mute <> Attrs.open, 0.75))
    , ('-', (Attrs.mute <> Attrs.open, 1))
    , ('n', (Attrs.mute, 0.75))
    -- I would use +, but if you start with one it parses as an attr.
    , ('x', (Attrs.mute, 1))
    ]

c_tumpuk_auto :: Derive.Generator Derive.Note
c_tumpuk_auto = Derive.generator module_ "tumpuk-auto" Tags.inst
    "A variant of `tumpuk` that randomly picks a pattern."
    $ Sig.call place_env $ \place -> Sub.inverting $ \args -> do
        randoms <- Call.randoms
        let rnd1 : rnd2 : _ = randoms -- randoms is infinite
        event_dur <- Args.real_duration args
        (notes, dur) <- Derive.require_right id $
            select_pattern event_dur rnd1 rnd2
        tumpuk args place dur notes

-- | If dur is long, use the slow end of the patterns.  If dur is short, try to
-- fit the pattern in the first 2/3, dropping patterns if they exceed that at
-- their fastest.
select_pattern :: RealTime -> Double -> Double
    -> Either Text ([TumpukNote], RealTime)
select_pattern dur rnd1 rnd2 = justErr err $ select fits_well <|> select fits
    where
    select ps = case NonEmpty.nonEmpty ps of
        Nothing -> Nothing
        Just ps -> Just $ pick_dur (Call.pick ps rnd1)
    pick_dur (p, (slow, fast)) = (p, dur)
        where
        dur = Num.scale (Num.clamp slow fast min_slow) fast
            (RealTime.seconds rnd2)
        min_slow = desired_dur / fromIntegral (length p)
    err = "no patterns fit duration " <> pretty dur
    -- Try to take up only this much dur with tumpuk notes, but take up to
    -- the full duration if nothing fits.
    desired_dur = dur * (2/3)
    fits_well = filter ((<= desired_dur) . pattern_dur) tumpuk_patterns
    fits = filter ((<= dur) . pattern_dur) tumpuk_patterns
    pattern_dur (p, (_, fast)) = fromIntegral (length p) * fast

-- | Pattern and usable time range.
-- TODO probably I can vary -=, nx, and .o, or just randomize the dyn for
-- non-final notes.
tumpuk_patterns :: [([TumpukNote], (RealTime, RealTime))]
tumpuk_patterns = expect_right $ mapM (firstA parse_tumpuk)
    [ ("p-0o", (1/10, 1/14))
    , ("p-0.o", (1/12, 1/18))
    , ("p.0.o", (1/14, 1/18))
    , ("1.0.o", (1/12, 1/18))
    , ("p.0.pm0o", (1/18, 1/20))
    ]
    where
    expect_right = either (errorStack . ("tumpuk_patterns: "<>)) id
    firstA f (a, c) = (,) <$> f a <*> pure c

-- * c_byong

c_byong :: Derive.Generator Derive.Note
c_byong = Derive.generator module_ "byong" Tags.inst
    "Play the byong notes, but only for the current voice, and following\
    \ normal damping rules."
    $ Sig.call0 $ Sub.inverting $ \args -> do
        voice <- Derive.get_val EnvKey.voice
        position <- Derive.require ("unknown position: " <> showt voice) $
            Seq.at reyong_positions (voice - 1 :: Int)
        (_, show_pitch, _) <- Call.get_pitch_functions
        let realize = Call.place args
                . realize_note show_pitch voice (Args.start args)
        mconcatMap (realize . (, mempty)) (pos_byong position)

c_pitches :: [Pitch.Pitch] -> Derive.Generator Derive.Note
c_pitches pitches = Derive.generator module_ "pitches" Tags.inst
    ("Play notes for each pitch: " <> ShowVal.doc pitches
        <> ". Really only for `4e` and `5i` for the penyorog.")
    $ Sig.call0 $ Sub.inverting $ \args ->
        mconcatMap (Call.place args . realize (Args.start args)) pitches
    where
    realize start pitch =
        Call.transposed_pitched_note =<< Call.eval_pitch_ start pitch

-- * cancel

c_cancel_kotekan :: Derive.Transformer Derive.Note
c_cancel_kotekan = Derive.transformer module_ "cancel-kotekan" Tags.postproc
    "This is like the `cancel` call, except it understands flags set by\
    \ kotekan, and cancels based on reyong voice." $
    Postproc.make_cancel Gangsa.cancel_strong_final Post.voice_key

-- * kilitan

c_norot :: Maybe Bool -> Derive.Generator Derive.Note
c_norot default_prepare =
    Derive.generator module_ "norot" Tags.inst "Reyong norot."
    $ Sig.call ((,,,)
    <$> Sig.defaulted "prepare" default_prepare
        "Whether or not to prepare for the next pitch. If Nothing, infer based\
        \ on the next note."
    <*> Gangsa.dur_env <*> Gangsa.infer_initial_final_env <*> voices_env
    ) $ \(prepare, note_dur, initial_final, voices) -> Sub.inverting $
    \args -> do
        (pitch, show_pitch) <- get_parsed_pitch args
        next_pitch <- infer_prepare args prepare
        let orientation = Args.orientation args
        let (sustain_params, prepare_params) = Gangsa.prepare_sustain
                (Maybe.isJust next_pitch) note_dur initial_final
                orientation (Args.range args)
        sustain <- case sustain_params of
            Nothing -> return mempty
            Just (initial_final, range) ->
                realize_positions
                    (realize_notes range orientation initial_final
                        show_pitch Gangsa.Repeat note_dur)
                    voices norot_patterns pitch
        prepare <- case (,) <$> next_pitch <*> prepare_params of
            Nothing -> return Nothing
            Just (next, (initial_final, range)) ->
                Just <$> realize_positions
                    (realize_notes range orientation initial_final
                        show_pitch Gangsa.Once note_dur)
                    voices norot_prepare_patterns next
        maybe sustain (sustain<>) prepare

realize_positions :: ((Voice, note) -> Derive.NoteDeriver)
    -> [Voice] -> Map Pitch.PitchClass [note]
    -> Pitch.Pitch -> Derive.Deriver Derive.NoteDeriver
realize_positions realize voices patterns pitch = do
    positions <- Derive.require
        ("no pattern for pitch: " <> pretty pitch)
        (Map.lookup (Pitch.pitch_pc pitch) patterns)
    return $ mconcatMap realize (filter_voices voices positions)

-- | Kilitan is implemented as a set of patterns indexed by an absolute pitch
-- degree.  The patterns are similar to kotekan, except with absolute pitches,
-- and without a polos \/ sangsih division.
realize_pattern :: Pattern -> Derive.Generator Derive.Note
realize_pattern pattern =
    Derive.generator module_ "reyong" Tags.inst "Emit reyong pattern."
    $ Sig.call ((,,)
    <$> Gangsa.dur_env <*> Gangsa.infer_initial_final_env <*> voices_env)
    $ \(dur, initial_final, voices) -> Sub.inverting $ \args -> do
        (pitch, show_pitch) <- get_parsed_pitch args
        positions <- Derive.require ("no pattern for pitch: " <> pretty pitch)
            (Map.lookup (Pitch.pitch_pc pitch) pattern)
        mconcatMap
            (realize_notes_args args initial_final show_pitch Gangsa.Repeat dur)
            (filter_voices voices positions)

-- | Select the given Voice indices.
filter_voices :: [Voice] -> [a] -> [(Voice, a)]
filter_voices voices positions
    | null voices = zip [1..] positions
    | otherwise = filter ((`elem` voices) . fst) (zip [1..] positions)

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
    <*> Gangsa.dur_env <*> Gangsa.infer_initial_final_env <*> voices_env
    ) $ \(kernel_s, dir, dur, initial_final, voices) -> Sub.inverting $
    \args -> do
        kernel <- Derive.require_right id $ Gangsa.make_kernel (untxt kernel_s)
        (pitch, show_pitch) <- get_parsed_pitch args
        pattern <- Derive.require "empty pattern" $ kernel_to_pattern dir kernel
        let positions = kotekan_pattern 5 (map pos_cek reyong_positions)
                pattern (Pitch.pitch_pc pitch)
        mconcatMap
            (realize_notes_args args initial_final show_pitch Gangsa.Repeat dur)
            (filter_voices voices positions)

kernel_doc :: Doc.Doc
kernel_doc = "Transposition steps for the part that ends on the destination\
    \ pitch. It should consist of `-`, `1`, and `2`. You can start with `k` to\
    \ avoid needing quotes. Starting with `k` will also require the length to\
    \ be a multiple of 4."

realize_notes_args :: Derive.PassedArgs a -> (Maybe Bool, Bool)
    -> (Pitch.Pitch -> Maybe Pitch.Note) -> Gangsa.Repeat -> ScoreTime
    -> (Voice, [[Note]]) -> Derive.NoteDeriver
realize_notes_args args initial_final =
    realize_notes (Args.range args) (Args.orientation args)
        (Gangsa.infer_initial args initial_final)

realize_notes :: (ScoreTime, ScoreTime) -> Types.Orientation -> (Bool, Bool)
    -> (Pitch.Pitch -> Maybe Pitch.Note) -> Gangsa.Repeat -> ScoreTime
    -> (Voice, [Chord]) -> Derive.NoteDeriver
realize_notes (start, end) orientation initial_final show_pitch repeat dur
        (voice, position) =
    Gangsa.realize_notes (realize_note show_pitch voice start) $
        Gangsa.realize_pattern repeat orientation initial_final start end dur
            (const position)

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
        Gangsa.Gap -> Nothing
        Gangsa.Rest -> Nothing
        Gangsa.Low -> Just 0
        Gangsa.High -> Just 1
    infer_sangsih a = case direction of
        Call.Up -> case a of
            Gangsa.Gap -> Nothing
            Gangsa.Rest -> Just 2
            Gangsa.Low -> Just 3
            Gangsa.High -> Nothing
        Call.Down -> case a of
            Gangsa.Gap -> Nothing
            Gangsa.Rest -> Just (-1)
            Gangsa.Low -> Nothing
            Gangsa.High -> Just (-2)

-- | Like 'Gangsa.get_pitch', but get the symbolic pitch.
get_parsed_pitch :: Derive.PassedArgs a
    -> Derive.Deriver (Pitch.Pitch, Pitch.Pitch -> Maybe Pitch.Note)
get_parsed_pitch args = do
    (parse_pitch, show_pitch, _) <- Call.get_pitch_functions
    pitch <- Call.get_parsed_pitch parse_pitch =<< Args.real_start args
    return (pitch, show_pitch)

-- | Like 'Gangsa.infer_prepare', except return a parsed Pitch.Pitch.
infer_prepare :: Derive.PassedArgs a -> Maybe Bool
    -> Derive.Deriver (Maybe Pitch.Pitch)
infer_prepare args prepare = do
    (parse_pitch, _, _) <- Call.get_pitch_functions
    justm (Gangsa.infer_prepare args prepare) $ \_ ->
        maybe (return Nothing) (Args.lookup_parsed_pitch_at parse_pitch)
            (Args.next_start args)

-- * articulation

make_articulation :: [Position] -> Bool -> Derive.CallName
    -> (Position -> [Pitch.Pitch]) -> Attrs.Attributes
    -> Derive.Generator Derive.Note
make_articulation positions double name get_notes attrs =
    Derive.generator module_ name Tags.inst
    "Reyong articulation. The doubled variants emit two notes, and rely on\
    \ start time randomization so they're not exactly simultaneous." $
    Sig.call voices_env $ \voices -> Sub.inverting $ \args -> do
        (_, show_pitch, _) <- Call.get_pitch_functions
        mconcat $ concat $ replicate (if double then 2 else 1) $
            map (realize show_pitch args) (filter_voices voices positions)
    where
    realize show_pitch args (voice, position) = mconcatMap
        (Call.place args . realize_note show_pitch voice (Args.start args))
        (map (\p -> (p, attrs)) (get_notes position))

type Voice = Int

realize_note :: (Pitch.Pitch -> Maybe Pitch.Note) -> Voice -> ScoreTime
    -> Note -> Derive.NoteDeriver
realize_note show_pitch voice start (pitch, attrs) =
    -- TODO I could maybe get rid of all the show_pitch args by using
    -- Call.eval_pitch_, but would that be inefficient?
    Call.add_attributes attrs $ Derive.with_val EnvKey.voice voice $
        Call.transposed_pitched_note =<< Call.eval_pitch show_pitch start pitch


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

type Pattern = Map Pitch.PitchClass [[Chord]]
type Chord = [Note]
type Note = (Pitch.Pitch, Attrs.Attributes)

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
    distance (_, last) center = abs $ Pitch.diff_pc per_octave last center

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
        (errorStack $ "Reyong.parse_kotekan: not a digit: " <> showt c)
        (Num.readDigit c)

-- ** absolute

-- | Map a char to the notes it represents for a certain position.  Each
-- position has different set of notes available.  This is just to interpret a
-- mini-notation for each position.
type NoteTable = Map Char Chord

-- | Saih lima pitch degree.
data Degree = I | O | E | U | A deriving (Eq, Ord, Enum, Show)
instance Pretty Degree where pretty = showt

to_pc :: Degree -> Pitch.PitchClass
to_pc = fromEnum

parse_absolute :: NoteTable -> [Char] -> [Chord]
parse_absolute table = map $ \c -> fromMaybe
    (errorStack $ "parse_absolute: not in table: " <> showt c)
    (Map.lookup c table)

parse_note :: NoteTable -> Char -> Note
parse_note table = extract . head . parse_absolute table . (:[])
    where
    extract [x] = x
    extract xs = errorStack $ "parse_note: expected only one: " <> pretty xs

{-
    1 penyorog        3 ponggang
             2 pengenter    4 pemetit
    |------|---       |---|---
             |------|---    |---------|
    4e 4u 4a 5i 5o 5e 5u 5a 6i 6o 6e 6u 6a 7i
-}
norot_prepare_patterns :: Map Pitch.PitchClass [[Chord]]
norot_prepare_patterns = Map.fromList $ zip [0..] $ map parse by_degree
    where
    parse = zipWith parse_absolute (map pos_table reyong_positions)
    by_degree =
        -- 1       2       3       4
        [ ["eua-", "Iioi", "Uua-", "Iioi"] -- i
        , ["ua:-", "Ooeo", "uai-", "Ooeo"] -- o
        , ["Eeue", "Eeie", "uau-", "Eeue"] -- e
        , ["Uuau", "ioe-", "Uuau", "ioeu"] -- u
        , ["Aaea", "Eeie", "Aa-a", "Eeie"] -- a
        ]

norot_patterns :: Map Pitch.PitchClass [[Chord]]
norot_patterns = Map.fromList $ zip [0..] $ map parse by_degree
    where
    parse = zipWith parse_absolute (map pos_table reyong_positions)
    by_degree =
        -- 1       2       3       4
        [ ["aua-", "oioi", "aua-", "oioi"] -- i
               -- i oeio
        , [":-:-", "eoeo", "iai-", "eoeo"] -- o
        , ["ueue", "ieie", "u-u-", "ueue"] -- e
        , ["auau", "eoe-", "auau", "eueu"] -- u
        , ["eaea", "ieie", "-a-a", "ieie"] -- a
        ]

-- | Map letters to chords, starting from the given octave and Degree.  Capital
-- letters get 'Attrs.mute'.
note_table :: Pitch.Octave -> Degree -> NoteTable
note_table octave start = Map.fromList $
    ('-', []) : take (length notes) (drop (to_pc start) pitches)
        ++ take (length notes) muted
    where
    pitches =
        [ (char, [(Pitch.Pitch oct (Pitch.Degree pc 0), mempty)])
        | oct <- [octave..], (pc, char) <- zip [0..] notes
        ]
    muted = [(Char.toUpper char, mute notes) | (char, notes) <- pitches]
    mute = map (fmap (<>Attrs.mute))
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
        where table = note_table 4 E

position2 :: Position
position2 = make_position (note_table 5 I) 'o' "ie"

position3 :: Position
position3 = make_position (note_table 5 U) 'a' "ui"

position4 :: Position
position4 = make_position (note_table 6 I) 'e' "ou"

make_position :: NoteTable -> Char -> [Char] -> Position
make_position table cek byong = Position
    { pos_cek = parse cek
    , pos_byong = map parse byong
    , pos_table = table
    }
    where parse = fst . parse_note table

-- * damping

c_hand_damp :: Derive.Transformer Derive.Note
c_hand_damp = Derive.transformer module_ "hand-damp" Tags.postproc
    "Damping when the parts are already divided by hand."
    $ Sig.callt ((,)
        <$> Sig.required "insts" "Apply damping to these instruments."
        <*> Sig.defaulted "dur" (Sig.control "damp-dur" 0.15)
            "There must be at least this much time between the end of a note\
            \ and the start of the next to damp."
    ) $ \(insts, dur) _args deriver -> do
        dur <- Call.to_function dur
        hand_damp (Set.fromList insts) (RealTime.seconds . dur) <$> deriver

hand_damp :: Set ScoreT.Instrument -> (RealTime -> RealTime)
    -> Stream.Stream Score.Event -> Stream.Stream Score.Event
hand_damp damped_insts dur_at =
    Post.emap_asc_ infer_damp . Post.next_by Post.hand_key
    where
    infer_damp (event, _)
        | Score.event_instrument event `Set.notMember` damped_insts = [event]
    infer_damp (event, Just next) | too_close event next = [event]
    infer_damp (event, _) = damp event
    damp event = [event, make_damp 0 1 event]
    too_close event next =
        (Score.event_start next - Score.event_end event)
            <= dur_at (Score.event_end event)

c_infer_damp :: Derive.Transformer Derive.Note
c_infer_damp = Derive.transformer module_ "infer-damp" Tags.postproc
    ("Add damping for reyong parts based on a simulation of the hand technique.\
    \ The " <> ShowVal.doc damped <> " attribute will force a damp, while "
    <> ShowVal.doc undamped <> " will prevent damping. The latter can cause a\
    \ previously undamped note to become damped because the hand is now freed\
    \ up.\
    \\nThe output is additional notes with `+mute` and zero duration at note\
    \ end.")
    $ Sig.callt ((,,)
        <$> Sig.required "insts" "Apply damping to these instruments."
        <*> Sig.defaulted "dur" (Sig.control "damp-dur" 0.15)
            "This is how fast the player is able to damp. A note is only damped\
            \ if there is a hand available which has this much time to move\
            \ into position for the damp stroke, and then move into position\
            \ for its next note afterwards."
        <*> Sig.defaulted "early" (Sig.control "early" 0.025)
            "Damp this much before the next note, if it would be simultaneous\
            \ with the next start."
    ) $ \(insts, dur, early) _args deriver -> do
        dur <- Call.to_function dur
        early <- Call.to_function early
        -- infer_damp preserves order, so Post.apply is safe.  TODO Is there a
        -- way to express this statically?
        Post.apply_m (Derive.run_logs
            . infer_damp_voices (Set.fromList insts) (RealTime.seconds . dur)
                    (RealTime.seconds . early))
                =<< deriver

-- | Multiply this by 'Controls.dynamic' for the dynamic of +mute notes created
-- by infer-damp.
damp_control :: ScoreT.Control
damp_control = "damp"

-- | Divide notes into voices.  Assign each note to a hand.  The end of each
-- note needs a free hand to damp.  That can be the same hand if the next note
-- with that hand is a sufficiently long time from now, or the opposite hand if
-- it is not too busy.
infer_damp_voices :: Set ScoreT.Instrument
    -> (RealTime -> RealTime) -- ^ duration required to damp
    -> (RealTime -> RealTime) -> [Score.Event] -> Log.LogId [Score.Event]
infer_damp_voices damped_insts dur_at early_at events = do
    unless (null skipped) $
        Log.warn $ "skipped events without pitch: "
            <> Score.short_events skipped
    return damped
    where
    (damped, skipped) = bimap (Seq.merge_lists Score.event_start) concat
        . unzip . map infer1 . Seq.keyed_group_sort Post.voice_key $ events
    infer1 ((inst, _voice), events)
        | inst `Set.notMember` damped_insts = (events, [])
        | otherwise = (,skipped) $ Seq.merge_on Score.event_start events $ do
            (Just damp, (event, next)) <- zip damps (Seq.zip_next events)
            -- Only apply early_at if this damp would be simultaneous with the
            -- next one.
            let early = case next of
                    Just n | Score.event_end event >= Score.event_start n ->
                        early_at (Score.event_start event)
                    _ -> 0
            return $ make_damp early damp event
        where
        (damps, skipped) = infer_damp dur_at events

-- | Create a damped note at the end of the given note.
make_damp :: RealTime -> Signal.Y -> Score.Event -> Score.Event
make_damp early damp_dyn event =
    Score.add_attributes Attrs.mute $ Score.set_dynamic damp $ event
        { Score.event_start = Score.event_end event - early
        , Score.event_duration = 0
        }
    where
    damp = damp_dyn * maybe 1 ScoreT.typed_val
        (Score.control_at (Score.event_end event) damp_control event)

infer_damp :: (RealTime -> RealTime) -> [Score.Event]
    -> ([Maybe Signal.Y], [Score.Event])
    -- ^ dump level for each event, or Nothing if undamped
infer_damp dur_at =
    first (snd . List.mapAccumL infer (0, 0) . Seq.zip_nexts) . assign_hands
    where
    -- TODO also infer damp level
    infer prev ((hand, event), nexts) =
        (hands_state, if damp then Just 1 else Nothing)
        where
        damp = Score.has_attribute damped event
            || (could_damp event
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
            | damp = case hand of
                L -> (now, snd prev)
                R -> (fst prev, now)
            | otherwise = prev
        next hand = Seq.head $ filter ((==hand) . fst) nexts
        enough_time = maybe True
            ((>=dur) . subtract now . Score.event_start . snd)
        dur = dur_at (Score.event_start event)

-- | True for events which could get an inferred damp.
could_damp :: Score.Event -> Bool
could_damp event =
    Score.event_duration event > 0
        && not (any (Attrs.contain attrs) [undamped, cek, Attrs.mute])
    where attrs = Score.event_attributes event

damped :: Attrs.Attributes
damped = Attrs.attr "damped"

undamped :: Attrs.Attributes
undamped = Attrs.attr "undamped"

data Hand = L | R deriving (Eq, Show)
instance Pretty Hand where pretty = showt

other :: Hand -> Hand
other L = R
other R = L

-- | Assign hands based on the direction of the pitches.  This is a bit
-- simplistic but hopefully works well enough.
assign_hands :: [Score.Event] -> ([(Hand, Score.Event)], [Score.Event])
assign_hands =
    first (snd . List.mapAccumL assign (L, 999))
        . Seq.partition_on (\e -> (,e) <$> Score.initial_nn e)
    where
    assign (prev_hand, prev_pitch) (pitch, event) =
        ((hand, pitch), (hand, event))
        where
        hand
            | pitch == prev_pitch = prev_hand
            | pitch > prev_pitch = R
            | otherwise = L

-- * patterns

baris :: [([Char], [Char])]
baris =
    [ ( "-e-oe-eo-eo-oe-oeo-eo-oe-oe-eo-e"
      , "-ai-aia-ia-i-ai-a-ia-i-ai-aia-ia"
      )
    ]

-- * realize-reyong

c_realize_reyong :: Derive.Transformer Derive.Note
c_realize_reyong = StaticMacro.check "c_realize_reyong" $
    StaticMacro.transformer module_ "realize-reyong" Tags.postproc doc
        -- infer-damp relies on having the ngoret pitches and will get confused
        -- by uncalled notes, so it has to go after those.
        [ StaticMacro.Call c_infer_damp [StaticMacro.Var, StaticMacro.Var]
        , StaticMacro.Call c_cancel_kotekan [StaticMacro.Var]
        , StaticMacro.Call Gender.c_realize_ngoret []
        ]
    where doc = "Combine the reyong realize calls in the right order."

c_realize_trompong :: Derive.Transformer Derive.Note
c_realize_trompong = StaticMacro.check "c_realize_trompong" $
    StaticMacro.transformer module_ "realize-trompong" Tags.postproc doc
        -- infer-damp relies on having the ngoret pitches and will get confused
        -- by uncalled notes, so it has to go after those.
        [ StaticMacro.Call c_hand_damp [StaticMacro.Var, StaticMacro.Var]
        , StaticMacro.Call Gender.c_realize_ngoret []
        ]
    where doc = "Combine the reyong realize calls in the right order."

-- * octave transposition

c_lower_octave_note :: Derive.Transformer Derive.Note
c_lower_octave_note = Derive.transformer module_ "lower-octave-note"
    (Tags.postproc <> Tags.under_invert)
    "Double a note with a single note one octave down, and add\
    \ 'Derive.Flags.infer_duration'."
    $ Sig.call0t $ Sub.under_invert $ \args deriver -> do
        start <- Args.real_start args
        pitch <- Call.get_pitch start
        let note = Call.add_flags Flags.infer_duration $
                Derive.at (Args.start args) $ Call.pitched_note $
                PSignal.add_control Controls.octave (-1) pitch
        -- The transposed goes second so realize-ngoret infers to the upper
        -- pitch rather than the lower one.  This is kind of subtle and
        -- unsatisfying, but works.
        deriver <> note

c_upper :: Library.Calls Derive.Note
c_upper = Make.transform_notes module_ "upper" Tags.inst
    ("Double a part with `" <> Doc.pretty Controls.octave
        <> "=+1` and `" <> Doc.pretty EnvKey.voice
        <> "=2`. If reyong subtracks have `v=+1` and `v=+2` respectively,\
        \ they'll wind up with the right voices.")
    (pure ()) $ \() deriver ->
        deriver <> Call.add_constant Controls.octave 1
            (Derive.with_val EnvKey.voice (2 :: Int) deriver)

-- * solkattu

solkattu_module :: Module.Module
solkattu_module = module_ <> "solkattu"

-- | These calls are used by the 'Derive.Solkattu.Instrument.Reyong'
-- realization.
c_solkattu_note :: [Pitch.Step] -> Derive.Generator Derive.Note
c_solkattu_note steps = Derive.generator solkattu_module "solkattu-note"
    Tags.inst "A pitched note, as generated by reyong solkattu."
    $ Sig.call0 $ Sub.inverting $ \args ->
        mconcatMap (realize (Args.start args) (Args.next args)) steps
    where
    realize start next step =
        Derive.place start (next - start) $
        Call.add_constant Controls.diatonic (fromIntegral step)
        Call.note
