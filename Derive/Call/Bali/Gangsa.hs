-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveFunctor #-}
{- | Calls for gangsa techniques.  Gangsa come in polos and sangsih pairs,
    and play either kotekan patterns or play unison or parallel parts.

    Kotekan patterns have a number of features in common.  They are all
    transpositions from a base pitch.  Rhythmically, they consist of notes with
    a constant duration, that line up at the end of an event's range, and the
    last duration is negative (i.e. implicit, depending on the next note).
    They use polos and sangsih and may switch patterns when the a kotekan speed
    threshold is passed.  Notes are also possible muted.

    There are a number of ways this can be extended:

    - Use any attribute instead of just mute.
    - More instruments than just polos and sangsih.
    - Multiple kotekan thresholds.

    The first two are supported at the 'PatternNote' level of abstraction.  For
    the others, I have to either directly use 'Note's or create a new
    abstraction:

    - Variable durations.
    - Line up at the start of the event instead of the end.
-}
module Derive.Call.Bali.Gangsa where
import qualified Data.Maybe as Maybe

import qualified Util.CallStack as CallStack
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Ui.Event as Event
import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call as Call
import qualified Derive.Call.Bali.Gender as Gender
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Post.Postproc as Postproc
import qualified Derive.Call.StaticMacro as StaticMacro
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Flags as Flags
import qualified Derive.PSignal as PSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Scale as Scale
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import Global
import Types


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.call_maps
    [ ("norot", c_norot Nothing)
    -- Alias for norot.  It's separate so I can rebind this locally.
    , ("nt", c_norot Nothing)
    , ("nt>", c_norot (Just True))
    , ("nt-", c_norot (Just False))
    , ("gnorot", c_gender_norot)
    , ("k_\\", c_kotekan_irregular Pat $ irregular_pattern
        "-11-1321" "-44-4324"
        "-11-1-21"
        "3-32-32-" "-44-43-4")
    , ("k//\\\\", c_kotekan_irregular Pat $ irregular_pattern
        "-123123213213123" "-423423243243423"
        "-12-12-21-21-12-"
        "3-23-232-32-3-23" "44-34-3-43-434-3")
    -- There are two ways to play k\\, either 21321321 or 31321321.  The first
    -- one is irregular since sangsih starts on 2 but there's no unison polos.
    , ("k\\\\", c_kotekan_irregular Telu $ irregular_pattern
        "21321321" "24324324"
        "-1-21-21"
        "2-32-32-" "-43-43-4")
    , ("k//", c_kotekan_irregular Telu $ irregular_pattern
        "23123123" "20120120"
        "-3-23-23"
        "2-12-12-" "-01-01-0")
    , ("k\\\\2", c_kotekan_regular (Just "-1-21-21"))
    , ("k//2",   c_kotekan_regular (Just "-2-12-12"))

    , ("kotekan", c_kotekan_kernel)
    , ("k", c_kotekan_regular Nothing)

    , ("'", c_ngoret $ pure Nothing)
    , ("'n", c_ngoret $ Just <$> Gender.interval_arg)
    , ("'^", c_ngoret $ pure $ Just $ Pitch.Diatonic (-1))
    , ("'_", c_ngoret $ pure $ Just $ Pitch.Diatonic 1)
    ]
    [ ("i+", Make.with_environ_val module_ "i+" "initial" True
        "Kotekan calls will emit a note on the initial beat.")
    , ("i-", Make.with_environ_val module_ "i-" "initial" False
        "Kotekan calls won't emit a note on the initial beat.")
    , ("nyog", c_nyogcag)
    , ("unison", c_unison)
    , ("kempyung", c_kempyung)
    , ("noltol", c_noltol)
    , ("realize-gangsa", c_realize_gangsa)
    , ("realize-noltol", c_realize_noltol)
    , ("realize-ngoret", Derive.set_module module_ Gender.c_realize_ngoret)
    , ("cancel-pasang", c_cancel_pasang)
    ]

val_calls :: [Derive.LookupCall Derive.ValCall]
val_calls = Derive.call_map
    [ ("pasangan", c_pasangan)
    ]

module_ :: Module.Module
module_ = "bali" <> "gangsa"

-- * instrument transform

-- | Variable mute for gangsa.  Intended for the 'Cmd.Cmd.inst_postproc' field.
-- This interprets 'Controls.mute' and turns it into either a @%mod@ control or
-- @mute_attr@.
mute_postproc :: Attrs.Attributes -> Score.Event -> Score.Event
mute_postproc mute_attr event =
    case Score.control_at (Score.event_start event) Controls.mute event of
        Nothing -> set_mod 0 event
        Just tval
            | mute >= threshold -> Score.add_attributes mute_attr event
            | mute <= 0 -> set_mod 0 event
            -- The mod control goes from 1 (least muted) to 0 (most muted).
            -- Bias mod towards the higher values, since the most audible
            -- partial mutes are from .75--1.
            | otherwise -> set_mod (1 - mute**2) $ Score.set_duration 0 event
            where
            mute = Score.typed_val tval
    where
    set_mod = Score.set_control Controls.mod . Score.untyped . Signal.constant
    -- Use the mute_attr above this threshold.
    threshold = 0.85

-- * ngoret

c_ngoret :: Sig.Parser (Maybe Pitch.Transpose) -> Derive.Generator Derive.Note
c_ngoret = Gender.ngoret module_ False $ Sig.defaulted "damp"
    (Sig.typed_control "ngoret-damp" 0.15 Score.Real)
    "Time that the grace note overlaps with this one. So the total\
    \ duration is time+damp, though it will be clipped to the\
    \ end of the current note."

-- * patterns

-- | There are 4 ways to realize a kotekan:
--
-- 1. Undivided.  Since it's undivided it could be unison or kempyung.
-- 2. Slow but divided.  Play all the notes, but sangsih and polos are kempyung
-- on the outer notes.
-- 3, 4. Ngotek, in telu and pat versions.
data KotekanPattern = KotekanPattern {
    kotekan_telu :: ![Maybe Pitch.Step]
    , kotekan_pat :: ![Maybe Pitch.Step]
    , kotekan_interlock_telu :: !(Pasang [Maybe Pitch.Step])
    , kotekan_interlock_pat :: !(Pasang [Maybe Pitch.Step])
    } deriving (Eq, Show)

instance Pretty.Pretty KotekanPattern where
    format (KotekanPattern telu pat itelu ipat) = Pretty.record "KotekanPattern"
        [ ("telu", Pretty.format telu)
        , ("pat", Pretty.format pat)
        , ("interlock_telu", Pretty.format itelu)
        , ("interlock_pat", Pretty.format ipat)
        ]

data Pasang a = Pasang {
    polos :: a
    , sangsih :: a
    } deriving (Eq, Show)

instance Pretty.Pretty a => Pretty.Pretty (Pasang a) where
    format (Pasang polos sangsih) = Pretty.record "Pasang"
        [ ("polos", Pretty.format polos)
        , ("sangsih", Pretty.format sangsih)
        ]

-- TODO use instead of pairs, and in Cycle
data Realization a = Realization {
    interlocking :: [a]
    , non_interlocking :: [a]
    } deriving (Eq, Show)

irregular_pattern :: CallStack.Stack => [Char] -> [Char]
    -> [Char] -> [Char] -> [Char] -> KotekanPattern
irregular_pattern polos sangsih4 polos_i sangsih_i3 sangsih_i4 = KotekanPattern
    { kotekan_telu = parse1 polos
    , kotekan_pat = parse1 sangsih4
    , kotekan_interlock_telu =
        Pasang { polos = parse1 polos_i, sangsih = parse1 sangsih_i3 }
    , kotekan_interlock_pat =
        Pasang { polos = parse1 polos_i, sangsih = parse1 sangsih_i4 }
    }
    where
    -- TODO the CallStack.Stack doesn't actually work because all these
    -- functions would have to have it too.
    parse1 = parse_pattern destination . check
    check ns
        | length ns == length polos = ns
        | otherwise =
            CallStack.errorStack $ "not same length as polos: " <> showt ns
    destination = fromMaybe (CallStack.errorStack "no final pitch") $
        Seq.last $ Maybe.catMaybes $ parse_pattern 0 polos

parse_pattern :: CallStack.Stack => Pitch.Step -> [Char] -> [Maybe Pitch.Step]
parse_pattern destination = map (fmap (subtract destination) . parse1)
    where
    parse1 '-' = Nothing
    parse1 c = Just $ fromMaybe
        (CallStack.errorStack $ "not a digit: " <> showt c) $ Num.readDigit c

kotekan_pattern :: KotekanPattern -> KotekanStyle -> Pasang Score.Instrument
    -> Cycle
kotekan_pattern pattern style pasang =
    (realize *** realize) $ pattern_steps style pasang pattern
    where realize = map (map (uncurry kotekan_note))

pattern_steps :: KotekanStyle -> Pasang Score.Instrument -> KotekanPattern
    -> ([[(Maybe Score.Instrument, Pitch.Step)]],
        [[(Maybe Score.Instrument, Pitch.Step)]])
pattern_steps style pasang (KotekanPattern telu pat itelu ipat) =
    (interlock, normal)
    where
    normal = case style of
        Telu -> map (realize Nothing) telu
        Pat -> interlocking (Pasang { polos = telu, sangsih = pat })
    realize inst n = maybe [] ((:[]) . (inst,)) n
    interlock = case style of
        Telu -> interlocking itelu
        Pat -> interlocking ipat
    interlocking part =
        [ realize (Just (polos pasang)) p ++ realize (Just (sangsih pasang)) s
        | (p, s) <- zip (polos part) (sangsih part)
        ]

-- ** norot

-- | Initially I implemented this as a postproc, but it now seems to me that
-- it would be more convenient as a generator.  In any case, as a postproc it
-- gets really complicated.
c_norot :: Maybe Bool -> Derive.Generator Derive.Note
c_norot default_prepare = Derive.generator module_ "norot" Tags.inst
    "Emit the basic norot pattern."
    $ Sig.call ((,,,,,,)
    <$> Sig.defaulted "prepare" default_prepare
        "Whether or not to prepare for the next pitch. If Nothing, infer based\
        \ on the next note."
    <*> Sig.defaulted "style" Default "Norot style."
    <*> dur_env <*> kotekan_env <*> instrument_top_env <*> pasang_env
    <*> infer_initial_final_env
    ) $ \(prep, style, dur, kotekan, inst_top, pasang, (maybe_initial, final))
    -> Sub.inverting $ \args -> do
        next_pitch <- infer_prepare args prep
        start <- Args.real_start args
        scale <- Call.get_scale
        under_threshold <- under_threshold_function kotekan dur
        let has_prepare = Maybe.isJust next_pitch
        -- False if all the time is taken up by the prepare.
        let has_sustain = not has_prepare
                || (Args.duration args > dur*4
                    || Args.duration args > dur*3 && maybe_initial == Just True)
        sustain <- if not has_sustain then return mempty else do
            pitch <- Call.get_pitch start
            pitch_t <- Derive.resolve_pitch start pitch
            let steps = norot_steps scale inst_top pitch_t style
            -- Default to no initial if this is immediately going into
            -- a prepare.  This is so I can use a 'nt>' for just prepare but
            -- line it up on the beat.
            let initial = fromMaybe has_sustain maybe_initial
            let sustain_end = Args.end args - if has_prepare then dur*3 else 0
            return $ realize_kotekan_pattern
                (initial, if has_prepare then False else final)
                (Args.start args, sustain_end) dur pitch under_threshold
                Repeat (gangsa_norot style pasang steps)
        prepare <- case next_pitch of
            Just next -> do
                next_t <- Derive.resolve_pitch start next
                let steps = norot_steps scale inst_top next_t style
                return $ Just $ realize_kotekan_pattern (True, final)
                    (Args.end args - dur*3, Args.end args)
                    dur next under_threshold
                    Once (gangsa_norot_arrival style pasang steps)
            Nothing -> return Nothing
        maybe sustain (sustain<>) prepare

-- | Prepare for the next pitch if the next notes starts at the end of this one
-- and has a different pitch.
infer_prepare :: Derive.PassedArgs a -> Maybe Bool
    -> Derive.Deriver (Maybe PSignal.Pitch)
infer_prepare _ (Just False) = return Nothing
infer_prepare args (Just True) = Args.lookup_next_pitch args
infer_prepare args Nothing
    | Args.next_start args /= Just (Args.end args) = return Nothing
    | otherwise = justm (Args.lookup_next_pitch args) $ \next -> do
        cur <- Call.get_pitch =<< Args.real_start args
        return $ if Pitches.equal cur next then Nothing else Just next

gangsa_norot :: NorotStyle -> Pasang Score.Instrument
    -> ((Pitch.Step, Pitch.Step), (Pitch.Step, Pitch.Step)) -> Cycle
gangsa_norot style pasang (pstep, sstep) = (interlock, normal)
    where
    interlock = map (:[]) [s (fst pstep), p (snd pstep)]
    normal = case style of
        Default -> map ((:[]) . both) [fst pstep, snd pstep]
        Diamond ->
            [ [p (fst pstep), s (fst sstep)]
            , [p (snd pstep), s (snd sstep)]
            ]
    both = kotekan_note Nothing
    p = kotekan_note (Just (polos pasang))
    s = kotekan_note (Just (sangsih pasang))

gangsa_norot_arrival :: NorotStyle -> Pasang Score.Instrument
    -> ((Pitch.Step, Pitch.Step), (Pitch.Step, Pitch.Step)) -> Cycle
gangsa_norot_arrival style pasang ((p1, p2), (s1, s2)) = (interlock, normal)
    where
    interlock =
        [ [p p2, s p2]
        , [p p2, s p2]
        , [s p1]
        , [p p2]
        ]
    normal = case style of
        Default -> map (:[]) [muted_note (both p2), both p2, both p1, both p2]
        Diamond ->
            [ map muted_note [p p2, s s2]
            , [p p2, s s2]
            , [p p1, s s1]
            , [p p2, s s2]
            ]
    both = kotekan_note Nothing
    p = kotekan_note (Just (polos pasang))
    s = kotekan_note (Just (sangsih pasang))

norot_steps :: Scale.Scale -> Maybe Pitch.Pitch -> PSignal.Transposed
    -- ^ this is to figure out if the sangsih part will be in range
    -> NorotStyle -> ((Pitch.Step, Pitch.Step), (Pitch.Step, Pitch.Step))
norot_steps scale inst_top pitch style
    | out_of_range 1 = ((-1, 0), (-1, 0))
    | otherwise = case style of
        Diamond -> ((1, 0), (-1, 0))
        -- Sangsih is only used if non-interlocking and using Diamond style.
        -- So the snd pair should be ignored.
        Default -> ((1, 0), (1, 0))
    where
    out_of_range steps = note_too_high scale inst_top $
        Pitches.transpose_d steps pitch

-- TODO update
c_gender_norot :: Derive.Generator Derive.Note
c_gender_norot = Derive.generator module_ "gender-norot" Tags.inst
    "Gender-style norot."
    $ Sig.call ((,,,)
    <$> dur_env <*> kotekan_env <*> pasang_env <*> infer_initial_final_env)
    $ \(dur, kotekan, pasang, initial_final) -> Sub.inverting $ \args -> do
        pitch <- Call.get_pitch =<< Args.real_start args
        under_threshold <- under_threshold_function kotekan dur
        realize_kotekan_pattern (infer_initial args initial_final)
            (Args.range args) dur pitch under_threshold Repeat
            (gender_norot pasang)

gender_norot :: Pasang Score.Instrument -> Cycle
gender_norot pasang = (interlocking, normal)
    where
    interlocking = [[s 1], [p 0], [s 1], [p 0]]
    normal =
        [ [p (-1), s 1]
        , [p (-2), s 0]
        , [p (-1), s 1]
        , if include_unison then [p 0, s 0] else [s 0]
        ]
    include_unison = True -- TODO chance based on signal
    p = kotekan_note (Just (polos pasang))
    s = kotekan_note (Just (sangsih pasang))

-- * kotekan

kotekan_doc :: Text
kotekan_doc =
    "Kotekan calls perform a pattern with `inst-polos` and `inst-sangsih`.\
    \ They line up at the end of the event but may also emit a note at the\
    \ start of the event, so use `cancel-pasang` to cancel the extra notes."

c_kotekan_irregular :: KotekanStyle -> KotekanPattern
    -> Derive.Generator Derive.Note
c_kotekan_irregular default_style pattern =
    Derive.generator module_ "kotekan" Tags.inst
    ("Render a kotekan pattern where both polos and sangsih are explicitly\
    \ specified. This is for irregular patterns.\n" <> kotekan_doc)
    $ Sig.call ((,,,,)
    <$> style_arg default_style
    <*> dur_env <*> kotekan_env <*> pasang_env <*> infer_initial_final_env
    ) $ \(style, dur, kotekan, pasang, initial_final) ->
    Sub.inverting $ \args -> do
        pitch <- get_pitch args
        under_threshold <- under_threshold_function kotekan dur
        realize_kotekan_pattern (infer_initial args initial_final)
            (Args.range args) dur pitch under_threshold Repeat
            (kotekan_pattern pattern style pasang)

-- ** regular

c_kotekan_kernel :: Derive.Generator Derive.Note
c_kotekan_kernel =
    Derive.generator module_ "kotekan" Tags.inst
    ("Render a kotekan pattern from a kernel. The sangsih part is inferred.\n"
        <> kotekan_doc)
    $ Sig.call ((,,,,,,,,)
    <$> Sig.defaulted "rotation" 0 "Rotate kernel to make a different pattern."
    <*> style_arg Telu
    <*> Sig.defaulted "sangsih" Call.Up
        "Whether sangsih is above or below polos."
    <*> Sig.environ "invert" Sig.Prefixed False "Flip the pattern upside down."
    <*> Sig.required_environ "kernel" Sig.Prefixed kernel_doc
    <*> dur_env <*> kotekan_env <*> pasang_env <*> infer_initial_final_env
    ) $ \(rotation, style, sangsih_above, inverted, kernel_s, dur, kotekan,
        pasang, initial_final) ->
    Sub.inverting $ \args -> do
        kernel <- Derive.require_right id $ make_kernel (untxt kernel_s)
        pitch <- get_pitch args
        under_threshold <- under_threshold_function kotekan dur
        let cycle = realize_kernel inverted sangsih_above style pasang
                (rotate rotation kernel)
        realize_kotekan_pattern (infer_initial args initial_final)
            (Args.range args) dur pitch under_threshold Repeat cycle

-- | For regular kotekan, the sangsih can be automatically derived from the
-- polos.
c_kotekan_regular :: Maybe Text -> Derive.Generator Derive.Note
c_kotekan_regular maybe_kernel =
    Derive.generator module_ "kotekan" Tags.inst
    ("Render a kotekan pattern from a kernel representing the polos.\
    \ The sangsih is inferred.\n" <> kotekan_doc)
    $ Sig.call ((,,,,,,)
    <$> maybe (Sig.required "kernel" kernel_doc) pure maybe_kernel
    <*> style_arg Telu
    <*> Sig.defaulted "sangsih" Nothing
        "Whether sangsih is above or below polos. If not given, sangsih will\
        \ be above if the polos ends on a low note or rest, below otherwise."
    <*> dur_env <*> kotekan_env <*> pasang_env <*> infer_initial_final_env
    ) $ \(kernel_s, style, maybe_sangsih_above, dur, kotekan, pasang,
        initial_final
    ) -> Sub.inverting $ \args -> do
        kernel <- Derive.require_right id $ make_kernel (untxt kernel_s)
        let sangsih_above = fromMaybe (infer_sangsih kernel) maybe_sangsih_above
        pitch <- get_pitch args
        under_threshold <- under_threshold_function kotekan dur
        let cycle = realize_kernel False sangsih_above style pasang kernel
        realize_kotekan_pattern (infer_initial args initial_final)
            (Args.range args) dur pitch under_threshold Repeat cycle
    where
    infer_sangsih kernel = case Seq.last kernel of
        Nothing -> Call.Up
        Just Rest -> Call.Up
        Just Low -> Call.Up
        Just High -> Call.Down

kernel_doc :: Text
kernel_doc = "Polos part in transposition steps.\
    \ This will be normalized to end on the destination pitch.\
    \ It should consist of `-`, `1`, and `2`. You can start with `k` to\
    \ avoid needing quotes. Starting with `k` will also require the length to\
    \ be a multiple of 4."

realize_kernel :: Bool -> Call.UpDown -> KotekanStyle -> Pasang Score.Instrument
    -> Kernel -> Cycle
realize_kernel inverted sangsih_above style pasang kernel =
    end_on_zero $ kernel_to_pattern
        ((if inverted then invert else id) kernel) sangsih_above style pasang

-- *** implementation

-- | Take a Cycle, which is an abstract description of a pattern via
-- 'KotekanNote's, to real notes in a NoteDeriver.
realize_kotekan_pattern :: (Bool, Bool) -- ^ include (initial, final)
    -> (ScoreTime, ScoreTime) -> ScoreTime
    -> PSignal.Pitch -> (ScoreTime -> Bool) -> Repeat -> Cycle
    -> Derive.NoteDeriver
realize_kotekan_pattern initial_final (start, end) dur pitch
        under_threshold repeat cycle =
    realize_notes realize $
        realize_pattern repeat initial_final start end dur get_cycle
    where
    get_cycle t
        | under_threshold t = fst cycle
        | otherwise = snd cycle
    realize (KotekanNote inst steps muted) =
        maybe id Derive.with_instrument inst $
        -- TODO the kind of muting should be configurable.  Or, rather I should
        -- dispatch to a zero dur note call, which will pick up whatever form
        -- of mute is configured.
        (if muted then Call.add_attributes Attrs.mute else id) $
        Call.pitched_note (Pitches.transpose_d steps pitch)
    -- TODO It should no longer be necessary to strip flags from
    -- 'Call.pitched_note', because "" only puts flags on if the event is
    -- at the end of the track, and that shouldn't happen for these.  Still,
    -- Call.pitched_note should use a lower level note call that doesn't do
    -- things like that.

type Kernel = [Atom]
data Atom = Rest | Low | High deriving (Eq, Ord, Show)

instance Pretty.Pretty Atom where
    format = Pretty.char . to_char
    formatList cs =
        "make_kernel \"" <> Pretty.text (txt (map to_char cs)) <> "\""

make_kernel :: [Char] -> Either Text Kernel
make_kernel ('k':cs)
    | length cs `mod` 4 /= 0 =
        Left $ "kernel's length " <> showt (length cs)
            <> " is not a multiple of 4: " <> showt cs
    | otherwise = mapM from_char cs
make_kernel cs = mapM from_char cs

from_char :: Char -> Either Text Atom
from_char '-' = Right Rest
from_char '1' = Right Low
from_char '2' = Right High
from_char c = Left $ "kernel must be `-`, `1` or `2`, but got " <> showt c

to_char :: Atom -> Char
to_char c = case c of
    Rest -> '-'
    Low -> '1'
    High -> '2'

-- | Make both parts end on zero by subtracting the pitch of the final
-- non-interlocking note.
end_on_zero :: Cycle -> Cycle
end_on_zero (interlocking, non_interlocking) =
    (add (-steps) interlocking, add (-steps) non_interlocking)
    where
    add steps = map $ map $ \note ->
        note { note_steps = steps + note_steps note }
    steps = fromMaybe 0 $ do
        final : _ <- Seq.last non_interlocking
        return $ note_steps final

kernel_to_pattern :: Kernel -> Call.UpDown -> KotekanStyle
    -> Pasang Score.Instrument -> Cycle
kernel_to_pattern kernel sangsih_above kotekan_style pasang =
    (interlocking, non_interlocking)
    where
    interlocking = map interlock kernel
    non_interlocking = map non_interlock kernel
    interlock atom = case (sangsih_above, kotekan_style) of
        (Call.Up, Telu) -> case atom of
            Rest -> [s 2]
            Low -> [p 0]
            High -> [p 1, s 1]
        (Call.Up, Pat) -> case atom of
            Rest -> [s 2]
            Low -> [p 0, s 3]
            High -> [p 1]
        (Call.Down, Telu) -> case atom of
            Rest -> [s (-1)]
            Low -> [p 0, s 0]
            High -> [p 1]
        (Call.Down, Pat) -> case atom of
            Rest -> [s (-1)]
            Low -> [p 0]
            High -> [p 1, s (-2)]
    non_interlock atom = case (sangsih_above, kotekan_style) of
        (Call.Up, Telu) -> case atom of
            Rest -> [both 2]
            Low -> [both 0]
            High -> [both 1]
        (Call.Up, Pat) -> case atom of
            Rest -> [p 2, s 2]
            Low -> [p 0, s 3]
            High -> [p 1, s 1]
        (Call.Down, Telu) -> case atom of
            Rest -> [both (-1)]
            Low -> [both 0]
            High -> [both 1]
        (Call.Down, Pat) -> case atom of
            Rest -> [p (-1), s (-1)]
            Low -> [p 0, s 0]
            High -> [p 1, s (-2)]
    p = kotekan_note (Just (polos pasang))
    s = kotekan_note (Just (sangsih pasang))
    both = kotekan_note Nothing

rotate :: Int -> [a] -> [a]
rotate n xs = cycle (rotations xs) !! n

rotations :: [a] -> [[a]]
rotations xs = xs : go xs (reverse xs)
    where
    go [] _ = []
    go _ [_] = []
    go _ [] = []
    go xs (z:zs) = p : go p zs
        where p = take len (z : xs)
    len = length xs

invert :: Kernel -> Kernel
invert = map $ \x -> case x of
    Rest -> Rest
    High -> Low
    Low -> High

-- *** all kernels

-- | Find a kernel as a rotation or inversion of one of the standard ones.
find_kernel :: Kernel -> Maybe (Kernel, Bool, Int)
find_kernel kernel = lookup kernel variants
    where
    variants =
        [ (variant, (kernel, inverted, rotation))
        | kernel <- all_kernels
        , (variant, (inverted, rotation)) <- variations kernel
        ]
    all_kernels = [kernel_12_1_21, kernel_1_21_21, kernel_2_21_21]
    Right kernel_12_1_21 = make_kernel "-12-1-21"
    Right kernel_1_21_21 = make_kernel "-1-21-21"
    Right kernel_2_21_21 = make_kernel "-2-21-21"

    variations :: Kernel -> [(Kernel, (Bool, Int))]
    variations kernel_ = Seq.unique_on fst
        [ (variant, (inverted, rotate))
        | (inverted, kernel) <- [(False, kernel_), (True, invert kernel_)]
        , (rotate, variant) <- zip [0..] (rotations kernel)
        ]

-- ** implementation

data Repeat = Repeat | Once deriving (Show)
instance Pretty.Pretty Repeat where pretty = showt

-- | (interlocking pattern, non-interlocking pattern)
--
-- Each list represents coincident notes.  [] is a rest.
type Cycle = ([[KotekanNote]], [[KotekanNote]])

data Note a = Note {
    note_start :: !ScoreTime
    , note_duration :: !ScoreTime
    -- | Used for initial_flag or final_flag.
    , note_flags :: !Flags.Flags
    , note_data :: !a
    } deriving (Functor, Show)

instance Pretty.Pretty a => Pretty.Pretty (Note a) where
    format (Note start dur flags d) = Pretty.format (start, dur, flags, d)

add_flag :: Flags.Flags -> Note a -> Note a
add_flag flag n = n { note_flags = flag <> note_flags n }

-- | High level description of a note.  This goes into Note before it becomes
-- a Derive.NoteDeriver.
data KotekanNote = KotekanNote {
    -- | If Nothing, retain the instrument in scope.  Presumably it will be
    -- later split into polos and sangsih by a @unison@ or @kempyung@ call.
    note_instrument :: !(Maybe Score.Instrument)
    , note_steps :: !Pitch.Step
    , note_muted :: !Bool
    } deriving (Show)

kotekan_note :: Maybe Score.Instrument -> Pitch.Step -> KotekanNote
kotekan_note inst steps = KotekanNote
    { note_instrument = inst
    , note_steps = steps
    , note_muted = False
    }

muted_note :: KotekanNote -> KotekanNote
muted_note note = note { note_muted = True }

instance Pretty.Pretty KotekanNote where
    format (KotekanNote inst steps muted) =
        Pretty.format (inst, steps, if muted then "+mute" else "+open" :: Text)

under_threshold_function :: BaseTypes.ControlRef -> ScoreTime
    -> Derive.Deriver (ScoreTime -> Bool) -- ^ say if a note at this time
    -- with the given duration would be under the kotekan threshold
under_threshold_function kotekan dur = do
    to_real <- Derive.real_function
    kotekan <- Call.to_function kotekan
    return $ \t ->
        let real = to_real t
        in to_real (t+dur) - real < RealTime.seconds (kotekan real)

-- | Repeatedly call a cycle generating function to create notes.  The result
-- will presumably be passed to 'realize_notes' to convert the notes into
-- NoteDerivers.
realize_pattern :: Repeat -- ^ Once will just call get_cycle at the start
    -- time.  Repeat will start the cycle at t+1 because t is the initial, so
    -- it's the end of the cycle.
    -> (Bool, Bool)
    -> ScoreTime -> ScoreTime -> ScoreTime
    -> (ScoreTime -> [[a]]) -- ^ Get one cycle of notes, starting at the time.
    -> [Note a]
realize_pattern repeat (initial, final) start end dur get_cycle =
    case repeat of
        Once -> concatMap realize $
            zip (get_cycle start) (Seq.range start end dur)
        Repeat -> concat $ concat $ cycles $ Seq.range start end dur
    where
    cycles [] = []
    -- Since cycles are end-weighted, I have to get the end of a cycle if an
    -- initial note is wanted.
    cycles (t:ts)
        | t == start && initial =
            [realize (fromMaybe [] (Seq.last (get_cycle t)), t)] : cycles ts
        | t == start = cycles ts
        | otherwise = map realize pairs : cycles rest_ts
        where (pairs, rest_ts) = Seq.zip_remainder (get_cycle t) (t:ts)
    realize (chord, t)
        | t >= end = if final
            then map (add_flag (Flags.infer_duration <> final_flag)) ns
            else []
        | t == start = if initial
            then map (add_flag initial_flag) ns
            else []
        | otherwise = ns
        where ns = map (Note t dur mempty) chord

-- | Repeatedly call a cycle generating function to create notes.  The result
-- will presumably be passed to 'realize_notes' to convert the notes into
-- NoteDerivers.  Start the cycle at t+1 because t is the initial, so it's the
-- end of the cycle.
cycle_pattern :: (Bool, Bool) -> ScoreTime -> ScoreTime -> ScoreTime
    -> (ScoreTime -> [[a]]) -- ^ Get one cycle of notes, starting at the time.
    -> [Note a]
cycle_pattern (initial, final) start end dur get_cycle =
    concat $ concat $ cycles $ Seq.range start end dur
    where
    cycles [] = []
    -- Since cycles are end-weighted, I have to get the end of a cycle if an
    -- initial note is wanted.
    cycles (t:ts)
        | t == start && initial =
            [realize (fromMaybe [] (Seq.last (get_cycle t)), t)] : cycles ts
        | t == start = cycles ts
        | otherwise = map realize pairs : cycles rest_ts
        where (pairs, rest_ts) = Seq.zip_remainder (get_cycle t) (t:ts)
    realize (chord, t)
        | t >= end = if final
            then map (add_flag (Flags.infer_duration <> final_flag)) ns
            else []
        | t == start = if initial
            then map (add_flag initial_flag) ns
            else []
        | otherwise = ns
        where ns = map (Note t dur mempty) chord

-- | Turn Notes into a NoteDeriver.
realize_notes :: (a -> Derive.NoteDeriver) -> [Note a] -> Derive.NoteDeriver
realize_notes realize = mconcatMap $ \(Note start dur flags note) ->
    Derive.place start dur $ Call.add_flags flags $ realize note

-- | Style for non-interlocking norot.  Interlocking norot is always the upper
-- neighbor (or lower on the top key).
data NorotStyle =
    -- | Norot is emitted as the current instrument, which should be converted
    -- into kempyung or unison by a postproc.
    Default
    -- | Norot in the diamond pattern, where sangsih goes down.
    | Diamond
    deriving (Bounded, Eq, Enum, Show)

instance ShowVal.ShowVal NorotStyle where show_val = Typecheck.enum_show_val
instance Typecheck.Typecheck NorotStyle
instance Typecheck.TypecheckSymbol NorotStyle

data KotekanStyle = Telu | Pat deriving (Bounded, Eq, Enum, Show)
instance ShowVal.ShowVal KotekanStyle where
    show_val = Typecheck.enum_show_val
instance Typecheck.Typecheck KotekanStyle
instance Typecheck.TypecheckSymbol KotekanStyle

-- * postproc

c_unison :: Derive.Transformer Derive.Note
c_unison = Derive.transformer module_ "unison" Tags.postproc
    "Split part into unison polos and sangsih."
    $ Sig.callt pasang_env $ \pasang _args deriver -> do
        inst <- Call.get_instrument
        pasang <- Pasang <$> Derive.get_instrument (polos pasang)
            <*> Derive.get_instrument (sangsih pasang)
        Post.emap_asc_ (unison inst pasang) <$> deriver
    where
    unison inst pasang event
        | Score.event_instrument event == inst =
            [ Score.add_log msg $ Post.set_instrument (polos pasang) event
            , Score.add_log msg $ Post.set_instrument (sangsih pasang) event
            ]
        | otherwise = [event]
        where msg = "unison from " <> pretty inst

-- | I could do this in two different ways:  Eval normally, then eval with
-- +kempyung, and make instrument note call understand it.  Or, postproc,
-- transpose, and check if the nn is above a limit.  The first one would let
-- the instrument choose how it wants to interpret +kempyung while letting this
-- call remain generic, but let's face it, it only really means one thing.  The
-- second seems a little simpler since it doesn't need a cooperating note call.
--
-- So postproc it is.
c_kempyung :: Derive.Transformer Derive.Note
c_kempyung = Derive.transformer module_ "kempyung" Tags.postproc
    "Split part into kempyung, with `polos-inst` below and `sangsih-inst`\
    \ above."
    $ Sig.callt ((,)
    <$> instrument_top_env <*> pasang_env
    ) $ \(maybe_top, pasang) _args deriver -> do
        inst <- Call.get_instrument
        pasang <- Pasang <$> Derive.get_instrument (polos pasang)
            <*> Derive.get_instrument (sangsih pasang)
        scale <- Call.get_scale
        let too_high = pitch_too_high scale maybe_top
        Post.emap_asc_ (kempyung too_high inst pasang) <$> deriver
    where
    kempyung too_high inst pasang event
        | Score.event_instrument event == inst =
            [ Score.add_log ("low kempyung from " <> pretty inst) $
                Post.set_instrument (polos pasang) event
            , Score.add_log ("high kempyung from " <> pretty inst) $
                transpose too_high $ Post.set_instrument (sangsih pasang) event
            ]
        | otherwise = [event]
    transpose too_high event
        | too_high transposed = event
        | otherwise = transposed
        where
        transposed = event
            { Score.event_untransformed_pitch =
                PSignal.map_y (Pitches.transpose (Pitch.Diatonic 3))
                    (Score.event_untransformed_pitch event)
            }

c_nyogcag :: Derive.Transformer Derive.Note
c_nyogcag = Derive.transformer module_ "nyog" Tags.postproc
    "Split a single part into polos and sangsih parts by assigning\
    \ polos and sangsih to alternating notes."
    $ Sig.callt pasang_env $ \pasang _args deriver ->
        snd . Post.emap_asc (nyogcag pasang) True <$> deriver

nyogcag :: Pasang Score.Instrument -> Bool -> Score.Event
    -> (Bool, [Score.Event])
nyogcag pasang is_polos event = (not is_polos, [with_inst])
    where
    with_inst = event
        { Score.event_instrument =
            if is_polos then polos pasang else sangsih pasang
        }

-- * realize calls

c_realize_gangsa :: Derive.Transformer Derive.Note
c_realize_gangsa = StaticMacro.check "c_realize_gangsa" $
    StaticMacro.transformer module_ "realize-gangsa" Tags.postproc doc
        [ StaticMacro.Call c_realize_noltol []
        , StaticMacro.Call c_cancel_pasang [StaticMacro.Var]
        , StaticMacro.Call Gender.c_realize_ngoret []
        ]
    where doc = "Combine the gangsa realize calls in the right order."

-- | (noltol-time, kotekan-dur)
type NoltolArg = (RealTime, RealTime)

noltol_arg :: Text
noltol_arg = "noltol"

c_noltol :: Derive.Transformer Derive.Note
c_noltol = Derive.transformer module_ "noltol" Tags.delayed
    "Play the transformed notes in noltol style. If the space between \
    \ notes of the same (instrument, hand) is above a threshold,\
    \ end the note with a `+mute`d copy of itself. This only happens if\
    \ the duration of the note is at or below the `kotekan-dur`."
    $ Sig.callt ((,)
    <$> Sig.defaulted "time" (Sig.control "noltol" 0.1)
        "Play noltol if the time available exceeds this threshold."
    <*> dur_env
    ) $ \(threshold, max_dur) args deriver -> do
        max_dur <- Call.real_duration (Args.start args) max_dur
        events <- deriver
        times <- Post.time_control threshold events
        return $ Post.emap1_ (put max_dur) $ Stream.zip times events
        where
        put max_dur (threshold, event) =
            Score.put_arg noltol_arg ((threshold, max_dur) :: NoltolArg) event

c_realize_noltol :: Derive.Transformer Score.Event
c_realize_noltol = Derive.transformer module_ "realize-noltol"
    Tags.realize_delayed "Perform the annotations added by `noltol`."
    $ Sig.call0t $ \_args deriver -> realize_noltol_call =<< deriver

realize_noltol_call :: Stream.Stream Score.Event -> Derive.NoteDeriver
realize_noltol_call =
    Post.emap_asc_m_ fst realize . Post.next_by Score.event_instrument id
    where
    realize (event, next) = do
        (event, maybe_arg) <- Derive.require_right id $
            Score.take_arg noltol_arg event
        return $ case maybe_arg of
            Nothing -> [event]
            Just arg -> realize_noltol arg event next

-- | If the next note of the same instrument is below a threshold, the note's
-- off time is replaced with a +mute.
realize_noltol :: NoltolArg -> Score.Event -> Maybe Score.Event -> [Score.Event]
realize_noltol (threshold, max_dur) event next
    | should_noltol threshold max_dur next = [event, muted]
    | otherwise = [event]
    where
    -- TODO reapply a note with dur 0 to create the mute
    muted = Score.add_attributes Attrs.mute $
        -- TODO dynamic should probably be configurable
        Score.modify_dynamic (*0.65) $ Score.duration (const 0) $
        Score.move (+ Score.event_duration event) $ Score.copy event
    should_noltol threshold max_dur maybe_next =
        Score.event_duration event RealTime.<= max_dur
        && maybe True ((>= threshold) . space) maybe_next
    space next = Score.event_start next - Score.event_end event

-- ** cancel-pasang

c_cancel_pasang :: Derive.Transformer Derive.Note
c_cancel_pasang = Derive.transformer module_ "cancel-pasang" Tags.postproc
    "This is like the `cancel` call, except it also knows how to cancel out\
    \ pasang instruments such that adjacent kotekan calls can have initial and\
    \ final notes, but won't get doubled notes."
    $ Postproc.make_cancel cancel_pasang pasang_key

-- | The order of precedence is normals, then finals, then initials.
-- The final note also gets 'Flags.infer_duration', but since it will lose to
-- normal notes, the infer will only happen if there is no next note.  So it
-- won't ever merge with the duration of a cancelled note.
cancel_pasang :: [Score.Event] -> Either Text [Score.Event]
cancel_pasang events =
    Postproc.cancel_strong_weak Postproc.infer_duration_merged $
        case Seq.partition2 (has final_flag) (has initial_flag) events of
            (_, _, normals@(_:_)) -> normals
            (finals@(_:_), _, []) -> finals
            ([], initials, []) -> initials
    where has = Score.has_flags

-- | Match any of polos, sangsih, and pasang to each other.  Since polos and
-- sangsih together are considered one voice, a sangsih start is note end for
-- a polos note.
pasang_key :: Postproc.Key
    (Either Score.Instrument (Score.Instrument, Score.Instrument), Maybe Text)
pasang_key e = (inst, get EnvKey.hand)
    where
    inst = case (get inst_polos, get inst_sangsih) of
        (Just p, Just s) -> Right (p, s)
        _ -> Left (Score.event_instrument e)
    get k = Env.maybe_val k (Score.event_environ e)

-- * util

c_pasangan :: Derive.ValCall
c_pasangan = Derive.val_call module_ "pasangan" mempty
    ("Choose a value depending on the value of the "
    <> ShowVal.doc EnvKey.role <> " variable."
    ) $ Sig.call ((,,)
    <$> Sig.required "polos" "Value for polos."
    <*> Sig.required "sangsih" "Value for sangsih."
    <*> role_env
    ) $ \(polos, sangsih, role) _args -> case role of
        Polos -> return (polos :: BaseTypes.Val)
        Sangsih -> return (sangsih :: BaseTypes.Val)

-- * implementation

-- | Get pitch for a kotekan call.  For Negative events, get the pitch at the
-- end.
get_pitch :: Derive.PassedArgs a -> Derive.Deriver PSignal.Pitch
get_pitch args = Call.get_pitch =<< Args.real_trigger args

style_arg :: KotekanStyle -> Sig.Parser KotekanStyle
style_arg deflt = Sig.defaulted_env "style" Sig.Both deflt "Kotekan style."

dur_env :: Sig.Parser ScoreTime
dur_env = Sig.environ_quoted "kotekan-dur" Sig.Unprefixed
    (BaseTypes.quoted "ts" [BaseTypes.str "e"]) "Duration of derived notes."

kotekan_env :: Sig.Parser BaseTypes.ControlRef
kotekan_env =
    Sig.environ "kotekan" Sig.Unprefixed (BaseTypes.constant_control 0.15)
        "If note durations are below this, divide the parts between polos and\
        \ sangsih."

infer_initial_final_env :: Sig.Parser (Maybe Bool, Bool)
infer_initial_final_env = (,)
    <$> Sig.environ "initial" Sig.Unprefixed Nothing
        "If true, include an initial note, which is the same as the final note.\
        \ This is suitable for the start of a sequence of kotekan calls.\
        \ If not given, infer false for negative duration, true for positive."
    <*> Sig.environ "final" Sig.Unprefixed True
        "If true, include the final note, at the event end."

infer_initial :: Derive.PassedArgs a -> (Maybe Bool, Bool) -> (Bool, Bool)
infer_initial args =
    first $ fromMaybe (not $ Event.is_negative (Args.event args))

initial_final_env :: Sig.Parser (Bool, Bool)
initial_final_env = (,)
    <$> Sig.environ "initial" Sig.Unprefixed True
        "If true, include an initial note, which is the same as the final note.\
        \ This is suitable for the start of a sequence of kotekan calls.\
        \ If not given, infer false for negative duration, true for positive."
    <*> Sig.environ "final" Sig.Unprefixed True
        "If true, include the final note, at the event end."

instrument_top_env :: Sig.Parser (Maybe Pitch.Pitch)
instrument_top_env =
    Sig.environ (BaseTypes.unsym EnvKey.instrument_top) Sig.Unprefixed Nothing
        "Top pitch this instrument can play. Normally the instrument sets\
        \ it via the instrument environ."

note_too_high :: Scale.Scale -> Maybe Pitch.Pitch -> PSignal.Transposed
    -> Bool
note_too_high scale maybe_top pitchv = fromMaybe False $ do
    top <- maybe_top
    note <- either (const Nothing) Just $ PSignal.pitch_note pitchv
    pitch <- either (const Nothing) Just $ Scale.scale_read scale mempty note
    return $ pitch > top

pitch_too_high :: Scale.Scale -> Maybe Pitch.Pitch -> Score.Event -> Bool
pitch_too_high scale maybe_top =
    maybe False (note_too_high scale maybe_top) . Score.initial_pitch

pasang_env :: Sig.Parser (Pasang Score.Instrument)
pasang_env = Pasang
    <$> Sig.required_environ (BaseTypes.unsym inst_polos) Sig.Unprefixed
        "Polos instrument."
    <*> Sig.required_environ (BaseTypes.unsym inst_sangsih) Sig.Unprefixed
        "Sangsih instrument."

inst_polos :: Env.Key
inst_polos = "inst-polos"

inst_sangsih :: Env.Key
inst_sangsih = "inst-sangsih"

data Role = Polos | Sangsih deriving (Bounded, Eq, Enum, Show)
instance ShowVal.ShowVal Role where show_val = Typecheck.enum_show_val
instance Typecheck.Typecheck Role
instance Typecheck.TypecheckSymbol Role

role_env :: Sig.Parser Role
role_env = Sig.required_environ (BaseTypes.unsym EnvKey.role) Sig.Unprefixed
    "Instrument role."

initial_flag, final_flag :: Flags.Flags
initial_flag = Flags.flag "initial"
final_flag = Flags.flag "final"
