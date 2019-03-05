-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Library of basic high level pitch calls.
--
-- High level calls do something a little more abstract and \"musical\"
-- than the low level calls in "Derive.Call.Prelude.Pitch".  Generally they
-- have complete-word names, while low level calls are just single letters.
--
-- TODO this module has a dumb name.  What would be better?
module Derive.C.Prelude.PitchHigh (library) where
import qualified Util.Doc as Doc
import qualified Derive.Args as Args
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.C.Prelude.Pitch as Call.Pitch
import qualified Derive.Call as Call
import qualified Derive.Call.ControlUtil as ControlUtil
import qualified Derive.Call.Module as Module
import qualified Derive.Call.PitchUtil as PitchUtil
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Sig as Sig
import           Derive.Sig (defaulted)
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import           Global
import           Types


library :: Library.Library
library = mconcat
    [ Library.transformers
        [ ("lift", c_lift_note)
        , ("drop", c_drop_note)
        , ("Lift", c_lift_note_start)
        , ("Drop", c_drop_note_start)
        ]
    , Library.generators
        [ ("drop", c_drop)
        , ("lift", c_lift)
        , ("ad", c_approach_dyn)
        ]
    ]

-- * note calls

c_lift_note :: Derive.Transformer Derive.Note
c_lift_note = make_note_fade "lift"
    "Raise pitch and drop `dyn` at the end of the note. Same as the `drop`\
    \ note call, except it defaults to going up instead of down."
    PitchLift AlignEnd AlignStart

c_drop_note :: Derive.Transformer Derive.Note
c_drop_note = make_note_fade "drop"
    "Drop pitch and `dyn` at the end of the note."
    PitchDrop AlignEnd AlignStart

c_lift_note_start :: Derive.Transformer Derive.Note
c_lift_note_start = make_note_fade "Lift"
    "Attack the note from a lower neighbor.  This is like the `n` pitch call,\
    \ but it's a note call, and it fades in `dyn` at the same time."
    PitchDrop AlignStart AlignEnd
    -- The PitchDirection is reversed because a lift at the beginning of a note
    -- comes from below.  Also, I align the pitch to the end of the fade
    -- for symmetry with 'drop' and 'lift', I'm not sure if it's actually more
    -- musically useful that way.

c_drop_note_start :: Derive.Transformer Derive.Note
c_drop_note_start = make_note_fade "Drop"
    "Like `Lift`, but attack the note from a higher neighbor."
    PitchLift AlignStart AlignEnd
    -- Like 'c_lift_note_start', the PitchDirection is reversed.

make_note_fade :: Derive.CallName -> Doc.Doc -> PitchDirection -> Align
    -> Align -> Derive.Transformer Derive.Note
make_note_fade name doc pitch_dir align align_fade =
    Derive.transformer Module.prelude name Tags.under_invert doc
    $ Sig.callt fade_args
    $ \(interval, Typecheck.DefaultReal time, maybe_fade, curve) ->
    Sub.under_invert $ \args deriver -> do
        let fade = case maybe_fade of
                Nothing -> time
                Just (Typecheck.DefaultReal t) -> t
        ranges@((pitch_start, _), _) <- pitch_fade_ranges align align_fade
            fade time (Args.start args) (Args.end args)
        Derive.pitch_at pitch_start >>= \case
            Nothing -> deriver
            Just pitch -> do
                (slide, dyn) <-
                    pitch_fade align curve pitch pitch_dir interval ranges
                pitch_sig <- Internal.get_dynamic Derive.state_pitch
                let merged = case align of
                        -- Since the initial slide has to override the base
                        -- pitch, I can't just merge normally.
                        AlignStart -> PSignal.prepend slide pitch_sig
                        AlignEnd -> pitch_sig <> slide
                Call.multiply_control Score.c_dynamic (ScoreT.untyped dyn) $
                    Derive.with_pitch merged deriver

fade_args :: Sig.Parser (Either Pitch.Transpose PSignal.Pitch,
    Typecheck.DefaultReal, Maybe Typecheck.DefaultReal, ControlUtil.Curve)
fade_args = (,,,)
    <$> defaulted "interval" (Left (Pitch.Chromatic 7))
        "Interval or destination pitch."
    <*> defaulted "time" (Typecheck.real 0.25) "Time to the destination pitch."
    <*> defaulted "fade" Nothing
        "Time to fade from or to nothing. If the fade is longer than the pitch\
        \ time, the pitch will finish moving before the dyn has faded out."
    <*> ControlUtil.curve_env

-- * pitch calls

c_drop :: Derive.Generator Derive.Pitch
c_drop = make_pitch_fade "drop" "Drop pitch and `dyn`." PitchDrop

c_lift :: Derive.Generator Derive.Pitch
c_lift = make_pitch_fade "lift"
    "Lift pitch and drop `dyn`. This is the same as `drop`, except that it\
    \ defaults to going up instead of down."
    PitchLift

make_pitch_fade :: Derive.CallName -> Doc.Doc -> PitchDirection
    -> Derive.Generator Derive.Pitch
make_pitch_fade name doc pitch_dir =
    Derive.generator1 Module.prelude name Tags.cmod doc
    $ Sig.call fade_args
    $ \(interval, Typecheck.DefaultReal time, maybe_fade, curve) args -> do
        let fade = case maybe_fade of
                Nothing -> time
                Just (Typecheck.DefaultReal t) -> t
        case Args.prev_pitch args of
            Nothing -> return mempty
            Just (_, prev_pitch) -> do
                (slide, dyn) <- pitch_fade AlignEnd curve prev_pitch pitch_dir
                    interval =<< pitch_fade_ranges AlignStart AlignStart
                        fade time (Args.start args) (Args.start args)
                next <- Derive.real (Args.next args)
                ControlUtil.multiply_dyn next dyn
                return slide

c_approach_dyn :: Derive.Generator Derive.Pitch
c_approach_dyn = Derive.generator1 Module.prelude "approach-dyn"
    (Tags.cmod <> Tags.next)
    "Like `approach`, slide to the next pitch, but also drop the `dyn`."
    $ Sig.call ((,,)
    <$> defaulted "time" (Typecheck.real 0.2)
        "Time to get to destination pitch and dyn."
    <*> defaulted "dyn" 0.25 "Drop `dyn` by this factor."
    <*> ControlUtil.curve_env
    ) $ \(Typecheck.DefaultReal time, dyn, curve) args -> do
        (start, end) <- Call.duration_from_start args time
        ControlUtil.multiply_dyn end
            =<< ControlUtil.make_segment ControlUtil.Linear start 1 end dyn
        Call.Pitch.approach args curve start end

-- * fade implementation

data Align = AlignStart | AlignEnd deriving (Show)
data PitchDirection = PitchDrop | PitchLift deriving (Show)

pitch_fade :: Align -> ControlUtil.Curve -> PSignal.Pitch -> PitchDirection
    -> Either Pitch.Transpose PSignal.Pitch
    -> ((RealTime, RealTime), (RealTime, RealTime))
    -> Derive.Deriver (PSignal.PSignal, Signal.Control)
pitch_fade align curve pitch pitch_dir interval
        ((pitch_start, pitch_end), (fade_start, fade_end)) =
    (,) <$> pitch_segment align curve (min pitch_start fade_start) pitch_start
                pitch_end pitch interval pitch_dir
        <*> segment ControlUtil.Linear fade_start dyn1 fade_end dyn2
    where
    (dyn1, dyn2) = case align of
        AlignStart -> (0, 1)
        AlignEnd -> (1, 0)

-- | Create envelope start and end times for pitch and fade as follows:
--
-- @
--     --------     align   align_fade
--     ffff         Start   Start
--     pp-->
--     ffff         Start   End
--     <-pp
--         ffff     End     Start
--        <pp--
--         ffff     End     End
--        <--pp
-- @
pitch_fade_ranges :: Align -> Align
    -> BaseTypes.Duration -> BaseTypes.Duration
    -> ScoreTime -> ScoreTime
    -> Derive.Deriver ((RealTime, RealTime), (RealTime, RealTime))
pitch_fade_ranges align align_fade fade_time pitch_time start end = do
    let dur_from = case align of
            AlignStart -> start
            AlignEnd -> end
    fade_time <- Call.real_duration dur_from fade_time
    pitch_time <- Call.real_duration dur_from pitch_time
    (fade_start, fade_end) <- case align of
        AlignStart -> (\p -> (p, p + fade_time)) <$> Derive.real start
        AlignEnd -> (\p -> (p - fade_time, p)) <$> Derive.real end
    let (pitch_start, pitch_end) = case align_fade of
            AlignStart ->
                -- Since there's no point doing anything after the fade is
                -- done, extend backwards if pitch is longer than fade.
                min (fade_start, fade_start + pitch_time)
                    (fade_end - pitch_time, fade_end)
            AlignEnd ->
                max (fade_end - pitch_time, fade_end)
                    (fade_start, fade_start + pitch_time)
    return ((pitch_start, pitch_end), (fade_start, fade_end))

segment :: ControlUtil.Curve -> RealTime -> Signal.Y -> RealTime
    -> Signal.Y -> Derive.Deriver Signal.Control
segment curve x1 y1 x2 y2 = do
    sig <- ControlUtil.make_segment curve x1 y1 x2 y2
    -- TODO why do I need this leading sample?
    return $ Signal.from_sample 0 y1 <> sig

pitch_segment :: Align -> ControlUtil.Curve
    -> RealTime -- ^ start pitch at this time
    -> RealTime -- ^ start segment
    -> RealTime -- ^ end segment
    -> PSignal.Pitch
    -> Either Pitch.Transpose PSignal.Pitch -> PitchDirection
    -> Derive.Deriver PSignal.PSignal
pitch_segment align curve start0 start end pitch interval pitch_dir =
    case align of
        -- If the pitch segment is at the start of the note, then I may need to
        -- override its base pitch with a flat segment.
        AlignStart -> (initial dest <>) <$>
            PitchUtil.make_segment curve start dest end pitch
        AlignEnd -> PitchUtil.make_segment curve start pitch end dest
    where
    initial p = PSignal.from_sample start0 p
    dest = case interval of
        Left degrees -> Pitches.transpose (negate_interval degrees) pitch
        Right p -> p
    negate_interval = case pitch_dir of
        PitchDrop -> Pitch.modify_transpose negate
        PitchLift -> id
