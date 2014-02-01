-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Library of basic high level pitch calls.
--
-- High level calls do something a little more abstract and \"musical\"
-- than the low level calls in "Derive.Call.Pitch".  Generally they have
-- complete-word names, while low level calls are just single letters.
--
-- TODO this module has a dumb name.  What would be better?
module Derive.Call.PitchHigh where
import Util.Control
import qualified Derive.Args as Args
import qualified Derive.Call.Control as Control
import qualified Derive.Call.Pitch as Call.Pitch
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import Derive.Sig (defaulted)
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import Types


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.call_maps []
    [ ("lift", c_lift_note)
    , ("drop", c_drop_note)
    , ("Lift", c_lift_note_start)
    , ("Drop", c_drop_note_start)
    ]

pitch_calls :: Derive.CallMaps Derive.Pitch
pitch_calls = Derive.call_maps
    [ ("drop", c_drop)
    , ("lift", c_lift)
    , ("ad", c_approach_dyn)
    ]
    []

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

make_note_fade :: Text -> Text -> PitchDirection -> Align -> Align
    -> Derive.Transformer Derive.Note
make_note_fade name doc pitch_dir align align_fade =
    Derive.transformer name Tags.under_invert doc
    $ Sig.callt fade_args
    $ \(interval, TrackLang.DefaultReal time, mb_fade) args deriver -> do
        let fade = case mb_fade of
                Nothing -> time
                Just (TrackLang.DefaultReal t) -> t
        ranges@((pitch_start, _), _) <- pitch_fade_ranges align align_fade
            fade time (Args.start args) (Args.end args)
        Derive.pitch_at pitch_start >>= \x -> case x of
            Nothing -> deriver
            Just pitch -> do
                (slide, dyn) <- pitch_fade align pitch pitch_dir interval ranges
                pitch_sig <- Internal.get_dynamic Derive.state_pitch
                let merged = case align of
                        -- Since the initial slide has to override the base
                        -- pitch, I can't just merge normally.
                        AlignStart -> PitchSignal.prepend slide pitch_sig
                        AlignEnd -> pitch_sig <> slide
                multiply_dyn dyn $ Derive.with_pitch Nothing merged deriver

multiply_dyn :: Signal.Control -> Derive.Deriver a -> Derive.Deriver a
multiply_dyn = Derive.with_multiplied_control Score.c_dynamic . Score.untyped

fade_args :: Sig.Parser (Either Pitch.Transpose PitchSignal.Pitch,
    TrackLang.DefaultReal, Maybe TrackLang.DefaultReal)
fade_args = ((,,)
    <$> defaulted "interval" (Left (Pitch.Chromatic 7))
        "Interval or destination pitch."
    <*> defaulted "time" (TrackLang.real 0.25) "Time to the destination pitch."
    <*> defaulted "fade" Nothing
        "Time to fade from or to nothing. If the fade is longer than the pitch\
        \ time, the pitch will finish moving before the dyn has faded out."
    )

-- * pitch calls

c_drop :: Derive.Generator Derive.Pitch
c_drop = make_pitch_fade "drop" "Drop pitch and `dyn`." PitchDrop

c_lift :: Derive.Generator Derive.Pitch
c_lift = make_pitch_fade "lift"
    "Lift pitch and drop `dyn`. This is the same as `drop`, except that it\
    \ defaults to going up instead of down."
    PitchLift

make_pitch_fade :: Text -> Text -> PitchDirection
    -> Derive.Generator Derive.Pitch
make_pitch_fade name doc pitch_dir = Derive.generator1 name Tags.cmod doc
    $ Sig.call fade_args
    $ \(interval, TrackLang.DefaultReal time, mb_fade) args -> do
        let fade = case mb_fade of
                Nothing -> time
                Just (TrackLang.DefaultReal t) -> t
        Args.prev_val args >>= \x -> case x of
            Nothing -> return mempty
            Just (_, prev_pitch) -> do
                next <- Derive.real (Args.next args)
                (slide, dyn) <- pitch_fade AlignEnd prev_pitch pitch_dir
                    interval =<< pitch_fade_ranges AlignStart AlignStart
                        fade time (Args.start args) (Args.start args)
                Control.multiply_dyn next dyn
                return slide

c_approach_dyn :: Derive.Generator Derive.Pitch
c_approach_dyn = Derive.generator1 "approach-dyn" (Tags.cmod <> Tags.next)
    "Like `approach`, slide to the next pitch, but also drop the `dyn`."
    $ Sig.call ((,)
    <$> defaulted "time" (TrackLang.real 0.2)
        "Time to get to destination pitch and dyn."
    <*> defaulted "dyn" 0.25 "Drop `dyn` by this factor."
    ) $ \(TrackLang.DefaultReal time, dyn) args -> do
        (start, end) <- Util.duration_from_start args time
        Control.multiply_dyn end
            =<< Control.make_signal id start 1 end dyn
        Call.Pitch.approach args start end

-- * fade implementation

data Align = AlignStart | AlignEnd deriving (Show)
data PitchDirection = PitchDrop | PitchLift deriving (Show)

pitch_fade :: Align -> PitchSignal.Pitch -> PitchDirection
    -> Either Pitch.Transpose PitchSignal.Pitch
    -> ((RealTime, RealTime), (RealTime, RealTime))
    -> Derive.Deriver (PitchSignal.Signal, Signal.Control)
pitch_fade align pitch pitch_dir interval
        ((pitch_start, pitch_end), (fade_start, fade_end)) =
    (,) <$> pitch_segment align (min pitch_start fade_start) pitch_start
                pitch_end pitch interval pitch_dir
        <*> segment id fade_start dyn1 fade_end dyn2
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
    -> TrackLang.Duration -> TrackLang.Duration
    -> ScoreTime -> ScoreTime
    -> Derive.Deriver ((RealTime, RealTime), (RealTime, RealTime))
pitch_fade_ranges align align_fade fade_time pitch_time start end = do
    let dur_from = case align of
            AlignStart -> start
            AlignEnd -> end
    fade_time <- Util.real_dur' dur_from fade_time
    pitch_time <- Util.real_dur' dur_from pitch_time
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

segment :: (Double -> Double) -> RealTime -> Signal.Y -> RealTime
    -> Signal.Y -> Derive.Deriver Signal.Control
segment f x1 y1 x2 y2 = do
    sig <- Control.make_signal f x1 y1 x2 y2
    return $ Signal.signal [(0, y1)] <> sig

pitch_segment :: Align -> RealTime -- ^ start pitch at this time
    -> RealTime -- ^ start segment
    -> RealTime -- ^ end segment
    -> PitchSignal.Pitch
    -> Either Pitch.Transpose PitchSignal.Pitch -> PitchDirection
    -> Derive.Deriver PitchSignal.Signal
pitch_segment align start0 start end pitch interval pitch_dir = case align of
    -- If the pitch segment is at the start of the note, then I may need to
    -- override its base pitch with a flat segment.
    AlignStart -> (initial dest <>) <$>
        Call.Pitch.make_interpolator id False start dest end pitch
    AlignEnd -> Call.Pitch.make_interpolator id False start pitch end dest
    where
    initial p = PitchSignal.signal [(start0, p)]
    dest = case interval of
        Left degrees -> Pitches.transpose (negate_interval degrees) pitch
        Right p -> p
    negate_interval = case pitch_dir of
        PitchDrop -> Pitch.modify_transpose negate
        PitchLift -> id
