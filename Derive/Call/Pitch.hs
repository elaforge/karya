-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Library of basic low level pitch calls.
--
-- Low level calls should do simple orthogonal things and their names are
-- generally just one or two characters.
module Derive.Call.Pitch where
import qualified Data.Set as Set

import Util.Control
import qualified Util.Num as Num
import qualified Util.Seq as Seq

import qualified Ui.Event as Event
import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Call.Control as Control
import qualified Derive.Call.Equal as Equal
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Sig as Sig
import Derive.Sig (defaulted, required)
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import Types


-- * pitch

pitch_calls :: Derive.CallMaps Derive.Pitch
pitch_calls = Derive.call_maps
    [ ("", c_set)
    , ("set", c_set)
    , ("'", c_set_prev)

    , ("i", c_linear_prev)
    , ("i<<", c_linear_prev_const)
    , ("i>", c_linear_next)
    , ("i>>", c_linear_next_const)
    , ("e", c_exp_prev)
    , ("e<<", c_exp_prev_const)
    , ("e>", c_exp_next)
    , ("e>>", c_exp_next_const)

    , ("n", c_neighbor)
    , ("a", c_approach)
    , ("u", c_up)
    , ("d", c_down)
    , ("p", c_porta)
    ]
    [ ("=", Equal.c_equal) ]

-- | This should contain the calls that require the previous value.  It's used
-- by a hack in 'Derive.Slice.slice'.
require_previous :: Set.Set Text
require_previous = Set.fromList
    ["'", "i>", "i>>", "i<<", "e>", "e>>", "e<<", "a", "u", "d"]

c_set :: Derive.Generator Derive.Pitch
c_set = Derive.generator1 "set" mempty "Emit a pitch with no interpolation." $
    -- This could take a transpose too, but then set has to be in
    -- 'require_previous', it gets shadowed for "" because of scales that use
    -- numbers, and it's not clearly useful.
    Sig.call (required "pitch" "Destination pitch.") $ \pitch args -> do
        pos <- Args.real_start args
        return $ PitchSignal.signal [(pos, pitch)]

-- | Re-set the previous val.  This can be used to extend a breakpoint.
c_set_prev :: Derive.Generator Derive.Pitch
c_set_prev = Derive.generator "set-prev" (Tags.prelude <> Tags.prev)
    "Re-set the previous pitch.  This can be used to extend a breakpoint."
    $ Sig.call0 $ \args -> Args.prev_val args >>= \x -> case x of
        Nothing -> return []
        Just (prev_x, prev_y) -> do
            pos <- Args.real_start args
            return [PitchSignal.signal [(pos, prev_y)] | pos > prev_x]

-- * linear

type Transpose = Either PitchSignal.Pitch Pitch.Transpose

-- | Linear interpolation, with different start times.
linear_interpolation :: (TrackLang.Typecheck time) => Text -> time -> Text
    -> (Derive.PitchArgs -> time -> Derive.Deriver TrackLang.Duration)
    -> Derive.Generator Derive.Pitch
linear_interpolation name time_default time_default_doc get_time =
    Derive.generator1 name Tags.prev doc $ Sig.call
        ((,) <$> pitch_arg <*> defaulted "time" time_default time_doc) $
    \(pitch, time) args -> interpolate id args pitch =<< get_time args time
    where
    doc = "Interpolate from the previous pitch to the given one in a straight\
        \ line."
    time_doc = "Time to reach destination. " <> time_default_doc

c_linear_prev :: Derive.Generator Derive.Pitch
c_linear_prev = linear_interpolation "linear-prev" Nothing
    "If not given, start from the previous sample." Control.default_prev

c_linear_prev_const :: Derive.Generator Derive.Pitch
c_linear_prev_const =
    linear_interpolation "linear-prev-const" (TrackLang.real (-0.1)) "" $
        \_ -> return . TrackLang.default_real

c_linear_next :: Derive.Generator Derive.Pitch
c_linear_next =
    linear_interpolation "linear-next" Nothing
        "If not given, default to the start of the next event." $
    \args maybe_time ->
        return $ maybe (next_dur args) TrackLang.default_real maybe_time
    where next_dur args = TrackLang.Score $ Args.next args - Args.start args

c_linear_next_const :: Derive.Generator Derive.Pitch
c_linear_next_const =
    linear_interpolation "linear-next-const" (TrackLang.real 0.1) "" $
        \_ -> return . TrackLang.default_real


-- * exponential

-- | Exponential interpolation, with different start times.
exponential_interpolation :: (TrackLang.Typecheck time) =>
    Text -> time -> Text
    -> (Derive.PitchArgs -> time -> Derive.Deriver TrackLang.Duration)
    -> Derive.Generator Derive.Pitch
exponential_interpolation name time_default time_default_doc get_time =
    Derive.generator1 name Tags.prev doc $ Sig.call ((,,)
    <$> pitch_arg
    <*> defaulted "exp" 2 Control.exp_doc
    <*> defaulted "time" time_default time_doc
    ) $ \(pitch, exp, time) args ->
        interpolate (Control.expon exp) args pitch =<< get_time args time
    where
    doc = "Interpolate from the previous pitch to the given one in a curve."
    time_doc = "Time to reach destination. " <> time_default_doc

c_exp_prev :: Derive.Generator Derive.Pitch
c_exp_prev = exponential_interpolation "exp-prev" Nothing
    "If not given, start from the previous sample." Control.default_prev

c_exp_prev_const :: Derive.Generator Derive.Pitch
c_exp_prev_const =
    exponential_interpolation "exp-prev-const" (TrackLang.real (-0.1)) "" $
        \_ -> return . TrackLang.default_real

c_exp_next :: Derive.Generator Derive.Pitch
c_exp_next = exponential_interpolation "exp-next" Nothing
        "If not given default to the start of the next event." $
    \args maybe_time ->
        return $ maybe (next_dur args) TrackLang.default_real maybe_time
    where next_dur args = TrackLang.Score $ Args.next args - Args.start args

c_exp_next_const :: Derive.Generator Derive.Pitch
c_exp_next_const =
    exponential_interpolation "exp-next-const" (TrackLang.real 0.1) "" $
        \_ -> return . TrackLang.default_real

pitch_arg :: Sig.Parser Transpose
pitch_arg = required "pitch"
    "Destination pitch, or a transposition from the previous one."

-- * misc

c_neighbor :: Derive.Generator Derive.Pitch
c_neighbor = Derive.generator1 "neighbor" mempty
    ("Emit a slide from a neighboring pitch to the given one."
    ) $ Sig.call ((,,)
    <$> required "pitch" "Destination pitch."
    <*> defaulted "neighbor" (Pitch.Chromatic 1) "Neighobr interval."
    <*> defaulted "time" (TrackLang.real 0.1)
        "Time to get to destination pitch."
    ) $ \(pitch, neighbor, TrackLang.DefaultReal time) args -> do
        (start, end) <- Util.duration_from_start args time
        let pitch1 = Pitches.transpose neighbor pitch
        make_interpolator id True start pitch1 end pitch

c_approach :: Derive.Generator Derive.Pitch
c_approach = Derive.generator1 "approach" Tags.next
    "Slide to the next pitch." $ Sig.call
    ( defaulted "time" (TrackLang.real 0.2) "Time to get to destination pitch."
    ) $ \(TrackLang.DefaultReal time) args -> do
        (start, end) <- Util.duration_from_start args time
        approach args start end

approach :: Derive.PitchArgs -> RealTime -> RealTime
    -> Derive.Deriver PitchSignal.Signal
approach args start end = do
    maybe_next <- next_pitch args
    Args.prev_val args >>= \x -> case (x, maybe_next) of
        (Just (_, prev), Just next) ->
            make_interpolator id True start prev end next
        _ -> return mempty

next_pitch :: Derive.PassedArgs d -> Derive.Deriver (Maybe PitchSignal.Pitch)
next_pitch = maybe (return Nothing) eval_pitch . Seq.head . Args.next_events

eval_pitch :: Event.Event -> Derive.Deriver (Maybe PitchSignal.Pitch)
eval_pitch event =
    justm (either (const Nothing) Just <$> Call.eval_event event) $ \strm -> do
    start <- Derive.real (Event.start event)
    return $ PitchSignal.at start $ mconcat $ LEvent.events_of strm

c_up :: Derive.Generator Derive.Pitch
c_up = Derive.generator1 "up" Tags.prev
    "Ascend at the given speed until the next event." $ slope "Ascend" 1

c_down :: Derive.Generator Derive.Pitch
c_down = Derive.generator1 "down" Tags.prev
    "Descend at the given speed until the next event." $ slope "Descend" (-1)

slope :: Text -> Double -> Derive.WithArgDoc
    (Derive.PitchArgs -> Derive.Deriver PitchSignal.Signal)
slope word sign =
    Sig.call (defaulted "slope" (Pitch.Chromatic 1)
        (word <> " this many steps per second.")) $
    \slope args -> Args.prev_val args >>= \x -> case x of
        Nothing -> return mempty
        Just (_, prev_pitch) -> do
            start <- Args.real_start args
            next <- Derive.real (Args.next args)
            let dest = Pitches.transpose transpose prev_pitch
                transpose = Pitch.modify_transpose
                    (* (RealTime.to_seconds (next - start) * sign)) slope
            make_interpolator id True start prev_pitch next dest

c_porta :: Derive.Generator Derive.Pitch
c_porta = linear_interpolation "porta" (TrackLang.real 0.1)
    "Emit a linear slide from the previous pitch to the given one.\
    \ This is the same as i>>, but intended to be higher level, in that\
    \ instruments or scores can override it to represent an idiomatic\
    \ portamento." $
    \_ -> return . TrackLang.default_real

-- * util

type Interpolator = Bool -- ^ include the initial sample or not
    -> RealTime -> PitchSignal.Pitch -> RealTime -> PitchSignal.Pitch
    -- ^ start -> starty -> end -> endy
    -> PitchSignal.Signal

-- | Create an interpolating call, from a certain duration (positive or
-- negative) from the event start to the event start.
interpolate :: (Double -> Double) -> Derive.PitchArgs
    -> Transpose -> TrackLang.Duration
    -> Derive.Deriver PitchSignal.Signal
interpolate f args pitch_transpose dur = do
    (start, end) <- Util.duration_from_start args dur
    Args.prev_val args >>= \x -> case x of
        Nothing -> return $ case pitch_transpose of
            Left pitch -> PitchSignal.signal [(start, pitch)]
            Right _ -> PitchSignal.signal []
        Just (_, prev) -> do
            -- I always set include_initial.  It might be redundant, but if the
            -- previous call was sliced off, it won't be.
            make_interpolator f True (min start end) prev (max start end) $
                either id (flip Pitches.transpose prev) pitch_transpose

-- | Create samples according to an interpolator function.  The function is
-- passed values from 0--1 representing position in time and is expected to
-- return values from 0--1 representing the Y position at that time.  So linear
-- interpolation is simply @id@.
make_interpolator :: (Double -> Double)
    -> Bool -- ^ include the initial sample or not
    -> RealTime -> PitchSignal.Pitch -> RealTime -> PitchSignal.Pitch
    -> Derive.Deriver PitchSignal.Signal
make_interpolator f include_initial x1 y1 x2 y2 = do
    srate <- Util.get_srate
    return $ (if include_initial then id else PitchSignal.drop 1)
        (interpolate_segment True srate f x1 y1 x2 y2)

-- | This is bundled into 'make_interpolator', but calls still use this to
-- create interpolations.
interpolator :: RealTime -> (Double -> Double) -> Interpolator
interpolator srate f include_initial x1 y1 x2 y2 =
    (if include_initial then id else PitchSignal.drop 1)
        (interpolate_segment True srate f x1 y1 x2 y2)

-- | Interpolate between the given points.
interpolate_segment :: Bool -> RealTime -> (Double -> Double)
    -> RealTime -> PitchSignal.Pitch -> RealTime -> PitchSignal.Pitch
    -> PitchSignal.Signal
interpolate_segment include_end srate f x1 y1 x2 y2 =
    PitchSignal.unfoldr go (Seq.range_ x1 srate)
    where
    go [] = Nothing
    go (x:xs)
        | x >= x2 = if include_end then Just ((x2, y2), []) else Nothing
        | otherwise = Just ((x, y_of x), xs)
    y_of = Pitches.interpolated y1 y2
        . f . Num.normalize (secs x1) (secs x2) . secs
    secs = RealTime.to_seconds
