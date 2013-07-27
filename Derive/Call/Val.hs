-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Val where
import Util.Control
import qualified Util.Num as Num
import qualified Util.Seq as Seq

import qualified Ui.Event as Event
import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Call.Control as Control
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import Derive.Sig (defaulted, required)
import qualified Derive.TrackInfo as TrackInfo
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import Types


val_calls :: Derive.ValCallMap
val_calls = Derive.make_calls
    [ (">", c_next_val)
    , ("<", c_prev_val)
    , ("t", c_timestep)
    , ("ts", c_timestep_reciprocal)
    , ("1/", c_reciprocal)
    , ("nn", c_nn)
    , ("hz", c_hz)

    -- signals
    , ("i>", c_linear_next)
    , ("e>", c_exp_next)
    ]

c_next_val :: Derive.ValCall
c_next_val = Derive.val_call "next-val" Tags.next
    "Evaluate the next event. Only works on pitch and control tracks, and\
    \ if the next event doesn't need its previous event."
    $ Sig.call0 $ \args -> do
        event <- Derive.require "no next event" $
            Seq.head (Args.next_events args)
        start <- Derive.real (Event.start event)
        case Derive.info_track_type (Derive.passed_info args) of
            Just TrackInfo.ControlTrack -> eval_control start event
            Just TrackInfo.TempoTrack -> eval_control start event
            Just TrackInfo.PitchTrack -> do
                signal <- eval event
                case PitchSignal.at start signal of
                    Nothing ->
                        Derive.throw "next pitch event didn't emit a pitch"
                    Just pitch -> return $ TrackLang.VPitch pitch
            Just TrackInfo.NoteTrack -> Derive.throw
                "can't get next value for note tracks"
            Nothing -> Derive.throw "no track type"
    where
    eval_control start event = do
        signal <- eval event
        return $ TrackLang.VNum $ Score.untyped $
            Signal.at start (signal :: Signal.Control)
    eval event = mconcat . LEvent.events_of <$>
        (either Derive.throw return =<< Call.eval_event event)

c_prev_val :: Derive.ValCall
c_prev_val = Derive.val_call "prev-val" Tags.prev
    "Return the previous value. Only works on pitch and control tracks."
    $ Sig.call0 $ \args -> Args.prev_val args >>= \x -> case x of
        Just (_, Derive.TagControl y) -> return $ TrackLang.num y
        Just (_, Derive.TagPitch y) -> return $ TrackLang.VPitch y
        _ -> Derive.throw "no previous value"

eval_pitch :: Event.Event -> Derive.Deriver (Maybe PitchSignal.Pitch)
eval_pitch event =
    justm (either (const Nothing) Just <$> Call.eval_event event) $ \strm -> do
    start <- Derive.real (Event.start event)
    return $ PitchSignal.at start $ mconcat $ LEvent.events_of strm

c_timestep :: Derive.ValCall
c_timestep = Derive.val_call "timestep" mempty
    ("Compute the duration of the given RelativeMark timestep at the current\
    \ position. This is for durations, so it only works with RelativeMark, and\
    \ in fact prepends `r:`, so e.g. a quarter note is just `q`."
    ) $ Sig.call ((,)
    <$> required "rank" "Emit a duration of this rank, as accepted by\
        \ `TimeStep.parse_rank`."
    <*> defaulted "steps" 1 "Step this number of times, negative to step back."
    ) $ \(rank, steps) args ->
        TrackLang.score_time <$>
            Util.parsed_meter_duration (Args.start args) rank steps

c_timestep_reciprocal :: Derive.ValCall
c_timestep_reciprocal = Sig.modify_vcall c_timestep "timestep-reciprocal"
    ("This is the same as `timestep` except it returns the reciprocal. This is\
    \ useful for e.g. trills which take cycles per second rather than duration."
    ) reciprocal
    where
    reciprocal (TrackLang.VNum num) = TrackLang.VNum $ recip <$> num
    reciprocal val = val

c_reciprocal :: Derive.ValCall
c_reciprocal = Derive.val_call "reciprocal" mempty
    "Find the reciprocal of a number. Useful for tempo, e.g. set the tempo to\
    \ 1/time." $ Sig.call (required "num" "") $ \num _ ->
        if num == 0 then Derive.throw "1/0"
            else return $ TrackLang.num (1 / num)

c_nn :: Derive.ValCall
c_nn = Derive.val_call "nn" mempty
    "Convert a pitch or hz to a NoteNumber." $ Sig.call (required "val" "") $
    \val _ -> case val of
        Left pitch -> TrackLang.num . realToFrac <$> Pitches.pitch_nn pitch
        Right hz -> return $ TrackLang.num . realToFrac $ Pitch.hz_to_nn hz

c_hz :: Derive.ValCall
c_hz = Derive.val_call "hz" mempty
    "Convert a pitch or NoteNumber to hz." $ Sig.call (required "val" "") $
    \val _ -> case val of
        Left pitch -> TrackLang.num . Pitch.nn_to_hz <$> Pitches.pitch_nn pitch
        Right nn ->
            return $ TrackLang.num $ Pitch.nn_to_hz (Pitch.NoteNumber nn)

-- * signals

c_linear_next :: Derive.ValCall
c_linear_next = Derive.val_call "linear-next" mempty
    "Create straight lines between the given breakpoints."
    $ Sig.call breakpoints_arg $ \vals args -> do
        (start, end) <- Args.real_range_or_next args
        srate <- Util.get_srate
        return $ TrackLang.VControl $ TrackLang.ControlSignal $ Score.untyped $
            interpolate_breakpoints srate id start end vals

c_exp_next :: Derive.ValCall
c_exp_next = Derive.val_call "exp-next" mempty
    "Create curved lines between the given breakpoints."
    $ Sig.call ((,)
    <$> defaulted "exp" 2 Control.exp_doc
    <*> breakpoints_arg
    ) $ \(exp, vals) args -> do
        (start, end) <- Args.real_range_or_next args
        srate <- Util.get_srate
        return $ TrackLang.VControl $ TrackLang.ControlSignal $ Score.untyped $
            interpolate_breakpoints srate (Control.expon exp) start end vals

breakpoints_arg :: Sig.Parser [Signal.Y]
breakpoints_arg = Sig.many "val" "Breakpoints are distributed evenly between the\
    \ event start and the next event."

interpolate_breakpoints :: RealTime -> (Double -> Double) -> RealTime
    -> RealTime -> [Signal.Y] -> Signal.Control
interpolate_breakpoints srate f start end =
    mconcat . map line . Seq.zip_next . make_breakpoints start end
    where
    line ((x1, y1), Just (x2, y2)) =
        Control.interpolate_segment srate f x1 y1 x2 y2
    line ((x1, y1), Nothing) = Signal.signal [(x1, y1)]

make_breakpoints :: RealTime -> RealTime -> [Signal.Y] -> [(RealTime, Signal.Y)]
make_breakpoints start end vals = case vals of
    [] -> []
    [x] -> [(start, x)]
    _ -> [(Num.scale start end (n / (len - 1)), x)
        | (n, x) <- zip (Seq.range_ 0 1) vals]
    where len = fromIntegral (length vals)
