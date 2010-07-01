-- | Create val calls for scale degrees.
module Derive.Call.Pitch where

import qualified Util.Num as Num

import Ui
import qualified Ui.Types as Types

import qualified Derive.Call as Call
import qualified Derive.Call.Control as Control
import qualified Derive.Derive as Derive
import qualified Derive.CallSig as CallSig
import qualified Derive.TrackLang as TrackLang
import Derive.CallSig (optional, required)

import qualified Perform.Pitch as Pitch
import qualified Perform.PitchSignal as PitchSignal
import qualified Perform.Signal as Signal


-- | Create a note val call for the given scale degree.  This is intended to
-- be used by scales to generate their val calls, but of course each scale may
-- define degrees in its own way.
--
-- [frac /Num/ @0@] Add this many scale degrees to the output.  Intended for
-- fractional scale degrees.
--
-- [hz /Num/ @0@] Add an absolute hz value to the output.
degree_call :: String
    -> Pitch.Degree -> (Pitch.Degree -> Pitch.Hz -> Pitch.Degree)
    -> Derive.ValCall
degree_call name degree add_hz = Derive.ValCall ("degree: " ++ name) $ \args ->
    CallSig.call2 args (optional "frac" 0, optional "hz" 0) $ \frac hz -> do
        let Pitch.Degree p = add_hz (degree + Pitch.Degree frac) hz
        return $ TrackLang.VNum p

-- * pitch

-- TODO pitch_interpolate, c_note_slide, and c_neighbor all share a certain
-- amount of code.  think of a way to factor that out.

pitch_calls :: Derive.PitchCallMap
pitch_calls = Derive.make_calls
    [ ("=", Call.c_equal Derive.no_pitch)
    , ("", c_note_set)
    , ("set", c_note_set)
    , ("i", c_note_linear)
    , ("e", c_note_exponential)
    , ("s", c_note_slide)

    , ("n", c_neighbor) -- this clearly needs a symbol
    , ("neighbor", c_neighbor)
    ]

c_note_set :: Derive.PitchCall
c_note_set = Derive.generate_one "note_set" $ \args -> CallSig.call1 args
    (required "val") $ \note -> do
        scale <- Derive.require_val TrackLang.v_scale
        pos <- Derive.now
        degree <- Call.lookup_note scale note
        return $ PitchSignal.signal (Pitch.scale_id scale)
            [(pos, PitchSignal.degree_to_y degree)]

c_note_linear :: Derive.PitchCall
c_note_linear = Derive.generate_one "note_linear" $ \args ->
    case Derive.passed_vals args of
        [] -> case Derive.passed_prev_val args of
            Nothing -> return $
                Derive.throw "can't set to previous val when there was none"
            Just (_, prev_y) -> return $ do
                pos <- Derive.now
                scale <- Derive.require_val TrackLang.v_scale
                return $ PitchSignal.signal (Pitch.scale_id scale)
                    [(pos, prev_y)]
        _ -> CallSig.call1 args (required "note") $ \note ->
            pitch_interpolate id note args

c_note_exponential :: Derive.PitchCall
c_note_exponential = Derive.generate_one "note_exponential" $ \args ->
    CallSig.call2 args (required "note", optional "exp" 2) $ \note exp ->
        pitch_interpolate (Control.expon exp) note args

c_note_slide :: Derive.PitchCall
c_note_slide = Derive.generate_one "note_slide" $ \args -> CallSig.call2 args
    (required "note", optional "time" 0.1) $ \note time -> do
        start <- Derive.now
        end <- case Derive.passed_next_begin args of
            Nothing -> return $ start + RealTime time
            Just n -> do
                next <- Derive.score_to_real n
                return $ min (start + RealTime time) next
        scale <- Derive.require_val TrackLang.v_scale
        srate <- Call.get_srate
        degree <- Call.lookup_note scale note
        let signal = PitchSignal.signal (Pitch.scale_id scale)
        case Derive.passed_prev_val args of
                Nothing -> do
                    Derive.warn "no previous value to slide from"
                    return $ signal [(start, PitchSignal.degree_to_y degree)]
                Just (_, prev_y) -> return $ signal $
                    interpolate_pitch True srate id
                        start (PitchSignal.y_to_degree prev_y) end degree

-- | Emit a quick slide from a neighboring pitch in absolute time.
--
-- [neighbor /Number/ @1@] Neighbor note, in scale degrees.
--
-- [time /Number/ @.3@] Duration of ornament, in seconds.
c_neighbor :: Derive.PitchCall
c_neighbor = Derive.generate_one "neighbor" $ \args ->
    CallSig.call3 (Call.default_relative_note args)
    (required "note", optional "neighbor" 1, optional "time" 0.1) $
    \note neighbor time -> do
        start <- Derive.now
        let end = start + RealTime time
        scale <- Derive.require_val TrackLang.v_scale
        degree <- Call.lookup_note scale note
        srate <- Call.get_srate
        let signal = PitchSignal.signal (Pitch.scale_id scale)
        return $ signal $ interpolate_pitch True srate id
            start (Pitch.Degree neighbor + degree) end degree


-- ** pitch util

pitch_interpolate :: (Double -> Signal.Y) -> Pitch.Note
    -> Derive.PassedArgs Derive.Pitch -> Derive.PitchDeriver
pitch_interpolate f note args = do
    start <- Derive.now
    scale <- Derive.require_val TrackLang.v_scale
    srate <- Call.get_srate
    degree <- Call.lookup_note scale note
    let signal = PitchSignal.signal (Pitch.scale_id scale)
    case Derive.passed_prev_val args of
        Nothing -> do
            Derive.warn $ "no previous val to interpolate from"
            return $ signal [(start, PitchSignal.degree_to_y degree)]
        Just (prev, prev_y) -> return $ signal $
            interpolate_pitch False srate f
                prev (PitchSignal.y_to_degree prev_y) start degree

interpolate_pitch :: Bool -> RealTime -> (Double -> Double)
    -> RealTime -> Pitch.Degree -> RealTime -> Pitch.Degree
    -> [(RealTime, PitchSignal.Y)]
interpolate_pitch include_initial srate f x0 y0 x1 y1
    | include_initial = sig
    | otherwise = drop 1 sig
    where
    sig = [(x, (fy0, fy1, y_of x)) | x <- Control.range x0 x1 srate]
    y_of = Num.d2f . f . Types.real_to_double . Num.normalize x0 x1
    (fy0, fy1) = (to_f y0, to_f y1)
    to_f (Pitch.Degree d) = Num.d2f d
