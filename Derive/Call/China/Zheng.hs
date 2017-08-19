-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls for 箏.
module Derive.Call.China.Zheng where
import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Derive.Args as Args
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call as Call
import qualified Derive.Call.Europe.Grace as Grace
import qualified Derive.Call.Idiom.String as String
import qualified Derive.Call.India.Gamakam as Gamakam
import qualified Derive.Call.Ly as Ly
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Prelude.Trill as Trill
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Expr as Expr
import qualified Derive.PSignal as PSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.Typecheck as Typecheck

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Global
import Types


module_ :: Module.Module
module_ = "china" <> "zheng"

note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.call_maps
    ([ ("gliss-a", make_gliss "gliss-a" True)
    , ("gliss", make_gliss "gliss" False)
    ] ++ trill_variations c_note_trill)
    [ ("bent-string", Derive.set_module module_ String.c_bent_string)
    ]

pitch_calls :: Derive.CallMaps Derive.Pitch
pitch_calls = Derive.generator_call_map $ trill_variations c_pitch_trill

trill_variations :: (Maybe Trill.Direction -> call) -> [(Expr.Symbol, call)]
trill_variations make =
    [ (Expr.Symbol $ "tr" <> Trill.direction_affix end, make end)
    | end <- dirs
    ]
    where dirs = [Nothing, Just Trill.High, Just Trill.Low]

-- * gliss

-- | Gracelessly factor both forms of glissando.
--
-- The other option would be a single call with an environ to switch between
-- the two behaviours, and expect to bind locally.  But it seems like both
-- would be useful simultaneously, and why not have a reasonable default
-- vocabulary if I can manage it?
make_gliss :: Derive.CallName -> Bool -> Derive.Generator Derive.Note
make_gliss name is_absolute = Derive.generator module_ name mempty
    "Glissando along the open strings. The standard version divides the `time`\
    \ among the number of notes, while the -a (absolute) version gives `time`\
    \ to each note."
    $ Sig.call ((,,,)
    <$> Sig.required "start"
        "Start this many strings above or below the destination pitch."
    <*> (if is_absolute
        then Sig.defaulted "time" (Typecheck.real 0.25)
            "Time in which to play the glissando."
        else Sig.defaulted "time" (Typecheck.real 0.075)
            "Time between each note.")
    <*> Sig.defaulted "dyn" Nothing "Start at this dyn, and interpolate\
        \ to the destination dyn. If not given, the dyn is constant."
    <*> String.open_strings_env
    ) $ \(gliss_start, time, maybe_start_dyn, open_strings) -> Sub.inverting $
    \args -> do
        end <- Args.real_start args
        time <- Call.real_duration end time
        dest_pitch <- Call.get_transposed end
        dest_dyn <- Call.dynamic end
        let start_dyn = fromMaybe dest_dyn maybe_start_dyn
        pitches <- gliss_pitches open_strings dest_pitch gliss_start
        let total_time = if is_absolute then time
                else time * fromIntegral (length pitches)
        Ly.when_lilypond (Grace.lily_grace args (end - time) pitches) $
            gliss pitches total_time start_dyn dest_dyn end
                <> Call.placed_note args

gliss_pitches :: [PSignal.Pitch] -> PSignal.Transposed -> Int
    -> Derive.Deriver [PSignal.Pitch]
gliss_pitches open_strings dest_pitch gliss_start = do
    dest_nn <- Pitches.pitch_nn dest_pitch
    -- TODO shouldn't need to eval them all
    open_nns <- mapM (Pitches.pitch_nn . PSignal.coerce) open_strings
    let strings = Seq.sort_on snd $ zip open_strings open_nns
    -- 0 2 4 6 8 10
    return $ if gliss_start >= 0
        -- 5 -> 6 8 10 -> 10 8 6 5
        then reverse $ take gliss_start $ map fst $
            dropWhile ((<=dest_nn) . snd) strings
        -- 5 -> 0 2 4
        else Seq.rtake (-gliss_start) $ map fst $
            takeWhile ((<dest_nn) . snd) strings

gliss :: [PSignal.Pitch] -> RealTime -> Signal.Y -> Signal.Y -> RealTime
    -> Derive.NoteDeriver
gliss pitches time start_dyn end_dyn end = do
    let dur = time / fromIntegral (length pitches)
        start = end - time
        ts = take (length pitches) (Seq.range_ start dur)
        dyns = map (Num.scale start_dyn end_dyn . RealTime.to_seconds
            . Num.normalize start end) ts
    score_ts <- mapM Derive.score ts
    score_dur <- Call.score_duration end dur
    let note (t, p, dyn) = Derive.place t score_dur $ Call.with_dynamic dyn $
            Call.pitched_note p
    mconcat $ map note $ zip3 score_ts pitches dyns

-- * trill

c_note_trill :: Maybe Trill.Direction -> Derive.Generator Derive.Note
c_note_trill start_dir = Derive.generator module_ "tr" Tags.ly
    "A trill with smooth transitions."
    $ Sig.call ((,,) <$> neighbor_arg <*> speed_arg <*> Trill.hold_env
    ) $ \(neighbor, speed, hold) -> Sub.inverting $ \args ->
    Ly.note_code (Ly.AppendFirst, "\\trill") args $ do
        pitch <- Call.get_pitch =<< Args.real_start args
        sig <- trill_signal start_dir pitch neighbor speed hold args
        Derive.with_pitch sig $ Call.placed_note args

c_pitch_trill :: Maybe Trill.Direction -> Derive.Generator Derive.Pitch
c_pitch_trill start_dir = Derive.generator1 module_ "tr" mempty
    "A trill with smooth transitions."
    $ Sig.call ((,,,)
    <$> Sig.required "pitch" "Base pitch."
    <*> neighbor_arg <*> speed_arg <*> Trill.hold_env
    ) $ \(pitch, neighbor, speed, hold) args ->
        trill_signal start_dir pitch neighbor speed hold args

trill_signal :: Maybe Trill.Direction -> PSignal.Pitch
    -> BaseTypes.ControlRef -> BaseTypes.ControlRef -> BaseTypes.Duration
    -> Derive.PassedArgs a -> Derive.Deriver PSignal.PSignal
trill_signal start_dir pitch neighbor speed hold args = do
    (neighbor, control) <- Call.to_transpose_function Typecheck.Nn neighbor
    transpose <- Gamakam.kampita start_dir Nothing Trill.Shorten neighbor
        speed transition hold lilt args
    start <- Args.real_start args
    return $ PSignal.apply_control control
        (Score.untyped transpose) $ PSignal.signal [(start, pitch)]
    where
    transition :: RealTime
    transition = 0.08
    lilt = 0

neighbor_arg :: Sig.Parser BaseTypes.ControlRef
neighbor_arg = Sig.defaulted "neighbor"
    (Sig.typed_control "tr-neighbor" 1 Score.Nn)
    "Alternate with a pitch at this interval."

speed_arg :: Sig.Parser BaseTypes.ControlRef
speed_arg = Sig.defaulted "speed" (Sig.typed_control "tr-speed" 20 Score.Real)
    "Alternate pitches at this speed."
