-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls for 箏.
module Derive.Call.China.Zheng where
import Util.Control
import qualified Util.Num as Num
import qualified Util.Seq as Seq

import qualified Derive.Args as Args
import qualified Derive.Call.Idiom.String as String
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Types


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.call_maps
    [ ("gliss-a", make_gliss "gliss-a" True)
    , ("gliss", make_gliss "gliss" False)
    ]
    [ ("bent-string", String.c_bent_string)
    ]

module_ :: Module.Module
module_ = "china" <> "zheng"


-- * gliss

gliss :: [PitchSignal.Pitch] -> RealTime -> Signal.Y -> Signal.Y -> RealTime
    -> Derive.NoteDeriver
gliss pitches time start_dyn end_dyn end = do
    let dur = time / fromIntegral (length pitches)
        start = end - time
        ts = take (length pitches) (Seq.range_ start dur)
        dyns = map (Num.scale start_dyn end_dyn . RealTime.to_seconds
            . Num.normalize start end) ts
    score_ts <- mapM Derive.score ts
    score_dur <- Util.score_duration end dur
    let note (t, p, dyn) = Derive.place t score_dur $ Util.with_dynamic dyn $
            Util.pitched_note p
    mconcat $ map note $ zip3 score_ts pitches dyns

gliss_pitches :: [PitchSignal.Pitch] -> PitchSignal.Pitch -> Int
    -> Derive.Deriver [PitchSignal.Pitch]
gliss_pitches open_strings dest_pitch gliss_start = do
    dest_nn <- Pitches.pitch_nn dest_pitch
    -- TODO shouldn't need to eval them all
    open_nns <- mapM Pitches.pitch_nn open_strings
    let strings = Seq.sort_on snd $ zip open_strings open_nns
    -- 0 2 4 6 8 10
    return $ if gliss_start >= 0
        -- 5 -> 6 8 10 -> 10 8 6 5
        then reverse $ take gliss_start $ map fst $
            dropWhile ((<=dest_nn) . snd) strings
        -- 5 -> 0 2 4
        else Seq.rtake (-gliss_start) $ map fst $
            takeWhile ((<dest_nn) . snd) strings

-- | Gracelessly factor both forms of glissando.
--
-- The other option would be a single call with an environ to switch between
-- the two behaviours, and expect to bind locally.  But it seems like both
-- would be useful simultaneously, and why not have a reasonable default
-- vocabulary if I can manage it?
make_gliss :: Text -> Bool -> Derive.Generator Derive.Note
make_gliss name is_absolute = Derive.make_call module_ name mempty
    "Glissando along the open strings, taking an absolute amount of time."
    $ Sig.call ((,,,)
    <$> Sig.required "start"
        "Start this many strings above or below the destination pitch."
    <*> (if is_absolute
        then Sig.defaulted "time" (TrackLang.real 0.25)
            "Time in which to play the glissando."
        else Sig.defaulted "time" (TrackLang.real 0.075)
            "Time between each note.")
    <*> Sig.defaulted "dyn" Nothing "Start at this dyn, and interpolate\
        \ to the destination dyn. If not given, the dyn is constant."
    <*> String.open_strings_env
    ) $ \(gliss_start, time, maybe_start_dyn, open_strings) -> Sub.inverting $
    \args -> do
        end <- Args.real_start args
        time <- Util.real_duration end time
        dest_pitch <- Util.get_pitch end
        dest_dyn <- Util.dynamic end
        let start_dyn = fromMaybe dest_dyn maybe_start_dyn
        pitches <- gliss_pitches open_strings dest_pitch gliss_start
        gliss pitches
            (if is_absolute then time else time * fromIntegral (length pitches))
            start_dyn dest_dyn end
            <> Util.placed_note args
