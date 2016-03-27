-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Convert 'Score.Event's to the low-level event format, 'Note.Note'.
module Perform.Im.Convert (write) where
import qualified Control.Monad.Identity as Identity
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import qualified Util.Log as Log
import qualified Cmd.Cmd as Cmd
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Controls as Controls
import qualified Derive.LEvent as LEvent
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.ScoreTypes as ScoreTypes

import qualified Perform.ConvertUtil as ConvertUtil
import qualified Perform.Im.Patch as Patch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal

import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Instrument.InstTypes as InstTypes

import qualified Synth.Sampler.Control as Control
import qualified Synth.Sampler.Note as Note
import qualified Synth.Sampler.Signal as Signal

import Global


-- | Serialize the events to the given patch.  This is done atomically because
-- this is run from the derive thread, which can be killed at any time.
write :: (Score.Instrument -> Maybe (Cmd.Inst, InstTypes.Qualified))
    -> FilePath -> Vector.Vector Score.Event -> IO ()
write lookup_inst filename events = do
    notes <- LEvent.write_logs $ convert lookup_inst (Vector.toList events)
    Note.serialize filename notes

convert :: (Score.Instrument -> Maybe (Cmd.Inst, InstTypes.Qualified))
    -> [Score.Event] -> [LEvent.LEvent Note.Note]
convert = ConvertUtil.convert $ \event backend name -> case backend of
    Inst.Im patch -> convert_event event patch name
    _ -> []

convert_event :: Score.Event -> Patch.Patch -> InstTypes.Name
    -> [LEvent.LEvent Note.Note]
convert_event event patch name = run $ do
    let supported = Patch.patch_controls patch
        -- TODO trim controls
        controls = Score.event_transformed_controls event
    pitch <- if Map.member (to_control Control.pitch) supported
        then Just . convert_signal <$>
            convert_pitch (Score.event_environ event) controls
                (Score.event_transformed_pitch event)
        else return Nothing
    return $ Note.Note
        { instrument = name
        , start = RealTime.to_seconds $ Score.event_start event
        , duration = RealTime.to_seconds $ Score.event_duration event
        , controls =
            let converted = convert_controls controls
            in maybe converted (\p -> Map.insert Control.pitch p converted)
                pitch
        , attributes = fromMaybe mempty $
            Common.lookup_attributes (Score.event_attributes event)
                (Patch.patch_attribute_map patch)
        }

run :: Log.LogT Identity.Identity a -> [LEvent.LEvent a]
run = merge . Identity.runIdentity . Log.run
    where merge (note, logs) = LEvent.Event note : map LEvent.Log logs

-- | TODO use the same type
to_control :: Control.Control -> Score.Control
to_control (Control.Control a) = ScoreTypes.Control a

-- | TODO use the same type... but won't I need different interpolation
-- behaviour?  Also I kind of like the simpler monomorphic version?
-- This implementation is really inefficient but for testing I don't care.
convert_signal :: Perform.Signal.Signal a -> Signal.Signal
convert_signal = Signal.fromList . map (first RealTime.to_seconds)
    . Perform.Signal.unsignal

convert_controls :: Score.ControlMap -> Map.Map Control.Control Signal.Signal
convert_controls controls = Map.fromList $ concat
    [ [ (Control.envelope, convert_signal (Score.typed_val sig))
      | Just sig <- [Map.lookup Controls.dynamic controls]
      ]
    ]

convert_pitch :: Log.LogMonad m => BaseTypes.Environ
    -> Score.ControlMap -> PSignal.PSignal -> m Perform.Signal.NoteNumber
convert_pitch env controls psig = do
    let (sig, nn_errs) = PSignal.to_nn $ PSignal.apply_controls controls $
            PSignal.apply_environ env psig
    unless (null nn_errs) $ Log.warn $
        "convert pitch: " <> Text.intercalate ", " (map pretty nn_errs)
    return sig
