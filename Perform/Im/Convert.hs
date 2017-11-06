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
import qualified Derive.Env as Env
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.ScoreTypes as ScoreTypes

import qualified Perform.ConvertUtil as ConvertUtil
import qualified Perform.Im.Patch as Patch
import qualified Perform.Signal

import qualified Instrument.Common as Common
import qualified Instrument.InstTypes as InstTypes
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import Global


-- | Serialize the events to the given patch.  This is done atomically because
-- this is run from the derive thread, which can be killed at any time.
write :: (Score.Instrument -> Maybe Cmd.ResolvedInstrument)
    -> FilePath -> Vector.Vector Score.Event -> IO ()
write lookup_inst filename events = do
    notes <- LEvent.write_logs $ convert lookup_inst (Vector.toList events)
    Note.serialize filename notes

convert :: (Score.Instrument -> Maybe Cmd.ResolvedInstrument)
    -> [Score.Event] -> [LEvent.LEvent Note.Note]
convert = ConvertUtil.convert $ \event resolved ->
    case Cmd.inst_backend resolved of
        Just (Cmd.Im patch) -> convert_event event patch name
            where InstTypes.Qualified _ name = Cmd.inst_qualified resolved
        _ -> []

convert_event :: Score.Event -> Patch.Patch -> InstTypes.Name
    -> [LEvent.LEvent Note.Note]
convert_event event patch name = run $ do
    let supported = Patch.patch_controls patch
    let controls = Score.event_transformed_controls event
    pitch <- if Map.member Control.pitch supported
        then Just . convert_signal <$> convert_pitch event
        else return Nothing
    return $ Note.Note
        { patch = name
        , instrument = ScoreTypes.instrument_name (Score.event_instrument event)
        , element = fromMaybe "" $ convert_element_key patch event
        , start = Score.event_start event
        , duration = Score.event_duration event
        , controls = maybe id (Map.insert Control.pitch) pitch $
            convert_controls supported controls
        , control_vals = convert_control_vals supported $
            Score.event_control_vals event
        , attributes = maybe mempty snd $
            Common.lookup_attributes (Score.event_attributes event)
                (Patch.patch_attribute_map patch)
        }

convert_element_key :: Patch.Patch -> Score.Event -> Maybe Text
convert_element_key patch event = do
    key <- Patch.patch_element_key patch
    Env.maybe_val key (Score.event_environ event)

run :: Log.LogT Identity.Identity a -> [LEvent.LEvent a]
run = merge . Identity.runIdentity . Log.run
    where merge (note, logs) = LEvent.Event note : map LEvent.Log logs

convert_signal :: Perform.Signal.Signal a -> Signal.Signal
convert_signal = Perform.Signal.sig_vec

-- TODO trim controls?
convert_controls :: Map Control.Control a -> Score.ControlMap
    -> Map Control.Control Signal.Signal
convert_controls supported controls = Map.fromList
    [ (to_control c, convert_signal sig)
    | (c, ScoreTypes.Typed _ sig) <- Map.toList controls
    , Map.member (to_control c) supported
    ]

to_control :: ScoreTypes.Control -> Control.Control
to_control = Control.Control . ScoreTypes.control_name

convert_control_vals :: Map Control.Control a -> Score.ControlValMap
    -> Map Control.Control Signal.Y
convert_control_vals supported cmap = Map.fromList
    [ (to_control c, v)
    | (c, v) <- Map.toList cmap
    , Map.member (to_control c) supported
    ]

convert_pitch :: Log.LogMonad m => Score.Event -> m Perform.Signal.NoteNumber
convert_pitch event = do
    let (sig, warns) = Score.nn_signal event
    unless (null warns) $ Log.warn $
        "convert pitch: " <> Text.intercalate ", " (map pretty warns)
    return sig
