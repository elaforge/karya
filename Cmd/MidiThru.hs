module Cmd.MidiThru where

import qualified Midi.Midi as Midi
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg

import qualified Perform.Pitch as Pitch
import qualified Perform.Midi.Controller as Controller
import qualified Perform.Midi.Instrument as Instrument


-- | Send midi thru, remapping notes and controllers to the given Addr.
-- NoteOns and NoteOffs are remapped based on the scale.
cmd_midi_thru :: Pitch.ScaleId -> Instrument.Addr -> Controller.PbRange
    -> Cmd.Cmd
cmd_midi_thru scale_id (wdev, chan) pb_range msg = do
    chan_msg <- Cmd.require $ case Msg.midi msg of
        Just (Midi.ChannelMessage _ m) -> Just m
        _ -> Nothing
    scale <- Cmd.get_scale "cmd_midi_thru" scale_id
    tuned_msgs <- case chan_msg of
        Midi.NoteOn key vel -> input_to_midi scale pb_range True vel
            (Pitch.InputKey (fromIntegral key `divMod` 12))
        Midi.NoteOff key vel -> input_to_midi scale pb_range False vel
            (Pitch.InputKey (fromIntegral key `divMod` 12))
        _ -> return [chan_msg]
    mapM_ (Cmd.midi wdev) (map (Midi.ChannelMessage chan) tuned_msgs)
    return Cmd.Continue

input_to_midi :: (Monad m) => Pitch.Scale -> Controller.PbRange -> Bool
    -> Midi.Velocity -> Pitch.InputKey -> Cmd.CmdT m [Midi.ChannelMessage]
input_to_midi scale pb_range note_on vel input = do
    nn <- input_to_nn scale input
    let (key, pb) = Controller.pitch_to_midi pb_range (Pitch.un_nn nn)
    let set_pb = if Pitch.scale_set_pitch_bend scale
            then [Midi.PitchBend pb] else []
    return $ if note_on
        then set_pb ++ [Midi.NoteOn key vel]
        else [Midi.NoteOff key vel]

input_to_nn :: (Monad m) => Pitch.Scale -> Pitch.InputKey
    -> Cmd.CmdT m Pitch.NoteNumber
input_to_nn scale input = do
    -- input -> note in scale -> nn
    let msg = show (Pitch.scale_id scale) ++ ": "
    note <- case Pitch.scale_input_to_note scale input of
        Nothing -> Cmd.throw $ msg ++ "input out of range: " ++ show input
        Just note -> return note
    case Pitch.scale_note_to_nn scale note of
        Nothing -> Cmd.throw $ msg ++ "can't convert to nn: " ++ show input
        Just nn -> return nn
