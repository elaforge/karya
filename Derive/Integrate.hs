module Derive.Integrate where
{-
Record starts play and sets record_start time.  Record continues until I hit
"stop", which may be longer than the playback.

Record prepends all received msgs to the rec_buffer.  Stop record looks at the
record state.  If it's been recording, it passes the record state to the
integrator, which will modify the State accordingly.

The integrator comes out of the schema, and is a function
'RecordConfig -> RecordState -> Cmd'.  The default one
-}

{-
data RecordState {
    rec_start :: Timestamp.Timestamp
    , rec_track :: (Block.BlockId, Block.TrackNum)
    , rec_buffer :: [Midi.ReadMessage]
    }

-- | Filter data that's recorded.  Of course, if a track can't be found, the
-- data will be tossed (or say saved so you can add a track?  or auto-add
-- a track?).
data RecordConfig {
    rconfig_notes :: Bool
    , rconfig_velocity :: Bool
    , rconfig_controllers :: Map.Map Midi.Controller Controller.Controller
    }


cmd_start_recording block_id tracknum = do
    now <- get_now_timestamp
    let rec_state = RecordState now (block_id, tracknum) []
    Cmd.modify_state $ \st ->
        st { Cmd.state_record = Just (rec_state, cmd_record) }

cmd_stop_recording = do
    rec <- require_rec_state "stop recording"
    let (block_id, _) = rec_track rec
    integrate <- get_integrator block_id
    integrate rec_state

cmd_record msg = do
    rec_state <- Maybe.fromMaybe
        (Cmd.throw "record cmd active with no record state")
        (fmap Cmd.state_record Cmd.get_state)
    midi_msg <- Cmd.require $ case msg of
        Msg.Midi msg -> Just msg
        _ -> Nothing
    let buf = midi_msg : rec_buffer rec_state
    Cmd.put_state $
        rec_state { Cmd.state_record = rec_state { rec_buffer = buf } }

-}
