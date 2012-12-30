module Perform.Midi.Play_test where
import qualified Util.Log as Log
import Util.Test
import qualified Midi.Midi as Midi
import qualified Derive.LEvent as LEvent
import qualified Perform.Midi.Play as Play


test_cycle_messages = do
    let f = extract . Play.cycle_messages (Just 4)
        extract = map $
            LEvent.either (Left . Midi.wmsg_ts) (Right . Log.msg_string)
        msg = LEvent.Log $ Log.msg Log.Notice Nothing "hi"
    equal (take 5 (f [note 2, msg, note 3]))
        [ Left 2, Right "hi", Left 3
        , Left 6, Left 7
        ]

note ts = LEvent.Event $ Midi.WriteMessage (Midi.write_device "d") ts
    (Midi.ChannelMessage 0 (Midi.NoteOn 10 20))
