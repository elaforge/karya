module Midi.Interface_test where
import qualified Data.IORef as IORef

import Util.Control
import Util.Test
import qualified Midi.Interface as Interface
import qualified Midi.Midi as Midi


test_note_tracker = do
    let f = fmap extract . track
        extract = map extract_msg . filter (Midi.is_note . Midi.wmsg_msg)
    let dev1 = Midi.write_device "dev1"
        dev2 = Midi.write_device "dev2"
        note_on k = Midi.NoteOn k 1
        note_off k = Midi.NoteOff k 0
    let end_with_off msgs = map midi_msg msgs ++ [Interface.AllNotesOff 0]
        midi_msg = Interface.Midi . mkmsg

    -- Note off is emitted.
    io_equal (f (end_with_off [(dev1, 0, note_on 10)]))
        [(dev1, 0, note_on 10), (dev1, 0, note_off 10)]
    io_equal (f (end_with_off [(dev1, 0, note_on 1), (dev1, 0, note_on 2)]))
        [ (dev1, 0, note_on 1), (dev1, 0, note_on 2)
        , (dev1, 0, note_off 1), (dev1, 0, note_off 2)
        ]
    io_equal (f (end_with_off [(dev1, 0, note_on 1), (dev1, 1, note_on 1)]))
        [ (dev1, 0, note_on 1), (dev1, 1, note_on 1)
        , (dev1, 0, note_off 1), (dev1, 1, note_off 1)
        ]
    io_equal (f (end_with_off [(dev1, 0, note_on 1), (dev2, 0, note_on 1)]))
        [ (dev1, 0, note_on 1), (dev2, 0, note_on 1)
        , (dev1, 0, note_off 1), (dev2, 0, note_off 1)
        ]

    -- State is reset after the AllNotesOff.
    io_equal (f (end_with_off [(dev1, 0, note_on 10)] ++ end_with_off []))
        [(dev1, 0, note_on 10), (dev1, 0, note_off 10)]

    -- Note is already off.
    io_equal
        (f (end_with_off [(dev1, 0, note_on 10), (dev1, 0, note_off 10)]))
        [(dev1, 0, note_on 10), (dev1, 0, note_off 10)]

mkmsg :: (Midi.WriteDevice, Midi.Channel, Midi.ChannelMessage)
    -> Midi.WriteMessage
mkmsg (dev, chan, msg) =
    Midi.WriteMessage dev 0 (Midi.ChannelMessage chan msg)

extract_msg :: Midi.WriteMessage
    -> (Midi.WriteDevice, Midi.Channel, Midi.ChannelMessage)
extract_msg (Midi.WriteMessage dev _ (Midi.ChannelMessage chan msg)) =
    (dev, chan, msg)
extract_msg msg = error $ "unexpected msg: " ++ show msg

track :: [Interface.Message] -> IO [Midi.WriteMessage]
track msgs = do
    (out, writer) <- make_writer
    tracked <- Interface.note_tracker writer
    mapM_ tracked msgs
    reverse <$> IORef.readIORef out

make_writer :: IO (IORef.IORef [a], a -> IO Bool)
make_writer = do
    msgs <- IORef.newIORef []
    return (msgs, \msg -> IORef.modifyIORef msgs (msg:) >> return True)
