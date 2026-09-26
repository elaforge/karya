-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Midi.Interface_test where
import qualified Data.IORef as IORef

import qualified Midi.Interface as Interface
import qualified Midi.Midi as Midi

import           Global
import           Util.Test


test_note_tracker :: Test
test_note_tracker = do
    let f = fmap extract . track
        extract = map extractMsg . filter (Midi.isNote . Midi.wmsgMsg)
    let dev1 = Midi.writeDevice "dev1"
        dev2 = Midi.writeDevice "dev2"
        noteOn k = Midi.NoteOn k 1
        noteOff k = Midi.NoteOff k 0
    let endWithOff msgs = map midiMsg msgs ++ [Interface.AllNotesOff 0]
        midiMsg = Interface.Midi . mkmsg

    -- Note off is emitted.
    ioEqual (f (endWithOff [(dev1, 0, noteOn 10)]))
        [(dev1, 0, noteOn 10), (dev1, 0, noteOff 10)]
    -- Nested NoteOns emit multiple NoteOffs.  Most synths don't care about
    -- this, but some (SWAM strings at least) actually track them.
    ioEqual (f (endWithOff [(dev1, 0, noteOn 10), (dev1, 0, noteOn 10)]))
        [ (dev1, 0, noteOn 10), (dev1, 0, noteOn 10)
        , (dev1, 0, noteOff 10), (dev1, 0, noteOff 10)
        ]
    ioEqual (f (endWithOff [(dev1, 0, noteOn 1), (dev1, 0, noteOn 2)]))
        [ (dev1, 0, noteOn 1), (dev1, 0, noteOn 2)
        , (dev1, 0, noteOff 1), (dev1, 0, noteOff 2)
        ]
    ioEqual (f (endWithOff [(dev1, 0, noteOn 1), (dev1, 1, noteOn 1)]))
        [ (dev1, 0, noteOn 1), (dev1, 1, noteOn 1)
        , (dev1, 0, noteOff 1), (dev1, 1, noteOff 1)
        ]
    ioEqual (f (endWithOff [(dev1, 0, noteOn 1), (dev2, 0, noteOn 1)]))
        [ (dev1, 0, noteOn 1), (dev2, 0, noteOn 1)
        , (dev1, 0, noteOff 1), (dev2, 0, noteOff 1)
        ]

    -- State is reset after the AllNotesOff.
    ioEqual (f (endWithOff [(dev1, 0, noteOn 10)] ++ endWithOff []))
        [(dev1, 0, noteOn 10), (dev1, 0, noteOff 10)]

    -- Note is already off.
    ioEqual
        (f (endWithOff [(dev1, 0, noteOn 10), (dev1, 0, noteOff 10)]))
        [(dev1, 0, noteOn 10), (dev1, 0, noteOff 10)]

mkmsg :: (Midi.WriteDevice, Midi.Channel, Midi.ChannelMessage)
    -> Midi.WriteMessage
mkmsg (dev, chan, msg) =
    Midi.WriteMessage dev 0 (Midi.ChannelMessage chan msg)

extractMsg :: Midi.WriteMessage
    -> (Midi.WriteDevice, Midi.Channel, Midi.ChannelMessage)
extractMsg (Midi.WriteMessage dev _ (Midi.ChannelMessage chan msg)) =
    (dev, chan, msg)
extractMsg msg = error $ "unexpected msg: " ++ show msg

track :: [Interface.Message] -> IO [Midi.WriteMessage]
track msgs = do
    (out, writer) <- makeWriter
    tracked <- Interface.noteTracker False writer
    mapM_ tracked msgs
    reverse <$> IORef.readIORef out

makeWriter :: IO (IORef.IORef [a], a -> IO (Maybe Text))
makeWriter = do
    msgs <- IORef.newIORef []
    pure (msgs, \msg -> IORef.modifyIORef msgs (msg:) >> pure Nothing)
