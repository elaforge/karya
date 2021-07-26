-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Midi.Play_test where
import qualified Util.Log as Log
import qualified Derive.LEvent as LEvent
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Play as Play

import           Types
import           Util.Test


test_cycle_messages :: Test
test_cycle_messages = do
    let f repeat_at = extract . Play.cycle_messages repeat_at
        extract = map $
            LEvent.either (Left . Midi.wmsg_ts) (Right . Log.msg_string)
        msg = LEvent.Log $ Log.msg Log.Notice Nothing "hi"
    equal (take 5 (f 4 [note 2, msg, note 3]))
        [ Left 2, Right "hi", Left 3
        , Left 6, Left 7
        ]
    equal (f 1 [note 2]) []

note :: RealTime -> LEvent.LEvent Midi.WriteMessage
note ts = LEvent.Event $ Midi.WriteMessage (Midi.write_device "d") ts
    (Midi.ChannelMessage 0 (Midi.NoteOn 10 20))
