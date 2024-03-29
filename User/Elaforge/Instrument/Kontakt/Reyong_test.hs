-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module User.Elaforge.Instrument.Kontakt.Reyong_test where
import qualified Derive.DeriveTest as DeriveTest
import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Ui.UiTest as UiTest
import qualified User.Elaforge.Instrument.Kontakt.KontaktTest as KontaktTest

import           Global
import           Util.Test


test_aftertouch :: Test
test_aftertouch = do
    let run notes = extract $
            DeriveTest.perform_result (KontaktTest.perform allocs) $
            KontaktTest.derive allocs title $ UiTest.note_spec ("r", notes, [])
        allocs = DeriveTest.simple_allocs [("r", "kontakt/reyong")]
        extract ((_, midi), logs) = (mapMaybe e_midi midi, logs)
        e_midi msg = case Midi.channel_message $ Midi.wmsg_msg msg of
            Just (m@(Midi.NoteOn {})) -> Just m
            Just (m@(Midi.Aftertouch {})) -> Just m
            _ -> Nothing
    equal (run [(0, 1, "4i")])
        ([Midi.Aftertouch Key.c4 0, Midi.NoteOn Key.c4 127], [])
    equal (run [(0, 1, "4i"), (1, 1, "4o")])
        ( [ Midi.Aftertouch Key.c4 0, Midi.NoteOn Key.c4 127
          , Midi.Aftertouch Key.d4 0, Midi.NoteOn Key.d4 127
          ]
        , []
        )
    equal (run [(0, 1, "4i"), (1, 1, "+mute -- 4i")])
        ( [ Midi.Aftertouch Key.c4 0, Midi.NoteOn Key.c4 127
          , Midi.Aftertouch Key.c4 1, Midi.NoteOn Key.c4 127
          ]
        , []
        )

title :: Text
title = "scale=selisir | tuning=umbang"
