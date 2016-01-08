-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Local.Instrument.Kontakt.Reyong_test where
import Util.Test
import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Ui.UiTest as UiTest
import qualified Derive.Derive as Derive
import qualified Local.Instrument.Kontakt.KontaktTest as KontaktTest
import Global


test_aftertouch = do
    let run notes = extract $ KontaktTest.perform aliases $
            Derive.r_events $ KontaktTest.derive aliases title $
                UiTest.note_spec ("r", notes, [])
        aliases = [("r", "kontakt/reyong")]
        extract (_, midi, logs) = (mapMaybe e_midi midi, logs)
        e_midi msg = case Midi.channel_message $ Midi.wmsg_msg msg of
            Just (m@(Midi.NoteOn {})) -> Just m
            Just (m@(Midi.Aftertouch {})) -> Just m
            _ -> Nothing
    -- AT keyswitch to open, damp to 0, release damp to +mute+open.
    equal (run [(0, 1, "4i")])
        ( [ Midi.Aftertouch Key.c3 127, Midi.Aftertouch Key.c3 0
          , Midi.NoteOn 12 64, Midi.NoteOn Key.c3 127
          ]
        , []
        )
    -- AT keyswitch to open, damp to 1, release damp to +mute+closed.
    equal (run [(0, 1, "%damp=1 | +damp-closed -- 5i")])
        ( [ Midi.Aftertouch Key.c4 127, Midi.Aftertouch Key.c4 122
          , Midi.NoteOn 13 64, Midi.NoteOn Key.c4 127
          ]
        , []
        )
    -- Damp to 0, AT keyswitch to +mute.
    equal (run [(0, 1, "+mute -- 5i")])
        ( [ Midi.Aftertouch Key.c4 0, Midi.Aftertouch Key.c4 126
          , Midi.NoteOn Key.c4 127
          ]
        , []
        )

title :: String
title = "scale=legong | tuning=umbang"
