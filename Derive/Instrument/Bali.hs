-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls for Balinese instruments.
module Derive.Instrument.Bali where
import Util.Control
import qualified Cmd.Cmd as Cmd
import qualified Cmd.InputNote as InputNote
import qualified Cmd.MidiThru as MidiThru
import qualified Cmd.Msg as Msg
import qualified Cmd.NoteEntry as NoteEntry
import qualified Cmd.Perf as Perf
import qualified Cmd.Selection as Selection

import qualified Derive.Call.Bali.Kotekan as Kotekan
import qualified App.MidiInst as MidiInst


-- | Emit events for both polos and sangsih.
pasang_code :: MidiInst.Code
pasang_code = MidiInst.cmd pasang_thru

-- | Dispatch MIDI through to both polos and sangsih instruments.
pasang_thru :: Cmd.Cmd
pasang_thru msg = do
    NoteEntry.run_cmds_with_input [cmd] msg
    -- This cmd just does midi thru, so I don't want it to return Done and
    -- prevent track editing cmds from happening.
    return Cmd.Continue
    where
    cmd msg = do
        input <- case msg of
            Msg.InputNote input -> return input
            _ -> Cmd.abort
        (block_id, _, track_id, _) <- Selection.get_insert
        polos <- Perf.lookup_val block_id (Just track_id) Kotekan.inst_polos
        sangsih <- Perf.lookup_val block_id (Just track_id) Kotekan.inst_sangsih
        whenJust polos $ \inst ->
            MidiThru.midi_thru_instrument inst input
        whenJust sangsih $ \inst ->
            MidiThru.midi_thru_instrument inst $
                InputNote.multiply_note_id 1 input
        return Cmd.Continue
