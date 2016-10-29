-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls for Balinese instruments.
module Cmd.Instrument.Bali where
import qualified Util.Doc as Doc
import qualified Cmd.Cmd as Cmd
import qualified Cmd.InputNote as InputNote
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Cmd.MidiThru as MidiThru
import qualified Cmd.Msg as Msg
import qualified Cmd.NoteEntry as NoteEntry
import qualified Cmd.Perf as Perf
import qualified Cmd.Selection as Selection

import qualified Derive.Args as Args
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call.Bali.Gangsa as Gangsa
import qualified Derive.Call.Prelude.Articulation as Articulation
import qualified Derive.Call.Prelude.Note as Note
import qualified Derive.Call.Sub as Sub
import qualified Derive.Eval as Eval
import qualified Derive.Instrument.DUtil as DUtil
import qualified Derive.ShowVal as ShowVal

import Global


-- | Emit events for both polos and sangsih.
pasang_code :: MidiInst.Code
pasang_code = MidiInst.cmd pasang_thru

-- | Dispatch MIDI through to both polos and sangsih instruments.
pasang_thru :: Cmd.M m => Msg.Msg -> m Cmd.Status
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
        track <- Selection.track
        polos <- Perf.lookup_val track Gangsa.inst_polos
        sangsih <- Perf.lookup_val track Gangsa.inst_sangsih
        whenJust polos $ \inst -> do
            attrs <- Cmd.get_instrument_attributes inst
            MidiThru.midi_thru_instrument inst attrs input
        whenJust sangsih $ \inst -> do
            attrs <- Cmd.get_instrument_attributes inst
            MidiThru.midi_thru_instrument inst attrs $
                InputNote.multiply_note_id 1 input
        return Cmd.Continue

zero_dur_mute :: MidiInst.Code
zero_dur_mute = zero_dur_reapply Articulation.mute_call
    " By default, this will emit a muted note, but the instrument can override\
    \ it as appropriate."

zero_dur_reapply :: BaseTypes.Symbol -> Doc.Doc -> MidiInst.Code
zero_dur_reapply mute_call doc = MidiInst.note_calls $ MidiInst.null_call $
    DUtil.zero_duration "note"
    ("When the event has zero duration, dispatch to the "
        <> ShowVal.doc mute_call <> " call." <> doc)
    (\args -> Eval.reapply_call (Args.context args) mute_call [])
    (Sub.inverting $ Note.default_note Note.use_attributes)
