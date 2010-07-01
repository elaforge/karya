-- | Collect the various calls into one place.
module Derive.Call.All where
import qualified Data.Map as Map

import qualified Derive.Derive as Derive

import qualified Derive.Call.Note as Note
import qualified Derive.Call.Control as Control
import qualified Derive.Call.Pitch as Pitch
import qualified Derive.Call.Echo as Echo
import qualified Derive.Call.Rambat as Rambat
import qualified Derive.Call.Trill as Trill


call_map :: Derive.CallMap
call_map = Derive.CallMap note_calls control_calls pitch_calls val_calls

note_calls :: Derive.NoteCallMap
note_calls = Map.unions [Note.note_calls, Echo.note_calls, Rambat.note_calls,
    Trill.note_calls]

control_calls :: Derive.ControlCallMap
control_calls = Map.unions [Control.control_calls]

pitch_calls :: Derive.PitchCallMap
pitch_calls = Map.unions [Pitch.pitch_calls, Trill.pitch_calls]

val_calls :: Derive.ValCallMap
val_calls = Map.unions []
