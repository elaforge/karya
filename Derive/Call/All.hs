-- | Collect the various calls into one place.
module Derive.Call.All where
import qualified Data.Map as Map

import qualified Util.Map as Map
import qualified Derive.Call.Attribute as Attribute
import qualified Derive.Call.Block as Block
import qualified Derive.Call.Control as Control
import qualified Derive.Call.Echo as Echo
import qualified Derive.Call.Gender as Gender
import qualified Derive.Call.Idiom.String as String
import qualified Derive.Call.Integrate as Integrate
import qualified Derive.Call.Note as Note
import qualified Derive.Call.NoteTransformer as NoteTransformer
import qualified Derive.Call.Ornament as Ornament
import qualified Derive.Call.Pitch as Pitch
import qualified Derive.Call.Post.NegativeDur as NegativeDur
import qualified Derive.Call.Post.Reverse as Reverse
import qualified Derive.Call.Random as Random
import qualified Derive.Call.Sekar as Sekar
import qualified Derive.Call.Trill as Trill
import qualified Derive.Derive as Derive
import qualified Derive.TrackLang as TrackLang


scope :: Derive.Scope
scope = Derive.Scope
    { Derive.scope_note = note_lookups
    , Derive.scope_control = control_lookups
    , Derive.scope_pitch = map_lookup pitch_calls
    , Derive.scope_val = Derive.empty_scope_type
        { Derive.stype_builtin = [Derive.map_val_lookup val_calls] }
    }

-- | Note calls are special in that they look for a block with that name first.
note_lookups :: Derive.ScopeType Derive.NoteCall
note_lookups = Derive.empty_scope_type { Derive.stype_builtin =
    [Block.lookup_note_block, Derive.map_lookup note_calls] }

-- | Well ok, control calls are special too.
control_lookups :: Derive.ScopeType Derive.ControlCall
control_lookups = Derive.empty_scope_type
    { Derive.stype_builtin =
        [ Block.lookup_control_block
        , Derive.map_lookup control_calls
        ]
    }

map_lookup :: Map.Map TrackLang.CallId (Derive.Call d)
    -> Derive.ScopeType (Derive.Call d)
map_lookup cmap = Derive.empty_scope_type
    { Derive.stype_builtin = [Derive.map_lookup cmap] }

note_calls :: Derive.NoteCallMap
(note_calls, shadowed_notes) = unions
    [ Attribute.note_calls, Block.note_calls, Echo.note_calls
    , Gender.note_calls, Integrate.note_calls
    , NegativeDur.note_calls, Note.note_calls, NoteTransformer.note_calls
    , Ornament.note_calls , Random.note_calls, Reverse.note_calls
    , Sekar.note_calls, String.note_calls, Trill.note_calls
    ]

control_calls :: Derive.ControlCallMap
(control_calls, shadowed_controls) = unions
    [Control.control_calls, Random.control_calls, Trill.control_calls]

pitch_calls :: Derive.PitchCallMap
(pitch_calls, shadowed_pitches) = unions
    [Pitch.pitch_calls, Random.pitch_calls, Trill.pitch_calls]

val_calls :: Derive.ValCallMap
(val_calls, shadowed_vals) = unions
    [Random.val_calls]

unions :: [Map.Map TrackLang.Symbol a]
    -> (Map.Map TrackLang.Symbol a, [TrackLang.Symbol])
unions fms = (m, Map.keys shadowed)
    where (m, shadowed) = Map.unique_unions fms
