-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Collect the various calls into one place.
module Derive.Call.All where
import qualified Data.Map as Map

import Util.Control
import qualified Util.Map as Map
import qualified Derive.Call.Articulation as Articulation
import qualified Derive.Call.Block as Block
import qualified Derive.Call.Control as Control
import qualified Derive.Call.Echo as Echo
import qualified Derive.Call.Gender as Gender
import qualified Derive.Call.Grace as Grace
import qualified Derive.Call.Idiom.String as String
import qualified Derive.Call.Integrate as Integrate
import qualified Derive.Call.Lily as Lily
import qualified Derive.Call.Note as Note
import qualified Derive.Call.NoteTransformer as NoteTransformer
import qualified Derive.Call.Pitch as Pitch
import qualified Derive.Call.Post.ArrivalNote as Post.ArrivalNote
import qualified Derive.Call.Post.Idiom as Post.Idiom
import qualified Derive.Call.Post.Reverse as Post.Reverse
import qualified Derive.Call.Random as Random
import qualified Derive.Call.Reyong as Reyong
import qualified Derive.Call.Sekar as Sekar
import qualified Derive.Call.Trill as Trill
import qualified Derive.Call.Val as Val
import qualified Derive.Derive as Derive
import qualified Derive.TrackLang as TrackLang


scopes :: Derive.Scopes
scopes = Derive.Scopes
    { Derive.scopes_generator = Derive.Scope
        { Derive.scope_note = note_map_g <> note_lookup_g
        , Derive.scope_control = control_map_g <> control_lookup_g
        , Derive.scope_pitch = pitch_map_g
        }
    , Derive.scopes_transformer = Derive.Scope
        { Derive.scope_note = note_map_t <> note_lookup_t
        , Derive.scope_control = control_map_t
        , Derive.scope_pitch = pitch_map_t
        }
    , Derive.scopes_val = builtin [Derive.map_val_lookup val_calls]
    }
    where
    (note_map_g, note_map_t) = maps_lookup note_maps
    (control_map_g, control_map_t) = maps_lookup control_maps
    (pitch_map_g, pitch_map_t) = maps_lookup pitch_maps

-- | Warnings for shadowed symbols.
--
-- (symbol, qualifiers)
type Shadowed = (TrackLang.Symbol, [Text])

shadowed :: [Shadowed]
shadowed = concat
    [ add "note" shadowed_notes
    , add "control" shadowed_controls
    , add "pitch" shadowed_pitches
    , add "val" shadowed_vals
    ]
    where add tag = map (fmap (tag:))

builtin :: [Derive.LookupCall call] -> Derive.ScopeType call
builtin lookups = mempty { Derive.stype_builtin = lookups }

-- | Some calls have special lookups.
note_lookup_g :: Derive.ScopeType (Derive.Generator Derive.Note)
note_lookup_g =
    builtin [Articulation.lookup_attr_generator, Block.lookup_note_block]

note_lookup_t :: Derive.ScopeType (Derive.Transformer Derive.Note)
note_lookup_t = builtin [Articulation.lookup_attr_transformer]

control_lookup_g :: Derive.ScopeType (Derive.Generator Derive.Control)
control_lookup_g = builtin [Control.lookup_number, Block.lookup_control_block]

-- * map lookups

-- | Lookups created from maps.
maps_lookup :: Derive.CallMaps d
    -> (Derive.ScopeType (Derive.Generator d),
        Derive.ScopeType (Derive.Transformer d))
maps_lookup = (mk *** mk)
    where mk = builtin . (:[]) . Derive.map_lookup

note_maps :: Derive.CallMaps Derive.Note
(note_maps, shadowed_notes) = union_calls
    [ Articulation.note_calls, Block.note_calls, Echo.note_calls
    , Gender.note_calls, Integrate.note_calls
    , Lily.note_calls
    , Note.note_calls, NoteTransformer.note_calls
    , Pitch.note_calls
    , Grace.note_calls
    , Post.Idiom.note_calls, Post.ArrivalNote.note_calls
    , Post.Reverse.note_calls
    , Reyong.note_calls
    , Random.note_calls
    , Sekar.note_calls, String.note_calls, Trill.note_calls
    ]

control_maps :: Derive.CallMaps Derive.Control
(control_maps, shadowed_controls) = union_calls
    [Control.control_calls, Random.control_calls, Trill.control_calls]

pitch_maps :: Derive.CallMaps Derive.Pitch
(pitch_maps, shadowed_pitches) = union_calls
    [Pitch.pitch_calls, Random.pitch_calls, Trill.pitch_calls]

val_calls :: Derive.ValCallMap
(val_calls, shadowed_vals) = fmap (map (flip (,) [])) $ unions
    [Random.val_calls, Val.val_calls]

union_calls :: [Derive.CallMaps d] -> (Derive.CallMaps d, [Shadowed])
union_calls call_maps = ((gmap, tmap), shadowed)
    where
    (generators, transformers) = unzip call_maps
    shadowed = map (flip (,) ["generator"]) gshadowed
        ++ map (flip (,) ["transformer"]) tshadowed
    (gmap, gshadowed) = unions generators
    (tmap, tshadowed) = unions transformers

unions :: [Map.Map TrackLang.Symbol a]
    -> (Map.Map TrackLang.Symbol a, [TrackLang.Symbol])
unions fms = (m, Map.keys shadowed)
    where (m, shadowed) = Map.unique_unions fms
