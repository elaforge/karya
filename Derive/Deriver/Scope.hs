-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities to manipulate 'Derive.Scope's.
module Derive.Deriver.Scope where
import qualified Data.Map as Map

import qualified Derive.Derive as Derive
import qualified Derive.TrackLang as TrackLang


-- * note

add_note :: Derive.LookupCall Derive.NoteCall -> Derive.Scope -> Derive.Scope
add_note = modify_note . add_override

modify_note ::
    (Derive.ScopeType Derive.NoteCall -> Derive.ScopeType Derive.NoteCall)
     -> Derive.Scope -> Derive.Scope
modify_note f scope = scope
    { Derive.scope_note = f (Derive.scope_note scope) }

-- * pitch

add_pitch :: Derive.LookupCall Derive.PitchCall -> Derive.Scope -> Derive.Scope
add_pitch = modify_pitch . add_override

modify_pitch ::
    (Derive.ScopeType Derive.PitchCall -> Derive.ScopeType Derive.PitchCall)
     -> Derive.Scope -> Derive.Scope
modify_pitch f scope = scope
    { Derive.scope_pitch = f (Derive.scope_pitch scope) }

-- * control

add_control :: Derive.LookupCall Derive.ControlCall -> Derive.Scope
    -> Derive.Scope
add_control = modify_control . add_override

modify_control ::
    (Derive.ScopeType Derive.ControlCall -> Derive.ScopeType Derive.ControlCall)
     -> Derive.Scope -> Derive.Scope
modify_control f scope = scope
    { Derive.scope_control = f (Derive.scope_control scope) }

-- * val

add_val :: Derive.LookupCall Derive.ValCall -> Derive.Scope -> Derive.Scope
add_val = modify_val . add_override

modify_val ::
    (Derive.ScopeType Derive.ValCall -> Derive.ScopeType Derive.ValCall)
     -> Derive.Scope -> Derive.Scope
modify_val f scope = scope
    { Derive.scope_val = f (Derive.scope_val scope) }

-- * implementation

add_builtin :: Derive.LookupCall call -> Derive.ScopeType call
    -> Derive.ScopeType call
add_builtin lookup stype =
    stype { Derive.stype_builtin = lookup : Derive.stype_builtin stype }

add_override :: Derive.LookupCall call -> Derive.ScopeType call
    -> Derive.ScopeType call
add_override lookup stype =
    stype { Derive.stype_override = lookup : Derive.stype_override stype }

-- * make lookup

single_lookup :: TrackLang.CallId -> Derive.Call d
    -> Derive.LookupCall (Derive.Call d)
single_lookup name = Derive.map_lookup . Map.singleton name

single_val_lookup :: TrackLang.CallId -> Derive.ValCall
    -> Derive.LookupCall Derive.ValCall
single_val_lookup name = Derive.map_val_lookup . Map.singleton name
