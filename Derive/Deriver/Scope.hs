-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities to manipulate 'Derive.Scope's.
module Derive.Deriver.Scope where
import qualified Derive.Derive as Derive


add_override_note_lookup :: Derive.LookupCall Derive.NoteCall -> Derive.Scope
    -> Derive.Scope
add_override_note_lookup lookup scope = scope
    { Derive.scope_note = add_override lookup (Derive.scope_note scope) }

add_note_lookup :: Derive.LookupCall Derive.NoteCall -> Derive.Scope
    -> Derive.Scope
add_note_lookup lookup scope = scope
    { Derive.scope_note = add_builtin lookup (Derive.scope_note scope) }

add_control_lookup :: Derive.LookupCall Derive.ControlCall -> Derive.Scope
    -> Derive.Scope
add_control_lookup lookup scope = scope
    { Derive.scope_control = add_builtin lookup (Derive.scope_control scope) }

add_val_lookup :: Derive.LookupCall Derive.ValCall -> Derive.Scope
    -> Derive.Scope
add_val_lookup lookup scope = scope
    { Derive.scope_val = add_builtin lookup (Derive.scope_val scope) }

add_builtin :: Derive.LookupCall call -> Derive.ScopeType call
    -> Derive.ScopeType call
add_builtin lookup stype =
    stype { Derive.stype_builtin = lookup : Derive.stype_builtin stype }

add_override :: Derive.LookupCall call -> Derive.ScopeType call
    -> Derive.ScopeType call
add_override lookup stype =
    stype { Derive.stype_override = lookup : Derive.stype_override stype }
