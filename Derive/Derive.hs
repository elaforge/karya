-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-| Re-export DeriveInternal and DeriveLib.  Derivers should probably be
    importing this module rather than the lower level ones.

    "Derive.DeriveInternal" contains an explanation of the split, and
    "Derive.DeriveLib" an overview of derivation in general.
-}
module Derive.Derive (
    module Derive.Deriver.Monad
    , module Derive.Deriver.Lib
    , module Derive.Deriver.Internal
) where
import Derive.Deriver.Monad
import Derive.Deriver.Lib
import Derive.Deriver.Internal
    ( Time, score, real, score_to_real, real_to_score
    , d_at, d_stretch, d_place, in_real_time
    , get_block_dur
    , get_ui_state, get_ui_config, get_track, get_block, eval_ui
    )
