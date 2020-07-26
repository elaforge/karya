-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Cmd.Repl.LKeycaps where
import qualified Cmd.Cmd as Cmd
import qualified Cmd.SyncKeycaps as SyncKeycaps
import qualified Ui.KeycapsT as KeycapsT


open :: Cmd.M m => m ()
open = SyncKeycaps.open

close :: Cmd.M m => m ()
close = SyncKeycaps.close

get_bindings :: Cmd.M m => m KeycapsT.Bindings
get_bindings = SyncKeycaps.get_bindings $ Cmd.KeycapsState
    { kc_mods = mempty
    , kc_octave = 0
    , kc_is_kbd_entry = False
    , kc_track_type = Nothing
    , kc_instrument = Nothing
    , kc_scale_id = Nothing
    }
