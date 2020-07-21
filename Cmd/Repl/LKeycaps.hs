-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Cmd.Repl.LKeycaps where
import qualified Cmd.Cmd as Cmd
import qualified Cmd.SyncKeycaps as SyncKeycaps


open :: Cmd.M m => m ()
open = SyncKeycaps.open

close :: Cmd.M m => m ()
close = SyncKeycaps.close
