-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Perf_test where
import Util.Test
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Perf as Perf

import qualified Derive.Scale.Twelve as Twelve
import qualified Perform.Pitch as Pitch


run :: Cmd.CmdId val -> Either String (Maybe val, [String])
run = CmdTest.extract id . CmdTest.run_tracks []
