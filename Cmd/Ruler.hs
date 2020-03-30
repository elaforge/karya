-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Cmds to manipulate the ruler.
--
-- This is a subset of the features in "Cmd.Repl.LRuler", specifically the
-- subset I want to bind to keys.  Perhaps I should move the logic from LRuler
-- here and have LRuler use these definitions.
module Cmd.Ruler where
import qualified Util.Seq as Seq
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Ruler.Meter as Meter
import qualified Cmd.Ruler.Modify as Ruler.Modify
import qualified Cmd.Ruler.RulerUtil as RulerUtil
import qualified Cmd.Selection as Selection

import Types


local_clip :: Cmd.M m => m ()
local_clip = do
    (block_id, _, _, pos) <- Selection.get_insert
    RulerUtil.local RulerUtil.Block block_id (clip pos)
    return ()

clip :: ScoreTime -> RulerUtil.ModifyRuler
clip pos = Ruler.Modify.meter $ Meter.extract 0 (Meter.time_to_duration pos)

local_double :: Cmd.M m => m ()
local_double = do
    block_id <- Cmd.get_focused_block
    RulerUtil.local RulerUtil.Block block_id double
    return ()

double :: RulerUtil.ModifyRuler
double = Ruler.Modify.meter $ \meter -> Seq.rdrop 1 meter <> meter
