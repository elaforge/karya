-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Cmds to manipulate the ruler.
--
-- This is a subset of the features in "Cmd.Repl.LRuler", specifically the
-- subset I want to bind to keys.  Perhaps I should move the logic from LRuler
-- here and have LRuler use these definitions.
module Cmd.RulerCmd where
import qualified Util.Lists as Lists
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Ruler.RulerUtil as RulerUtil
import qualified Cmd.Selection as Selection

import qualified Ui.Meter.Meter as Meter
import qualified Ui.Meter.Meters as Meters


local_clip :: Cmd.M m => m ()
local_clip = do
    (block_id, _, _, pos) <- Selection.get_insert
    RulerUtil.local_meter RulerUtil.Block block_id $
        Meter.modify_sections (Meter.sections_take pos)
    return ()

local_double :: Cmd.M m => m ()
local_double = local $ Meter.modify_sections $ \sections -> sections <> sections

local_add_section :: Cmd.M m => m ()
local_add_section = local $ modify_final $ \section -> [section, section]

local_extend :: Cmd.M m => Meter.Measures -> m ()
local_extend n = local (extend n)

extend :: Meter.Measures -> Meter.Meter -> Meter.Meter
extend n = modify_final $ \section -> (:[]) $ section
    { Meter.section_measures = n + Meter.section_measures section }

modify_final :: (Meter.MSection -> [Meter.MSection]) -> Meter.Meter
    -> Meter.Meter
modify_final modify = Meter.modify_sections $ \ss -> case Lists.unsnoc ss of
    Just (ss, s) -> ss ++ modify s
    Nothing -> modify default_section

local :: Cmd.M m => (Meter.Meter -> Meter.Meter) -> m ()
local action = do
    block_id <- Cmd.get_focused_block
    RulerUtil.local_meter RulerUtil.Block block_id action
    return ()

-- TODO should it be configurable?
default_section :: Meter.MSection
default_section = Meter.MSection 4 1 Meters.m44
