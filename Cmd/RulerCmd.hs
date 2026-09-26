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


localClip :: Cmd.M m => m ()
localClip = do
    (blockId, _, _, pos) <- Selection.getInsert
    RulerUtil.localMeter RulerUtil.Block blockId $
        Meter.modifySections (Meter.sectionsTake pos)
    pure ()

localDouble :: Cmd.M m => m ()
localDouble = local $ Meter.modifySections $ \sections -> sections <> sections

localAddSection :: Cmd.M m => m ()
localAddSection = local $ modifyFinal $ \section -> [section, section]

localExtend :: Cmd.M m => Meter.Measures -> m ()
localExtend n = local (extend n)

extend :: Meter.Measures -> Meter.Meter -> Meter.Meter
extend n = modifyFinal $ \section -> (:[]) $ section
    { Meter.sectionMeasures = n + Meter.sectionMeasures section }

modifyFinal :: (Meter.MSection -> [Meter.MSection]) -> Meter.Meter
    -> Meter.Meter
modifyFinal modify = Meter.modifySections $ \ss -> case Lists.unsnoc ss of
    Just (ss, s) -> ss ++ modify s
    Nothing -> modify defaultSection

local :: Cmd.M m => (Meter.Meter -> Meter.Meter) -> m ()
local action = do
    blockId <- Cmd.getFocusedBlock
    RulerUtil.localMeter RulerUtil.Block blockId action
    pure ()

-- TODO should it be configurable?
defaultSection :: Meter.MSection
defaultSection = Meter.MSection 4 1 Meters.m44
