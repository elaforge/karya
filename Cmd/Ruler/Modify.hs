-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Export 'modify_meter'.  This has to be separate from "Cmd.Ruler.Meter" to
-- avoid a circular dependency.
module Cmd.Ruler.Modify (modify_meter) where
import qualified Data.Map as Map

import qualified Ui.Ruler as Ruler
import qualified Cmd.Ruler.Gong as Gong
import qualified Cmd.Ruler.Meter as Meter
import qualified Cmd.Ruler.RulerUtil as RulerUtil
import qualified Cmd.Ruler.Tala as Tala

import Global


modify_meter :: (Meter.LabeledMeter -> Meter.LabeledMeter)
    -> RulerUtil.ModifyRuler
modify_meter modify ruler = case flip Map.lookup meter_types =<< mtype of
    Nothing -> Left $ "unknown meter type: " <> pretty mtype
    Just renumber -> Right $ Ruler.set_marklist Ruler.meter mtype new ruler
        where
        new = Meter.labeled_marklist $ renumber $ modify $
            Meter.marklist_labeled mlist
    where (mtype, mlist) = Ruler.get_marklist Ruler.meter ruler

-- | In order to perform generic operations on meters, such as doubling the
-- length, I need a way to renumber them.  So rulers keep track of their
-- created type and use that to look up the 'Renumber' function.
meter_types :: Map Ruler.MeterType Meter.Renumber
meter_types = Map.fromList
    [ (Meter.mtype, Meter.renumber_meter Meter.default_config)
    , (Gong.mtype, Meter.renumber_meter Gong.config)
    , (Tala.mtype, Meter.renumber_topmost)
    ]
    -- TODO just take a [MeterConfig] and use 'config_meter_type'
