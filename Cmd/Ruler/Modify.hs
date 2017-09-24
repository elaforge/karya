-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Export 'meter'.  This has to be separate from "Cmd.Ruler.Meter" to
-- avoid a circular dependency.
module Cmd.Ruler.Modify (meter, renumber, configs) where
import qualified Data.Map as Map

import qualified Util.Seq as Seq
import qualified Ui.Ruler as Ruler
import qualified Cmd.Ruler.Gong as Gong
import qualified Cmd.Ruler.Meter as Meter
import qualified Cmd.Ruler.RulerUtil as RulerUtil
import qualified Cmd.Ruler.Tala as Tala

import Global


meter :: (Meter.LabeledMeter -> Meter.LabeledMeter) -> RulerUtil.ModifyRuler
meter m = Right . start_and_meter id m

renumber :: Meter.Start -> RulerUtil.ModifyRuler
renumber start = Right . start_and_meter (const start) id

start_and_meter :: (Meter.Start -> Meter.Start)
    -> (Meter.LabeledMeter -> Meter.LabeledMeter) -> Ruler.Ruler -> Ruler.Ruler
start_and_meter modify_start modify ruler = Ruler.set_meter config new ruler
    where
    (old_config, mlist) = get_meter ruler
    config = old_config { Ruler.config_start_measure =
            modify_start (Ruler.config_start_measure old_config) }
    measure_rank = Meter.config_measure_rank $
        Map.findWithDefault Meter.default_config (Ruler.config_name config)
            configs
    new = Meter.labeled_marklist $
        Meter.renumber_measures measure_rank
            (Ruler.config_start_measure config) $
        modify $ Meter.marklist_labeled mlist

get_meter :: Ruler.Ruler -> (Ruler.MeterConfig, Ruler.Marklist)
get_meter ruler
    | ruler == Ruler.no_ruler =
        (Meter.ruler_config Meter.default_config, Ruler.empty_marklist)
    | otherwise = Ruler.get_meter ruler

-- The only reason I need this is that Gong.config counts at Meter.Section,
-- while the rest count at Meter.W.
configs :: Map Text Meter.Config
configs = Map.fromList $ Seq.key_on Meter.config_name
    [Meter.default_config, Gong.config, Tala.dummy_config]
