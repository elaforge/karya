-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Export 'meter'.  This has to be separate from "Cmd.Ruler.Meter" to
-- avoid a circular dependency.
module Cmd.Ruler.Modify (meter, renumber, start_and_meter) where
import qualified Data.Map as Map

import qualified Util.Seq as Seq
import qualified Ui.Ruler as Ruler
import qualified Cmd.Ruler.Gong as Gong
import qualified Cmd.Ruler.Meter as Meter
import qualified Cmd.Ruler.RulerUtil as RulerUtil
import qualified Cmd.Ruler.Tala as Tala

import Global


meter :: (Meter.LabeledMeter -> Meter.LabeledMeter) -> RulerUtil.ModifyRuler
meter = start_and_meter id

renumber :: Int -> RulerUtil.ModifyRuler
renumber start = start_and_meter (const start) id

start_and_meter :: (Meter.Start -> Meter.Start)
    -> (Meter.LabeledMeter -> Meter.LabeledMeter) -> RulerUtil.ModifyRuler
start_and_meter modify_start modify ruler = do
    (mtype_, mlist) <- get_marklist ruler
    let (name, start) = Meter.parse_meter_type mtype_
    let mtype = Meter.make_meter_type name (modify_start start)
    let measure_rank = Meter.config_measure_rank $
            Map.findWithDefault Meter.default_config mtype configs
    let new = Meter.labeled_marklist $
            Meter.renumber_measures measure_rank start $ modify $
            Meter.marklist_labeled mlist
    return $ Ruler.set_marklist Ruler.meter (Just mtype) new ruler

get_marklist :: Ruler.Ruler -> Either Text (Ruler.MeterType, Ruler.Marklist)
get_marklist ruler
    | ruler == Ruler.no_ruler = Right (Meter.mtype, Ruler.empty_marklist)
    | otherwise = case Ruler.get_marklist Ruler.meter ruler of
        (Nothing, _) -> Left "no meter type"
        (Just mtype, mlist) -> Right (mtype, mlist)

-- The only reason I need this is that Gong.config counts at Meter.Section,
-- while the rest count at Meter.W.
configs :: Map Ruler.MeterType Meter.Config
configs = Map.fromList $ Seq.key_on Meter.config_meter_type
    [Meter.default_config, Gong.config, Tala.make_config []]
