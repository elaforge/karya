-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Export 'meter'.  This has to be separate from "Cmd.Ruler.Meter" to
-- avoid a circular dependency.
module Cmd.Ruler.Modify (
    meter, generate_until, renumber, configs, replace
) where
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Util.Seq as Seq
import qualified Util.Then as Then
import qualified Cmd.Ruler.Gong as Gong
import qualified Cmd.Ruler.Meter as Meter
import qualified Cmd.Ruler.RulerUtil as RulerUtil
import qualified Cmd.Ruler.Tala as Tala

import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.Ui as Ui

import           Global
import           Types


meter :: (Meter.LabeledMeter -> Meter.LabeledMeter) -> RulerUtil.ModifyRuler
meter m = Right . start_and_meter id m

-- | Cycle the marks until the given time and renumber them.
generate_until :: TrackTime -> [Meter.LabeledMark] -> RulerUtil.ModifyRuler
generate_until end labeled = meter (const generate)
    where
    generate = trim $ cycle $ Seq.rdrop 1 labeled
    trim = map snd . Then.takeWhile1 ((<end) . fst)
        . Seq.scanl_on (+) Meter.m_duration 0

renumber :: Meter.Start -> RulerUtil.ModifyRuler
renumber start = Right . start_and_meter (const start) id

start_and_meter :: (Meter.Start -> Meter.Start)
    -> (Meter.LabeledMeter -> Meter.LabeledMeter) -> Ruler.Ruler -> Ruler.Ruler
start_and_meter modify_start modify ruler = Ruler.set_meter config new ruler
    where
    (old_config, mlist) = get_meter ruler
    config = old_config
        { Ruler.config_start_measure =
            modify_start (Ruler.config_start_measure old_config)
        }
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

-- | Create or replace a ruler for the given block.
replace :: Ui.M m => BlockId -> RulerUtil.ModifyRuler -> m RulerId
replace block_id modify = do
    whenM (Maybe.isNothing <$> Ui.lookup_ruler ruler_id) $
        void $ Ui.create_ruler (Id.unpack_id ruler_id) (Ruler.ruler [])
    Ui.modify_ruler ruler_id modify
    return ruler_id
    where
    ruler_id = Id.RulerId $ Id.unpack_id block_id
