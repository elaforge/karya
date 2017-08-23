-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Export 'modify_meter'.  This has to be separate from "Cmd.Ruler.Meter" to
-- avoid a circular dependency.
module Cmd.Ruler.Modify (modify_meter, renumber) where
import qualified Ui.Ruler as Ruler
import qualified Cmd.Ruler.Gong as Gong
import qualified Cmd.Ruler.Meter as Meter
import qualified Cmd.Ruler.RulerUtil as RulerUtil
import qualified Cmd.Ruler.Tala as Tala

import Global


modify_meter :: (Meter.LabeledMeter -> Meter.LabeledMeter)
    -> RulerUtil.ModifyRuler
modify_meter modify ruler = do
    (mtype, mlist) <- get_marklist ruler
    renumber <- tryJust ("unknown meter type: " <> showt mtype) $
        lookup_meter_type mtype
    let new = Meter.labeled_marklist $ renumber $ modify $
            Meter.marklist_labeled mlist
    return $ Ruler.set_marklist Ruler.meter (Just mtype) new ruler

renumber :: Int -> RulerUtil.ModifyRuler
renumber start ruler = do
    (mtype, mlist) <- get_marklist ruler
    let (name, _) = Meter.parse_meter_type mtype
    modify_meter id $ Ruler.set_marklist Ruler.meter
        (Just (Meter.make_meter_type name start)) mlist ruler

get_marklist :: Ruler.Ruler -> Either Text (Ruler.MeterType, Ruler.Marklist)
get_marklist ruler
    | ruler == Ruler.no_ruler = Right (Meter.mtype, Ruler.empty_marklist)
    | otherwise = case Ruler.get_marklist Ruler.meter ruler of
        (Nothing, _) -> Left "no meter type"
        (Just mtype, mlist) -> Right (mtype, mlist)

-- | In order to perform generic operations on meters, such as doubling the
-- length, I need a way to renumber them.  So rulers keep track of their
-- created type and use that to look up the 'Renumber' function.
lookup_meter_type :: Ruler.MeterType -> Maybe Meter.Renumber
lookup_meter_type mtype
    | name == Meter.mtype =
        Just $ Meter.renumber_meter (Meter.measure_from start)
    | name == Gong.mtype = Just $ Meter.renumber_meter (Gong.config start)
    -- TODO now that I have an explicit start time, I don't have to infer
    | name == Tala.mtype = Just Meter.renumber_topmost
    | otherwise = Nothing
    where
    (name, start) = Meter.parse_meter_type mtype
