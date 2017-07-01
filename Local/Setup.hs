-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities to set up the initial score.
-- Mostly just testing hackery.
module Local.Setup where
import qualified Control.Monad.Trans as Trans
import qualified Data.Map as Map
import qualified System.FilePath as FilePath

import qualified Util.Seq as Seq
import qualified Ui.Id as Id
import qualified Ui.Ui as Ui
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Load.Med as Med
import qualified Cmd.Load.Midi as Load.Midi
import qualified Cmd.Load.Mod as Load.Mod
import qualified Cmd.Load.Mod2 as Mod2
import qualified Cmd.Ruler.Meter as Meter
import qualified Cmd.Ruler.Meters as Meters
import qualified Cmd.Ruler.RulerUtil as RulerUtil

import Global


load_med :: FilePath -> Cmd.CmdT IO Cmd.Status
load_med fn = do
    let inst_map = Map.findWithDefault mempty (FilePath.takeFileName fn)
            inst_maps
    mod <- liftIO $ Med.load inst_map fn
    state <- Cmd.require_right pretty $ Mod2.convert (fn_to_ns fn) mod
    Ui.put state
    return Cmd.Done

inst_maps :: Map FilePath (Map Text Text)
inst_maps = Map.fromList
    [ ("underwater", Map.fromList
        [ ("Takerimba", "marim")
        , ("SoftShake", "shake")
        , ("Thumb Bass", "bass")
        , ("HeavyBassDrum", "bd")
        , ("SD1", "sd")
        , ("FireHiSyn", "lead")
        , ("VCO Bass", "synb")
        , ("Chin-PanFluteLooped", "pan")
        , ("WoodPf (4/29)", "wood")
        , ("RainyHiMajor", "maj")
        , ("RainyHiMinor", "min")
        , ("technoRush-loud", "rush1")
        , ("technoRush2", "rush2")
        , ("D50-PizzaGogo", "pizz")
        , ("filter.maj", "fmaj")
        ])
    , ("piano", Map.fromList
        [ ("UpPiano (4/1)", "pno")
        , ("UprtBass (6/20)", "bass")
        , ("Glockn2 (6/36)", "glock")
        , ("BigPipe (7/13)", "pipe")
        , ("String2 (5/17)", "string")
        , ("TubeBe1 (6/37)", "bell")
        ])
    , ("Elektrodes", Map.fromList
        [ ("Elektrodes", "elec")
        , ("Jazz Man", "bass")
        , ("440thick-bk", "bd")
        , ("AquaSnare", "sn")
        , ("AlesisHihatC", "hh-c")
        , ("AlesisHihatO", "hh-o")
        , ("AlesisHihatM", "hh-m")
        , ("CheckHiSyn-loud", "syn")
        , ("ClassPiano", "pno")
        , ("BstTom", "tom")
        , ("SundanceJazzHit", "hit")
        ])
    ]

load_mod :: FilePath -> Cmd.CmdT IO Cmd.Status
load_mod fn = do
    blocks <- either Cmd.throw return =<< Trans.liftIO (Load.Mod.parse fn)
    let blocks2 = map
            (Load.Mod.map_block (Load.Mod.add_default_volume 1 38)) blocks
    Load.Mod.create (fn_to_ns fn)
        (Load.Mod.convert_blocks 0.25 blocks2)
    return Cmd.Done

fn_to_ns :: FilePath -> Id.Namespace
fn_to_ns = Id.namespace . txt . head . Seq.split "." . FilePath.takeFileName

load_midi :: FilePath -> Cmd.CmdT IO Cmd.Status
load_midi fn = do
    block_id <- Load.Midi.load fn
    Create.unfitted_view block_id
    return Cmd.Done

empty_block :: Cmd.M m => m Cmd.Status
empty_block = do
    rid <- Create.ruler "m44-4" $
        RulerUtil.meter_ruler Meter.default_config 16 (replicate 4 Meters.m44_4)
    bid <- Create.block rid
    Create.track bid 1 "" mempty
    Ui.set_track_width bid 1 40
    Create.view bid
    return Cmd.Done
