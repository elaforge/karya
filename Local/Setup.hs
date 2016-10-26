-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities to set up the initial score.
-- Mostly just testing hackery.
module Local.Setup where
import qualified Control.Monad.Trans as Trans

import qualified Util.Seq as Seq
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Load.Midi as Load.Midi
import qualified Cmd.Load.Mod as Load.Mod
import qualified Cmd.Meter as Meter
import qualified Cmd.Meters as Meters
import qualified Cmd.RulerUtil as RulerUtil

import Global


load_mod :: FilePath -> Cmd.CmdT IO Cmd.Status
load_mod fn = do
    blocks <- either Cmd.throw return =<< Trans.liftIO (Load.Mod.parse fn)
    let blocks2 = map
            (Load.Mod.map_block (Load.Mod.add_default_volume 1 38)) blocks
    Load.Mod.create (Id.namespace $ txt $ head $ Seq.split "." fn)
        (Load.Mod.convert_blocks 0.25 blocks2)
    return Cmd.Done

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
    State.set_track_width bid 1 40
    Create.view bid
    return Cmd.Done
