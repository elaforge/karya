-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities to set up the initial score.
-- Mostly just testing hackery.
module Local.Setup where
import qualified Control.Monad.Trans as Trans

import Util.Control
import qualified Util.Seq as Seq
import qualified Midi.Midi as Midi
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Load.Midi as Load.Midi
import qualified Cmd.Load.Mod as Load.Mod
import qualified Cmd.Meters as Meters
import qualified Cmd.RulerUtil as RulerUtil
import qualified Cmd.Save as Save

import qualified Derive.Score as Score
import qualified Perform.Midi.Instrument as Instrument


auto_setup_cmd :: Cmd.CmdIO
auto_setup_cmd = load_default

-- | Load from @save/default@, but don't set the save file so a save doesn't
-- overwrite it.
load_default :: Cmd.CmdIO
load_default = do
    (state, _) <- Save.read_state "save/default"
    State.put (State.clear state)
    State.set_namespace (Id.namespace "untitled")
    return Cmd.Done

load_mod :: FilePath -> Cmd.CmdIO
load_mod fn = do
    blocks <- either Cmd.throw return =<< Trans.liftIO (Load.Mod.parse fn)
    let blocks2 = map
            (Load.Mod.map_block (Load.Mod.add_default_volume 1 38)) blocks
    Load.Mod.create (Id.namespace $ head (Seq.split "." fn))
        (Load.Mod.convert_blocks 0.25 blocks2)
    State.set_midi_config $ make_midi_config "ptq" [("ptq/c1", [0..8])]
    return Cmd.Done

load_midi :: FilePath -> Cmd.CmdIO
load_midi fn = do
    block_id <- Load.Midi.load fn
    Create.unfitted_view block_id
    return Cmd.Done

empty_block :: Cmd.M m => m Cmd.Status
empty_block = do
    rid <- Create.ruler "m44-4"
        (RulerUtil.meter_ruler 16 (replicate 4 Meters.m44_4))
    bid <- Create.block rid
    Create.track bid 1 "" mempty
    State.set_track_width bid 1 40
    Create.view bid
    return Cmd.Done

make_midi_config :: Text -> [(Text, [Midi.Channel])] -> Instrument.Configs
make_midi_config dev config = Instrument.configs
    [(Score.Instrument inst, map mkaddr chans) | (inst, chans) <- config]
    where mkaddr chan = (Midi.write_device dev, chan)
