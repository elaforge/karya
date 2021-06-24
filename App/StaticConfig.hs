-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | 'StaticConfig' is an app-level configuration hook.  The idea is that
-- the local configuration can use it to override things.
module App.StaticConfig where
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified App.Config as Config
import qualified App.Path as Path
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.SaveGit as SaveGit

import qualified Derive.Derive as Derive
import qualified Instrument.Inst as Inst
import qualified Midi.Interface as Interface
import qualified Midi.Midi as Midi
import qualified Synth.Shared.Config as Shared.Config
import qualified Ui.Color as Color

import Global


-- * static config

data StaticConfig = StaticConfig {
    instrument_db :: Cmd.InstrumentDb
    -- | These Cmds are run before any of the usual ones, and can implement
    -- local global keymaps and whatnot.
    , global_cmds :: [Msg.Msg -> Cmd.CmdT IO Cmd.Status]

    -- | Default global namespace for deriver calls.
    , builtins :: Derive.Builtins

    -- | Run this on startup, given the app's argv.  It can set up an initial
    -- block, load a given file, or do nothing.
    -- Remember that no block is focused when this is run, so cmds that need
    -- a focused block will abort.
    , setup_cmd :: [String] -> Either Text (Cmd.CmdT IO Cmd.Status)
        -- ^ Left on error
    , post_setup_cmd :: Cmd.CmdT IO ()
    , midi :: Midi
    , highlight_colors :: Map Color.Highlight Color.Color
    -- | See 'Cmd.config_im_play_direct'.
    , im_play_direct :: Bool
    }

empty :: StaticConfig
empty = StaticConfig
    { instrument_db = Inst.empty
    , global_cmds = []
    , builtins = mempty
    , setup_cmd = const (Left "StaticConfig.setup_cmd not configured")
    , post_setup_cmd = return ()
    , midi = empty_midi
    , highlight_colors = mempty
    , im_play_direct = False
    }

data Midi = Midi {
    -- | Reroute the hardware level read and write devices.  This way,
    -- instruments and saved scores can use symbolic names which are then
    -- mapped to the devices exported by the MIDI driver.
    --
    -- Because input devices are likely to be relatively static, the
    -- read device map is only configured here.
    rdev_map :: Map Midi.ReadDevice Midi.ReadDevice
    -- | WriteDevices may vary per score, e.g. softsynths may listen at any
    -- number of virtual devices.  This map is taken as a default, but may
    -- be overridden by the score loaded.
    , wdev_map :: Map Midi.WriteDevice Midi.WriteDevice

    -- | Open the given devices on startup.  Even if they aren't plugged in,
    -- they'll be added to the read list so they're automatically connected if
    -- they are plugged in.  If you open a virtual device for both reading and
    -- writing you'll get a loop, so don't do that.
    --
    -- There's no corresponding write_devices because if you don't want
    -- to write to a device, just don't write to it!
    , read_devices :: Set Midi.ReadDevice
    } deriving (Show)

empty_midi :: Midi
empty_midi = Midi Map.empty Map.empty Set.empty

make_rdev_map :: [(Text, Text)] -> Map Midi.ReadDevice Midi.ReadDevice
make_rdev_map = Map.fromList . map (bimap Midi.read_device Midi.read_device)

make_wdev_map :: [(Text, Text)] -> Map Midi.WriteDevice Midi.WriteDevice
make_wdev_map = Map.fromList . map (bimap Midi.write_device Midi.write_device)

make_read_devices :: [Text] -> Set Midi.ReadDevice
make_read_devices = Set.fromList . map Midi.read_device

-- | Create a 'Cmd.Config' from a StaticConfig.
cmd_config :: Path.AppDir -> Path.Canonical -> Interface.Interface
    -> StaticConfig -> SaveGit.User -> Cmd.Config
cmd_config app_dir save_dir interface config git_user = Cmd.Config
    { config_app_dir = app_dir
    , config_save_dir = save_dir
    , config_midi_interface = interface
    , config_ky_paths = map (Path.to_absolute app_dir) Config.ky_paths
    , config_rdev_map = rdev_map midi_config
    , config_wdev_map = wdev_map midi_config
    , config_instrument_db = instrument_db config
    , config_builtins = builtins config
    , config_highlight_colors = highlight_colors config
    , config_im = Shared.Config.config app_dir
    , config_git_user = git_user
    , config_im_play_direct = im_play_direct config
    }
    where midi_config = midi config
