-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module App.StaticConfig where
import qualified Data.Map as Map
import qualified Data.Set as Set

import Util.Control
import qualified Midi.Midi as Midi
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Derive.Derive as Derive
import qualified Instrument.Db


-- * static config

data StaticConfig = StaticConfig {
    instrument_db :: Cmd.InstrumentDb
    -- | Path to directories containing Local.Repl modules.  They will be
    -- included in the repl namespace.
    , local_repl_dirs :: [FilePath]

    -- | These Cmds are run before any of the usual ones, and can implement
    -- local global keymaps and whatnot.
    , global_cmds :: [Msg.Msg -> Cmd.CmdIO]

    -- | Default global namespace for deriver calls.
    , library :: Derive.Library

    -- | Run this on startup, given the app's argv.  It can set up an initial
    -- block, load a given file, or do nothing.
    -- Remember that no block is focused when this is run, so cmds that need
    -- a focused block will abort.
    , setup_cmd :: [String] -> Cmd.CmdIO
    , midi :: Midi
    }

empty :: StaticConfig
empty = StaticConfig {
    instrument_db = Instrument.Db.empty
    , local_repl_dirs = []
    , global_cmds = []
    , library = mempty
    , setup_cmd = const (return Cmd.Done)
    , midi = empty_midi
    }

data Midi = Midi {
    -- | Reroute the hardware level read and write devices.  This way,
    -- instruments and saved scores can use symbolic names which are then
    -- mapped to the devices exported by the MIDI driver.
    --
    -- Because input devices are likely to be relatively static, the
    -- read device map is only configured here.
    rdev_map :: Map.Map Midi.ReadDevice Midi.ReadDevice
    -- | WriteDevices may vary per score, e.g. softsynths may listen at any
    -- number of virtual devices.  This map is taken as a default, but may
    -- be overridden by the score loaded.
    , wdev_map :: Map.Map Midi.WriteDevice Midi.WriteDevice

    -- | Open the given devices on startup.  Even if they aren't plugged in,
    -- they'll be added to the read list so they're automatically connected if
    -- they are plugged in.  If you open a virtual device for both reading and
    -- writing you'll get a loop, so don't do that.
    --
    -- There's no corresponding write_devices because if you don't want
    -- to write to a device, just don't write to it!
    , read_devices :: Set.Set Midi.ReadDevice
    } deriving (Show)

empty_midi :: Midi
empty_midi = Midi Map.empty Map.empty Set.empty

make_rdev_map :: [(Text, Text)] -> Map.Map Midi.ReadDevice Midi.ReadDevice
make_rdev_map =
    Map.fromList . map (\(k, v) -> (Midi.read_device k, Midi.read_device v))

make_wdev_map :: [(Text, Text)] -> Map.Map Midi.WriteDevice Midi.WriteDevice
make_wdev_map =
    Map.fromList . map (\(k, v) -> (Midi.write_device k, Midi.write_device v))

make_read_devices :: [Text] -> Set.Set Midi.ReadDevice
make_read_devices = Set.fromList . map Midi.read_device
