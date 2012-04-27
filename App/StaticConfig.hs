module App.StaticConfig where
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Midi.Midi as Midi
import qualified Cmd.Cmd as Cmd

import qualified Derive.Derive as Derive

import qualified Instrument.Db


-- * static config

data StaticConfig = StaticConfig {
    instrument_db :: Cmd.InstrumentDb
    -- | Path to directories containing Local.Lang modules.  They will be
    -- included in the lang namespace.
    , local_lang_dirs :: [FilePath]

    -- | These Cmds are run before any of the usual ones, and can implement
    -- local global keymaps and whatnot.
    , global_cmds :: [Cmd.Cmd]

    -- | Default global namespace for deriver calls.
    , global_scope :: Derive.Scope

    -- | Run this on startup, given the app's argv.  It can set up an initial
    -- block, load a given file, or do nothing.
    -- Remember that no block is focused when this is run, so cmds that need
    -- a focused block will abort.
    , setup_cmd :: [String] -> Cmd.CmdIO

    -- | Reroute the hardware level read and write devices.  This way,
    -- instruments and saved scores can use symbolic names which are then
    -- mapped to the devices exported by the MIDI driver.
    --
    -- Because input devices are likely to be relatively static, the
    -- read device map is only configured here.
    , rdev_map :: Map.Map Midi.ReadDevice Midi.ReadDevice
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
    }

empty :: StaticConfig
empty = StaticConfig {
    instrument_db = Instrument.Db.empty
    , local_lang_dirs = []
    , global_cmds = []
    , global_scope = Derive.empty_scope
    , setup_cmd = const (return Cmd.Done)
    , rdev_map = Map.empty
    , wdev_map = Map.empty
    , read_devices = Set.empty
    }
