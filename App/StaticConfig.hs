module App.StaticConfig where
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Midi.Midi as Midi
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg

import qualified Derive.Derive as Derive
import qualified Derive.Schema as Schema

import qualified Instrument.Db


-- * static config

data StaticConfig = StaticConfig {
    config_instrument_db :: Cmd.InstrumentDb
    , config_schema_map :: Schema.SchemaMap
    -- | Path to directories containing Local.Lang modules.  They will be
    -- included in the lang namespace.
    , config_local_lang_dirs :: [FilePath]

    -- | These Cmds are run before any of the usual ones, and can implement
    -- local global keymaps and whatnot.  They're in IO for flexibility.
    --
    -- Cmds that are local to a Block are part of the schema db.
    , config_global_cmds :: [Msg.Msg -> Cmd.CmdIO]

    -- | Default global namespace for deriver calls.
    , config_global_scope :: Derive.Scope

    -- | Run this on startup, given the app's argv.  It can set up an initial
    -- block, load a given file, or do nothing.
    -- Remember that no block is focused when this is run, so cmds that need
    -- a focused block will abort.
    , config_setup_cmd :: [String] -> Cmd.CmdIO

    -- | Map the hardware level read and write devices through these maps.
    -- This centralizes midi routing configuration in one place and lets
    -- the scattered instrument configs (some saved in the inst.db) use
    -- symbolic names.
    , config_read_device_map :: Map.Map Midi.ReadDevice Midi.ReadDevice
    , config_write_device_map :: Map.Map Midi.WriteDevice Midi.WriteDevice

    -- | Only the given devices are opened for reading, if present.  If you
    -- open a virtual device for both reading and writing you'll get a loop, so
    -- don't do that.
    --
    -- There's no corresponding config_write_devices because if you don't want
    -- to write to a device, just don't write to it!
    , config_read_devices :: Set.Set Midi.ReadDevice
    }

empty :: StaticConfig
empty = StaticConfig {
    config_instrument_db = Instrument.Db.empty
    , config_schema_map = Map.empty
    , config_local_lang_dirs = []
    , config_global_cmds = []
    , config_global_scope = Derive.empty_scope
    , config_setup_cmd = const (return Cmd.Done)
    , config_read_device_map = Map.empty
    , config_write_device_map = Map.empty
    , config_read_devices = Set.empty
    }
