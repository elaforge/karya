module App.StaticConfig where
import qualified Data.Map as Map

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Derive.Schema as Schema

import qualified Instrument.Db


-- * static config

data StaticConfig = StaticConfig {
    config_instrument_db :: Instrument.Db.Db
    , config_schema_map :: Schema.SchemaMap
    -- | Path to directories containing Local.Lang modules.  They will be
    -- included in the lang namespace.
    , config_local_lang_dirs :: [FilePath]

    -- | These Cmds are run before any of the usual ones, and can implement
    -- local global keymaps and whatnot.  They're in IO for flexibility.
    --
    -- Cmds that are local to a Block are part of the schema db.
    , config_global_cmds :: [Msg.Msg -> Cmd.CmdIO]

    -- | Run this on startup, given the app's argv.  It can set up an initial
    -- block, load a given file, or do nothing.
    -- Remember that no block is focused when this is run, so cmds that need
    -- a focused block will abort.
    , config_setup_cmd :: [String] -> Cmd.CmdIO
    }

empty_config = StaticConfig {
    config_instrument_db = Instrument.Db.empty
    , config_schema_map = Map.empty
    , config_local_lang_dirs = []
    , config_global_cmds = []
    , config_setup_cmd = const (return Cmd.Done)
    }
