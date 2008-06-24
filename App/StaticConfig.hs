module App.StaticConfig where
import qualified Control.Monad.Identity as Identity

import qualified Ui.Block as Block

import qualified Cmd.Cmd as Cmd
import qualified Derive.Derive as Derive
import qualified Derive.Schema as Schema

import qualified Instrument.Db


-- * static config

data StaticConfig = StaticConfig {
    config_instrument_db :: Instrument.Db.Db
    , config_schema_db :: LookupSchema
    -- | Path to directories containing Local.Lang modules.  They will be
    -- included in the lang namespace.
    , config_local_lang_dirs :: [FilePath]

    -- | These Cmds are run before any of the usual ones, and can implement
    -- local global keymaps and whatnot.  Block local Cmds are part of the
    -- schema db.
    , config_global_cmds :: [Cmd.Cmd]

    -- | Run this on startup, given the app's argv.  It can set up an initial
    -- block, load a given file, or do nothing.
    , config_startup_cmd :: [String] -> Cmd.CmdT IO ()
    }

-- | The Schema leaves the UI and Deriver types polymorphic, but they only
-- wind up being run in Identity, so locking them down here keeps the type
-- signature simpler.
-- TODO maybe do this for Schema too?
type LookupSchema = Block.SchemaId ->
    Maybe (Schema.Schema (Cmd.CmdT Identity.Identity)
        (Derive.DeriveT Identity.Identity))

empty_config = StaticConfig {
    config_instrument_db = Instrument.Db.empty
    , config_schema_db = const Nothing
    , config_local_lang_dirs = []
    , config_global_cmds = []
    , config_startup_cmd = const (return ())
    }
