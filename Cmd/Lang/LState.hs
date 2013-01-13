{-# LANGUAGE NoMonomorphismRestriction #-}
-- | Lang cmds providing general UI state operations.
module Cmd.Lang.LState where
import qualified Data.Time as Time
import qualified System.FilePath as FilePath
import qualified System.Posix as Posix

import Util.Control
import qualified Util.File as File
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Derive.Score as Score
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal


-- * configure

get_config :: Cmd.CmdL State.Config
get_config = State.config <#> State.get

get_default :: Cmd.CmdL State.Default
get_default = State.config#State.default_ <#> State.get

set_default_tempo :: Signal.Y -> Cmd.CmdL ()
set_default_tempo t = modify_config $ State.default_#State.tempo #= t

set_default_inst :: String -> Cmd.CmdL ()
set_default_inst inst = modify_config $
    State.default_#State.instrument #= Just (Score.Instrument inst)

set_default_scale :: String -> Cmd.CmdL ()
set_default_scale scale = modify_config $
    State.default_#State.scale #= Pitch.ScaleId scale

modify_config :: (State.Config -> State.Config) -> Cmd.CmdL ()
modify_config f = State.modify_config f >> Cmd.invalidate_performances

-- | 'State.config_global_transform' is an expression that's applied to the
-- output of derivation.
set_global_transform :: String -> Cmd.CmdL ()
set_global_transform = State.modify . (State.config#State.global_transform #=)

get_global_transform :: Cmd.CmdL String
get_global_transform = State.config#State.global_transform <#> State.get

-- ** meta

get_meta :: Cmd.CmdL State.Meta
get_meta = State.config#State.meta <#> State.get

set_creation_time :: Cmd.CmdL ()
set_creation_time = do
    now <- liftIO Time.getCurrentTime
    State.modify $ State.config#State.meta#State.creation #= now

set_notes :: String -> Cmd.CmdL ()
set_notes = State.modify . (State.config#State.meta#State.notes #=)


-- * transform

-- | Set the score namespace to the given one.  Also update the project_dir
-- and move the actual directory.
rename :: String -> Cmd.CmdL ()
rename ns = case Id.namespace ns of
    Nothing -> Cmd.throw $ "invalid namespace: " ++ show ns
    Just ns -> do
        Create.rename_project ns
        old_dir <- State.gets (State.config#State.project_dir #$)
        new_dir <- State.gets $
            flip FilePath.replaceFileName (Id.un_namespace ns)
            . (State.config#State.project_dir #$)
        liftIO $ print (old_dir, new_dir)
        -- System.Directory.renameDirectory deletes the destination diretory
        -- for some reason.  I'd rather throw an exception.
        Cmd.rethrow_io $ File.ignore_enoent $ Posix.rename old_dir new_dir
        State.modify_config $ State.project_dir #= new_dir
