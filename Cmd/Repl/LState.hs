-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE NoMonomorphismRestriction #-}
-- | Repl cmds providing general UI state operations.
module Cmd.Repl.LState where
import qualified Data.Map as Map
import qualified Data.Time as Time
import qualified Data.Vector as Vector

import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import System.FilePath ((</>))
import qualified System.Posix as Posix
import qualified System.Process as Process

import Util.Control
import qualified Util.File as File
import qualified Util.Lens as Lens
import qualified Util.Pretty as Pretty

import qualified Midi.Midi as Midi
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Ui.Transform as Transform

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.DiffPerformance as DiffPerformance
import qualified Cmd.Load.Midi as Load.Midi
import qualified Cmd.PlayUtil as PlayUtil
import qualified Cmd.Repl.LBlock as LBlock
import qualified Cmd.Repl.LEvent as Repl.LEvent
import qualified Cmd.Repl.LTrack as LTrack
import qualified Cmd.Save as Save

import qualified Derive.LEvent as LEvent
import qualified Perform.Signal as Signal
import qualified App.Config as Config
import Types


-- | Find text in block titles, track titles, or events.
find :: Text -> Cmd.CmdL String
find search = do
    blocks <- LBlock.find search
    tracks <- LTrack.find search
    events <- Repl.LEvent.find search
    return $ unlines $ concatMap section $ concat
        [ [("blocks:", Pretty.formatted blocks) | not (null blocks)]
        , [("tracks:", Pretty.formatted tracks) | not (null tracks)]
        , [("events:", Pretty.formatted events) | not (null events)]
        ]
    where section (title, doc) = [title, doc]

-- * configure

get_config :: Cmd.CmdL State.Config
get_config = State.config <#> State.get

get_default :: Cmd.CmdL State.Default
get_default = State.config#State.default_ <#> State.get

get_default_tempo :: Cmd.CmdL Signal.Y
get_default_tempo = State.config#State.default_#State.tempo <#> State.get

set_default_tempo :: Signal.Y -> Cmd.CmdL ()
set_default_tempo t = modify_config $ State.default_#State.tempo #= t

modify_config :: (State.Config -> State.Config) -> Cmd.CmdL ()
modify_config f = State.modify_config f >> Cmd.invalidate_performances

-- | 'State.config_global_transform' is an expression that's applied to the
-- output of derivation.
transform :: Text -> Cmd.CmdL ()
transform = State.modify . (State.config#State.global_transform #=)

get_transform :: Cmd.CmdL Text
get_transform = State.config#State.global_transform <#> State.get

-- ** meta

get_meta :: Cmd.CmdL State.Meta
get_meta = State.config#State.meta <#> State.get

set_creation_time :: Cmd.CmdL ()
set_creation_time = do
    now <- liftIO Time.getCurrentTime
    State.modify $ State.config#State.meta#State.creation #= now

set_notes :: Text -> Cmd.CmdL ()
set_notes = State.modify . (State.config#State.meta#State.notes #=)

-- | Save the current root performances as \"correct\".
save_performance :: Cmd.CmdL ()
save_performance = do
    block_id <- State.get_root_id
    midi <- midi_performance block_id
    time <- lift Time.getCurrentTime
    patch <- either Cmd.throw return =<< lift get_current_patch
    let perf = State.Performance midi time patch
    State.modify_config $
        State.meta#State.performances %= Map.insert block_id perf

get_performance :: BlockId -> Cmd.CmdL State.Performance
get_performance block_id =
    Cmd.require_msg ("saved performance for " ++ show block_id)
    =<< State.get_config (State.meta#State.performances # Lens.map block_id #$)

-- | This assumes the current dir is in the darcs repo.
get_current_patch :: IO (Either String Text)
get_current_patch = do
    (exit, stdout, stderr) <- Process.readProcessWithExitCode "darcs"
        ["changes", "--last=1"] ""
    return $ case exit of
        Exit.ExitFailure n -> Left $ "darcs failed with " <> show n
            <> ": " <> stderr
        Exit.ExitSuccess -> Right $ txt stdout

-- | Compare the current root block performance against the saved one.
verify_performance :: Cmd.CmdL Text
verify_performance = do
    block_id <- State.get_root_id
    perf <- get_performance block_id
    msgs <- midi_performance block_id
    return $ DiffPerformance.verify perf msgs

midi_performance :: Cmd.M m => BlockId -> m (Vector.Vector Midi.WriteMessage)
midi_performance block_id = do
    perf <- Cmd.get_performance block_id
    Vector.fromList . LEvent.events_of <$> PlayUtil.perform_from 0 perf


-- * transform

set_namespace :: String -> Cmd.CmdL ()
set_namespace ns = do
    ns <- Cmd.require_msg ("invalid namespace: " ++ show ns) (Id.namespace ns)
    Create.rename_project ns

-- | Set the score namespace to the given one.  Also update the project_dir
-- and move the actual directory.
rename :: String -> Cmd.CmdL ()
rename ns = do
    ns <- Cmd.require_msg ("invalid namespace: " ++ show ns) (Id.namespace ns)
    Create.rename_project ns
    Cmd.gets Cmd.state_save_file >>= \x -> case x of
        Nothing -> return ()
        Just (Cmd.SaveState fn) -> Cmd.modify $ \st -> st
            { Cmd.state_save_file = Just $ Cmd.SaveState $ replace_dir ns fn }
        Just (Cmd.SaveRepo repo) -> do
            -- System.Directory.renameDirectory deletes the destination
            -- diretory for some reason.  I'd rather throw an exception.
            let old_dir = FilePath.takeDirectory repo
                new_dir = FilePath.replaceFileName old_dir (Id.un_namespace ns)
            Cmd.rethrow_io $ File.ignoreEnoent $ Posix.rename old_dir new_dir
            Cmd.modify $ \st -> st
                { Cmd.state_save_file = Just $ Cmd.SaveState $
                    new_dir </> FilePath.takeFileName repo
                }
    where
    replace_dir ns path = new_dir </> FilePath.takeFileName path
        where
        new_dir = FilePath.replaceFileName (FilePath.takeDirectory path)
            (Id.un_namespace ns)

-- * load

load_midi :: FilePath -> Cmd.CmdL BlockId
load_midi = Load.Midi.load

-- | Load the state from the file and merge it with the current state.  This
-- will fail if any IDs collide, so hopefully they live in different
-- namespaces.  In fact, this is why IDs have namespaces.
--
-- TODO option to rename on load?
load_merge :: FilePath -> Cmd.CmdL ()
load_merge fn = do
    (new_state, _) <- Save.read fn
    new_state <- State.exec_rethrow "strip clip" new_state $
        Transform.destroy_namespace Config.clip_namespace
    state <- State.get
    merged <- Cmd.require_right (("merge state: "<>) . pretty) $
        Transform.merge_states state new_state
    State.put merged
