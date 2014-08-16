-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE NoMonomorphismRestriction #-}
-- | Repl cmds providing general UI state operations.
module Cmd.Repl.LState where
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
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
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty

import qualified Midi.Midi as Midi
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Ui.Transform as Transform

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.DiffPerformance as DiffPerformance
import qualified Cmd.Lilypond
import qualified Cmd.Load.Midi as Load.Midi
import qualified Cmd.PlayUtil as PlayUtil
import qualified Cmd.Repl.LBlock as LBlock
import qualified Cmd.Repl.LEvent as Repl.LEvent
import qualified Cmd.Repl.LTrack as LTrack
import qualified Cmd.Save as Save

import qualified Derive.Derive as Derive
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

modify_transform :: (Text -> Text) -> Cmd.CmdL ()
modify_transform = State.modify . (State.config#State.global_transform %=)

get_transform :: Cmd.CmdL Text
get_transform = State.config#State.global_transform <#> State.get

set_definitions :: Maybe FilePath -> Cmd.CmdL ()
set_definitions = State.modify_config . (State.definition_file #=)

-- ** meta

get_meta :: Cmd.CmdL State.Meta
get_meta = State.config#State.meta <#> State.get

set_creation_time :: Cmd.CmdL ()
set_creation_time = do
    now <- liftIO Time.getCurrentTime
    State.modify $ State.config#State.meta#State.creation #= now

set_notes :: Text -> Cmd.CmdL ()
set_notes = State.modify . (State.config#State.meta#State.notes #=)

-- *** midi performance

-- | Save the current root MIDI performance as \"correct\".
save_midi :: Cmd.CmdL ()
save_midi = do
    block_id <- State.get_root_id
    midi <- midi_performance block_id
    perf <- make_performance (Vector.fromList midi)
    State.modify_config $
        State.meta#State.midi_performances %= Map.insert block_id perf

get_midi_performance :: BlockId -> Cmd.CmdL State.MidiPerformance
get_midi_performance block_id =
    Cmd.require ("saved performance for " ++ show block_id)
        =<< State.get_config
            (State.meta#State.midi_performances # Lens.map block_id #$)

-- | This assumes the current dir is in the darcs repo.
get_current_patch :: IO (Either String Text)
get_current_patch = do
    (exit, stdout, stderr) <- Process.readProcessWithExitCode "darcs"
        ["changes", "--last=1"] ""
    return $ case exit of
        Exit.ExitFailure n -> Left $ "darcs failed with " <> show n
            <> ": " <> stderr
        Exit.ExitSuccess -> Right $ Text.strip $ txt stdout

-- | Compare the current root block performance against the saved one.
verify_performance :: Cmd.CmdL Text
verify_performance = do
    block_id <- State.get_root_id
    perf <- get_midi_performance block_id
    msgs <- midi_performance block_id
    let (diffs, _expected, _got) =
            DiffPerformance.diff_midi_performance perf msgs
    return $ fromMaybe "ok!" diffs

midi_performance :: Cmd.M m => BlockId -> m [Midi.WriteMessage]
midi_performance block_id = do
    perf <- Cmd.get_performance block_id
    LEvent.events_of <$> PlayUtil.perform_from 0 perf

make_performance :: a -> Cmd.CmdT IO (State.Performance a)
make_performance perf = do
    time <- liftIO Time.getCurrentTime
    patch <- either Cmd.throw return =<< liftIO get_current_patch
    return $ State.Performance
        { State.perf_performance = perf
        , State.perf_creation = time
        , State.perf_patch = patch
        }

-- *** lilypond performance

save_lilypond :: Cmd.CmdL ()
save_lilypond = do
    block_id <- State.get_root_id
    lily <- lilypond_performance block_id
    perf <- make_performance lily
    State.modify_config $
        State.meta#State.lilypond_performances %= Map.insert block_id perf

lilypond_performance :: Cmd.M m => BlockId -> m Text
lilypond_performance block_id = do
    (events, logs) <- LEvent.partition . Derive.r_events <$>
        Cmd.Lilypond.derive_block block_id
    mapM_ Log.write logs
    config <- State.config#State.lilypond <#> State.get
    let (result, logs) = Cmd.Lilypond.extract_movements config "title" events
    mapM_ Log.write logs
    Text.Lazy.toStrict <$> Cmd.require_right untxt result


-- * transform

set_namespace :: Id.Namespace -> Cmd.CmdL ()
set_namespace = Create.rename_project

-- | Set the score namespace to the given one.  Also update the project_dir
-- and move the actual directory.
rename :: Id.Namespace -> Cmd.CmdL ()
rename ns = do
    Create.rename_project ns
    Cmd.gets Cmd.state_save_file >>= \x -> case x of
        Nothing -> return ()
        Just (Cmd.SaveState fn) -> Cmd.modify $ \st -> st
            { Cmd.state_save_file = Just $ Cmd.SaveState $ replace_dir ns fn }
        Just (Cmd.SaveRepo repo) -> do
            -- System.Directory.renameDirectory deletes the destination
            -- diretory for some reason.  I'd rather throw an exception.
            let old_dir = FilePath.takeDirectory repo
                new_dir = FilePath.replaceFileName old_dir
                    (untxt (Id.un_namespace ns))
            Cmd.rethrow_io $ File.ignoreEnoent $ Posix.rename old_dir new_dir
            Cmd.modify $ \st -> st
                { Cmd.state_save_file = Just $ Cmd.SaveState $
                    new_dir </> FilePath.takeFileName repo
                }
    where
    replace_dir ns path = new_dir </> FilePath.takeFileName path
        where
        new_dir = FilePath.replaceFileName (FilePath.takeDirectory path)
            (untxt (Id.un_namespace ns))

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
