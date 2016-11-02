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

import qualified System.FilePath as FilePath
import System.FilePath ((</>))
import qualified System.Posix as Posix

import qualified Util.File as File
import qualified Util.Lens as Lens
import qualified Util.PPrint as PPrint
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
import qualified Derive.Stream as Stream

import qualified Perform.Signal as Signal
import qualified App.Config as Config
import qualified App.ReplProtocol as ReplProtocol
import Global
import qualified Shake.SourceControl as SourceControl
import Types


-- | Find text in block titles, track titles, or events.
find :: Text -> Cmd.CmdL Text
find search = do
    blocks <- LBlock.find search
    tracks <- LTrack.find search
    events <- Repl.LEvent.find search
    return $ Text.unlines $ concatMap section $ concat
        [ [("blocks:", Pretty.formatted blocks) | not (null blocks)]
        , [("tracks:", Pretty.formatted tracks) | not (null tracks)]
        , [("events:", Pretty.formatted events) | not (null events)]
        ]
    where section (title, doc) = [title, doc]

-- | Summarize the various types.  Is this really useful?
summary :: Cmd.CmdL Text
summary = do
    State.State views blocks tracks rulers _ <- State.get
    let f fm = PPrint.list (map show (Map.keys fm))
    return $ txt $ PPrint.record
        [ ("views", f views), ("blocks", f blocks)
        , ("tracks", f tracks), ("rulers", f rulers)
        ]

-- * configure

get_config :: Cmd.CmdL State.Config
get_config = State.config <#> State.get

get_default :: Cmd.CmdL State.Default
get_default = State.config#State.default_ <#> State.get

get_default_tempo :: Cmd.CmdL Signal.Y
get_default_tempo = State.config#State.default_#State.tempo <#> State.get

set_default_tempo :: Signal.Y -> Cmd.CmdL ()
set_default_tempo t = State.modify_config $ State.default_#State.tempo #= t

-- | 'State.config_global_transform' is an expression that's applied to the
-- output of derivation.
set_transform :: Text -> Cmd.CmdL ()
set_transform = State.modify_config . (State.global_transform #=)

get_transform :: Cmd.CmdL Text
get_transform = State.config#State.global_transform <#> State.get

edit_transform :: Cmd.CmdL ReplProtocol.Result
edit_transform = do
    expr <- get_transform
    return $ ReplProtocol.Edit expr "LState.set_transform"

transform :: (Text -> Text) -> Cmd.CmdL ()
transform = State.modify_config . (State.global_transform %=)

set_ky :: Maybe FilePath -> Cmd.CmdL ()
set_ky = State.modify_config . (State.ky_file #=)

-- ** meta

get_meta :: Cmd.CmdL State.Meta
get_meta = State.config#State.meta <#> State.get

set_creation_time :: Cmd.CmdL ()
set_creation_time = do
    now <- liftIO Time.getCurrentTime
    State.modify_config $ State.meta#State.creation #= now

set_notes :: Text -> Cmd.CmdL ()
set_notes = State.modify_config . (State.meta#State.notes #=)

-- *** midi performance

-- | Save the current root MIDI performance as \"correct\".
save_midi :: Cmd.CmdL ()
save_midi = do
    block_id <- State.get_root_id
    midi <- perform_midi block_id
    perf <- make_performance (Vector.fromList midi)
    State.modify_config $
        State.meta#State.midi_performances %= Map.insert block_id perf

get_midi_performance :: BlockId -> Cmd.CmdL State.MidiPerformance
get_midi_performance block_id =
    Cmd.require ("no saved performance for " <> showt block_id)
        =<< State.get_config
            (State.meta#State.midi_performances # Lens.map block_id #$)

-- | Compare the current root block performance against the saved one.
verify_performance :: Cmd.CmdL Text
verify_performance = do
    block_id <- State.get_root_id
    perf <- get_midi_performance block_id
    midi <- perform_midi block_id
    let name = Id.ident_name block_id
    dir <- Cmd.require "need a save dir to put tmp files"
        =<< Cmd.gets Cmd.state_save_dir
    (maybe_diff, _) <- liftIO $
        DiffPerformance.diff_midi_performance (untxt name) dir perf midi
    return $ fromMaybe "ok!" maybe_diff

perform_midi :: Cmd.M m => BlockId -> m [Midi.WriteMessage]
perform_midi block_id = do
    perf <- Cmd.get_performance block_id
    LEvent.events_of <$> PlayUtil.perform_from 0 perf

make_performance :: a -> Cmd.CmdT IO (State.Performance a)
make_performance perf = do
    time <- liftIO Time.getCurrentTime
    patch <- either (Cmd.throw . txt) (return . txt)
        =<< liftIO SourceControl.currentPatch
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
    events <- Stream.write_logs . Derive.r_events
        =<< Cmd.Lilypond.derive_block block_id
    config <- State.config#State.lilypond <#> State.get
    result <- LEvent.write_snd $
        Cmd.Lilypond.extract_movements config "title" events
    Text.Lazy.toStrict <$> Cmd.require_right id result


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
        Just (_, Cmd.SaveState fn) -> Cmd.modify $ \st -> st
            { Cmd.state_save_file =
                -- Assume the new name is new, and thus defaults to ReadWrite.
                Just (Cmd.ReadWrite, Cmd.SaveState $ replace_dir ns fn)
            }
        Just (_, Cmd.SaveRepo repo) -> do
            -- System.Directory.renameDirectory deletes the destination
            -- diretory for some reason.  I'd rather throw an exception.
            let old_dir = FilePath.takeDirectory repo
                new_dir = FilePath.replaceFileName old_dir
                    (untxt (Id.un_namespace ns))
            Cmd.rethrow_io $ File.ignoreEnoent $ Posix.rename old_dir new_dir
            Cmd.modify $ \st -> st
                { Cmd.state_save_file =
                    Just (Cmd.ReadWrite,
                        Cmd.SaveState $ new_dir </> FilePath.takeFileName repo)
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
load_merge :: Bool -> FilePath -> Cmd.CmdL ()
load_merge = load_as_ Nothing

-- | Load another score and put it in a new namespace.  Will probably fail if
-- the score itself uses multiple namespaces.
load_as :: Text -> Bool -- ^ if True, open views
    -> FilePath -> Cmd.CmdL ()
load_as = load_as_ . Just . Id.namespace

load_as_ :: Maybe Id.Namespace -> Bool -> FilePath -> Cmd.CmdL ()
load_as_ maybe_ns open_views fn = do
    (new_state, _) <- Save.read fn
    new_state <- State.exec_rethrow "strip clip" new_state $ do
        Transform.destroy_namespace Config.clip_namespace
        whenJust maybe_ns $ Transform.map_namespace . const
        unless open_views $
            mapM_ State.destroy_view =<< State.all_view_ids
    state <- State.get
    merged <- Cmd.require_right (("merge state: "<>) . pretty) $
        Transform.merge_states state new_state
    State.put merged

-- | Destroy the given namespace.
unload :: State.M m => Text -> m ()
unload = Transform.destroy_namespace . Id.namespace
