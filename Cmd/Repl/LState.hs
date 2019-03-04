-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Repl cmds providing general UI state operations.
module Cmd.Repl.LState where
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Time as Time
import qualified Data.Vector as Vector

import qualified System.FilePath as FilePath
import           System.FilePath ((</>))
import qualified System.Posix as Posix

import qualified Util.File as File
import qualified Util.Lens as Lens
import qualified Util.Log as Log
import qualified Util.PPrint as PPrint
import qualified Util.Pretty as Pretty
import qualified Util.SourceControl as SourceControl

import qualified App.Config as Config
import qualified App.Path as Path
import qualified App.ReplProtocol as ReplProtocol

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

import qualified Midi.Midi as Midi
import qualified Perform.Signal as Signal
import qualified Ui.Id as Id
import qualified Ui.Transform as Transform
import qualified Ui.Ui as Ui

import           Global
import           Types


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
    Ui.State views blocks tracks rulers _ <- Ui.get
    let f fm = PPrint.list (map show (Map.keys fm))
    return $ txt $ PPrint.record
        [ ("views", f views), ("blocks", f blocks)
        , ("tracks", f tracks), ("rulers", f rulers)
        ]

-- * configure

get_config :: Cmd.CmdL Ui.Config
get_config = Ui.config <#> Ui.get

get_default :: Cmd.CmdL Ui.Default
get_default = Ui.config#Ui.default_ <#> Ui.get

get_default_tempo :: Cmd.CmdL Signal.Y
get_default_tempo = Ui.config#Ui.default_#Ui.tempo <#> Ui.get

set_default_tempo :: Signal.Y -> Cmd.CmdL ()
set_default_tempo t = Ui.modify_config $ Ui.default_#Ui.tempo #= t

ky :: Cmd.CmdL ReplProtocol.Result
ky = do
    ky <- get_ky
    return $ ReplProtocol.Edit $ ReplProtocol.Editor
        { _file = ReplProtocol.Text ky
        , _line_number = 0
        , _on_save = Just "LState.set_ky %s"
        , _on_send = Nothing
        }

get_ky :: Ui.M m => m Text
get_ky = Ui.config#Ui.ky <#> Ui.get

set_ky :: Ui.M m => Text -> m ()
set_ky = Ui.modify_config . (Ui.ky #=)

-- ** meta

get_meta :: Cmd.CmdL Ui.Meta
get_meta = Ui.config#Ui.meta <#> Ui.get

set_creation_time :: Cmd.CmdL ()
set_creation_time = do
    now <- liftIO Time.getCurrentTime
    Ui.modify_config $ Ui.meta#Ui.creation #= now

set_notes :: Text -> Cmd.CmdL ()
set_notes = Ui.modify_config . (Ui.meta#Ui.notes #=)

-- *** midi performance

-- | Save the current root MIDI performance as \"correct\".
save_midi :: Cmd.CmdL ()
save_midi = do
    block_id <- Ui.get_root_id
    midi <- perform_midi block_id
    perf <- make_performance (Vector.fromList midi)
    Ui.modify_config $ Ui.meta#Ui.midi_performances %= Map.insert block_id perf

get_midi_performance :: BlockId -> Cmd.CmdL Ui.MidiPerformance
get_midi_performance block_id =
    Cmd.require ("no saved performance for " <> showt block_id)
        =<< Ui.get_config (Ui.meta#Ui.midi_performances # Lens.map block_id #$)

-- | Compare the current root block performance against the saved one.
verify_performance :: Cmd.CmdL Text
verify_performance = do
    block_id <- Ui.get_root_id
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

make_performance :: a -> Cmd.CmdT IO (Ui.Performance a)
make_performance perf = do
    time <- liftIO Time.getCurrentTime
    patch <- Cmd.require_right txt =<< liftIO (SourceControl.current ".")
    return $ Ui.Performance
        { perf_performance = perf
        , perf_creation = time
        , perf_patch = Text.unlines $ map ($patch)
            [ SourceControl._hash
            , SourceControl.showDate . SourceControl._date
            , SourceControl._summary
            ]
        }

-- *** lilypond performance

save_lilypond :: Cmd.CmdL ()
save_lilypond = do
    block_id <- Ui.get_root_id
    lily <- lilypond_performance block_id
    perf <- make_performance lily
    Ui.modify_config $
        Ui.meta#Ui.lilypond_performances %= Map.insert block_id perf

lilypond_performance :: Cmd.M m => BlockId -> m Text
lilypond_performance block_id = do
    events <- Stream.write_logs . Derive.r_events
        =<< Cmd.Lilypond.derive_block block_id
    config <- Ui.config#Ui.lilypond <#> Ui.get
    result <- LEvent.write_snd $
        Cmd.Lilypond.extract_movements config "title" events
    case result of
        Left err -> Log.write err >> return (Log.msg_text err)
        Right ly -> return $ Text.Lazy.toStrict ly


-- * transform

set_namespace :: Id.Namespace -> Cmd.CmdL ()
set_namespace = Create.rename_project

-- | Set the score namespace to the given one.  Also update the project_dir
-- and move the actual directory.
rename :: Id.Namespace -> Cmd.CmdL ()
rename ns = do
    Create.rename_project ns
    Cmd.gets Cmd.state_save_file >>= \case
        Nothing -> return ()
        Just (_, Cmd.SaveState fn) -> do
            new_path <- liftIO $ Path.canonical $
                replace_dir ns (Path.to_path fn)
            Cmd.modify $ \st -> st
                { Cmd.state_save_file =
                    -- Assume the new name is new, and thus defaults to
                    -- ReadWrite.
                    Just (Cmd.ReadWrite, Cmd.SaveState new_path)
                }
        Just (_, Cmd.SaveRepo repo) -> do
            let old_dir = FilePath.takeDirectory (Path.to_path repo)
                new_dir = FilePath.replaceFileName old_dir
                    (untxt (Id.un_namespace ns))
            -- System.Directory.renameDirectory deletes the destination
            -- diretory for some reason.  I'd rather throw an exception.
            Cmd.rethrow_io $ File.ignoreEnoent $ Posix.rename old_dir new_dir
            new_path <- liftIO $ Path.canonical $
                new_dir </> FilePath.takeFileName (Path.to_path repo)
            Cmd.modify $ \st -> st
                { Cmd.state_save_file =
                    Just (Cmd.ReadWrite, Cmd.SaveState new_path)
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
    new_state <- Ui.exec_rethrow "strip clip" new_state $ do
        Transform.destroy_namespace Config.clip_namespace
        whenJust maybe_ns $ Transform.map_namespace . const
        unless open_views $
            mapM_ Ui.destroy_view =<< Ui.all_view_ids
    state <- Ui.get
    merged <- Cmd.require_right (("merge state: "<>) . pretty) $
        Transform.merge_states state new_state
    Ui.put merged

-- | Destroy the given namespace.
unload :: Ui.M m => Text -> m ()
unload = Transform.destroy_namespace . Id.namespace
