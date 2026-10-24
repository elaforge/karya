-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Repl cmds providing general UI state operations.
module Cmd.Repl.LState where
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Time as Time
import qualified Data.Vector as Vector

import qualified System.FilePath as FilePath
import           System.FilePath ((</>))
import qualified System.Posix as Posix

import qualified Util.Exceptions as Exceptions
import qualified Util.Lens as Lens
import qualified Util.Log as Log
import qualified Util.PPrint as PPrint
import qualified Util.Pretty as Pretty

import qualified App.Path as Path
import qualified Cmd.Clip as Clip
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.DiffPerformance as DiffPerformance
import qualified Cmd.Ky as Ky
import qualified Cmd.Lilypond
import qualified Cmd.Load.Midi as Load.Midi
import qualified Cmd.Msg as Msg
import qualified Cmd.PlayUtil as PlayUtil
import qualified Cmd.Repl.LBlock as LBlock
import qualified Cmd.Repl.LEvent as Repl.LEvent
import qualified Cmd.Repl.LTrack as LTrack
import qualified Cmd.Save as Save

import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.Stream as Stream

import qualified Midi.Midi as Midi
import qualified Perform.Im.Convert as Im.Convert
import qualified Perform.Signal as Signal
import qualified Synth.Shared.Note as Shared.Note
import qualified Ui.Id as Id
import qualified Ui.Transform as Transform
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global
import           Types


-- | Find text in block titles, track titles, or events.
find :: Text -> Cmd.CmdL Text
find search = do
    blocks <- LBlock.find search
    tracks <- LTrack.find search
    events <- Repl.LEvent.find search
    pure $ Text.unlines $ concatMap section $ concat
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
    pure $ txt $ PPrint.record
        [ ("views", f views), ("blocks", f blocks)
        , ("tracks", f tracks), ("rulers", f rulers)
        ]

stats :: Cmd.CmdL Text
stats = Transform.showStats <$> Ui.get

-- * configure

getConfig :: Cmd.CmdL UiConfig.Config
getConfig = Ui.config <#> Ui.get

getDefault :: Cmd.CmdL UiConfig.Default
getDefault = Ui.config#UiConfig.default_ <#> Ui.get

getDefaultTempo :: Cmd.CmdL Signal.Y
getDefaultTempo = Ui.config#UiConfig.default_#UiConfig.tempo <#> Ui.get

setDefaultTempo :: Signal.Y -> Cmd.CmdL ()
setDefaultTempo t = Ui.modifyConfig $ UiConfig.default_#UiConfig.tempo #= t

reloadKy :: Cmd.CmdT IO ()
reloadKy = Ky.reload True Nothing

-- ** meta

getMeta :: Cmd.CmdL UiConfig.Meta
getMeta = Ui.config#UiConfig.meta <#> Ui.get

setCreationTime :: Cmd.CmdL ()
setCreationTime = do
    now <- liftIO Time.getCurrentTime
    Ui.modifyConfig $ UiConfig.meta#UiConfig.creation #= now

-- *** performance

savePerf :: Cmd.CmdT IO Text
savePerf = do
    config <- Ui.getConfig id
    midi <- if UiConfig.hasMidi (UiConfig.configAllocations config)
        then saveMidi >> pure (Just "saved midi") else pure Nothing
    im <- if UiConfig.hasIm (UiConfig.configAllocations config)
        then saveIm >> pure (Just "saved im") else pure Nothing
    -- TODO sc
    pure $ Text.intercalate ", " $ Maybe.catMaybes [midi, im]

-- | Save the current root MIDI performance as \"correct\".
saveMidi :: Cmd.CmdT IO ()
saveMidi = savePerformance UiConfig.midiPerformances
    (fmap Vector.fromList . performMidi)

saveIm :: Cmd.CmdT IO ()
saveIm = savePerformance UiConfig.imPerformances performIm

saveLilypond :: Cmd.CmdT IO ()
saveLilypond = savePerformance UiConfig.lilypondPerformances
    performLilypond

--

getMidiPerformance :: BlockId -> Cmd.CmdL UiConfig.MidiPerformance
getMidiPerformance blockId =
    Cmd.require ("no saved performance for " <> showt blockId)
        =<< Ui.getConfig
            (UiConfig.meta#UiConfig.midiPerformances # Lens.map blockId #$)

-- | Compare the current root block performance against the saved one.
verifyPerformance :: Cmd.CmdL Text
verifyPerformance = do
    blockId <- Ui.getRootId
    perf <- getMidiPerformance blockId
    midi <- performMidi blockId
    let name = Id.identName blockId
    dir <- Cmd.require "need a save dir to put tmp files"
        =<< Cmd.gets Cmd.stateSaveDir
    (maybeDiff, _) <- liftIO $
        DiffPerformance.diffMidi (untxt name) dir perf midi
    pure $ fromMaybe "ok!" maybeDiff

savePerformance :: Lens UiConfig.Meta (Map BlockId (UiConfig.Performance a))
    -> (BlockId -> Cmd.CmdT IO a) -> Cmd.CmdT IO ()
savePerformance field perform = do
    blockId <- Ui.getRootId
    events <- perform blockId
    perf <- liftIO $ UiConfig.makePerformance events
    Ui.modifyMeta $ field %= Map.insert blockId perf

--

performMidi :: Cmd.M m => BlockId -> m [Midi.WriteMessage]
performMidi blockId = do
    perf <- Cmd.getPerformance blockId
    LEvent.eventsOf . fst <$> PlayUtil.performFrom 0 (Cmd.perfEvents perf)

performIm :: Cmd.M m => BlockId -> m (Vector.Vector Shared.Note.Note)
performIm blockId = do
    perf <- Cmd.getPerformance blockId
    lookupInst <- Cmd.getLookupInstrument
    pure $ Vector.fromList $ LEvent.eventsOf $
        Im.Convert.convert blockId lookupInst $
        Vector.toList (Msg.perfEvents perf)

performLilypond :: Cmd.M m => BlockId -> m Text
performLilypond blockId = do
    events <- Stream.writeLogs . Derive.rEvents
        =<< Cmd.Lilypond.deriveBlock blockId
    config <- Ui.config#UiConfig.lilypond <#> Ui.get
    result <- LEvent.writeSnd $
        Cmd.Lilypond.extractMovements config "title" events
    case result of
        Left err -> Log.write err >> pure (Log.msgText err)
        Right ly -> pure $ Text.Lazy.toStrict ly


-- * transform

setNamespace :: Id.Namespace -> Cmd.CmdL ()
setNamespace = Create.renameProject

-- | Set the score namespace to the given one.  Also update the project_dir
-- and move the actual directory.
rename :: Id.Namespace -> Cmd.CmdL ()
rename ns = do
    Create.renameProject ns
    Cmd.gets Cmd.stateSaveFile >>= \case
        Nothing -> pure ()
        Just (_, Cmd.SaveState fn) -> do
            newPath <- liftIO $ Path.canonical $
                replaceDir ns (Path.toPath fn)
            Cmd.modify $ \st -> st
                { Cmd.stateSaveFile =
                    -- Assume the new name is new, and thus defaults to
                    -- ReadWrite.
                    Just (Cmd.ReadWrite, Cmd.SaveState newPath)
                }
        Just (_, Cmd.SaveRepo repo) -> do
            let oldDir = FilePath.takeDirectory (Path.toPath repo)
                newDir = FilePath.replaceFileName oldDir
                    (untxt (Id.unNamespace ns))
            -- System.Directory.renameDirectory deletes the destination
            -- diretory for some reason.  I'd rather throw an exception.
            Cmd.rethrowIo $ Exceptions.ignoreEnoent $
                Posix.rename oldDir newDir
            newPath <- liftIO $ Path.canonical $
                newDir </> FilePath.takeFileName (Path.toPath repo)
            Cmd.modify $ \st -> st
                { Cmd.stateSaveFile =
                    Just (Cmd.ReadWrite, Cmd.SaveState newPath)
                }
    where
    replaceDir ns path = newDir </> FilePath.takeFileName path
        where
        newDir = FilePath.replaceFileName (FilePath.takeDirectory path)
            (untxt (Id.unNamespace ns))

fix :: Cmd.M m => m [Text]
fix = Ui.fixState

-- * load

loadMidi :: FilePath -> Cmd.CmdL BlockId
loadMidi = Load.Midi.load

-- | Load the state from the file and merge it with the current state.  This
-- will fail if any IDs collide, so hopefully they live in different
-- namespaces.  In fact, this is why IDs have namespaces.
loadMerge :: Bool -> FilePath -> Cmd.CmdL ()
loadMerge = loadAs_ Nothing

-- | Load another score and put it in a new namespace.  Will probably fail if
-- the score itself uses multiple namespaces.
loadAs :: Text -> Bool -- ^ if True, open views
    -> FilePath -> Cmd.CmdL ()
loadAs = loadAs_ . Just . Id.namespace

loadAs_ :: Maybe Id.Namespace -> Bool -> FilePath -> Cmd.CmdL ()
loadAs_ maybeNs openViews fn = do
    (newState, _) <- Save.read True fn
    newState <- Ui.execRethrow "strip clip" newState $ do
        Transform.destroyNamespace Clip.clipNamespace
        whenJust maybeNs $ Transform.mapNamespace . const
        unless openViews $
            mapM_ Ui.destroyView =<< Ui.allViewIds
    state <- Ui.get
    merged <- Cmd.requireRight (("merge state: "<>) . pretty) $
        Transform.mergeStates state newState
    Ui.put merged

-- | Destroy the given namespace.
unload :: Ui.M m => Text -> m ()
unload = Transform.destroyNamespace . Id.namespace
