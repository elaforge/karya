-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Lilypond compiles are always kicked off manually.

    I used to have some support for automatically reinvoking lilypond after
    changes to a block, but it didn't seem too useful, since any useful amount
    of lilypond score takes quite a while to compile.

    Set 1t to equal one quarter note, quantize to 16th notes.  Configure
    \"inst\" with short and long names, then change them.

    > LLily.set_quarter_duration 1
    > LLily.set_quantize Lilypond.D16
    > LLily.modify_config =<< LLily.set_staves [("inst", "long", "short")]
    > LLily.modify_staff "inst" $ Lilypond.short #= "a" . Lilypond.long #= "b"
-}
module Cmd.Repl.LLily where
import qualified Data.Text.Lazy as Lazy
import qualified System.FilePath as FilePath

import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Processes as Processes
import qualified Util.Thread as Thread

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Lilypond
import qualified Cmd.Repl.LPerf as LPerf
import qualified Cmd.Repl.Util as Util
import qualified Cmd.Selection as Selection

import qualified Derive.Cache as Cache
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Stream as Stream

import qualified Perform.Lilypond as Lilypond
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global
import           Types


-- * config

getConfig :: Ui.M m => m Lilypond.Config
getConfig = Ui.config#UiConfig.lilypond <#> Ui.get

modifyConfig :: Ui.M m => (Lilypond.Config -> Lilypond.Config) -> m ()
modifyConfig modify = Ui.modifyConfig $ UiConfig.lilypond %= modify

withConfig :: Cmd.M m => Lilypond.Config -> m a -> m a
withConfig config = Ui.withConfig (UiConfig.lilypond #= config)

setQuarterDuration :: Ui.M m => RealTime -> m ()
setQuarterDuration dur = modifyConfig $ \config ->
    config { Lilypond.configQuarterDuration = dur }

setQuantize :: Ui.M m => Lilypond.Duration -> m ()
setQuantize dur = modifyConfig $ \config ->
    config { Lilypond.configQuantize = dur }

setDottedRests :: Ui.M m => Bool -> m ()
setDottedRests b = modifyConfig $ \config ->
    config { Lilypond.configDottedRests = b }

toggleDisplay :: Ui.M m => Util.Instrument -> m ()
toggleDisplay inst = modifyStaff inst $ Lilypond.display %= not

setCode :: Ui.M m => Util.Instrument -> [Text] -> m ()
setCode inst code = modifyStaff inst $ Lilypond.code #= code

modifyStaff :: Ui.M m => Util.Instrument
    -> (Lilypond.StaffConfig -> Lilypond.StaffConfig) -> m ()
modifyStaff inst_ modify = do
    config <- getConfig
    let staves = Lilypond.configStaves config
    case Lists.findModify ((==inst) . fst) (second modify) staves of
        Nothing -> Ui.throw $ "no staff config for " <> pretty inst
        Just staves -> modifyConfig $ const $
            config { Lilypond.configStaves = staves }
    where inst = Util.instrument inst_

-- | Set staff config, [(instrument, long_name, short_name)].  The order
-- determines the order of the staves on the page.
--
-- If there is no staff config, all instruments get staves.  Otherwise, only
-- instruments with 'Lilypond.StaffConfig's and 'Lilypond.staff_display' are
-- displayed.
setStaves :: Ui.M m => [(Text, Lilypond.Instrument, Lilypond.Instrument)]
    -> m (Lilypond.Config -> Lilypond.Config)
setStaves staves
    | not (null dups) = Ui.throw $ "duplicate instruments: " <> pretty dups
    | otherwise = pure $ \config ->
        config { Lilypond.configStaves = map mk staves }
    where
    dups = map fst $ snd $
        Lists.partitionDups id [Util.instrument inst | (inst, _, _) <- staves]
    mk (inst, long, short) = (,) (Util.instrument inst) $
        Lilypond.emptyStaffConfig
            { Lilypond.staffLong = long, Lilypond.staffShort = short }

-- * compile

-- | Compile multiple blocks, with an explicit movement structure.
blocks :: Lilypond.Title -> [(Lilypond.Title, BlockId)] -> Cmd.CmdL Text
blocks title movements = do
    events <- mapM ((LEvent.writeLogs <=< derive) . snd) movements
    compileExplicit title (zip (map fst movements) events)

-- | Compile the given block as lilypond.  If there are movements, they are
-- extracted from the events.
block :: BlockId -> Cmd.CmdL Text
block blockId = blockTitle (blockIdTitle blockId) blockId

-- | Compile the given block, but only with a single instrument.
blockInst :: Maybe Text -> Util.Instrument -> BlockId -> Cmd.CmdT IO Text
blockInst maybeTitle inst blockId = do
    config <- getConfig
    let title = fromMaybe (blockIdTitle blockId) maybeTitle
    withConfig (soloInstrument (Util.instrument inst) config) $
        blockTitle (title <> " - " <> inst) blockId

soloInstrument :: ScoreT.Instrument -> Lilypond.Config -> Lilypond.Config
soloInstrument inst = Lilypond.staves %= map solo
    where
    solo staff
        | fst staff == inst = staff
        | otherwise = second (Lilypond.display %= not) staff

blockTitle :: Lilypond.Title -> BlockId -> Cmd.CmdT IO Text
blockTitle title blockId =
    compileExtract title =<< LEvent.writeLogs =<< derive blockId

-- | Compile the current block.
current :: Cmd.CmdL Text
current = block =<< Cmd.getFocusedBlock

-- | Show the output of the lilypond for the given block.
viewBlock :: BlockId -> Cmd.CmdL ()
viewBlock blockId = do
    filename <- Cmd.Lilypond.lyFilename $ blockIdTitle blockId
    liftIO $ void $ Thread.start $ Processes.call
        "open" [FilePath.replaceExtension filename ".pdf"]

view :: Cmd.CmdL ()
view = viewBlock =<< Cmd.getFocusedBlock

-- * from events

fromEvents :: [Score.Event] -> Cmd.CmdL Text
fromEvents events = do
    blockId <- Cmd.getFocusedBlock
    compileExtract (blockIdTitle blockId) events

-- * compile_ly

-- | Run lilypond with explicit movements.
compileExplicit :: Lilypond.Title -> [Cmd.Lilypond.Movement]
    -> Cmd.CmdL Text
compileExplicit title movements = do
    config <- getConfig
    result <- LEvent.writeSnd $
        Cmd.Lilypond.explicitMovements config title movements
    case result of
        Left err -> do
            Log.write err
            pure $ Log.msgText err
        Right output -> do
            filename <- Cmd.Lilypond.lyFilename title
            liftIO $ Cmd.Lilypond.compileLy filename output
            pure ""

-- | Extract movements from the events and run lilypond.  Return any error.
compileExtract :: Lilypond.Title -> [Score.Event] -> Cmd.CmdT IO Text
compileExtract title events = do
    config <- getConfig
    result <- LEvent.writeSnd $
        Cmd.Lilypond.extractMovements config title events
    case result of
        Left err -> do
            Log.write err
            pure $ Log.msgText err
        Right output -> do
            filename <- Cmd.Lilypond.lyFilename title
            liftIO $ Cmd.Lilypond.compileLy filename output
            pure ""

blockIdTitle :: BlockId -> Lilypond.Title
blockIdTitle = Id.identName

-- * debugging

-- | Convert the block to lilypond score.
blockLy :: Cmd.M m => BlockId -> m Lazy.Text
blockLy blockId = do
    config <- getConfig
    (events, deriveLogs) <- LEvent.partition <$> derive blockId
    let (result, logs) = Cmd.Lilypond.extractMovements config
            (blockIdTitle blockId) events
    mapM_ Log.write $ deriveLogs ++ logs
    case result of
        Left err -> do
            Log.write err
            Cmd.throw $ Log.msgText err
        Right ly -> pure ly

-- | Derive the block to ly events.
convert :: Cmd.M m => BlockId -> m [Lilypond.Event]
convert blockId = do
    config <- getConfig
    (scoreEvents, deriveLogs) <- LEvent.partition <$> derive blockId
    let (events, logs) = Cmd.Lilypond.convert config scoreEvents
    mapM_ Log.write $ deriveLogs ++ logs
    pure events

eNote :: Lilypond.Event -> (Lilypond.Time, Lilypond.Time, Text)
eNote e = (Lilypond.eventStart e, Lilypond.eventDuration e,
    maybe "" Lilypond.toLily (Lilypond.eventPitch e))

-- ** LPerf

-- | Run a lilypond derive and return score events.
derive :: Cmd.M m => BlockId -> m [LEvent.LEvent Score.Event]
derive blockId =
    Stream.toList . Derive.rEvents <$> Cmd.Lilypond.deriveBlock blockId

blockEvents :: Cmd.M m => BlockId -> m [LEvent.LEvent Score.Event]
blockEvents = fmap LPerf.normalizeEvents . blockEventsUnnormalized

blockEventsUnnormalized :: Cmd.M m => BlockId -> m [LEvent.LEvent Score.Event]
blockEventsUnnormalized blockId =
    Stream.toList . Derive.rEvents <$> Cmd.Lilypond.deriveBlock blockId

-- | Like 'LPerf.sel_events', but use the lilypond derive.
selEvents :: Cmd.M m => m [LEvent.LEvent Score.Event]
selEvents = filterLogs <$> getSel False blockEvents

-- | Like 'sel_events' but take the root derivation.
rootSelEvents :: Cmd.M m => m [LEvent.LEvent Score.Event]
rootSelEvents = filterLogs <$> getSel True blockEvents

filterLogs :: [LEvent.LEvent a] -> [LEvent.LEvent a]
filterLogs = filter (LEvent.eventOr (not . Cache.isCacheLog))

getSel :: Cmd.M m => Bool -- ^ from root
    -> (BlockId -> m [LEvent.LEvent Score.Event])
    -> m [LEvent.LEvent Score.Event]
getSel fromRoot deriveEvents = do
    (blockId, _, trackIds, range) <- Selection.tracks
    let (start, end) = Events.rangeTimes range
    events <- deriveEvents
        =<< if fromRoot then Ui.getRootId else pure blockId
    -- Lilypond derivation skips tempo tracks, so the usual ScoreTime ->
    -- RealTime map doesn't work.
    pure $ LPerf.inScoreRange Score.eventStack [blockId] trackIds
        start end events
