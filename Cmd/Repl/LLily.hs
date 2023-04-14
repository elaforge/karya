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
import qualified Util.Seq as Seq
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

get_config :: Ui.M m => m Lilypond.Config
get_config = Ui.config#UiConfig.lilypond <#> Ui.get

modify_config :: Ui.M m => (Lilypond.Config -> Lilypond.Config) -> m ()
modify_config modify = Ui.modify_config $ UiConfig.lilypond %= modify

with_config :: Cmd.M m => Lilypond.Config -> m a -> m a
with_config config = Ui.with_config (UiConfig.lilypond #= config)

set_quarter_duration :: Ui.M m => RealTime -> m ()
set_quarter_duration dur = modify_config $ \config ->
    config { Lilypond.config_quarter_duration = dur }

set_quantize :: Ui.M m => Lilypond.Duration -> m ()
set_quantize dur = modify_config $ \config ->
    config { Lilypond.config_quantize = dur }

set_dotted_rests :: Ui.M m => Bool -> m ()
set_dotted_rests b = modify_config $ \config ->
    config { Lilypond.config_dotted_rests = b }

toggle_display :: Ui.M m => Util.Instrument -> m ()
toggle_display inst = modify_staff inst $ Lilypond.display %= not

set_code :: Ui.M m => Util.Instrument -> [Text] -> m ()
set_code inst code = modify_staff inst $ Lilypond.code #= code

modify_staff :: Ui.M m => Util.Instrument
    -> (Lilypond.StaffConfig -> Lilypond.StaffConfig) -> m ()
modify_staff inst_ modify = do
    config <- get_config
    let staves = Lilypond.config_staves config
    case Lists.findModify ((==inst) . fst) (second modify) staves of
        Nothing -> Ui.throw $ "no staff config for " <> pretty inst
        Just staves -> modify_config $ const $
            config { Lilypond.config_staves = staves }
    where inst = Util.instrument inst_

-- | Set staff config, [(instrument, long_name, short_name)].  The order
-- determines the order of the staves on the page.
--
-- If there is no staff config, all instruments get staves.  Otherwise, only
-- instruments with 'Lilypond.StaffConfig's and 'Lilypond.staff_display' are
-- displayed.
set_staves :: Ui.M m => [(Text, Lilypond.Instrument, Lilypond.Instrument)]
    -> m (Lilypond.Config -> Lilypond.Config)
set_staves staves
    | not (null dups) = Ui.throw $ "duplicate instruments: " <> pretty dups
    | otherwise = return $ \config ->
        config { Lilypond.config_staves = map mk staves }
    where
    dups = map fst $ snd $
        Seq.partition_dups id [Util.instrument inst | (inst, _, _) <- staves]
    mk (inst, long, short) = (,) (Util.instrument inst) $
        Lilypond.empty_staff_config
            { Lilypond.staff_long = long, Lilypond.staff_short = short }

-- * compile

-- | Compile multiple blocks, with an explicit movement structure.
blocks :: Lilypond.Title -> [(Lilypond.Title, BlockId)] -> Cmd.CmdL Text
blocks title movements = do
    events <- mapM ((LEvent.write_logs <=< derive) . snd) movements
    compile_explicit title (zip (map fst movements) events)

-- | Compile the given block as lilypond.  If there are movements, they are
-- extracted from the events.
block :: BlockId -> Cmd.CmdL Text
block block_id = block_title (block_id_title block_id) block_id

-- | Compile the given block, but only with a single instrument.
block_inst :: Maybe Text -> Util.Instrument -> BlockId -> Cmd.CmdT IO Text
block_inst maybe_title inst block_id = do
    config <- get_config
    let title = fromMaybe (block_id_title block_id) maybe_title
    with_config (solo_instrument (Util.instrument inst) config) $
        block_title (title <> " - " <> inst) block_id

solo_instrument :: ScoreT.Instrument -> Lilypond.Config -> Lilypond.Config
solo_instrument inst = Lilypond.staves %= map solo
    where
    solo staff
        | fst staff == inst = staff
        | otherwise = second (Lilypond.display %= not) staff

block_title :: Lilypond.Title -> BlockId -> Cmd.CmdT IO Text
block_title title block_id =
    compile_extract title =<< LEvent.write_logs =<< derive block_id

-- | Compile the current block.
current :: Cmd.CmdL Text
current = block =<< Cmd.get_focused_block

-- | Show the output of the lilypond for the given block.
view_block :: BlockId -> Cmd.CmdL ()
view_block block_id = do
    filename <- Cmd.Lilypond.ly_filename $ block_id_title block_id
    liftIO $ void $ Thread.start $ Processes.call
        "open" [FilePath.replaceExtension filename ".pdf"]

view :: Cmd.CmdL ()
view = view_block =<< Cmd.get_focused_block

-- * from events

from_events :: [Score.Event] -> Cmd.CmdL Text
from_events events = do
    block_id <- Cmd.get_focused_block
    compile_extract (block_id_title block_id) events

-- * compile_ly

-- | Run lilypond with explicit movements.
compile_explicit :: Lilypond.Title -> [Cmd.Lilypond.Movement]
    -> Cmd.CmdL Text
compile_explicit title movements = do
    config <- get_config
    result <- LEvent.write_snd $
        Cmd.Lilypond.explicit_movements config title movements
    case result of
        Left err -> do
            Log.write err
            return $ Log.msg_text err
        Right output -> do
            filename <- Cmd.Lilypond.ly_filename title
            liftIO $ Cmd.Lilypond.compile_ly filename output
            return ""

-- | Extract movements from the events and run lilypond.  Return any error.
compile_extract :: Lilypond.Title -> [Score.Event] -> Cmd.CmdT IO Text
compile_extract title events = do
    config <- get_config
    result <- LEvent.write_snd $
        Cmd.Lilypond.extract_movements config title events
    case result of
        Left err -> do
            Log.write err
            return $ Log.msg_text err
        Right output -> do
            filename <- Cmd.Lilypond.ly_filename title
            liftIO $ Cmd.Lilypond.compile_ly filename output
            return ""

block_id_title :: BlockId -> Lilypond.Title
block_id_title = Id.ident_name

-- * debugging

-- | Convert the block to lilypond score.
block_ly :: Cmd.M m => BlockId -> m Lazy.Text
block_ly block_id = do
    config <- get_config
    (events, derive_logs) <- LEvent.partition <$> derive block_id
    let (result, logs) = Cmd.Lilypond.extract_movements config
            (block_id_title block_id) events
    mapM_ Log.write $ derive_logs ++ logs
    case result of
        Left err -> do
            Log.write err
            Cmd.throw $ Log.msg_text err
        Right ly -> return ly

-- | Derive the block to ly events.
convert :: Cmd.M m => BlockId -> m [Lilypond.Event]
convert block_id = do
    config <- get_config
    (score_events, derive_logs) <- LEvent.partition <$> derive block_id
    let (events, logs) = Cmd.Lilypond.convert config score_events
    mapM_ Log.write $ derive_logs ++ logs
    return events

e_note :: Lilypond.Event -> (Lilypond.Time, Lilypond.Time, Text)
e_note e = (Lilypond.event_start e, Lilypond.event_duration e,
    maybe "" Lilypond.to_lily (Lilypond.event_pitch e))

-- ** LPerf

-- | Run a lilypond derive and return score events.
derive :: Cmd.M m => BlockId -> m [LEvent.LEvent Score.Event]
derive block_id =
    Stream.to_list . Derive.r_events <$> Cmd.Lilypond.derive_block block_id

block_events :: Cmd.M m => BlockId -> m [LEvent.LEvent Score.Event]
block_events = fmap LPerf.normalize_events . block_events_unnormalized

block_events_unnormalized :: Cmd.M m => BlockId -> m [LEvent.LEvent Score.Event]
block_events_unnormalized block_id =
    Stream.to_list . Derive.r_events <$> Cmd.Lilypond.derive_block block_id

-- | Like 'LPerf.sel_events', but use the lilypond derive.
sel_events :: Cmd.M m => m [LEvent.LEvent Score.Event]
sel_events = filter_logs <$> get_sel False block_events

-- | Like 'sel_events' but take the root derivation.
root_sel_events :: Cmd.M m => m [LEvent.LEvent Score.Event]
root_sel_events = filter_logs <$> get_sel True block_events

filter_logs :: [LEvent.LEvent a] -> [LEvent.LEvent a]
filter_logs = filter (LEvent.event_or (not . Cache.is_cache_log))

get_sel :: Cmd.M m => Bool -- ^ from root
    -> (BlockId -> m [LEvent.LEvent Score.Event])
    -> m [LEvent.LEvent Score.Event]
get_sel from_root derive_events = do
    (block_id, _, track_ids, range) <- Selection.tracks
    let (start, end) = Events.range_times range
    events <- derive_events
        =<< if from_root then Ui.get_root_id else return block_id
    -- Lilypond derivation skips tempo tracks, so the usual ScoreTime ->
    -- RealTime map doesn't work.
    return $ LPerf.in_score_range Score.event_stack [block_id] track_ids
        start end events
