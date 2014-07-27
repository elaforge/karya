-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE NoMonomorphismRestriction #-}
-- | Lilypond compiles are always kicked off manually.
--
-- I used to have some support for automatically reinvoking lilypond after
-- changes to a block, but it didn't seem too useful, since any useful amount
-- of lilypond score takes quite a while to compile.
module Cmd.Repl.LLily where
import qualified Data.Text.Lazy as Lazy
import qualified System.FilePath as FilePath
import qualified System.Process as Process

import Util.Control
import qualified Util.Log as Log
import qualified Util.Process
import qualified Util.Seq as Seq

import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Lilypond
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score

import qualified Perform.Lilypond as Lilypond
import Types


-- * config

get_config :: State.M m => m Lilypond.Config
get_config = State.config#State.lilypond <#> State.get

modify_config :: State.M m => (Lilypond.Config -> Lilypond.Config) -> m ()
modify_config modify = State.modify_config $ State.lilypond %= modify

make_config :: RealTime -> Lilypond.Duration -> Lilypond.Config
make_config quarter quantize = Lilypond.default_config
    { Lilypond.config_quarter_duration = quarter
    , Lilypond.config_quantize = quantize
    }

set_code :: State.M m => Text -> [Text] -> m ()
set_code inst code = modify_staff inst $ \staff -> staff
    { Lilypond.staff_code = code }

toggle_display :: State.M m => Text -> m ()
toggle_display inst = modify_staff inst $ \staff -> staff
    { Lilypond.staff_display = not $ Lilypond.staff_display staff }

modify_staff :: State.M m => Text
    -> (Lilypond.StaffConfig -> Lilypond.StaffConfig)
    -> m ()
modify_staff inst_ modify = do
    config <- get_config
    let staves = Lilypond.config_staves config
    case Seq.find_modify ((==inst) . fst) (second modify) staves of
        Nothing -> State.throw $ "no staff config for " <> pretty inst
        Just staves -> modify_config $ const $
            config { Lilypond.config_staves = staves }
    where inst = Score.Instrument inst_

-- | Set staff config, [(instrument, long_name, short_name)].
-- If there is no staff config, all instruments get staves.  Otherwise, only
-- instruments with 'Lilypond.StaffConfig's and 'Lilypond.staff_display' are
-- displayed.
set_staves :: [(Text, Lilypond.Instrument, Lilypond.Instrument)]
    -> Lilypond.Config -> Lilypond.Config
set_staves staves config =
    config { Lilypond.config_staves = map mk staves }
    where
    mk (inst, long, short) = (,) (Score.Instrument inst) $
        Lilypond.empty_staff_config
            { Lilypond.staff_long = long, Lilypond.staff_short = short }

-- * compile

-- | Compile multiple blocks, with an explicit movement structure.
blocks :: Lilypond.Title -> [(Lilypond.Title, BlockId)] -> Cmd.CmdL Text
blocks title movements = do
    events <- mapM ((LEvent.write_logs <=< derive) . snd) movements
    explicit_movements title (zip (map fst movements) events)

-- | Compile the given block as lilypond.  If there are movements, they are
-- extracted from the events.
block :: BlockId -> Cmd.CmdL Text
block block_id = do
    events <- LEvent.write_logs =<< derive block_id
    extract_movements (block_id_title block_id) events

block_title :: Lilypond.Title -> BlockId -> Cmd.CmdL Text
block_title title block_id =
    extract_movements title =<< LEvent.write_logs =<< derive block_id

-- | Compile the current block.
current :: Cmd.CmdL Text
current = block =<< Cmd.get_focused_block

-- | Show the output of the lilypond for the given block.
view_block :: BlockId -> Cmd.CmdL ()
view_block block_id = do
    filename <- Cmd.Lilypond.ly_filename $ block_id_title block_id
    liftIO $ Util.Process.logged $
        Process.proc "open" [FilePath.replaceExtension filename ".pdf"]
    return ()

view :: Cmd.CmdL ()
view = view_block =<< Cmd.get_focused_block

-- * from events

from_events :: [Score.Event] -> Cmd.CmdL Text
from_events events = do
    block_id <- Cmd.get_focused_block
    extract_movements (block_id_title block_id) events

-- * compile_ly

explicit_movements :: Lilypond.Title -> [Cmd.Lilypond.Movement]
    -> Cmd.CmdL Text
explicit_movements title movements = do
    config <- get_config
    let (result, logs) = Cmd.Lilypond.explicit_movements config title movements
    mapM_ Log.write logs
    case result of
        Left err -> do
            Log.warn $ "explicit_movements: " <> untxt err
            return err
        Right output -> do
            filename <- Cmd.Lilypond.ly_filename title
            liftIO $ Cmd.Lilypond.compile_ly filename output
            return ""

extract_movements :: Lilypond.Title -> [Score.Event] -> Cmd.CmdL Text
extract_movements title events = do
    config <- get_config
    let (result, logs) = Cmd.Lilypond.extract_movements config title events
    mapM_ Log.write logs
    case result of
        Left err -> do
            Log.warn $ "extract_movements: " <> untxt err
            return err
        Right output -> do
            filename <- Cmd.Lilypond.ly_filename title
            liftIO $ Cmd.Lilypond.compile_ly filename output
            return ""

block_id_title :: BlockId -> Lilypond.Title
block_id_title = Id.ident_name

-- * debugging

-- | Run a lilypond derive and return score events.
derive :: BlockId -> Cmd.CmdL Derive.Events
derive block_id = Derive.r_events <$> Cmd.Lilypond.derive_block block_id

-- | Convert current block to lilypond score.
make_ly :: Cmd.CmdL (Either Text Lazy.Text, [Log.Msg])
make_ly = do
    block_id <- Cmd.get_focused_block
    config <- get_config
    (events, derive_logs) <- LEvent.partition <$> derive block_id
    let (result, logs) = Cmd.Lilypond.extract_movements config
            (block_id_title block_id) events
    return (result, derive_logs ++ logs)

-- | Derive focused block to ly events.
convert :: Cmd.CmdL ([Lilypond.Event], [Log.Msg])
convert = do
    config <- get_config
    (score_events, derive_logs) <-
        LEvent.partition <$> (derive =<< Cmd.get_focused_block)
    let (events, logs) = Cmd.Lilypond.convert config score_events
    return (events, derive_logs ++ logs)
