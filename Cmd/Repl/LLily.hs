-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Lilypond compiles are always kicked off manually.

    I used to have some support for automatically reinvoking lilypond after
    changes to a block, but it didn't seem too useful, since any useful amount
    of lilypond score takes quite a while to compile.

    Set 1t to equal one quarter note, quantize to 16th notes.  Configure
    \"inst\" with short and long names, then change them.

    > LLily.modify_config $ LLily.make_config 1 Lilypond.D16
    > LLily.modify_config =<< LLily.set_staves [("inst", "long", "short")]
    > LLily.modify_staff "inst" $ Lilypond.short #= "a" . Lilypond.long #= "b"
-}
module Cmd.Repl.LLily where
import qualified Data.Text.Lazy as Lazy
import qualified System.FilePath as FilePath
import qualified System.Process as Process

import qualified Util.Log as Log
import qualified Util.Process
import qualified Util.Seq as Seq

import qualified Ui.Id as Id
import qualified Ui.Ui as Ui
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Lilypond
import qualified Cmd.Repl.Util as Util

import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.Stream as Stream

import qualified Perform.Lilypond as Lilypond
import Global
import Types


-- * config

get_config :: Ui.M m => m Lilypond.Config
get_config = Ui.config#Ui.lilypond <#> Ui.get

modify_config :: Ui.M m => (Lilypond.Config -> Lilypond.Config) -> m ()
modify_config modify = Ui.modify_config $ Ui.lilypond %= modify

with_config :: Cmd.M m => Lilypond.Config -> m a -> m a
with_config config = Ui.with_config (Ui.lilypond #= config)

make_config :: RealTime -> Lilypond.Duration -> Lilypond.Config
make_config quarter quantize = Lilypond.default_config
    { Lilypond.config_quarter_duration = quarter
    , Lilypond.config_quantize = quantize
    }

toggle_display :: Ui.M m => Util.Instrument -> m ()
toggle_display inst = modify_staff inst $ Lilypond.display %= not

modify_staff :: Ui.M m => Util.Instrument
    -> (Lilypond.StaffConfig -> Lilypond.StaffConfig) -> m ()
modify_staff inst_ modify = do
    config <- get_config
    let staves = Lilypond.config_staves config
    case Seq.find_modify ((==inst) . fst) (second modify) staves of
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

solo_instrument :: Score.Instrument -> Lilypond.Config -> Lilypond.Config
solo_instrument inst = Lilypond.staves %= map solo
    where
    solo staff
        | fst staff == inst = staff
        | otherwise = second (Lilypond.display %= not) staff

block_title :: Lilypond.Title -> BlockId -> Cmd.CmdL Text
block_title title block_id =
    compile_extract title =<< LEvent.write_logs =<< derive block_id

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
            Log.warn $ "compile_explicit: " <> err
            return err
        Right output -> do
            filename <- Cmd.Lilypond.ly_filename title
            liftIO $ Cmd.Lilypond.compile_ly filename output
            return ""

-- | Extract movements from the events and run lilypond.  Return any error.
compile_extract :: Lilypond.Title -> [Score.Event] -> Cmd.CmdL Text
compile_extract title events = do
    config <- get_config
    result <- LEvent.write_snd $
        Cmd.Lilypond.extract_movements config title events
    case result of
        Left err -> do
            Log.warn $ "compile_ly: " <> err
            return err
        Right output -> do
            filename <- Cmd.Lilypond.ly_filename title
            liftIO $ Cmd.Lilypond.compile_ly filename output
            return ""

block_id_title :: BlockId -> Lilypond.Title
block_id_title = Id.ident_name

-- * debugging

-- | Run a lilypond derive and return score events.
derive :: BlockId -> Cmd.CmdL [LEvent.LEvent Score.Event]
derive block_id =
    Stream.to_list . Derive.r_events <$> Cmd.Lilypond.derive_block block_id

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

e_note :: Lilypond.Event -> (Lilypond.Time, Lilypond.Time, Text)
e_note e = (Lilypond.event_start e, Lilypond.event_duration e,
    Lilypond.event_pitch e)
