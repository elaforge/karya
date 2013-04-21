{-# LANGUAGE NoMonomorphismRestriction #-}
-- | Lilypond compiles are always kicked off manually.
--
-- I used to have some support for automatically reinvoking lilypond after
-- changes to a block, but it didn't seem too useful, since any useful amount
-- of lilypond score takes quite a while to compile.
module Cmd.Repl.LLily where
import qualified System.FilePath as FilePath
import qualified System.Process as Process

import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Process
import qualified Util.Seq as Seq

import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Lilypond
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score

import qualified Perform.Lilypond.Convert as Convert
import qualified Perform.Lilypond.Lilypond as Lilypond
import Types


-- * config

get_config :: (State.M m) => m Lilypond.Config
get_config = State.config#State.lilypond <#> State.get

modify_config :: (State.M m) => (Lilypond.Config -> Lilypond.Config) -> m ()
modify_config modify = State.modify_config $ State.lilypond %= modify

make_config :: RealTime -> Lilypond.Duration -> Lilypond.Config
make_config quarter quantize = Lilypond.default_config
    { Lilypond.config_quarter_duration = quarter
    , Lilypond.config_quantize = quantize
    }

set_code :: (State.M m) => String -> [Text] -> m ()
set_code inst code = modify_staff inst $ \staff -> staff
    { Lilypond.staff_code = code }

toggle_display :: (State.M m) => String -> m ()
toggle_display inst = modify_staff inst $ \staff -> staff
    { Lilypond.staff_display = not $ Lilypond.staff_display staff }

modify_staff :: (State.M m) => String
    -> (Lilypond.StaffConfig -> Lilypond.StaffConfig)
    -> m ()
modify_staff inst_ modify = do
    config <- get_config
    let staves = Lilypond.config_staves config
    case Seq.find_modify ((==inst) . fst) (second modify) staves of
        Nothing -> State.throw $ "no staff config for " <> Pretty.pretty inst
        Just staves -> modify_config $ const $
            config { Lilypond.config_staves = staves }
    where inst = Score.Instrument inst_

set_staves :: [(String, Lilypond.Instrument, Lilypond.Instrument)]
    -> Lilypond.Config -> Lilypond.Config
set_staves staves config =
    config { Lilypond.config_staves = map mk staves }
    where
    mk (inst, long, short) = (,) (Score.Instrument inst) $
        Lilypond.empty_staff_config
            { Lilypond.staff_long = long, Lilypond.staff_short = short }

-- * compile

blocks :: Lilypond.Title -> [(Lilypond.Title, BlockId)] -> Cmd.CmdL ()
blocks title movements = do
    events <- mapM (LEvent.write_logs <=< derive) (map snd movements)
    compile_lys title (zip (map fst movements) events)

-- | Compile the given block as lilypond.
block :: BlockId -> Cmd.CmdL ()
block block_id = blocks (title_of block_id) [("", block_id)]

-- | Compile the current block.
current :: Cmd.CmdL ()
current = block =<< Cmd.get_focused_block

-- | Show the output of the lilypond for the given block.
view_block :: BlockId -> Cmd.CmdL ()
view_block block_id = do
    filename <- Cmd.Lilypond.block_id_filename block_id
    liftIO $ Util.Process.logged $
        Process.proc "open" [FilePath.replaceExtension filename ".pdf"]
    return ()

view :: Cmd.CmdL ()
view = view_block =<< Cmd.get_focused_block

-- * from events

filter_inst :: [String] -> [Score.Event] -> [Score.Event]
filter_inst inst_s = filter ((`elem` insts) . Score.event_instrument)
    where insts = map Score.Instrument inst_s

from_events :: [Score.Event] -> Cmd.CmdL ()
from_events events = do
    block_id <- Cmd.get_focused_block
    compile_lys (title_of block_id) [("", events)]

-- * compile_ly

compile_lys :: Lilypond.Title -> [(Lilypond.Title, [Score.Event])]
    -> Cmd.CmdL ()
compile_lys title movements = do
    filename <- Cmd.Lilypond.ly_filename (untxt title)
    config <- get_config
    (result, logs) <- liftIO $
        Cmd.Lilypond.compile_lys filename config title movements
    mapM_ Log.write logs
    _ <- Cmd.require_right ("compile_ly: "++) result
    return ()

title_of :: BlockId -> Lilypond.Title
title_of = txt . Id.ident_name

-- * debugging

-- | Run a lilypond derive and return score events.
derive :: BlockId -> Cmd.CmdL Derive.Events
derive block_id = Derive.r_events <$> Cmd.Lilypond.derive_block block_id

-- | Convert down to lilypond score.
make_ly :: Cmd.CmdL (Either String [Text], [Log.Msg])
make_ly = do
    block_id <- Cmd.get_focused_block
    config <- get_config
    (events, logs) <- LEvent.partition <$> derive block_id
    let (result, ly_logs) = Cmd.Lilypond.make_lys config "title" [("", events)]
    return (fst <$> result, logs ++ ly_logs)

-- | Derive focused block to ly events.
convert :: Cmd.CmdL ([Lilypond.Event], [Log.Msg])
convert = do
    config <- get_config
    score_events <- derive =<< Cmd.get_focused_block
    let (events, logs) = LEvent.partition $
            Convert.convert (Lilypond.config_quarter_duration config)
            score_events
    return (Convert.quantize (Lilypond.config_quantize config) events, logs)
