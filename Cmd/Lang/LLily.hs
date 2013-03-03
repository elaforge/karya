{-# LANGUAGE NoMonomorphismRestriction #-}
-- | Lilypond compiles are always kicked off manually.
--
-- I used to have some support for automatically reinvoking lilypond after
-- changes to a block, but it didn't seem too useful, since any useful amount
-- of lilypond score takes quite a while to compile.
module Cmd.Lang.LLily where
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified System.FilePath as FilePath
import qualified System.Process as Process

import Util.Control
import qualified Util.Log as Log
import qualified Util.Process

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

set_config :: (State.M m) => Lilypond.Config -> m ()
set_config config = State.modify_config $ State.lilypond #= config

get_config :: (State.M m) => m Lilypond.Config
get_config = State.config#State.lilypond <#> State.get

make_config :: RealTime -> Lilypond.Duration -> Lilypond.Config
make_config quarter quantize = Lilypond.default_config
    { Lilypond.config_quarter_duration = quarter
    , Lilypond.config_quantize = quantize
    }

set_staves :: [(String, String, String)] -> Lilypond.Config -> Lilypond.Config
set_staves staves config = config
    { Lilypond.config_staves =
        [(Score.Instrument inst, short, long) | (inst, short, long) <- staves]
    }

-- * compile

-- | Compile the given block as lilypond.
block :: BlockId -> Cmd.CmdL ()
block block_id = do
    events <- LEvent.write_logs =<< derive block_id
    config <- get_config
    compile_ly block_id config events

-- | Compile the current block.
current :: Cmd.CmdL ()
current = block =<< Cmd.get_focused_block

-- | Show the output of the lilypond for the given block.
view_block :: BlockId -> Cmd.CmdL ()
view_block block_id = do
    filename <- Cmd.Lilypond.ly_filename block_id
    liftIO $ Util.Process.logged $
        Process.proc "open" [FilePath.replaceExtension filename ".pdf"]
    return ()

view :: Cmd.CmdL ()
view = view_block =<< Cmd.get_focused_block

-- * debugging

filter_inst :: [String] -> [Score.Event] -> [Score.Event]
filter_inst inst_s = filter ((`elem` insts) . Score.event_instrument)
    where insts = map Score.Instrument inst_s

from_events :: Lilypond.Config -> [Score.Event] -> Cmd.CmdL ()
from_events config events = do
    block_id <- Cmd.get_focused_block
    compile_ly block_id config events

compile_ly :: BlockId -> Lilypond.Config -> [Score.Event] -> Cmd.CmdL ()
compile_ly block_id config events = do
    filename <- Cmd.Lilypond.ly_filename block_id
    (result, logs) <- liftIO $
        Cmd.Lilypond.compile_ly filename config (title_of block_id) events
    mapM_ Log.write logs
    stack_map <- Cmd.require_right ("compile_ly: "++) result
    Cmd.modify_play_state $ \st -> st
        { Cmd.state_lilypond_stack_maps = Map.insert block_id
            stack_map (Cmd.state_lilypond_stack_maps st)
        }

title_of :: BlockId -> Lilypond.Title
title_of = Id.ident_name

-- * debugging

-- | Run a lilypond derive and return score events.
derive :: BlockId -> Cmd.CmdL Derive.Events
derive block_id = Derive.r_events <$> Cmd.Lilypond.derive_block block_id

-- | Convert to lilypond events.
ly_events :: RealTime -> Derive.Events -> ([Lilypond.Event], [Log.Msg])
ly_events quarter = LEvent.partition . Convert.convert quarter

-- | Convert down to lilypond score.
make_ly :: Cmd.CmdL (Either String [Text.Text], [Log.Msg])
make_ly = do
    block_id <- Cmd.get_focused_block
    config <- get_config
    (events, logs) <- LEvent.partition <$> derive block_id
    let (result, ly_logs) = Cmd.Lilypond.make_ly config "title" events
    return (fst <$> result, logs ++ ly_logs)

convert :: Cmd.CmdL ([Lilypond.Event], [Log.Msg])
convert = do
    config <- get_config
    score_events <- derive =<< Cmd.get_focused_block
    let (events, logs) = LEvent.partition $
            Convert.convert (Lilypond.config_quarter_duration config)
            score_events
    return (Convert.quantize (Lilypond.config_quantize config) events, logs)
