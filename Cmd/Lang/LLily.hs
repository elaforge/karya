{-# LANGUAGE NoMonomorphismRestriction #-}
-- | Lilypond compiles are always kicked off manually.
--
-- I used to have some support for automatically reinvoking lilypond after
-- changes to a block, but it didn't seem too useful, since any useful amount
-- of lilypond score takes quite a while to compile.
module Cmd.Lang.LLily where
import qualified Data.Map as Map
import qualified System.FilePath as FilePath
import qualified System.Process as Process

import Util.Control
import qualified Util.Log as Log
import qualified Util.Process

import qualified Ui.Id as Id
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Lang.Global as Global
import qualified Cmd.Lang.LPerf as LPerf
import qualified Cmd.Lilypond

import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score

import qualified Perform.Lilypond.Convert as Convert
import qualified Perform.Lilypond.Lilypond as Lilypond
import Types


pipa :: Derive.Events -> Cmd.CmdL ()
pipa = from_events config . clean
    where
    config = Cmd.Lilypond.TimeConfig 0.125 Lilypond.D16
    clean = filter_inst ["fm8/pipa", "fm8/dizi", "ptq/yangqin"]
        . LEvent.events_of
        -- . filter_inst ["ptq/yangqin"]

bloom :: BlockId -> Cmd.CmdL ()
bloom = block (Cmd.Lilypond.TimeConfig 0.5 Lilypond.D16)

bloom_until :: RealTime -> Cmd.CmdL ()
bloom_until end = do
    -- (block_id, _, track_ids, _, _) <- Selection.tracks
    -- Selection.realtime is not accurate since lilypond derive ignores tempo
    let block_id = Global.bid "bloom/order"
    -- block_id <- Cmd.get_focused_block
    result <- Cmd.Lilypond.derive block_id
    events <- LEvent.write_logs $
        LPerf.in_range Score.event_start 0 end (Derive.r_events result)
    let config = Cmd.Lilypond.TimeConfig (measure / 5) Lilypond.D16
        measure = 10.166666666666666
    void $ compile_ly block_id config events

events :: BlockId -> Cmd.CmdL Derive.Events
events block_id = Derive.r_events <$> Cmd.Lilypond.derive block_id

ly_events :: RealTime -> Derive.Events -> ([Lilypond.Event], [Log.Msg])
ly_events quarter = LEvent.partition . Convert.convert quarter

filter_inst :: [String] -> [Score.Event] -> [Score.Event]
filter_inst inst_s = filter ((`elem` insts) . Score.event_instrument)
    where insts = map Score.Instrument inst_s

block :: Cmd.Lilypond.TimeConfig -> BlockId -> Cmd.CmdL ()
block config block_id = do
    events <- LEvent.write_logs . Derive.r_events
        =<< Cmd.Lilypond.derive block_id
    stack_map <- compile_ly block_id config events
    Cmd.modify_play_state $ \st -> st
        { Cmd.state_lilypond_stack_maps = Map.insert block_id
            stack_map (Cmd.state_lilypond_stack_maps st)
        }

from_events :: Cmd.Lilypond.TimeConfig -> [Score.Event] -> Cmd.CmdL ()
from_events config events = do
    block_id <- Cmd.get_focused_block
    stack_map <- compile_ly block_id config events
    Cmd.modify_play_state $ \st -> st
        { Cmd.state_lilypond_stack_maps = Map.insert block_id
            stack_map (Cmd.state_lilypond_stack_maps st)
        }

compile_ly :: BlockId -> Cmd.Lilypond.TimeConfig -> [Score.Event]
    -> Cmd.CmdL Cmd.StackMap
compile_ly block_id config events = do
    filename <- Cmd.Lilypond.ly_filename block_id
    result <- liftIO $
        Cmd.Lilypond.compile_ly filename config (title_of block_id) events
    Cmd.require_right ("compile_ly: "++) result

view_pdf :: BlockId -> Cmd.CmdL ()
view_pdf block_id = do
    filename <- Cmd.Lilypond.ly_filename block_id
    liftIO $ Util.Process.logged $
        Process.proc "open" [FilePath.replaceExtension filename ".pdf"]
    return ()

title_of :: BlockId -> Lilypond.Title
title_of = Id.ident_name
