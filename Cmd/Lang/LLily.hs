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


sonata :: BlockId -> Cmd.CmdL ()
sonata = block sonata_config

sonata_config = Lilypond.TimeConfig 1 Lilypond.D32

pipa :: Derive.Events -> Cmd.CmdL ()
pipa = from_events config . clean
    where
    config = Lilypond.TimeConfig 0.125 Lilypond.D16
    clean = filter_inst ["fm8/pipa", "fm8/dizi", "ptq/yangqin"]
        . LEvent.events_of
        -- . filter_inst ["ptq/yangqin"]

bloom :: BlockId -> Cmd.CmdL ()
bloom = block (Lilypond.TimeConfig 0.5 Lilypond.D16)

bloom_until :: RealTime -> Cmd.CmdL ()
bloom_until end = do
    -- (block_id, _, track_ids, _, _) <- Selection.tracks
    -- Selection.realtime is not accurate since lilypond derive ignores tempo
    let block_id = Global.bid "bloom/order"
    -- block_id <- Cmd.get_focused_block
    result <- Cmd.Lilypond.derive (make_config config) block_id
    events <- LEvent.write_logs $
        LPerf.in_range Score.event_start 0 end (Derive.r_events result)
    compile_ly block_id config events
    where
    config = Lilypond.TimeConfig (measure / 5) Lilypond.D16
    measure = 10.166666666666666

-- | For now I always use the same hardcoded Lilypond.Config, but maybe I'll
-- want to be fancier later.
make_config :: Lilypond.TimeConfig -> Derive.Lilypond
make_config config = Derive.Lilypond config
    (Lilypond.default_config (Lilypond.time_quarter config))

events :: Lilypond.TimeConfig -> BlockId -> Cmd.CmdL Derive.Events
events config block_id = Derive.r_events <$>
    Cmd.Lilypond.derive (make_config config) block_id

ly_events :: RealTime -> Derive.Events -> ([Lilypond.Event], [Log.Msg])
ly_events quarter = LEvent.partition . Convert.convert quarter

filter_inst :: [String] -> [Score.Event] -> [Score.Event]
filter_inst inst_s = filter ((`elem` insts) . Score.event_instrument)
    where insts = map Score.Instrument inst_s

block :: Lilypond.TimeConfig -> BlockId -> Cmd.CmdL ()
block config block_id = do
    events <- LEvent.write_logs =<< derive config block_id
    compile_ly block_id config events

from_events :: Lilypond.TimeConfig -> [Score.Event] -> Cmd.CmdL ()
from_events config events = do
    block_id <- Cmd.get_focused_block
    compile_ly block_id config events

compile_ly :: BlockId -> Lilypond.TimeConfig -> [Score.Event]
    -> Cmd.CmdL ()
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

view_pdf :: BlockId -> Cmd.CmdL ()
view_pdf block_id = do
    filename <- Cmd.Lilypond.ly_filename block_id
    liftIO $ Util.Process.logged $
        Process.proc "open" [FilePath.replaceExtension filename ".pdf"]
    return ()

title_of :: BlockId -> Lilypond.Title
title_of = Id.ident_name

-- * debugging

derive :: Lilypond.TimeConfig -> BlockId -> Cmd.CmdL Derive.Events
derive config = fmap Derive.r_events . Cmd.Lilypond.derive (make_config config)

make_ly :: Lilypond.TimeConfig
    -> Cmd.CmdL (Either String [Text.Text], [Log.Msg])
make_ly config = do
    block_id <- Cmd.get_focused_block
    (events, logs) <- LEvent.partition <$> derive config block_id
    let (result, ly_logs) = Cmd.Lilypond.make_ly config "title" events
    return (fst <$> result, logs ++ ly_logs)
