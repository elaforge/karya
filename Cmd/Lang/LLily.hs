module Cmd.Lang.LLily where
import qualified Control.Monad.Trans as Trans
import qualified Data.Map as Map
import qualified System.FilePath as FilePath
import qualified System.Process as Process

import qualified Util.Process
import qualified Ui.Block as Block
import qualified Ui.Id as Id
import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Lilypond
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Perform.Lilypond.Convert as Convert
import qualified Perform.Lilypond.Lilypond as Lilypond
import qualified Perform.Pitch as Pitch

import Types


-- | Turn on lilypond derivation for this block.
enable_lilypond :: String -> String -> BlockId -> Cmd.CmdL ()
enable_lilypond clef time_sig block_id =
    State.modify_block_meta block_id $
        Map.union (make_meta clef time_sig block_id)

make_meta :: String -> String -> BlockId -> Block.Meta
make_meta clef time_sig block_id = Map.fromList
    [ (Lilypond.meta_ly, "true")
    , (Lilypond.meta_title, Id.ident_string block_id)
    , (Lilypond.meta_clef, clef)
    , (Lilypond.meta_time_signature, time_sig)
    ]

make_score :: String -> String -> String -> BlockId
    -> Cmd.CmdL Lilypond.Score
make_score key_str clef time_sig block_id =
    case Lilypond.meta_to_score key meta of
        Nothing -> Cmd.throw "no score"
        Just (Left err) -> Cmd.throw $ "parsing lily score: " ++ err
        Just (Right score) -> return score
    where
    key = Just (Pitch.Key key_str)
    meta = make_meta clef time_sig block_id

pipa = from_events "c-maj" "treble" "4/4" config . clean
    where config = Cmd.Lilypond.TimeConfig 0.125 Lilypond.D16

ly_events quarter events = LEvent.partition $
    Convert.convert quarter (map LEvent.Event (clean events))

clean = filter_inst ["fm8/pipa", "fm8/dizi", "ptq/yangqin"]
    -- . filter_inst ["ptq/yangqin"]
    . LEvent.events_of

filter_inst :: [String] -> [Score.Event] -> [Score.Event]
filter_inst inst_s = filter ((`elem` insts) . Score.event_instrument)
    where insts = map Score.Instrument inst_s

from_events :: String -> String -> String -> Cmd.Lilypond.TimeConfig
    -> [Score.Event] -> Cmd.CmdL ()
from_events key clef time_sig config events = do
    block_id <- Cmd.get_focused_block
    score <- make_score key clef time_sig block_id
    filename <- Cmd.Lilypond.ly_filename block_id
    stack_map <- Trans.liftIO $
        Cmd.Lilypond.compile_ly filename config score events
    Cmd.modify_play_state $ \st -> st
        { Cmd.state_lilypond_stack_maps = Map.insert block_id
            stack_map (Cmd.state_lilypond_stack_maps st)
        }

view_pdf :: BlockId -> Cmd.CmdL ()
view_pdf block_id = do
    filename <- Cmd.Lilypond.ly_filename block_id
    Trans.liftIO $ Util.Process.logged $
        (Process.proc "open" [FilePath.replaceExtension filename ".pdf"])
    return ()
