module Instrument.Browser where
import Control.Monad
import qualified Control.Monad.State as State
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as STM
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Char as Char
import qualified Numeric
import Text.Printf

import qualified Util.Seq as Seq
import qualified Util.Fltk as Fltk
import qualified Ui.Diff as Diff

import qualified Midi.Midi as Midi

import qualified Derive.Score as Score
import qualified Instrument.Db as Db
import qualified Instrument.BrowserC as BrowserC
import qualified Instrument.Search as Search
import qualified Perform.Midi.Controller as Controller
import qualified Perform.Midi.Instrument as Instrument

import qualified Local.Instrument

-- import Util.PPrint
-- import qualified Instrument.Search_test


main :: IO ()
main = do
    db <- Local.Instrument.load "Local/Instrument"
    -- let db = Db.db Instrument.Search_test.midi_db
    win <- BrowserC.create 50 50 400 200
    Concurrent.forkIO (handle_msgs win db initial_state)
    Fltk.run

data State = State {
    state_displayed :: [Score.Instrument]
    }
initial_state = State []

handle_msgs :: BrowserC.Window -> Db.Db -> State -> IO ()
handle_msgs win db state = flip State.evalStateT state $ forever $ do
    (Fltk.Msg typ text) <- State.liftIO $ get_msg (Fltk.win_chan win)
    -- State.liftIO $ print (typ, text)
    case typ of
        BrowserC.Select -> State.liftIO $ show_info win db text
        BrowserC.Choose -> State.liftIO $ choose_instrument text
        BrowserC.Query -> do
            state <- State.get
            displayed <- State.liftIO $
                process_query win db (state_displayed state) text
            State.put (state { state_displayed = displayed })
        BrowserC.Unknown c -> State.liftIO $
            putStrLn $ "unknown msg type: " ++ show c

get_msg msg_chan = STM.atomically $ STM.readTChan msg_chan

-- | Look up the instrument, generate a info sheet on it, and send to the UI.
show_info :: BrowserC.Window -> Db.Db -> String -> IO ()
show_info win db inst_name = Fltk.send_action $ BrowserC.set_info win info
    where
    info = maybe ("not found: " ++ show inst_name) id $ do
        let score_inst = read_inst inst_name
        inst <- Db.db_lookup_midi db score_inst
        synth <- Db.db_lookup_synth db score_inst
        initialize <- Db.db_midi_initialize db score_inst
        return $ info_of db synth (initialize 0) inst score_inst

info_of db (Instrument.Synth synth_name (Midi.WriteDevice dev) synth_cmap)
        initialize inst score_inst =
    printf "%s -- %s -- %s\n" synth_name name dev
        ++ info_sections
            [ ("Instrument controllers", (cmap_info cmap))
            , ("Synth controllers", (cmap_info synth_cmap))
            , ("Initialization", initialize_info initialize)
            , ("Tags", tags)
            ]
    where
    name = let n = Instrument.inst_name inst in if null n then "*" else n
    cmap = Map.difference synth_cmap (Instrument.inst_controller_map inst)
    tags = maybe "" tags_info $
        Search.tags_of (Db.db_index db) score_inst

info_sections = unlines . filter (not.null) . map info_section
info_section (title, text)
    | null text = ""
    | otherwise = "\t" ++ title ++ ":\n" ++ (Seq.strip text) ++ "\n"

cmap_info cmap = -- Seq.join "\n" $ map (Seq.join ", ") $ groups 3
    Seq.join ", " [cont ++ " (" ++ show num ++ ")"
        | (Controller.Controller cont, num) <- Map.assocs cmap]

tags_info tags = unwords [quote k ++ "=" ++ quote v | (k, v) <- tags]

initialize_info Instrument.NoInitialization = "(none)"
initialize_info (Instrument.InitializeMessage msg) = "Message: " ++ msg
initialize_info (Instrument.InitializeMidi msgs) = unlines (map midi_info msgs)

midi_info (Midi.CommonMessage (Midi.SystemExclusive manuf bytes)) =
    "Sysex for " ++ Numeric.showHex manuf ""
    ++ " (" ++ show (length bytes) ++ " bytes)"
midi_info (Midi.ChannelMessage _ msg) = show msg
midi_info msg = show msg

quote s
    | any Char.isSpace s = "\"" ++ s ++ "\""
    | otherwise = s

groups _ [] = []
groups n xs = pre : groups n post
    where (pre, post) = List.splitAt n xs

-- | Send the chosen instrument to the sequencer.
choose_instrument :: String -> IO ()
choose_instrument inst_name =
    putStrLn $ "CHOOSE: " ++ show (Score.Instrument inst_name)

-- | Find instruments that match the query, and update the UI incrementally.
process_query :: BrowserC.Window -> Db.Db -> [Score.Instrument] -> String
    -> IO [Score.Instrument]
process_query win db displayed query = do
    -- putStrLn $ "query: " ++ show (Search.parse query)
    let matches = Db.db_search db (Search.parse query)
        diff = Diff.indexed_pairs (==) displayed matches
    forM_ diff $ \(i, old, new) -> case (old, new) of
        (Nothing, Just inst) -> Fltk.send_action $
            BrowserC.insert_line win (i+1) (show_inst inst)
        (Just _inst, Nothing) -> Fltk.send_action $
            BrowserC.remove_line win (i+1)
        _ -> return ()
    -- pprint (filter interesting diff)
    return [inst | (_, _, Just inst) <- diff]
    -- where
    -- interesting (_, Just _, Just _) = False
    -- interesting _ = True

show_inst (Score.Instrument inst) = inst
read_inst = Score.Instrument
