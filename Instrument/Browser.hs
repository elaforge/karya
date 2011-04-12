{-# LANGUAGE ScopedTypeVariables #-} -- for pattern type sig in catch
import Control.Monad
import qualified Control.Exception as Exception
import qualified Control.Monad.State as State
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified System.IO as IO
import Text.Printf

import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.Fltk as Fltk

import qualified Midi.Midi as Midi

import qualified Derive.Score as Score
import qualified Cmd.Cmd as Cmd

import qualified Instrument.BrowserC as BrowserC

import qualified Instrument.Db as Db
import qualified Instrument.MidiDb as MidiDb
import qualified Instrument.Parse as Parse
import qualified Instrument.Search as Search
import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Instrument as Instrument

import qualified Local.Instrument
import qualified App.Config as Config
import qualified App.SendCmd as SendCmd


main :: IO ()
main = SendCmd.initialize $ do
    app_dir <- Config.get_app_dir
    putStr "Loading instruments... "
    IO.hFlush IO.stdout
    db <- Local.Instrument.load app_dir
    putStrLn $ show (Db.size db)
    win <- BrowserC.create 50 50 400 200
    let index_db = Db db (Search.make_index (Db.db_midi_db db))
    Concurrent.forkIO $ handle_msgs win index_db
        `Exception.finally` putStrLn "handler thread died"
    Fltk.run

-- | Bundle a Db along with its search index.
data Db = Db {
    db_db :: Cmd.InstrumentDb
    , db_index :: Search.Index
    }

search :: Db -> Search.Search
search = Search.search . db_index

data State = State {
    state_displayed :: [Score.Instrument]
    }

handle_msgs :: BrowserC.Window -> Db -> IO ()
handle_msgs win db = do
    displayed <- State.liftIO $ process_query win db [] ""
    flip State.evalStateT (State displayed) $ forever $ do
        (Fltk.Msg typ text) <- State.liftIO $ get_msg (Fltk.win_chan win)
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
show_info :: BrowserC.Window -> Db -> String -> IO ()
show_info win db inst_name = Fltk.send_action $ BrowserC.set_info win info
    where
    info = maybe ("not found: " ++ show inst_name) id $ do
        let score_inst = read_inst inst_name
        info <- Db.db_lookup (db_db db) score_inst
        return $ info_of db score_inst info

info_of :: Db -> Score.Instrument -> MidiDb.Info code -> String
info_of db score_inst (MidiDb.Info synth patch _) =
    printf "%s -- %s -- %s\n\n" synth_name name dev
        ++ info_sections
            [ ("Instrument controls", cmap_info inst_cmap)
            , ("Flags", Seq.join ", " flags)
            , ("Keyswitches", show_keyswitches keyswitches)
            , ("Synth controls", cmap_info synth_cmap)
            , ("Pitchbend range", show (Instrument.inst_pitch_bend_range inst))
            , ("Initialization", initialize_info initialize)
            , ("Text", text)
            , ("Tags", tags)
            ]
    where
    Instrument.Synth synth_name maybe_dev synth_cmap = synth
    Instrument.Patch inst triggered initialize keyswitches _ text = patch
    flags = if triggered then ["triggered"] else []
    dev = maybe "<no default device>" (\(Midi.WriteDevice s) -> s) maybe_dev
    name = let n = Instrument.inst_name inst in if null n then "*" else n
    inst_cmap = Instrument.inst_control_map inst
    tags = maybe "" tags_info $
        Search.tags_of (db_index db) score_inst

-- | Pretty print Keyswitches
show_keyswitches :: Instrument.KeyswitchMap -> String
show_keyswitches (Instrument.KeyswitchMap ksmap) =
    Seq.join "\n" (map show_pair ksmap)
    where show_pair (attrs, ks) = Pretty.pretty attrs ++ ": " ++ show ks

info_sections = unlines . filter (not.null) . map info_section
info_section (title, raw_text)
    | null text = ""
    | length text < 40 && '\n' `notElem` text = title ++ ": " ++ text ++ "\n"
    | otherwise = "\t" ++ title ++ ":\n" ++ text ++ "\n"
    where text = Seq.strip raw_text

cmap_info cmap = -- Seq.join "\n" $ map (Seq.join ", ") $ groups 3
    Seq.join ", " [cont ++ " (" ++ show num ++ ")"
        | (Control.Control cont, num) <- Map.assocs cmap]

-- groups n xs
--     | null xs || n <= 0 = []
--     | otherwise = let (pre, post) = List.splitAt n xs in pre : groups n post

tags_info tags = unwords [quote k ++ "=" ++ quote v | (k, v) <- tags]

initialize_info Instrument.NoInitialization = "(none)"
initialize_info (Instrument.InitializeMessage msg) = "Message: " ++ msg
initialize_info (Instrument.InitializeMidi msgs) = unlines (map midi_info msgs)

manufacturer code = maybe "<unknown>" id $ lookup code Parse.manufacturer_codes

midi_info (Midi.CommonMessage (Midi.SystemExclusive manuf bytes)) =
    "Sysex for " ++ manufacturer manuf
    ++ " (" ++ show (ByteString.length bytes) ++ " bytes)"
midi_info (Midi.ChannelMessage _ msg) = show msg
midi_info msg = show msg

quote s
    | any Char.isSpace s = "\"" ++ s ++ "\""
    | otherwise = s

-- | Send the chosen instrument to the sequencer.
choose_instrument :: String -> IO ()
choose_instrument inst_name = do
    let cmd = "load_instrument " ++ show inst_name
    putStrLn $ "send: " ++ cmd
    response <- SendCmd.send cmd
        `Exception.catch` \(exc :: Exception.SomeException) ->
            return ("error: " ++ show exc)
    unless (null response) $
        putStrLn $ "response: " ++ response

-- | Find instruments that match the query, and update the UI incrementally.
process_query :: BrowserC.Window -> Db -> [Score.Instrument] -> String
    -> IO [Score.Instrument]
process_query win db displayed query = do
    -- putStrLn $ "query: " ++ show (Search.parse query)
    let matches = search db (Search.parse query)
        diff = Seq.indexed_pairs (==) displayed matches
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
