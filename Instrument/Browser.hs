{-# LANGUAGE ScopedTypeVariables #-} -- for pattern type sig in catch
{- | The instrument browser is a standalone program to browse the instrument
    database.

    The query syntax is documented at 'Search.Query'.
-}
module Instrument.Browser where
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception
import qualified Control.Monad.State as State
import Control.Monad.Trans (liftIO)

import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified System.IO as IO
import Text.Printf

import Util.Control
import qualified Util.Fltk as Fltk
import qualified Util.Format as Format
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Cmd.CallDoc as CallDoc
import qualified Cmd.Cmd as Cmd
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.BrowserC as BrowserC
import qualified Instrument.Db as Db
import qualified Instrument.MidiDb as MidiDb
import qualified Instrument.Search as Search

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
    win <- BrowserC.create 50 50 500 300
    let index_db = Db db (Search.make_index (Db.db_midi_db db))
    Concurrent.forkIO $ handle_msgs win index_db
        `Exception.finally` putStrLn "handler thread died"
    Fltk.run

-- | Bundle a Db along with its search index.
data Db = Db {
    db_db :: Cmd.InstrumentDb
    , db_index :: Search.Index
    }

data State = State {
    state_displayed :: [Score.Instrument]
    }

handle_msgs :: BrowserC.Window -> Db -> IO ()
handle_msgs win db = do
    displayed <- liftIO $ process_query win db [] ""
    flip State.evalStateT (State displayed) $ forever $ do
        (Fltk.Msg typ text) <- liftIO $ STM.atomically $ Fltk.read_msg win
        let inst = Score.Instrument text
        case typ of
            BrowserC.Select -> liftIO $ show_info win db inst
            BrowserC.Choose -> liftIO $ choose_instrument inst
            BrowserC.Query -> do
                state <- State.get
                displayed <- liftIO $
                    process_query win db (state_displayed state) text
                State.put (state { state_displayed = displayed })
            BrowserC.Unknown c -> liftIO $
                putStrLn $ "unknown msg type: " ++ show c

-- | Look up the instrument, generate a info sheet on it, and send to the UI.
show_info :: BrowserC.Window -> Db -> Score.Instrument -> IO ()
show_info win db inst = Fltk.send_action $ BrowserC.set_info win info
    where
    info = maybe ("not found: " ++ Pretty.pretty inst) id $ do
        info <- Db.db_lookup (db_db db) inst
        return $ info_of db inst info

info_of :: Db -> Score.Instrument -> Cmd.MidiInfo -> String
info_of db score_inst (MidiDb.Info synth patch code) =
    printf "%s -- %s\n\n" synth_name name ++ info_sections
        -- important properties
        [ ("Flags", Seq.join ", " flags)
        , ("Instrument controls", show_control_map inst_cmap)
        , ("Synth controls", show_control_map synth_cmap)
        , ("Keymap", if Map.null (Instrument.inst_keymap inst) then ""
            else Pretty.pretty (Instrument.inst_keymap inst))
        -- code
        , ("Cmds", show_cmds (Cmd.inst_cmds code))
        , ("Note calls", show_calls note_calls)
        , ("Val calls", show_calls val_calls)
        , ("Environ", if TrackLang.null_environ (Cmd.inst_environ code) then ""
            else Pretty.pretty (Cmd.inst_environ code))

        -- implementation details
        , ("Keyswitches", if null keyswitches then ""
            else Pretty.pretty keyswitches)
        , ("Pitchbend range", show (Instrument.inst_pitch_bend_range inst))
        , ("Scale", maybe "" Pretty.pretty scale)
        , ("Attribute map",
            if Map.null attr_map then "" else Pretty.pretty attr_map)
        , ("Initialization", show_initialize initialize)
        -- info
        , ("Text", text)
        , ("File", file)
        , ("Tags", tags)
        ]
    where
    Instrument.Synth synth_name synth_cmap = synth
    Instrument.Patch {
        Instrument.patch_instrument = inst
        , Instrument.patch_scale = scale
        , Instrument.patch_flags = pflags
        , Instrument.patch_initialize = initialize
        , Instrument.patch_keyswitches = Instrument.KeyswitchMap keyswitches
        , Instrument.patch_attribute_map = attr_map
        , Instrument.patch_text = text
        , Instrument.patch_file = file
        } = patch
    flags = map show (Set.toList pflags)
    name = let n = Instrument.inst_name inst in if null n then "*" else n
    inst_cmap = Instrument.inst_control_map inst
    Derive.InstrumentCalls note_calls val_calls = Cmd.inst_calls code
    tags = maybe "" show_tags $ Search.tags_of (db_index db) score_inst

info_sections :: [(String, String)] -> String
info_sections = unlines . filter (not.null) . map info_section

info_section :: (String, String) -> String
info_section (title, raw_text)
    | null text = ""
    | length text < 40 && '\n' `notElem` text = title ++ ": " ++ text ++ "\n"
    | otherwise = "\t" ++ title ++ ":\n" ++ text ++ "\n"
    where text = Seq.strip raw_text

show_control_map :: Control.ControlMap -> String
show_control_map cmap =
    Seq.join ", " [cont ++ " (" ++ show num ++ ")"
        | (Control.Control cont, num) <- Map.assocs cmap]

show_cmds :: [Cmd.Cmd] -> String
show_cmds [] = ""
show_cmds cmds = show (length cmds) ++ " cmds (cmds can't be introspected yet)"

show_calls :: [Derive.LookupCall call] -> String
show_calls lookups =
    -- Pass a Nothing for width because I should let fltk do the wrapping.
    Text.unpack $ Format.run Nothing $
        mapM_ CallDoc.call_bindings_text call_bindings
    where
    call_bindings = CallDoc.lookup_docs (map Derive.lookup_docs lookups)

show_tags :: [(String, String)] -> String
show_tags tags =
    unwords [quote k ++ "=" ++ quote v | (k, v) <- Seq.sort_on fst tags]

show_initialize :: Instrument.InitializePatch -> String
show_initialize Instrument.NoInitialization = ""
show_initialize (Instrument.InitializeMessage msg) = "Message: " ++ msg
show_initialize (Instrument.InitializeMidi msgs) =
    unlines (map Pretty.pretty msgs)

quote :: String -> String
quote s
    | any Char.isSpace s = "\"" ++ s ++ "\""
    | otherwise = s

-- | Send the chosen instrument to the sequencer.
choose_instrument :: Score.Instrument -> IO ()
choose_instrument inst = do
    let cmd = "load_instrument " ++ show (Score.inst_name inst)
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
    let matches = Search.search (db_index db) (Search.parse query)
        diff = Seq.indexed_pairs (==) displayed matches
    forM_ diff $ \(i, paired) -> case paired of
        Seq.Second inst -> Fltk.send_action $
            BrowserC.insert_line win (i+1) (Score.inst_name inst)
        Seq.First _inst -> Fltk.send_action $
            BrowserC.remove_line win (i+1)
        _ -> return ()
    return matches
