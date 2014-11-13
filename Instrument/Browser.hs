-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ScopedTypeVariables #-} -- for pattern type sig in catch
{- | The instrument browser is a standalone program to browse the instrument
    database.

    Instruments are in the left pane, and the right pane has information on the
    selected instrument.  A search box above the instrument list accepts
    a simple query language, documneted at 'Search.Query'.

    If you double click on an instrument name, 'choose_instrument' is called on
    the instrument.

    The instrument info is basically just a pretty-printed version of the
    contents of 'Instrument.Patch'.  The 'Instrument.patch_tags' field is
    especially relevant, since that's what 'Search.Query' uses.

    Some parts of the instrument db may be generated offline, by
    "Instrument.MakeDb".
-}
module Instrument.Browser where
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception
import qualified Control.Monad.State as State

import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import Text.Printf (printf)

import qualified Util.Fltk as Fltk
import qualified Util.Format as Format
import qualified Util.Seq as Seq

import qualified Cmd.CallDoc as CallDoc
import qualified Cmd.Cmd as Cmd
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal

import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.BrowserC as BrowserC
import qualified Instrument.Db as Db
import qualified Instrument.MidiDb as MidiDb
import qualified Instrument.Search as Search

import qualified Local.Instrument
import qualified App.Config as Config
import qualified App.SendCmd as SendCmd
import Global


main :: IO ()
main = SendCmd.initialize $ do
    db <- Local.Instrument.load =<< Config.get_app_dir
    putStrLn $ "Loaded " ++ show (Db.size db) ++ " instruments."
    win <- Fltk.run_action $ BrowserC.create 50 50 500 300
    let index_db = Db db (Search.make_index (Db.db_midi_db db))
    chan <- Fltk.new_channel
    Concurrent.forkFinally (handle_msgs chan win index_db) $ \result -> do
        putStrLn $ "handler thread died: "
            ++ either show (const "no exception")
                (result :: Either Exception.SomeException ())
        Fltk.quit chan
    Fltk.event_loop chan

-- | Bundle a Db along with its search index.
data Db = Db {
    db_db :: Cmd.InstrumentDb
    , db_index :: Search.Index
    }

data State = State {
    state_displayed :: [Score.Instrument]
    }

handle_msgs :: Fltk.Channel -> BrowserC.Window -> Db -> IO ()
handle_msgs chan win db = do
    displayed <- liftIO $ process_query chan win db [] ""
    flip State.evalStateT (State displayed) $ forever $ do
        Fltk.Msg typ text <- liftIO $ STM.atomically $ Fltk.read_msg win
        let inst = Score.Instrument (txt text)
        case typ of
            BrowserC.Select -> liftIO $ show_info chan win db inst
            BrowserC.Choose -> liftIO $ choose_instrument inst
            BrowserC.Query -> do
                state <- State.get
                displayed <- liftIO $
                    process_query chan win db (state_displayed state) (txt text)
                State.put (state { state_displayed = displayed })
            BrowserC.Unknown c -> liftIO $
                putStrLn $ "unknown msg type: " ++ show c

-- | Look up the instrument, generate a info sheet on it, and send to the UI.
show_info :: Fltk.Channel -> BrowserC.Window -> Db -> Score.Instrument -> IO ()
show_info chan win db inst = Fltk.send_action chan $ BrowserC.set_info win info
    where
    info = maybe ("not found: " <> ShowVal.show_val inst) id $ do
        info <- Db.db_lookup (db_db db) inst
        return $ info_of db inst info

info_of :: Db -> Score.Instrument -> Cmd.MidiInfo -> Text
info_of db score_inst (MidiDb.Info synth patch code) =
    synth_name <> " -- " <> name <> " -- " <> synth_doc <> "\n\n" <> fields
        -- important properties
        [ ("Flags", Text.intercalate ", " flags)
        , ("Instrument controls", show_control_map inst_cmap)
        , ("Synth controls", show_control_map synth_cmap)
        -- code
        , ("Cmds", show_cmds (Cmd.inst_cmds code))
        , ("Note generators", show_calls CallDoc.GeneratorCall
            (map CallDoc.convert_call note_generators))
        , ("Note transformers", show_calls CallDoc.TransformerCall
            (map CallDoc.convert_call note_transformers))
        , ("Val calls", show_calls CallDoc.ValCall
            (map CallDoc.convert_val_call val_calls))
        , ("Environ",
            if environ == mempty then "" else prettyt environ)

        -- implementation details
        , ("Attribute map", show_attribute_map attr_map)
        , ("Pitchbend range", showt (Instrument.inst_pitch_bend_range inst))
        , ("Scale", maybe "" prettyt scale)
        , ("Initialization", show_initialize initialize)
        -- info
        , ("Text", text)
        , ("File", txt file)
        , ("Tags", tags)
        ]
    where
    Instrument.Synth synth_name synth_doc synth_cmap = synth
    Instrument.Patch {
        Instrument.patch_instrument = inst
        , Instrument.patch_scale = scale
        , Instrument.patch_restricted_environ = environ
        , Instrument.patch_flags = pflags
        , Instrument.patch_initialize = initialize
        , Instrument.patch_attribute_map = attr_map
        , Instrument.patch_text = text
        , Instrument.patch_file = file
        } = patch
    flags = map showt (Set.toList pflags)
    name = let n = Instrument.inst_name inst in if Text.null n then "*" else n
    inst_cmap = Instrument.inst_control_map inst
    Derive.InstrumentCalls note_generators note_transformers val_calls =
        Cmd.inst_calls code
    tags = maybe "" show_tags $ Search.tags_of (db_index db) score_inst

fields :: [(Text, Text)] -> Text
fields = Text.unlines . filter (not . Text.null) . map field

field :: (Text, Text) -> Text
field (title, raw_text)
    | Text.null text = ""
    | Text.length text < 40 && not ("\n" `Text.isInfixOf` text) =
        title <> ": " <> text <> "\n"
    | otherwise = "\t" <> title <> ":\n" <> text <> "\n"
    where text = Text.strip raw_text

show_attribute_map :: Instrument.AttributeMap -> Text
show_attribute_map (Instrument.AttributeMap table) =
    Text.unlines (map fmt table)
    where
    attrs = map (\(a, _, _) -> pretty a) table
    longest = fromMaybe 0 $ Seq.maximum (map length attrs)

    fmt (attrs, keyswitches, maybe_keymap) =
        -- Still not quite right for lining up columns.
        txt (printf "%-*s\t" longest (pretty attrs))
            <> prettyt keyswitches <> maybe "" ((" "<>) . prettyt) maybe_keymap

show_control_map :: Control.ControlMap -> Text
show_control_map cmap =
    Text.intercalate ", " [Score.control_name cont <> " (" <> showt num <> ")"
        | (cont, num) <- Map.toList cmap]

show_cmds :: [Cmd.Cmd] -> Text
show_cmds [] = ""
show_cmds cmds = showt (length cmds) <> " cmds (cmds can't be introspected yet)"

show_calls :: CallDoc.CallType -> [CallDoc.LookupCall] -> Text
show_calls ctype lookups =
    -- Pass a Nothing for width because I should let fltk do the wrapping.
    Format.run Nothing $ mapM_ CallDoc.call_bindings_text bindings
    where bindings = CallDoc.lookup_calls ctype lookups

show_tags :: [(Text, Text)] -> Text
show_tags tags =
    Text.unwords [quote k <> "=" <> quote v | (k, v) <- Seq.sort_on fst tags]

show_initialize :: Instrument.InitializePatch -> Text
show_initialize Instrument.NoInitialization = ""
show_initialize (Instrument.InitializeMessage msg) = "Message: " <> msg
show_initialize (Instrument.InitializeMidi msgs) =
    Text.unlines (map prettyt msgs)

quote :: Text -> Text
quote s
    | Text.any Char.isSpace s = "\"" <> s <> "\""
    | otherwise = s

-- | Send the chosen instrument to the sequencer.  This will send
-- @set_instrument \"synth/inst\"@ to the REPL port.
choose_instrument :: Score.Instrument -> IO ()
choose_instrument inst = do
    let cmd = "set_instrument " ++ show (Score.inst_name inst)
    putStrLn $ "send: " ++ cmd
    response <- (untxt <$> SendCmd.send (txt cmd))
        `Exception.catch` \(exc :: Exception.SomeException) ->
            return ("error: " <> show exc)
    unless (null response) $
        putStrLn $ "response: " ++ response

-- | Find instruments that match the query, and update the UI incrementally.
process_query :: Fltk.Channel -> BrowserC.Window -> Db -> [Score.Instrument]
    -> Text -> IO [Score.Instrument]
process_query chan win db displayed query = do
    let matches = Search.search (db_index db) (Search.parse query)
        diff = Seq.indexed_pairs (==) displayed matches
    forM_ diff $ \(i, paired) -> case paired of
        Seq.Second inst -> Fltk.send_action chan $
            BrowserC.insert_line win (i+1) (Score.inst_name inst)
        Seq.First _inst -> Fltk.send_action chan $
            BrowserC.remove_line win (i+1)
        _ -> return ()
    return matches
