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
    contents of 'Patch.Patch'.

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
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Lazy as Lazy

import qualified Text.Printf as Printf

import qualified Util.Doc as Doc
import qualified Util.Fltk as Fltk
import qualified Util.Format as Format
import qualified Util.Seq as Seq

import qualified Cmd.CallDoc as CallDoc
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg

import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Perform.Im.Patch as Im.Patch
import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Patch as Patch

import qualified Instrument.BrowserC as BrowserC
import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Instrument.InstTypes as InstTypes
import qualified Instrument.Search as Search
import qualified Instrument.Tag as Tag

import qualified Local.Instrument
import qualified App.Config as Config
import qualified App.SendCmd as SendCmd
import Global


main :: IO ()
main = SendCmd.initialize $ do
    db <- Local.Instrument.load =<< Config.get_app_dir
    putStrLn $ "Loaded " ++ show (Inst.size db) ++ " instruments."
    win <- Fltk.run_action $ BrowserC.create 50 50 550 600
    let index_db = Db db (Search.make_index db)
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
    state_displayed :: [InstTypes.Qualified]
    } deriving (Show)

handle_msgs :: Fltk.Channel -> BrowserC.Window -> Db -> IO ()
handle_msgs chan win db = do
    displayed <- liftIO $ process_query chan win db [] ""
    flip State.evalStateT (State displayed) $ forever $ do
        Fltk.Msg typ text <- liftIO $ STM.atomically $ Fltk.read_msg win
        let qualified = InstTypes.parse_qualified text
        case typ of
            BrowserC.Select -> liftIO $ show_info chan win db qualified
            BrowserC.Choose -> liftIO $ choose_instrument qualified
            BrowserC.Query -> do
                state <- State.get
                displayed <- liftIO $
                    process_query chan win db (state_displayed state) text
                State.put (state { state_displayed = displayed })
            BrowserC.Unknown c -> liftIO $
                putStrLn $ "unknown msg type: " ++ show c

-- | Look up the instrument, generate a info sheet on it, and send to the UI.
show_info :: Fltk.Channel -> BrowserC.Window -> Db -> InstTypes.Qualified
    -> IO ()
show_info chan win db qualified =
    Fltk.send_action chan $ BrowserC.set_info win info
    where
    info = fromMaybe ("not found: " <> InstTypes.show_qualified qualified) $ do
        let InstTypes.Qualified synth_name inst_name = qualified
        synth <- Inst.lookup_synth synth_name (db_db db)
        inst <- Map.lookup inst_name (Inst.synth_insts synth)
        let synth_doc = Inst.synth_doc synth <> " -- "
                <> case Inst.inst_backend inst of
                    Inst.Midi {} -> "MIDI"
                    Inst.Im {} -> "音"
        return $ info_of synth_name inst_name synth_doc inst tags
    tags = fromMaybe [] $ Search.tags_of (db_index db) qualified

info_of :: InstTypes.SynthName -> InstTypes.Name -> Text -> Cmd.Inst
    -> [Tag.Tag] -> Text
info_of synth_name name synth_doc (Inst.Inst backend common) tags =
    synth_name <> " -- " <> (if Text.null name then "*" else name) <> " -- "
        <> synth_doc <> "\n\n" <> body
    where
    body = format_fields $ common_fields tags common ++ backend_fields
    backend_fields = case backend of
        Inst.Midi inst -> instrument_fields name inst
        Inst.Im patch -> patch_fields patch

common_fields :: [Tag.Tag] -> Common.Common Cmd.InstrumentCode -> [(Text, Text)]
common_fields tags (Common.Common code env _tags (Doc.Doc doc)) =
    [ ("Environ", if env == mempty then "" else pretty env)
    -- code
    , ("Cmds", show_cmds (Cmd.inst_cmds code))
    , ("Note generators", show_calls CallDoc.GeneratorCall
        (map CallDoc.convert_call note_generators))
    , ("Note transformers", show_calls CallDoc.TransformerCall
        (map CallDoc.convert_call note_transformers))
    , ("Val calls", show_calls CallDoc.ValCall
        (map CallDoc.convert_val_call val_calls))
    -- info
    , ("Doc", doc)
    , ("Tags", show_tags tags)
    -- TODO lost the patch_file field
    ]
    where
    Derive.InstrumentCalls note_generators note_transformers val_calls =
        Cmd.inst_calls code

instrument_fields :: InstTypes.Name -> Patch.Patch -> [(Text, Text)]
instrument_fields name inst =
    -- important properties
    [ ("Flags", Text.intercalate ", " $ map showt $ Set.toList flags)
    , ("Controls", show_control_map control_map)
    -- implementation details
    , ("Attribute map", show_attribute_map attr_map)
    , ("Pitchbend range", pretty pb_range)
    , ("Decay", if decay == Nothing then "" else pretty decay)
    , ("Scale", maybe "" pretty scale)
    , ("Initialization", show_initialize initialize)
    , ("Original name", if name == orig_name then "" else showt orig_name)
    ]
    where
    Patch.Patch
        { patch_name = orig_name
        , patch_control_map = control_map
        , patch_initialize = initialize
        , patch_attribute_map = attr_map
        , patch_defaults = settings
        } = inst
    Patch.Settings
        { config_flags = flags
        , config_scale = scale
        , config_decay = decay
        , config_pitch_bend_range = pb_range
        } = settings

patch_fields :: Im.Patch.Patch -> [(Text, Text)]
patch_fields (Im.Patch.Patch controls attr_map flags) =
    [ ("Flags", Text.intercalate ", " $ map showt $ Set.toList flags)
    , ("Attributes", Text.intercalate ", " $ map pretty $
        Common.mapped_attributes attr_map)
    , ("Controls", Text.unlines [pretty control <> "\t" <> doc |
        (control, doc) <- Map.toAscList controls])
    ]

format_fields :: [(Text, Text)] -> Text
format_fields = Text.unlines . filter (not . Text.null) . map field

field :: (Text, Text) -> Text
field (title, raw_text)
    | Text.null text = ""
    | Text.length text < 40 && not ("\n" `Text.isInfixOf` text) =
        title <> ": " <> text <> "\n"
    | otherwise = "\t" <> title <> ":\n" <> text <> "\n"
    where text = Text.strip raw_text

show_attribute_map :: Patch.AttributeMap -> Text
show_attribute_map (Common.AttributeMap table) =
    Text.unlines $ map fmt (Seq.sort_on (low_key . snd) table)
    where
    attrs = map (prettys . fst) table
    longest = fromMaybe 0 $ Seq.maximum (map length attrs)
    -- If this instrument uses a keymap, it's easier to read the attribute map
    -- if I put it in keymap order.
    low_key (_, Just (Patch.UnpitchedKeymap k)) = Just k
    low_key (_, Just (Patch.PitchedKeymap k _ _)) = Just k
    low_key (_, Nothing) = Nothing
    fmt (attrs, (keyswitches, maybe_keymap)) =
        -- Still not quite right for lining up columns.
        txt (Printf.printf "%-*s\t" longest (prettys attrs))
            <> pretty keyswitches <> maybe "" ((" "<>) . pretty) maybe_keymap

show_control_map :: Control.ControlMap -> Text
show_control_map cmap =
    Text.intercalate ", " [Score.control_name cont <> " (" <> showt num <> ")"
        | (cont, num) <- Map.toList cmap]

show_cmds :: [Msg.Msg -> Cmd.CmdId Cmd.Status] -> Text
show_cmds [] = ""
show_cmds cmds = showt (length cmds) <> " cmds (cmds can't be introspected yet)"

show_calls :: CallDoc.CallType -> [CallDoc.LookupCall] -> Text
show_calls ctype lookups =
    -- Let fltk do the wrapping.  Of course it doesn't know how the indentation
    -- works, so wrapped lines don't get indented, but it doesn't look that
    -- bad.
    Lazy.toStrict $ Format.render "\t" 10000 $ Format.paragraphs $
        map (CallDoc.call_bindings_text False) bindings
    where bindings = CallDoc.lookup_calls ctype lookups

show_tags :: [(Text, Text)] -> Text
show_tags tags =
    Text.unwords [quote k <> "=" <> quote v | (k, v) <- Seq.sort_on fst tags]

show_initialize :: Patch.InitializePatch -> Text
show_initialize Patch.NoInitialization = ""
show_initialize (Patch.InitializeMessage msg) = "Message: " <> msg
show_initialize (Patch.InitializeMidi msgs) =
    Text.unlines (map pretty msgs)

quote :: Text -> Text
quote s
    | Text.any Char.isSpace s = "\"" <> s <> "\""
    | otherwise = s

-- | Send the chosen instrument to the sequencer.  This will send
-- @change_instrument \"synth/inst\"@ to the REPL port.
choose_instrument :: InstTypes.Qualified -> IO ()
choose_instrument qualified = do
    let cmd = "change_instrument " <> showt (InstTypes.show_qualified qualified)
    Text.IO.putStrLn $ "send: " <> cmd
    (response, logs) <- SendCmd.send Config.repl_port cmd
        `Exception.catch` \(exc :: Exception.SomeException) ->
            return ("error: " <> showt exc, [])
    mapM_ Text.IO.putStrLn logs
    unless (Text.null response) $
        Text.IO.putStrLn $ "response: " <> response

-- | Find instruments that match the query, and update the UI incrementally.
process_query :: Fltk.Channel -> BrowserC.Window -> Db -> [InstTypes.Qualified]
    -> Text -> IO [InstTypes.Qualified]
process_query chan win db displayed query = do
    let matches = Search.search (db_index db) (Search.parse query)
        diff = Seq.indexed_pairs (==) displayed matches
    forM_ diff $ \(i, paired) -> case paired of
        Seq.Second inst -> Fltk.send_action chan $
            BrowserC.insert_line win (i+1) (InstTypes.show_qualified inst)
        Seq.First _inst -> Fltk.send_action chan $
            BrowserC.remove_line win (i+1)
        _ -> return ()
    return matches
