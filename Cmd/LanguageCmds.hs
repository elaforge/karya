{- | Helper functions to be imported into LanguageEnviron.  LanguageEnviron
must be interpreted since it's the "top level" module, so I put the library
of commands in here.

Of course, lang commands can use anything in scope in LanguageEnviron, not just
these helpers.  That includes all the various cmd_* functions used by the
keybindings and everything in State.  Also, keybindings can be invoked directly
with the 'keybinding' helper.

The difference is that the functions here generally take simpler types like
strings, if they will just be wrapped up in a newtype, like BlockId.

The various show_* functions print out state generally in a 'show' format, but
not necessarily.  It's designed to be for human reading and may leave out
"uninteresting" data.

TODO:
- can I use Language.Haskell.Parser or haddock to generate a list of toplevel
names along with their documentation to give the REPL for completion and
interactive documentation?

-}
module Cmd.LanguageCmds where
import qualified Control.Monad.Identity as Identity
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Text.Printf

import qualified Util.Seq as Seq
-- import qualified Util.Log as Log
import Util.Pretty as Pretty
import qualified Util.PPrint as PPrint

import Ui.Types
import qualified Ui.Color as Color
import qualified Ui.Event as Event
import qualified Ui.Block as Block
import qualified Ui.Ruler as Ruler
import qualified Ui.Track as Track
import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd
-- import qualified Cmd.Save as Save
import qualified Cmd.Edit as Edit
import qualified Cmd.TimeStep as TimeStep
import qualified Cmd.Language as Language
import qualified Cmd.Play as Play
-- import qualified Cmd.MakeRuler as MakeRuler

import qualified Derive.Score as Score
import qualified Derive.Schema as Schema
import qualified Perform.InstrumentDb as InstrumentDb
import qualified Perform.Transport as Transport
import qualified Perform.Warning as Warning
import qualified Perform.Midi.Convert as Midi.Convert
import qualified Perform.Midi.Instrument as Midi.Instrument
import qualified Perform.Midi.Perform as Midi.Perform

import qualified Midi.Midi as Midi

import qualified App.Config as Config


-- * util

-- | pprint does ok for records, but it doesn't work so well if I want to print
-- part of the record, or change the types.  A real record system for haskell
-- would probably fix this.
show_record :: [(String, String)] -> String
show_record = concatMap (\(k, v) ->
    printf "%s:\n%s\n" k (indent_lines (Seq.strip v)))
indent_lines = Seq.rstrip . unlines . map (indent++) . lines
indent = "  "

show_list :: [String] -> String
show_list xs = concatMap (\(i, x) -> printf "%d. %s\n" i x) (Seq.enumerate xs)


_cmd_state :: (Cmd.State -> a) -> Cmd.CmdT Identity.Identity a
_cmd_state = flip fmap Cmd.get_state

-- * show / modify cmd state

show_save_file :: Cmd.CmdL String
show_save_file = _cmd_state (show . Cmd.state_default_save_file)
set_save_file :: String -> Cmd.CmdL ()
set_save_file s =
    Cmd.modify_state $ \st -> st { Cmd.state_default_save_file = s }

show_step :: Cmd.CmdL TimeStep.TimeStep
show_step = _cmd_state Cmd.state_step

set_step :: TimeStep.TimeStep -> Cmd.CmdL ()
set_step step = Cmd.modify_state $ \st -> st { Cmd.state_step = step }

show_octave :: Cmd.CmdL Int
show_octave = _cmd_state Cmd.state_kbd_entry_octave
set_octave :: Int -> Cmd.CmdL ()
set_octave n = Edit.cmd_modify_octave (const n) >> return ()

-- * load / save

quit :: Cmd.CmdL String
quit = return Language.magic_quit_string

-- Need to run in IO, not Identity for this.
-- I think I could have lang run in IO, but is it worth it just for this?
-- I don't see why not.  TODO
-- load, save :: Maybe String -> Cmd.CmdL ()
-- load fn = Save.cmd_load fn
-- save fn = Save.cmd_save fn

-- undo, redo

-- * show / modify UI state

vid = Block.ViewId
bid = Block.BlockId
_bid (Block.BlockId s) = s
sid = Block.SchemaId
rid = Ruler.RulerId
tid = Track.TrackId

show_state :: Cmd.CmdL String
show_state = do
    (State.State views blocks tracks rulers _midi_config) <- State.get
    -- midi config showed by show_midi_config
    let f m = PPrint.pshow (Map.keys m)
    return $ show_record
        [ ("views", f views), ("blocks", f blocks)
        , ("tracks", f tracks), ("rulers", f rulers)
        ]

-- ** views

get_views :: Cmd.CmdL [Block.ViewId]
get_views = fmap (Map.keys . State.state_views) State.get

create_view :: String -> String -> Cmd.CmdL Block.ViewId
create_view view_id block_id = do
    rect <- fmap (find_rect Config.view_size . map Block.view_rect . Map.elems
        . State.state_views) State.get
    State.create_view view_id
        (Block.view (bid block_id) rect Config.view_config)

-- TODO I also need the screen dimensions to do this right.  Before I go
-- too far here, though, I'll want to think about proper window manager stuff.
-- If I just allow the placement function to be passed as an arg...
find_rect (w, h) rects = Block.Rect (right, bottom) (w, h)
    where
    right = maximum $ 0 : map Block.rect_right rects
    bottom = 10

destroy_view :: String -> Cmd.CmdL ()
destroy_view view_id = State.destroy_view (vid view_id)

-- ** blocks

get_focused_block :: Cmd.CmdL Block.BlockId
get_focused_block = fmap Block.view_block
    (State.get_view =<< Cmd.get_focused_view)

show_block :: String -> Cmd.CmdL String
show_block block_id = do
    Block.Block { Block.block_title = title, Block.block_ruler_track = ruler,
        Block.block_tracks = tracks, Block.block_schema = schema }
            <- State.get_block (bid block_id)
    return $ show_record
        [ ("title", title)
        , ("ruler", show ruler)
        , ("tracks", show_list (map (show . fst) tracks))
        , ("schema", show schema)
        ]

get_skeleton :: String -> Cmd.CmdL Schema.Skeleton
get_skeleton block_id = do
    Schema.get_skeleton =<< State.get_block (bid block_id)

create_block :: (State.UiStateMonad m) =>
    String -> String -> String -> m Block.BlockId
create_block id_name ruler_id schema_id = State.create_block id_name
    (Block.block "" Config.block_config (ruler ruler_id) [] (sid schema_id))

set_schema :: String -> String -> Cmd.CmdL ()
set_schema block_id schema_id = do
    State.modify_block (bid block_id) $ \block ->
        block { Block.block_schema = sid schema_id }

-- ** tracks

-- | Some helpers to make it easier to make TracklikeIds.
track :: String -> String -> Block.TracklikeId
track track_id ruler_id = Block.TId (tid track_id) (rid ruler_id)
ruler :: String -> Block.TracklikeId
ruler ruler_id = Block.RId (rid ruler_id)
divider :: Color.Color -> Block.TracklikeId
divider color = Block.DId (Block.Divider color)

show_track :: String -> Cmd.CmdL String
show_track track_id = do
    track <- State.get_track (tid track_id)
    return $ show_list $ map Track.pretty_pos_event $
        Track.event_list (Track.track_events track)

insert_track block_id tracknum tracklike width = do
    State.insert_track (bid block_id) tracknum tracklike width
remove_track block_id tracknum = State.remove_track (bid block_id) tracknum

show_events :: String -> TrackPos -> TrackPos -> Cmd.CmdL String
show_events track_id start end = do
    track <- State.get_track (tid track_id)
    return $ (show_list . map Track.pretty_pos_event
        . Track.events_in_range start end . Track.track_events) track

-- ** events

-- | Events in the selection.
selected_events :: Cmd.CmdL [Event.Event]
selected_events = undefined

-- | Event that overlaps the insert pos, or abort.
close_event :: Cmd.CmdL Event.Event
close_event = undefined

-- ** rulers

{- Examples:
replace_marklist (rid "r1")
    (MakeRuler.regular_meter (TrackPos 2048) [4, 8, 4, 4])
copy_marklist "meter" (rid "r1") (rid "r1.overlay")
-}

show_ruler :: String -> Cmd.CmdL String
show_ruler ruler_id = do
    (Ruler.Ruler mlists bg show_names use_alpha full_width) <-
        State.get_ruler (rid ruler_id)
    return $ show_record
        [ ("bg", show bg)
        , ("show_names", show show_names), ("use_alpha", show use_alpha)
        , ("full_width", show full_width)
        , ("marklists", show_list (map fst mlists))
        ]

show_marklist :: String -> Ruler.MarklistName -> Cmd.CmdL String
show_marklist ruler_id marklist_name = do
    mlist <- get_marklist (rid ruler_id) marklist_name
    return $ show_list $
        map (\(pos, m) -> printf "%s - %s" (show pos) (pretty m))
            (Ruler.forward mlist (TrackPos 0))

get_marklist :: Ruler.RulerId -> Ruler.MarklistName -> Cmd.CmdL Ruler.Marklist
get_marklist ruler_id marklist_name = do
    ruler <- State.get_ruler ruler_id
    case lookup marklist_name (Ruler.ruler_marklists ruler) of
        Nothing -> State.throw $
            "no marklist " ++ show marklist_name ++ " in " ++ show ruler_id
        Just mlist -> return mlist

replace_marklist :: Ruler.RulerId -> Ruler.NameMarklist -> Cmd.CmdL ()
replace_marklist ruler_id (name, mlist) = do
    ruler <- State.get_ruler ruler_id
    i <- case List.findIndex ((==name) . fst) (Ruler.ruler_marklists ruler) of
        Nothing -> return 0
        Just i -> State.remove_marklist ruler_id i >> return i
    State.insert_marklist ruler_id i (name, mlist)

copy_marklist :: Ruler.MarklistName -> Ruler.RulerId -> Ruler.RulerId
    -> Cmd.CmdL ()
copy_marklist marklist_name from_ruler_id to_ruler_id = do
    mlist <- get_marklist from_ruler_id marklist_name
    replace_marklist to_ruler_id (marklist_name, mlist)

-- * show / modify keymap

-- | Run the Cmd that is bound to the given KeySpec, if there is one.
-- keymap :: Keymap.KeySpec -> Cmd.CmdL ()

-- Modify global keymap
-- Modify keymap for given schema_id.

-- * midi config

assign_instrument :: String -> Midi.Instrument.Addr -> Cmd.CmdL ()
assign_instrument inst_name addr = do
    inst <- case InstrumentDb.lookup (Score.Instrument inst_name) of
        Just (InstrumentDb.Midi midi_inst) -> return midi_inst
        Just inst -> State.throw $
            "you can't allocate a midi address to non-midi instrument "
            ++ show inst
        Nothing ->
            State.throw $ "instrument " ++ show inst_name ++ " not found"
    config <- State.get_midi_config
    State.set_midi_config $ config { Midi.Instrument.config_alloc =
        Map.insert addr inst (Midi.Instrument.config_alloc config) }

schema_instruments :: (State.UiStateMonad m) =>
    Block.BlockId -> m [Score.Instrument]
schema_instruments block_id = do
    skel <- Schema.get_skeleton =<< State.get_block block_id
    return (Schema.skeleton_instruments skel)

-- | Try to automatically create an instrument config based on the instruments
-- found in the given block.
auto_config :: (State.UiStateMonad m) =>
    Midi.WriteDevice -> Block.BlockId -> m Midi.Instrument.Config
auto_config write_device block_id = do
    score_insts <- schema_instruments block_id
    -- TODO warn about insts not found?
    let insts = Maybe.catMaybes $ map InstrumentDb.lookup score_insts
        addrs = [((write_device, chan), inst)
            | (InstrumentDb.Midi inst, chan) <- zip insts [0..]]
    return $ Midi.Instrument.config addrs (Just (write_device, 0))


-- * schema

-- ** derivation

derive_to_midi block_id = score_to_midi =<< derive block_id

derive :: String -> Cmd.CmdL [Score.Event]
derive block_id = do
    (result, _, _) <- Play.derive (bid block_id)
    case result of
        Left err -> State.throw $ "derive error: " ++ show err
        Right events -> return events

score_to_midi :: [Score.Event]
    -> Cmd.CmdL ([Midi.WriteMessage], [Warning.Warning])
score_to_midi events = do
    inst_config <- fmap State.state_midi_config State.get
    let (midi_events, convert_warnings) = Midi.Convert.convert events
        (midi_msgs, perform_warnings) =
            Midi.Perform.perform inst_config midi_events
    return (midi_msgs, convert_warnings ++ perform_warnings)
