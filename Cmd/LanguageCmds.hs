{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- This module imports modules that are possibly used only dynamically by
-- lang, just to make sure they get compiled.
{- | Helper functions to be imported into LanguageEnviron.  LanguageEnviron
must be interpreted since it's the "top level" module, so I put the library
of commands in here.

Of course, lang commands can use anything in scope in LanguageEnviron, not just
these helpers.  That includes all the various cmd_* functions used by the
keybindings and everything in State.  Also, keybindings can be invoked directly
with the 'keybinding' helper.  TODO not implemented

Functions which are not designed to be composed generally take simpler types
like strings, or get their block from the current focus, so they're easier to
type.

The various show_* functions print out state generally in a 'show' format, but
not necessarily.  It's designed to be for human reading and may leave out
"uninteresting" data.

TODO:
- can I use Language.Haskell.Parser or haddock to generate a list of toplevel
names along with their documentation to give the REPL for completion and
interactive documentation?

-}
module Cmd.LanguageCmds where
import Control.Monad
import Control.Monad.Trans (liftIO)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Text.Printf

import qualified Util.Seq as Seq
import qualified Util.Log as Log
import Util.Pretty as Pretty
import qualified Util.PPrint as PPrint

import Ui.Types
import qualified Ui.Id as Id
import qualified Ui.Color as Color
import qualified Ui.Event as Event
import qualified Ui.Block as Block
import qualified Ui.Ruler as Ruler
import qualified Ui.Track as Track
import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Edit as Edit
import qualified Cmd.Language as Language
import qualified Cmd.Play as Play
import qualified Cmd.Save as Save
import qualified Cmd.Selection as Selection
import qualified Cmd.Simple as Simple
import qualified Cmd.TimeStep as TimeStep

-- Just make sure these are compiled.
import qualified Cmd.MakeRuler ()
import qualified Cmd.Create ()

import qualified Derive.Score as Score
import qualified Derive.Schema as Schema
import qualified Perform.Warning as Warning
import qualified Perform.Midi.Convert as Midi.Convert
import qualified Perform.Midi.Instrument as Midi.Instrument
import qualified Perform.Midi.Perform as Midi.Perform
import qualified Perform.Midi.Controller as Midi.Controller
import qualified Instrument.MidiDb as MidiDb
import qualified Instrument.Db as Instrument.Db

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


_cmd_state :: (Cmd.State -> a) -> Cmd.CmdL a
_cmd_state = flip fmap Cmd.get_state

-- * show / modify cmd state

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

load, save :: FilePath -> Cmd.CmdL ()
load fn = Save.cmd_load fn
save fn = Save.cmd_save fn

-- * undo, redo

-- * show / modify UI state

vid = Block.ViewId . Id.read_id
bid = Block.BlockId . Id.read_id
sid = Block.SchemaId . Id.read_id
rid = Ruler.RulerId . Id.read_id
tid = Track.TrackId . Id.read_id

show_state :: Cmd.CmdL String
show_state = do
    (State.State project dir views blocks tracks rulers _midi_config)
        <- State.get
    -- midi config showed by show_midi_config
    let f m = PPrint.pshow (Map.keys m)
    return $ show_record
        [ ("project", project), ("dir", dir)
        , ("views", f views), ("blocks", f blocks)
        , ("tracks", f tracks), ("rulers", f rulers)
        ]

-- ** views

get_views :: Cmd.CmdL [Block.ViewId]
get_views = fmap (Map.keys . State.state_views) State.get

destroy_view :: String -> Cmd.CmdL ()
destroy_view view_id = State.destroy_view (vid view_id)

-- ** blocks

show_block :: Block.BlockId -> Cmd.CmdL String
show_block block_id = do
    block <- State.get_block block_id
    return $ show_record
        [ ("title", Block.block_title block)
        , ("tracks", show_list (map show (Block.block_track_widths block)))
        , ("schema", show (Block.block_schema block))
        ]

-- | TODO put this in Cmd?
get_skeleton :: Block.BlockId -> Cmd.CmdL Schema.Skeleton
get_skeleton block_id = do
    schema_map <- Cmd.get_schema_map
    Schema.get_skeleton schema_map =<< State.get_block block_id

create_block :: (State.UiStateMonad m) =>
    Id.Id -> String -> String -> m Block.BlockId
create_block block_id ruler_id schema_id = State.create_block block_id $
    Block.block "" Config.block_config [(ruler ruler_id, Config.ruler_width)]
        (sid schema_id)

set_schema :: Block.BlockId -> Block.SchemaId -> Cmd.CmdL ()
set_schema block_id schema_id = do
    State.modify_block block_id $ \block ->
        block { Block.block_schema = schema_id }

-- ** tracks

-- | Some helpers to make it easier to make TracklikeIds.
track :: String -> String -> Block.TracklikeId
track track_id ruler_id = Block.TId (tid track_id) (rid ruler_id)
ruler :: String -> Block.TracklikeId
ruler ruler_id = Block.RId (rid ruler_id)
divider :: Color.Color -> Block.TracklikeId
divider color = Block.DId (Block.Divider color)

show_track :: Track.TrackId -> Cmd.CmdL String
show_track track_id = do
    track <- State.get_track track_id
    return $ PPrint.pshow (track { Track.track_events = Track.empty_events })
        ++ "Events: " ++ show (Track.events_length (Track.track_events track))

show_events :: Track.TrackId -> TrackPos -> TrackPos -> Cmd.CmdL [Simple.Event]
show_events track_id start end = do
    track <- State.get_track track_id
    return $ (map Simple.event
        . Track.events_in_range start end . Track.track_events) track

set_render_style :: Track.RenderStyle -> Track.TrackId -> Cmd.CmdL ()
set_render_style style track_id = State.modify_track_render track_id $
    \render -> render { Track.render_style = style }

-- ** events

-- | Events in the selection.
selected_events :: Cmd.CmdL [Event.Event]
selected_events = undefined

-- | Event that overlaps the insert pos, or abort.
close_event :: Cmd.CmdL Event.Event
close_event = undefined

-- ** rulers

{- Examples:
Create.ruler [MakeRuler.meter_ruler 16 MakeRuler.m44] "meter_44"

replace_marklist (rid "r1") "meter" (MakeRuler.meter_ruler 16 MakeRuler.m44)
copy_marklist "meter" (rid "r1") (rid "r1.overlay")
-}

show_ruler :: Ruler.RulerId -> Cmd.CmdL String
show_ruler ruler_id = do
    (Ruler.Ruler mlists bg show_names use_alpha full_width) <-
        State.get_ruler ruler_id
    return $ show_record
        [ ("bg", show bg)
        , ("show_names", show show_names), ("use_alpha", show use_alpha)
        , ("full_width", show full_width)
        , ("marklists", show_list (map fst mlists))
        ]

show_marklist :: Ruler.RulerId -> Ruler.MarklistName -> Cmd.CmdL String
show_marklist ruler_id marklist_name = do
    mlist <- get_marklist ruler_id marklist_name
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

-- TODO
-- | Run the Cmd that is bound to the given KeySpec, if there is one.
-- keymap :: Keymap.KeySpec -> Cmd.CmdL ()

-- Modify global keymap
-- Modify keymap for given schema_id.

-- * midi config

track_type :: Block.BlockId -> Block.TrackNum -> Cmd.CmdL Schema.TrackType
track_type block_id tracknum = do
    skel <- get_skeleton block_id
    case Schema.track_type_of tracknum skel of
        Nothing -> State.throw $ "can't get track type for "
            ++ show block_id ++ " at " ++ show tracknum
        Just typ -> return typ

-- | Load a new instrument: deassign instrument from current track,
-- retitle track with new instrument, give new instrument a channel, and
-- send midi init for that channel.
load_instrument :: String -> Cmd.CmdL ()
load_instrument inst_name = do
    let inst = Score.Instrument inst_name
    block_id <- Cmd.get_focused_block
    tracknum <- Cmd.require =<< Cmd.get_insert_tracknum
    track_id <- Cmd.require =<< State.event_track_at block_id tracknum
    old_inst <- Cmd.require =<< fmap inst_type (track_type block_id tracknum)

    dealloc_instrument old_inst
    dev <- Cmd.require_msg ("no device for " ++ show inst)  =<< device_of inst
    chan <- find_chan_for dev
    alloc_instrument inst (dev, chan)

    State.set_track_title track_id (Schema.instrument_to_title inst)
    send_instrument_init inst chan
    Log.notice $ "deallocating " ++ show old_inst ++ ", allocating "
        ++ show (dev, chan) ++ " to " ++ show inst
    where
    inst_type (Schema.InstrumentTrack inst) = Just inst
        -- maybe also accept controller if there is just one inst
        -- but then I'd need some way to know the track_id
    inst_type _ = Nothing

find_chan_for :: Midi.WriteDevice -> Cmd.CmdL Midi.Channel
find_chan_for dev = do
    alloc <- fmap Midi.Instrument.config_alloc State.get_midi_config
    let addrs = map ((,) dev) [0..15]
    let match = fmap snd $ List.find (not . (`Map.member` alloc)) addrs
    Cmd.require_msg ("couldn't find free channel for " ++ show dev) match

send_instrument_init :: Score.Instrument -> Midi.Channel -> Cmd.CmdL ()
send_instrument_init inst chan = do
    info <- Cmd.require_msg ("inst not found: " ++ show inst)
        =<< Cmd.lookup_instrument_info inst
    let init = Midi.Instrument.patch_initialize (MidiDb.info_patch info)
        dev = Midi.Instrument.synth_device (MidiDb.info_synth info)
    send_initialization init inst dev chan

-- | This feels like it should go in another module... Cmd.Instrument?
-- I have too many things called Instrument!
send_initialization :: Midi.Instrument.InitializePatch
    -> Score.Instrument -> Midi.WriteDevice -> Midi.Channel -> Cmd.CmdL ()
send_initialization init inst dev chan = case init of
    Midi.Instrument.InitializeMidi msgs -> do
        Log.notice $ "sending midi init: " ++ show msgs
        mapM_ ((Cmd.midi dev) . Midi.set_channel chan) msgs
    Midi.Instrument.InitializeSysex bytes -> do
        msg <- Cmd.require_msg ("bogus sysex for " ++ show inst)
            (Midi.Instrument.sysex_to_msg bytes)
        Cmd.midi dev msg
    Midi.Instrument.InitializeMessage msg ->
        -- TODO warn doesn't seem quite right for this...
        Log.warn $ "initialize instrument " ++ show inst ++ ": " ++ msg
    Midi.Instrument.NoInitialization -> return ()

alloc_instrument :: Score.Instrument -> Midi.Instrument.Addr -> Cmd.CmdL ()
alloc_instrument inst addr = do
    config <- State.get_midi_config
    let alloc = Midi.Instrument.config_alloc config
    State.set_midi_config $ config
        { Midi.Instrument.config_alloc = Map.insert addr inst alloc }

dealloc_instrument :: Score.Instrument -> Cmd.CmdL ()
dealloc_instrument inst = do
    config <- State.get_midi_config
    let alloc = Midi.Instrument.config_alloc config
    State.set_midi_config $ config
        { Midi.Instrument.config_alloc = Map.filter (/=inst) alloc }

schema_instruments :: Block.BlockId -> Cmd.CmdL [Score.Instrument]
schema_instruments block_id = do
    skel <- get_skeleton block_id
    return (Schema.skeleton_instruments skel)

-- | Try to automatically create an instrument config based on the instruments
-- found in the given block.  It simply gives each instrument on a device a
-- single channel increasing from 0.
--
-- Example: auto_config (bid "b0") >>= State.set_midi_config
-- TODO: won't work if there are >1 block, need a merge config
auto_config :: Block.BlockId -> Cmd.CmdL Midi.Instrument.Config
auto_config block_id = do
    insts <- schema_instruments block_id
    devs <- mapM device_of insts
    let no_dev = [inst | (inst, Nothing) <- zip insts devs]
        inst_devs = [(inst, dev) | (inst, Just dev) <- zip insts devs]
        allocs = [((dev, fromIntegral i), inst)
            | (dev, by_dev) <- Seq.keyed_group_with snd inst_devs
            , (i, (inst, _dev)) <- Seq.enumerate by_dev]
        default_addr = case allocs of
            [] -> Nothing
            ((dev, chan), _inst) : _ -> Just (dev, chan)
    unless (null no_dev) $
        Log.warn $ "no synth found for instruments: " ++ show insts
    return $ Midi.Instrument.config allocs default_addr

device_of :: Score.Instrument -> Cmd.CmdL (Maybe Midi.WriteDevice)
device_of inst = do
    maybe_info <- Cmd.lookup_instrument_info inst
    return $ fmap (Midi.Instrument.synth_device . MidiDb.info_synth) maybe_info

controllers_of :: Score.Instrument -> [Midi.Controller.Controller]
controllers_of inst = undefined -- TODO


-- * schema

-- ** derivation

derive_to_midi block_id = score_to_midi =<< derive block_id

derive :: Block.BlockId -> Cmd.CmdL [Score.Event]
derive block_id = do
    schema_map <- Cmd.get_schema_map
    (result, _, _) <- Play.derive schema_map block_id
    case result of
        Left err -> State.throw $ "derive error: " ++ show err
        Right events -> return events

score_to_midi :: [Score.Event]
    -> Cmd.CmdL ([Midi.WriteMessage], [Warning.Warning])
score_to_midi events = do
    inst_config <- fmap State.state_midi_config State.get
    lookup <- Cmd.get_lookup_midi_instrument
    let (midi_events, convert_warnings) = Midi.Convert.convert lookup events
        (midi_msgs, perform_warnings) =
            Midi.Perform.perform lookup inst_config midi_events
    return (midi_msgs, convert_warnings ++ perform_warnings)
