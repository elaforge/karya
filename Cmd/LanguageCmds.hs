{- | Helper functions to be imported into LanguageEnviron.  LanguageEnviron
    must be interpreted since it's the \"top level\" module, so I put the
    library of commands in here.

    Of course, lang commands can use anything in scope in LanguageEnviron, not
    just these helpers.  That includes all the various cmd_* functions used by
    the keybindings and everything in State.  Also, keybindings can be invoked
    directly with the 'keybinding' helper.  TODO not implemented

    Functions which are not designed to be composed generally take simpler
    types like strings, or get their block from the current focus, so they're
    easier to type.

    The various show_* functions print out state generally in a 'show' format,
    but not necessarily.  It's designed to be for human reading and may leave
    out relatively uninteresting data.

    TODO Can I use Language.Haskell.Parser or haddock to generate a list of
    toplevel names along with their documentation to give the REPL for
    completion and interactive documentation?

    To keep this module from getting huge, only general purpose and common cmds
    should go here.  Cmds which are meant to be used from the REPL but may be
    more specialized can go in Cmd.Lang.L* modules.
-}
module Cmd.LanguageCmds where
import Control.Monad
import qualified Data.List as List
import qualified Data.Map as Map
import Text.Printf

import qualified Util.Seq as Seq
import qualified Util.Log as Log
import qualified Util.Map as Map
import Util.Pretty as Pretty
import qualified Util.PPrint as PPrint

import qualified Midi.Midi as Midi

import Ui
import qualified Ui.Block as Block
import qualified Ui.Color as Color
import qualified Ui.Event as Event
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Types as Types

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Edit as Edit
import qualified Cmd.Info as Info
import qualified Cmd.Language as Language
import qualified Cmd.Play as Play
import qualified Cmd.Save as Save
import qualified Cmd.Selection as Selection
import qualified Cmd.Simple as Simple
import qualified Cmd.TimeStep as TimeStep

-- Just make sure these are compiled.
import qualified Cmd.MakeRuler ()
import qualified Cmd.Lang.LPitch ()

import qualified Derive.Schema as Schema
import qualified Derive.Schema.Default as Default
import qualified Derive.Score as Score
import qualified Perform.Midi.Control as Midi.Control
import qualified Perform.Midi.Convert as Midi.Convert
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Perform as Midi.Perform
import qualified Perform.Pitch as Pitch
import qualified Perform.Timestamp as Timestamp
import qualified Perform.Warning as Warning

import qualified Instrument.MidiDb as MidiDb

import qualified App.Config as Config


-- * errors

-- | Called from logview
s :: String -> Cmd.CmdL ()
s stackpos = maybe (Cmd.throw $ "can't parse stackpos: " ++ show stackpos)
        highlight_error (Warning.parse_stack stackpos)

unerror :: Cmd.CmdL ()
unerror = do
    view_ids <- State.get_all_view_ids
    forM_ view_ids $ \vid -> do
        State.set_selection vid Config.error_selnum Nothing

highlight_error :: Warning.StackPos -> Cmd.CmdL ()
highlight_error (bid, maybe_tid, maybe_range) = do
    view_ids <- fmap Map.keys (State.get_views_of bid)
    -- mapM_ raise_view view_ids
    case (maybe_tid, maybe_range) of
        (Just tid, Just (from, to)) -> do
            tracknums <- State.track_id_tracknums bid tid
            forM_ view_ids $ \vid -> forM_ tracknums $ \tracknum ->
                Selection.select_and_scroll vid Config.error_selnum
                    (Types.selection tracknum from tracknum to)
        _ -> return ()

-- TODO implement
raise_view :: ViewId -> Cmd.CmdL ()
raise_view = undefined

-- * show / modify cmd state

show_step :: Cmd.CmdL TimeStep.TimeStep
show_step = _cmd_state Cmd.state_step

set_step :: TimeStep.TimeStep -> Cmd.CmdL ()
set_step step = Cmd.modify_state $ \st -> st { Cmd.state_step = step }

show_octave :: Cmd.CmdL Pitch.Octave
show_octave = _cmd_state Cmd.state_kbd_entry_octave
set_octave :: Pitch.Octave -> Cmd.CmdL ()
set_octave n = Edit.cmd_modify_octave (const n) >> return ()

-- * load / save

quit :: Cmd.CmdL String
quit = return Language.magic_quit_string

save :: Cmd.CmdL ()
load, save_as :: FilePath -> Cmd.CmdL ()
load fn = Save.cmd_load fn
save = Save.cmd_save =<< Save.get_save_file
save_as fn = Save.cmd_save fn

-- * undo, redo

undo, redo :: Cmd.CmdL ()
undo = Edit.undo
redo = Edit.redo

-- * show / modify UI state

vid = Types.ViewId . Id.read_id
bid = Types.BlockId . Id.read_id
sid = Types.SchemaId . Id.read_id
rid = Types.RulerId . Id.read_id
tid = Types.TrackId . Id.read_id

show_state :: Cmd.CmdL String
show_state = do
    (State.State project dir views blocks tracks rulers _midi_conf proj_scale)
        <- State.get
    -- midi config showed by show_midi_config
    let f fm = show_list (map show (Map.keys fm))
    return $ show_record
        [ ("project", project), ("dir", dir)
        , ("views", f views), ("blocks", f blocks)
        , ("tracks", f tracks), ("rulers", f rulers)
        , ("scale", show proj_scale)
        ]

-- ** views

get_views :: Cmd.CmdL [ViewId]
get_views = State.gets (Map.keys . State.state_views)

destroy_view :: String -> Cmd.CmdL ()
destroy_view view_id = State.destroy_view (vid view_id)

-- ** blocks

show_block :: BlockId -> Cmd.CmdL String
show_block block_id = do
    block <- State.get_block block_id
    tracks <- mapM show_block_track (Block.block_tracks block)
    return $ printf "%s %s\n%s"
        (show (Block.block_title block)) (show block_id) (show_list tracks)

show_block_track :: Block.BlockTrack -> Cmd.CmdL String
show_block_track track = do
    tracklike <- show_tracklike (Block.tracklike_id track)
    return $ printf "%s\n\t(flags %s) (merged %s)" tracklike
        (show (Block.track_flags track))
        (show (Block.track_merged track))

-- | Show all blocks whose block id match a string.
-- TODO not too useful?
show_blocks :: String -> Cmd.CmdL String
show_blocks match = do
    st <- State.get
    let block_ids = match_ids match $ Map.keys (State.state_blocks st)
    descs <- mapM show_block block_ids
    return $ Seq.join "\n" descs

-- | Filter ids containing a given substring.
match_ids :: (Id.Ident id) => String -> [id] -> [id]
match_ids sub = filter ((sub `List.isInfixOf`) . Id.show_id . Id.unpack_id)

-- | Tracks that don't appear in any block.
orphan_tracks :: Cmd.CmdL [TrackId]
orphan_tracks =
    fmap (\trefs -> [tid | (tid, refs) <- trefs, refs == 0]) track_refs

track_refs :: Cmd.CmdL [(TrackId, Int)]
track_refs = do
    st <- State.get
    let tids = concatMap Block.block_track_ids
            (Map.elems (State.state_blocks st))
        insert fm tid = Map.insertWith (+) tid 1 fm
        ref_map = List.foldl' insert Map.empty tids
    return [(tid, Map.get 0 tid ref_map)
        | tid <- Map.keys (State.state_tracks st)]

show_tracklike :: Block.TracklikeId -> Cmd.CmdL String
show_tracklike (Block.TId tid rid) = do
    track <- State.get_track tid
    let title = Track.track_title track
    return $ printf "%s %s %s" (show title) (show tid) (show rid)
show_tracklike (Block.RId rid) = return (show rid)
show_tracklike (Block.DId color) = return $ "Div " ++ show color

set_schema :: BlockId -> SchemaId -> Cmd.CmdL ()
set_schema block_id schema_id = do
    State.modify_block block_id $ \block ->
        block { Block.block_schema = schema_id }

collapse_track, expand_track :: BlockId -> TrackNum -> Cmd.CmdL ()
collapse_track block_id tracknum = do
    -- TODO if the track to collapse is a pitch track, merge it with its
    -- note track instead
    State.add_track_flag block_id tracknum Block.Collapse
    Default.set_inst_status block_id tracknum
expand_track block_id tracknum = do
    State.remove_track_flag block_id tracknum Block.Collapse
    Default.set_inst_status block_id tracknum

-- | Called from logview.
collapse, expand :: TrackNum -> Cmd.CmdL ()
collapse tracknum = flip collapse_track tracknum =<< Cmd.get_focused_block
expand tracknum = flip expand_track tracknum =<< Cmd.get_focused_block


-- ** tracks

-- | Some helpers to make it easier to make TracklikeIds.
track :: String -> String -> Block.TracklikeId
track track_id ruler_id = Block.TId (tid track_id) (rid ruler_id)
ruler :: String -> Block.TracklikeId
ruler ruler_id = Block.RId (rid ruler_id)
divider :: Color.Color -> Block.TracklikeId
divider color = Block.DId (Block.Divider color)

show_track :: TrackId -> Cmd.CmdL String
show_track track_id = do
    track <- State.get_track track_id
    return $ PPrint.pshow (track { Track.track_events = Track.empty_events })
        ++ "Events: " ++ show (Track.events_length (Track.track_events track))

show_events :: TrackId -> TrackPos -> TrackPos -> Cmd.CmdL [Simple.Event]
show_events track_id start end = do
    track <- State.get_track track_id
    return $ (map Simple.event
        . Track.events_in_range start end . Track.track_events) track

set_render_style :: Track.RenderStyle -> TrackId -> Cmd.CmdL ()
set_render_style style track_id = State.modify_track_render track_id $
    \render -> render { Track.render_style = style }

-- | Insert a track that already exists.
insert_track :: TrackId -> TrackNum -> Cmd.CmdL ()
insert_track track_id tracknum = do
    block_id <- Cmd.get_focused_block
    ruler_id <- Create.get_ruler_id block_id tracknum
    State.insert_track block_id tracknum
        (Block.block_track (Block.TId track_id ruler_id) Config.track_width)

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

show_ruler :: RulerId -> Cmd.CmdL String
show_ruler ruler_id = do
    (Ruler.Ruler mlists bg show_names use_alpha full_width) <-
        State.get_ruler ruler_id
    return $ show_record
        [ ("bg", show bg)
        , ("show_names", show show_names), ("use_alpha", show use_alpha)
        , ("full_width", show full_width)
        , ("marklists", show_list (map fst mlists))
        ]

show_marklist :: RulerId -> Ruler.MarklistName -> Cmd.CmdL String
show_marklist ruler_id marklist_name = do
    mlist <- get_marklist ruler_id marklist_name
    return $ show_list $
        map (\(pos, m) -> printf "%s - %s" (show pos) (pretty m))
            (Ruler.forward mlist (TrackPos 0))

get_marklist :: RulerId -> Ruler.MarklistName -> Cmd.CmdL Ruler.Marklist
get_marklist ruler_id marklist_name = do
    ruler <- State.get_ruler ruler_id
    case lookup marklist_name (Ruler.ruler_marklists ruler) of
        Nothing -> Cmd.throw $
            "no marklist " ++ show marklist_name ++ " in " ++ show ruler_id
        Just mlist -> return mlist

replace_marklist :: RulerId -> Ruler.NameMarklist -> Cmd.CmdL ()
replace_marklist ruler_id (name, mlist) = do
    ruler <- State.get_ruler ruler_id
    i <- case List.findIndex ((==name) . fst) (Ruler.ruler_marklists ruler) of
        Nothing -> return 0
        Just i -> State.remove_marklist ruler_id i >> return i
    State.insert_marklist ruler_id i (name, mlist)

copy_marklist :: Ruler.MarklistName -> RulerId -> RulerId
    -> Cmd.CmdL ()
copy_marklist marklist_name from_ruler_id to_ruler_id = do
    mlist <- get_marklist from_ruler_id marklist_name
    replace_marklist to_ruler_id (marklist_name, mlist)

-- | Replace the rulers in the block with the given ruler_id.  If there is an
-- overlay version, it will be given to all but the first track.
replace_ruler :: RulerId -> BlockId -> Cmd.CmdL ()
replace_ruler ruler_id block_id = do
    _ <- State.get_ruler ruler_id -- Just make sure it exists.
    let overlay_id = Create.add_overlay_suffix ruler_id
    overlay_id <- fmap (maybe ruler_id (const overlay_id)) $
        State.lookup_ruler overlay_id
    State.modify_block block_id $ \block -> block
        { Block.block_tracks = map_head_tail
            (set_r ruler_id) (set_r overlay_id) (Block.block_tracks block)
        }
    where
    map_head_tail _ _ [] = []
    map_head_tail f g (x:xs) = f x : map g xs
    set_r ruler_id track = Block.modify_id track (Block.set_rid ruler_id)

-- * show / modify keymap

-- TODO
-- | Run the Cmd that is bound to the given KeySpec, if there is one.
-- keymap :: Keymap.KeySpec -> Cmd.CmdL ()

-- Modify global keymap
-- Modify keymap for given schema_id.

-- * midi config

-- | Called from the browser.
lookup_instrument :: String -> Cmd.CmdL (Maybe Instrument.Instrument)
lookup_instrument inst_name = do
    lookup_inst <- Cmd.get_lookup_midi_instrument
    return $ lookup_inst Score.no_attrs (Score.Instrument inst_name)

inst_info :: String -> Cmd.CmdL String
inst_info inst_name = Info.inst_info (Score.Instrument inst_name)

all_inst_info :: Cmd.CmdL String
all_inst_info = do
    config <- State.get_midi_config
    info <- mapM Info.inst_info (Map.keys (Instrument.config_alloc config))
    return $ show (length info) ++ " instruments:\n" ++ Seq.join "\n\n" info

track_info :: BlockId -> TrackNum
    -> Cmd.CmdL (Schema.TrackType, Maybe Score.Instrument, Pitch.ScaleId)
track_info block_id tracknum = do
    track_tree <- State.get_track_tree block_id
    proj_scale <- State.get_project_scale
    case Schema.get_track_info proj_scale track_tree (Just tracknum) of
        (Nothing, _, _) -> Cmd.throw $ "can't get track type for "
            ++ show block_id ++ " at " ++ show tracknum
        (Just typ, inst, scale) -> return (typ, inst, scale)

-- | Steps to load a new instrument.  All of them are optional, depending on
-- the circumstances.
--
-- - Deallocate address asignments for the old instrument, if one is being
-- replaced.
--
-- - Allocate addresses for the new instrument.
--
-- - Title track with new instrument.
--
-- - Send midi init.
--
-- For example, typing a new instrument in a track title should only complain
-- if there is no allocation, but not necessarily deallocate the replaced
-- instrument or send midi init.
load_instrument :: String -> Cmd.CmdL ()
load_instrument inst_name = do
    let inst = Score.Instrument inst_name
    block_id <- Cmd.get_focused_block
    tracknum <- Cmd.require =<< Cmd.get_insert_tracknum
    track_id <- Cmd.require =<< State.event_track_at block_id tracknum
    old_inst <- Cmd.require =<< fmap inst_type (track_info block_id tracknum)

    dealloc_instrument old_inst
    dev <- Cmd.require_msg ("no device for " ++ show inst)  =<< device_of inst
    chan <- find_chan_for dev
    alloc_instrument inst [(dev, chan)]

    State.set_track_title track_id (Default.instrument_to_title inst)
    send_instrument_init inst chan
    Log.notice $ "deallocating " ++ show old_inst ++ ", allocating "
        ++ show (dev, chan) ++ " to " ++ show inst
    where
    inst_type (Schema.NoteTrack _ _, inst, _) = inst
        -- maybe also accept control if there is just one inst
        -- but then I'd need some way to know the track_id
    inst_type _ = Nothing

find_chan_for :: Midi.WriteDevice -> Cmd.CmdL Midi.Channel
find_chan_for dev = do
    alloc <- fmap Instrument.config_alloc State.get_midi_config
    let addrs = map ((,) dev) [0..15]
        taken = concat (Map.elems alloc)
    let match = fmap snd $ List.find (not . (`elem` taken)) addrs
    Cmd.require_msg ("couldn't find free channel for " ++ show dev) match

send_instrument_init :: Score.Instrument -> Midi.Channel -> Cmd.CmdL ()
send_instrument_init inst chan = do
    info <- Cmd.require_msg ("inst not found: " ++ show inst)
        =<< Cmd.lookup_instrument_info inst
    let init = Instrument.patch_initialize (MidiDb.info_patch info)
        dev = Instrument.synth_device (MidiDb.info_synth info)
    send_initialization init inst dev chan

-- | This feels like it should go in another module... Cmd.Instrument?
-- I have too many things called Instrument!
send_initialization :: Instrument.InitializePatch
    -> Score.Instrument -> Midi.WriteDevice -> Midi.Channel -> Cmd.CmdL ()
send_initialization init inst dev chan = case init of
    Instrument.InitializeMidi msgs -> do
        Log.notice $ "sending midi init: " ++ concatMap Midi.show_message msgs
        mapM_ ((Cmd.midi dev) . Midi.set_channel chan) msgs
    Instrument.InitializeMessage msg ->
        -- TODO warn doesn't seem quite right for this...
        Log.warn $ "initialize instrument " ++ show inst ++ ": " ++ msg
    Instrument.NoInitialization -> return ()

alloc_instrument :: Score.Instrument -> [Instrument.Addr] -> Cmd.CmdL ()
alloc_instrument inst addrs = do
    config <- State.get_midi_config
    let alloc = Instrument.config_alloc config
    State.set_midi_config $ config
        { Instrument.config_alloc = Map.insert inst addrs alloc }

dealloc_instrument :: Score.Instrument -> Cmd.CmdL ()
dealloc_instrument inst = do
    config <- State.get_midi_config
    let alloc = Instrument.config_alloc config
    State.set_midi_config $ config
        { Instrument.config_alloc = Map.delete inst alloc }

-- | Deallocate the old allocation, and set it to the new one.  Meant for
-- interactive use.
realloc_instrument :: String -> String -> [Midi.Channel] -> Cmd.CmdL ()
realloc_instrument inst_name wdev chans = do
    let inst = Score.Instrument inst_name
    dealloc_instrument inst
    alloc_instrument inst [(Midi.WriteDevice wdev, c) | c <- chans]

schema_instruments :: BlockId -> Cmd.CmdL [Score.Instrument]
schema_instruments block_id = do
    titles <- fmap (map State.track_title) (State.get_track_info block_id)
    return $ Seq.map_maybe Default.title_to_instrument titles

-- | Try to automatically create an instrument config based on the instruments
-- found in the given block.  It simply gives each instrument on a device a
-- single channel increasing from 0.
--
-- Example: @auto_config (bid \"b0\") >>= State.set_midi_config@
--
-- TODO: won't work if there are >1 block, need a merge config
-- TODO: same inst with different keyswitches should get the same addrs
auto_config :: BlockId -> Cmd.CmdL Instrument.Config
auto_config block_id = do
    insts <- schema_instruments block_id
    devs <- mapM device_of insts
    let no_dev = [inst | (inst, Nothing) <- zip insts devs]
        inst_devs = [(inst, dev) | (inst, Just dev) <- zip insts devs]
        allocs = [(inst, [(dev, fromIntegral i)])
            | (dev, by_dev) <- Seq.keyed_group_with snd inst_devs
            , (i, (inst, _dev)) <- Seq.enumerate by_dev]
        default_inst = case allocs of
            (inst, _):_ -> Just inst
            _ -> Nothing
    unless (null no_dev) $
        Log.warn $ "no synth found for instruments: " ++ show insts
    return $ Instrument.config allocs default_inst

device_of :: Score.Instrument -> Cmd.CmdL (Maybe Midi.WriteDevice)
device_of inst = do
    maybe_info <- Cmd.lookup_instrument_info inst
    return $ fmap (Instrument.synth_device . MidiDb.info_synth) maybe_info

controls_of :: Score.Instrument -> [Midi.Control.Control]
controls_of inst = undefined -- TODO


-- * schema

-- ** derivation

derive_to_midi :: BlockId -> Cmd.CmdL ([Midi.WriteMessage], [Warning.Warning])
derive_to_midi block_id = score_to_midi =<< derive block_id

derive_to_perf :: BlockId -> Cmd.CmdL ([Midi.Perform.Event], [Warning.Warning])
derive_to_perf block_id = do
    events <- derive block_id
    lookup <- Cmd.get_lookup_midi_instrument
    return $ Midi.Convert.convert lookup events

derive :: BlockId -> Cmd.CmdL [Score.Event]
derive block_id = do
    schema_map <- Cmd.get_schema_map
    (result, _, _) <- Play.derive schema_map block_id
    case result of
        Left err -> Cmd.throw $ "derive error: " ++ show err
        Right events -> return events

derive_tempo block_id ts = do
    schema_map <- Cmd.get_schema_map
    (_, tempo, inv_tempo) <- Play.derive schema_map block_id
    return $ map inv_tempo (map Timestamp.seconds [0..10])

score_to_midi :: [Score.Event]
    -> Cmd.CmdL ([Midi.WriteMessage], [Warning.Warning])
score_to_midi events = do
    inst_config <- State.gets State.state_midi_config
    lookup <- Cmd.get_lookup_midi_instrument
    let (midi_events, convert_warnings) = Midi.Convert.convert lookup events
        (midi_msgs, perform_warnings) =
            Midi.Perform.perform lookup inst_config midi_events
    return (midi_msgs, convert_warnings ++ perform_warnings)


-- * util

-- | pprint does ok for records, but it doesn't work so well if I want to print
-- part of the record, or change the types.  A real record system for haskell
-- would probably fix this.
show_record :: [(String, String)] -> String
show_record = concatMap $ \(k, v) ->
    let s = Seq.strip v in printf "%s:%s\n" k
            (if '\n' `elem` s then '\n' : indent_lines s else ' ' : s)
indent_lines = Seq.rstrip . unlines . map (indent++) . lines
indent = "  "

show_list :: [String] -> String
show_list xs = concatMap (\(i, x) -> printf "%d. %s\n" i x) (Seq.enumerate xs)


_cmd_state :: (Cmd.State -> a) -> Cmd.CmdL a
_cmd_state = flip fmap Cmd.get_state
