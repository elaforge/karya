{- | Helper functions to be imported into Cmd.Lang.Environ.  Cmd.Lang.Environ
    must be interpreted since it's the \"top level\" module, so I put the
    library of commands in here.  An unqualified import in Cmd.Lang.Environ
    means this module is in scope at the REPL.

    Of course, lang commands can use anything in scope in Cmd.Lang.Environ, not
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
module Cmd.Lang.Global where
import qualified Data.List as List
import qualified Data.Map as Map
import Text.Printf

import qualified Midi.Synth ()

import Util.Control
import qualified Util.Seq as Seq
import qualified Util.Map as Map
import qualified Util.PPrint as PPrint

import qualified Ui.Block as Block
import qualified Ui.Color as Color
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Types as Types

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Edit as Edit
import qualified Cmd.Info as Info
import qualified Cmd.Save as Save
import qualified Cmd.Selection as Selection
import qualified Cmd.Simple as Simple
import qualified Cmd.TimeStep as TimeStep
import qualified Cmd.ViewConfig as ViewConfig

-- Just make sure these are compiled.
import qualified Cmd.Lang.LEvent ()
import qualified Cmd.Lang.LInst as LInst
import qualified Cmd.Lang.LPerf ()
import qualified Cmd.Lang.LPitch ()
import qualified Cmd.Lang.LRuler ()
import qualified Cmd.Lang.LTrack ()

import qualified Derive.Score as Score
import qualified Derive.Stack as Stack

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import qualified App.Config as Config
import Types


-- * util

-- | Take a string and automatically figure out what kind of ID is expected and
-- add a namespace if one was not already in the string.
class AutoId a where auto_id :: Id.Namespace -> String -> a
instance AutoId ViewId where auto_id ns = Types.ViewId . Id.make ns
instance AutoId BlockId where auto_id ns = Types.BlockId . Id.make ns
instance AutoId RulerId where auto_id ns = Types.RulerId . Id.make ns
instance AutoId TrackId where auto_id ns = Types.TrackId . Id.make ns

vid = Types.ViewId . Id.read_id
bid = Types.BlockId . Id.read_id
rid = Types.RulerId . Id.read_id
tid = Types.TrackId . Id.read_id

block :: Cmd.CmdL BlockId
block = Cmd.get_focused_block

view :: Cmd.CmdL ViewId
view = Cmd.get_focused_view

-- | Some oprators to more conveniently string together monadic and non-monadic
-- functions in the REPL.
--
-- For instance:
--
-- @
--      block >>= LPerf.get_midi_cache $> Midi.Cache.cache_chunks
--              .> (!!1) .> Midi.Cache.chunk_state .> Perform.state_postproc
-- @

($>) :: (Functor f) => f a -> (a -> b) -> f b
($>) = flip (<$>)
infixl 0 $>

(.>) :: (a -> b) -> (b -> c) -> (a -> c)
(.>) = flip (.)
infixl 9 .>

-- * errors

-- | Called from logview
s :: String -> Cmd.CmdL ()
s "" = unerror
s stackpos = maybe (Cmd.throw $ "can't parse stackpos: " ++ show stackpos)
        highlight_error (Stack.parse_ui_frame stackpos)

unerror :: Cmd.CmdL ()
unerror = do
    view_ids <- State.get_all_view_ids
    forM_ view_ids $ \vid -> do
        Selection.set vid Config.error_selnum Nothing

highlight_error :: Stack.UiFrame -> Cmd.CmdL ()
highlight_error (bid, maybe_tid, maybe_range) = do
    view_ids <- fmap Map.keys (State.get_views_of bid)
    mapM_ ViewConfig.bring_to_front view_ids
    case (maybe_tid, maybe_range) of
        (Nothing, _) -> forM_ view_ids $ \vid ->
            Selection.set vid Config.error_selnum
                (Just (Types.selection 0 0 9999 9999))
        (Just tid, Nothing) -> do
            tracknums <- State.track_id_tracknums bid tid
            forM_ view_ids $ \vid -> forM_ tracknums $ \tracknum ->
                Selection.set vid Config.error_selnum
                    (Just (Types.selection tracknum 0 tracknum 9999))
        (Just tid, Just (from, to)) -> do
            tracknums <- State.track_id_tracknums bid tid
            forM_ view_ids $ \vid -> forM_ tracknums $ \tracknum ->
                Selection.set_and_scroll vid Config.error_selnum
                    (Types.selection tracknum to tracknum from)

-- * show / modify cmd state

show_step :: Cmd.CmdL TimeStep.TimeStep
show_step = Cmd.gets (Cmd.state_time_step . Cmd.state_edit)

set_step :: TimeStep.TimeStep -> Cmd.CmdL ()
set_step step = Cmd.modify_edit_state $
    \st -> st { Cmd.state_time_step = step }

set_note_duration :: TimeStep.TimeStep -> Cmd.CmdL ()
set_note_duration step = Cmd.modify_edit_state $ \st ->
    st { Cmd.state_note_duration = step }

-- | Set play step to current step.
set_play_step :: Cmd.CmdL ()
set_play_step = do
    step <- Cmd.get_current_step
    Cmd.modify_play_state $ \st -> st { Cmd.state_play_step = step }

show_octave :: Cmd.CmdL Pitch.Octave
show_octave = Cmd.gets (Cmd.state_kbd_entry_octave . Cmd.state_edit)

set_octave :: Pitch.Octave -> Cmd.CmdL ()
set_octave n = Edit.cmd_modify_octave (const n) >> return ()

set_default_tempo :: Signal.Y -> Cmd.CmdL ()
set_default_tempo t =
    State.modify_default $ \d -> d { State.default_tempo = t }

set_default_inst :: String -> Cmd.CmdL ()
set_default_inst inst = State.modify_default $ \d ->
    d { State.default_instrument = Just (Score.Instrument inst) }

set_default_scale :: String -> Cmd.CmdL ()
set_default_scale scale = State.modify_default $ \d ->
    d { State.default_scale = Pitch.ScaleId scale }

-- * load / save

save :: Cmd.CmdL ()
save = Save.cmd_save =<< Save.get_save_file

save_as :: FilePath -> Cmd.CmdL ()
save_as fn = Save.cmd_save fn

load :: FilePath -> Cmd.CmdL ()
load fn = Save.cmd_load fn

-- * show / modify UI state

show_state :: Cmd.CmdL String
show_state = do
    (State.State views blocks tracks rulers
        (State.Config project dir root _ (State.Default scale key inst tempo)))
            <- State.get
    -- midi config showed by show_midi_config
    let f fm = PPrint.list (map show (Map.keys fm))
    return $ PPrint.record
        [ ("project", project), ("dir", dir) , ("root", show root)
        , ("views", f views), ("blocks", f blocks)
        , ("tracks", f tracks), ("rulers", f rulers)
        , ("scale", show scale)
        , ("key", show key)
        , ("inst", show inst)
        , ("tempo", show tempo)
        ]

-- ** views

get_views :: Cmd.CmdL [ViewId]
get_views = State.gets (Map.keys . State.state_views)

-- | Show all views whose view id matches a string.
show_views :: String -> Cmd.CmdL String
show_views match = do
    st <- State.get
    let view_ids = match_ids match $ Map.keys (State.state_views st)
    descs <- mapM show_view view_ids
    return $ Seq.join "\n"
        ["** " ++ show view_id ++ ":\n" ++ desc
            | (view_id, desc) <- zip view_ids descs]

show_view :: ViewId -> Cmd.CmdL String
show_view = fmap PPrint.pshow . State.get_view

destroy_view :: String -> Cmd.CmdL ()
destroy_view view_id = State.destroy_view (vid view_id)

-- ** blocks

show_block :: BlockId -> Cmd.CmdL String
show_block block_id = do
    block <- State.get_block block_id
    tracks <- mapM show_block_track (Block.block_tracks block)
    return $ printf "%s %s\n%s"
        (show (Block.block_title block)) (show block_id) (PPrint.list tracks)

show_block_track :: Block.Track -> Cmd.CmdL String
show_block_track track = do
    tracklike <- show_tracklike (Block.tracklike_id track)
    return $ printf "%s\n\t(flags %s) (merged %s)" tracklike
        (show (Block.track_flags track))
        (show (Block.track_merged track))

-- | Show all blocks whose block id matches a string.
-- Useful for quick block inspection.
show_blocks :: String -> Cmd.CmdL String
show_blocks match = do
    st <- State.get
    let block_ids = match_ids match $ Map.keys (State.state_blocks st)
    descs <- mapM show_block block_ids
    return $ Seq.join "\n"
        ["** " ++ show block_id ++ ":\n" ++ desc
            | (block_id, desc) <- zip block_ids descs]

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

collapse_track, expand_track :: BlockId -> TrackNum -> Cmd.CmdL ()
collapse_track block_id tracknum = do
    -- TODO if the track to collapse is a pitch track, merge it with its
    -- note track instead
    State.add_track_flag block_id tracknum Block.Collapse
    Info.set_inst_status block_id tracknum
expand_track block_id tracknum = do
    State.remove_track_flag block_id tracknum Block.Collapse
    Info.set_inst_status block_id tracknum

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
    return $ PPrint.pshow (track { Track.track_events = Events.empty })
        ++ "Events: " ++ show (Events.length (Track.track_events track))

show_events :: TrackId -> ScoreTime -> ScoreTime -> Cmd.CmdL [Simple.Event]
show_events track_id start end = do
    track <- State.get_track track_id
    return $ (map Simple.event . Events.ascending
        . Events.in_range start end . Track.track_events) track

-- | Insert a track that already exists.
insert_track :: TrackId -> TrackNum -> Cmd.CmdL ()
insert_track track_id tracknum = do
    block_id <- Cmd.get_focused_block
    ruler_id <- Create.get_ruler_id block_id tracknum
    State.insert_track block_id tracknum
        (Block.track (Block.TId track_id ruler_id) Config.track_width)

-- ** events

-- | Events in the selection.
selected_events :: Cmd.CmdL [Event.Event]
selected_events = undefined

-- | Event that overlaps the insert pos, or abort.
close_event :: Cmd.CmdL Event.Event
close_event = undefined

-- * time

sel_to_real :: Cmd.CmdL [RealTime]
sel_to_real = do
    (block_id, _, track_id, pos) <- Selection.get_insert
    tempo <- Cmd.perf_tempo <$> Cmd.get_performance block_id
    return $ tempo block_id track_id pos

-- * show / modify keymap

-- TODO
-- | Run the Cmd that is bound to the given KeySpec, if there is one.
-- keymap :: Keymap.KeySpec -> Cmd.CmdL ()

-- Modify global keymap

-- | Called from the browser.
load_instrument :: String -> Cmd.CmdL ()
load_instrument = LInst.load
