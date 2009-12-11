{-# LANGUAGE PatternGuards #-}
{- | A schema is a transformation from a block to a deriver.  It may intuit the
    deriver solely from the structure of the block, or it may ignore the block
    entirely if it's specialized to one particular shape of block.

    The \"schema\" step is so that each block doesn't need to have its own
    deriver ID hardcoded into it.  Instead, many blocks can share a \"schema\"
    as long as they have the same general structure.

    The @SchemaId -> Schema@ mapping looks in a hardcoded list, a custom list
    passed via static configuration, and a dynamically loaded list.  The
    assumed usage is that you experiment with new Derivers and make minor
    changes via dynamic loading, and later incorporate them into the static
    configuration.

    TODO dynamic loaded schemas are not implemented yet

    TODO Since the tempo track is global now, I should lose the tempo scope
    parsing.  Except that I'd like it to not be global, so I'll just leave this
    as-is unless I decide global after all.  TODO is it really global!?
-}
module Derive.Schema (
    -- Re-export schema types from Cmd, to pretend they're defined here.
    -- * types
    Schema(..), SchemaDeriver
    , CmdContext(..), ContextCmds, SchemaMap

    -- * lookup
    , lookup_deriver, get_signal_deriver, get_cmds

    -- * parser
    , default_parser

    -- * util
    , cmd_context

    -- * exported for testing
    , get_defaults, get_track_info, TrackType(..)
    , compile, compile_to_signals
    , default_schema
) where
import Control.Monad
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Tree as Tree

import qualified Util.Log as Log
import qualified Util.Seq as Seq
import qualified Util.Tree

import Ui
import qualified Ui.Block as Block
import qualified Ui.Skeleton as Skeleton
import qualified Ui.State as State
import qualified Ui.Types as Types

import qualified Cmd.Cmd as Cmd
import Cmd.Cmd (Schema(..), SchemaDeriver, CmdContext(..), ContextCmds,
    SchemaMap)
import qualified Cmd.ControlTrack as ControlTrack
import qualified Cmd.Keymap as Keymap
import qualified Cmd.MidiThru as MidiThru
import qualified Cmd.NoteEntry as NoteEntry
import qualified Cmd.NoteTrack as NoteTrack
import qualified Cmd.NoteTrackKeymap as NoteTrackKeymap
import qualified Cmd.PitchTrack as PitchTrack

import qualified Derive.Control as Control
import qualified Derive.Derive as Derive
import qualified Derive.Note as Note
import qualified Derive.Score as Score
import qualified Derive.Schema.Default as Default

import qualified Perform.Signal as Signal
import qualified Perform.Pitch as Pitch
import qualified Perform.Midi.Instrument as Instrument

import qualified Instrument.MidiDb as MidiDb

import qualified App.Config as Config


hardcoded_schemas :: SchemaMap
hardcoded_schemas = Map.fromList [(Config.schema, default_schema)]

merge_schemas :: SchemaMap -> SchemaMap -> SchemaMap
merge_schemas map1 map2 = Map.union map2 map1


-- * lookup

-- | Create a LookupDeriver function.
lookup_deriver :: SchemaMap -> State.State -> Derive.LookupDeriver
lookup_deriver schema_map ui_state block_id = State.eval ui_state $ do
    block <- State.get_block block_id
    schema <- State.lookup_id (Block.block_schema block)
        (merge_schemas hardcoded_schemas schema_map)
    schema_deriver schema block_id

-- | Get the signal deriver for the given block.  Unlike the event deriver,
-- the signal deriver is only ever local to one block, so it doesn't need
-- a lookup mechanism.
get_signal_deriver :: (State.UiStateMonad m) => SchemaMap -> BlockId
    -> m (Derive.SignalDeriver Signal.Display)
get_signal_deriver schema_map block_id = do
    block <- State.get_block block_id
    schema <- State.lookup_id (Block.block_schema block)
        (merge_schemas hardcoded_schemas schema_map)
    state <- State.get
    State.eval_rethrow "get signal deriver" state
        (schema_signal_deriver schema block_id)

-- | A block's Schema also implies a set of Cmds, possibly based on the
-- focused track.  This is so that e.g. control tracks use control editing keys
-- and note tracks use note entry keys, and they can set up the midi thru
-- mapping appropriately.
get_cmds :: SchemaMap -> CmdContext -> SchemaId -> ContextCmds
get_cmds schema_map context schema_id =
    case Map.lookup schema_id (merge_schemas hardcoded_schemas schema_map) of
        Nothing -> ([], [])
        Just schema -> schema_cmds schema context

-- | Constructor for 'CmdContext'.
cmd_context :: Instrument.Config -> Pitch.ScaleId -> MidiDb.LookupMidiInstrument
    -> Cmd.EditMode -> Bool -> Maybe TrackNum -> State.TrackTree
    -> CmdContext
cmd_context midi_config proj_scale lookup_midi edit_mode kbd_entry
        focused_tracknum ttree =
    CmdContext default_inst proj_scale inst_addr lookup_midi edit_mode
        kbd_entry focused_tracknum ttree
    where
    default_inst = Instrument.config_default_inst midi_config
    -- The thru cmd has to pick a single addr for a give inst, so let's just
    -- pick the lowest one.
    inst_map = Map.fromList [ (inst, minimum addrs)
        | (inst, addrs) <- Map.toList (Instrument.config_alloc midi_config)
        , not (null addrs) ]
    inst_addr = flip Map.lookup inst_map


-- * default schema

-- | The default schema is supposed to be simple but useful, and rather
-- trackerlike.
default_schema :: Schema
default_schema =
    Schema default_schema_deriver default_schema_signal_deriver default_cmds

-- ** cmds

-- | This decides what track-specific commands are in scope based on the
-- current focus and information in the CmdContext.
--
-- TODO lookup scale here and return an error if it can't be found?
default_cmds :: CmdContext -> ContextCmds
default_cmds context = (cmds2, warns)
    where
    cmds1 = add_with_note $ case maybe_track_type of
        Nothing -> ([], [])
        Just (NoteTrack ptrack is_relative) -> case edit_mode of
            Cmd.NoEdit -> ([], [])
            Cmd.RawEdit -> ([NoteTrack.cmd_raw_edit scale_id], [])
            Cmd.ValEdit
                | is_relative -> ([], [NoteTrack.cmd_val_edit_relative ptrack])
                | otherwise -> ([NoteTrack.cmd_val_edit ptrack scale_id], [])
            Cmd.MethodEdit -> ([], [NoteTrack.cmd_method_edit ptrack])
        Just (PitchTrack is_relative) -> case edit_mode of
            Cmd.NoEdit -> ([], [])
            Cmd.RawEdit -> ([PitchTrack.cmd_raw_edit scale_id], [])
            Cmd.ValEdit
                | is_relative -> ([], [PitchTrack.cmd_val_edit_relative])
                | otherwise -> ([PitchTrack.cmd_val_edit scale_id], [])
            Cmd.MethodEdit -> ([], [PitchTrack.cmd_method_edit])
        Just (ControlTrack _) -> case edit_mode of
            Cmd.NoEdit -> ([], [])
            Cmd.RawEdit -> ([], [ControlTrack.cmd_raw_edit])
            Cmd.ValEdit -> ([], [ControlTrack.cmd_val_edit])
            Cmd.MethodEdit -> ([], [ControlTrack.cmd_method_edit])
    (cmds2, warns) = case maybe_track_type of
        Just (NoteTrack ptrack _) ->
            let (cmd_map, keymap_warns) = NoteTrackKeymap.make_keymap ptrack
            in (cmds1 ++ [Keymap.make_cmd cmd_map], keymap_warns)
        _ -> (cmds1, [])

    add_with_note (note_cmds, cmds) = with_note note_cmds : cmds
    with_note cmds = NoteEntry.cmds_with_note kbd_entry (universal ++ cmds)
    universal = PitchTrack.cmd_record_note_status scale_id : midi_thru
    edit_mode = ctx_edit_mode context
    kbd_entry = ctx_kbd_entry context
    midi_thru =
        maybe [] (\inst -> [MidiThru.cmd_midi_thru scale_id inst]) maybe_inst
    (maybe_track_type, maybe_inst, scale_id) = get_defaults context

get_defaults :: CmdContext
    -> (Maybe TrackType, Maybe Score.Instrument, Pitch.ScaleId)
    -- ^ (focused_track_type, inst_to_use, scale_id_to_use)
get_defaults context = (maybe_track_type, score_inst, scale_id)
    where
    (maybe_track_type, track_inst, scale_id) = get_track_info
        (ctx_project_scale context) (ctx_track_tree context)
        (ctx_focused_tracknum context)
    score_inst = track_inst `mplus` ctx_default_inst context

-- | Find the type of a track and the instrument and scale in scope.
--
-- First search up the call stack, since this will yield a track that has scope
-- over the current one.  Otherwise search down, which may yield multiple
-- possibilities, but in many cases will find an appropriate one.
--
-- TODO: if this leads to weird guesses, maybe return Nothing if there are
-- two or more matches?
get_track_info :: Pitch.ScaleId -> State.TrackTree -> Maybe TrackNum
    -> (Maybe TrackType, Maybe Score.Instrument, Pitch.ScaleId)
get_track_info proj_scale _ Nothing = (Nothing, Nothing, proj_scale)
get_track_info proj_scale track_tree (Just tracknum) =
    case Default.paths_of track_tree tracknum of
        Nothing -> (Nothing, Nothing, proj_scale)
        Just (track, parents, children) ->
            let inst = find_inst (track : parents ++ children)
                -- Only search parents for scale, since that would make one
                -- set scale override the project scale for higher level
                -- tracks.
                scale_id = maybe proj_scale id $ find_scale (track : parents)
            in (Just (track_type scale_id track parents), inst, scale_id)
    where
    find_inst = msum . map inst_of
    inst_of = Default.title_to_instrument . State.track_title
    find_scale = msum . map (Default.title_to_scale . State.track_title)

-- | Describe the type of a single track.  This is used to figure out what set
-- of cmds should apply to a given track.
data TrackType =
    -- | NoteTrack is paired with the first pitch track found for it.
    -- The Bool is True if this is a relative control track, or in the case of
    -- NoteTrack, the corresponding PitchTrack is relative.
    NoteTrack NoteTrack.PitchTrack Bool | PitchTrack Bool | ControlTrack Bool
    deriving (Show, Eq)

track_type :: Pitch.ScaleId -> State.TrackInfo -> [State.TrackInfo] -> TrackType
track_type scale_id (State.TrackInfo title _ tracknum) parents
    | Default.is_inst_track title = NoteTrack pitch_track pitch_rel
    | Default.is_pitch_track title = PitchTrack is_rel
    | otherwise = ControlTrack is_rel
    where
    (pitch_track, pitch_rel) = case List.find is_pitch parents of
        Nothing ->
            -- Assume new pitch tracks should be absolute.
            (NoteTrack.CreateTrack
                tracknum (Default.scale_to_title scale_id) (tracknum+1),
            False)
        Just ptrack -> (NoteTrack.ExistingTrack (State.track_tracknum ptrack),
            Default.is_relative_track (State.track_title ptrack))
    is_pitch = Default.is_pitch_track . State.track_title
    is_rel = Default.is_relative_track title


-- ** compile

default_schema_deriver :: SchemaDeriver Derive.EventDeriver
default_schema_deriver block_id =
    fmap compile (State.get_unmuted_track_tree block_id)

default_schema_signal_deriver ::
    SchemaDeriver (Derive.SignalDeriver Signal.Display)
default_schema_signal_deriver block_id =
    fmap compile_to_signals (State.get_track_tree block_id)

-- | Transform a deriver skeleton into a real deriver.  The deriver may throw
-- if the skeleton was malformed.
compile :: State.TrackTree -> Derive.EventDeriver
compile tree = Derive.with_msg "compile" $ do
    -- Support for the 'Derive.add_track_warp' hack.  If a block doesn't have
    -- a tempo track, 'd_tempo' -> 'd_warp' never gets called, so I have to
    -- start the warp here.
    unless (has_tempo_track tree) Derive.start_new_warp
    sub_compile tree

has_tempo_track :: State.TrackTree -> Bool
has_tempo_track = any $ \(Tree.Node track subs) ->
    Default.is_tempo_track (State.track_title track) || has_tempo_track subs

sub_compile :: State.TrackTree -> Derive.EventDeriver
sub_compile tree = Derive.d_merge =<< mapM with_track tree
    where
    with_track tree@(Tree.Node track _) =
        Derive.with_stack_track (State.track_id track) (_compile tree)

_compile :: Tree.Tree State.TrackInfo -> Derive.EventDeriver
_compile (Tree.Node track@(State.TrackInfo title track_id _) subs)
    | Default.is_inst_track title = if not (null subs)
        then Derive.throw $ "inst track " ++ show track ++ " has sub tracks "
            ++ show subs
        else Derive.with_track_warp Note.d_note_track track_id
    | otherwise = do
        when (null subs) $
            Log.warn $ "control " ++ show track ++ " has no sub tracks"
        Derive.with_stack_track track_id $
            compile_control title track_id (sub_compile subs)

compile_control :: String -> TrackId
    -> Derive.EventDeriver -> Derive.EventDeriver
compile_control title track_id subderiver
    | Default.is_tempo_track title = do
        -- A tempo track is derived like other signals, but gets special
        -- treatment because of the track warps chicanery.
        events <- Derive.with_track_warp_tempo Control.d_control_track track_id
        Derive.d_tempo track_id (Control.d_tempo_signal events) subderiver
    | otherwise = do
        events <- Derive.with_track_warp Control.d_control_track track_id
        -- TODO default to inst scale if none is given
        let deriver = case Default.parse_control_title title of
                (Nothing, Left cont) ->
                    Control.d_control cont (Control.d_signal events)
                (Just c_op, Left cont) ->
                    Control.d_relative cont c_op (Control.d_signal events)
                (Nothing, Right scale_id) ->
                    Control.d_pitch (Control.d_pitch_signal scale_id events)
                (Just c_op, Right scale_id) ->
                    Control.d_relative_pitch
                        c_op (Control.d_relative_pitch_signal scale_id events)
        deriver subderiver


-- | Compile a Skeleton to its SignalDeriver.  The SignalDeriver is like the
-- main Deriver except that it derives down to track signals instead of events.
-- While the events go on to performance, the track signals go to the UI so
-- it can draw pretty graphs.
--
-- TODO Think about this some more in light of more complicated derivers.  It
-- seems annoying to have to make a whole separate signal deriver.  Getting the
-- signals from the track could be more hardcoded and less work when writing
-- a new schema.
compile_to_signals :: State.TrackTree -> (Derive.SignalDeriver Signal.Display)
compile_to_signals tree = Derive.with_msg "compile_to_signals" $
    Derive.d_signal_merge =<< mapM _compile_to_signals tree

_compile_to_signals :: Tree.Tree State.TrackInfo
    -> Derive.SignalDeriver Signal.Display
_compile_to_signals (Tree.Node (State.TrackInfo title track_id _) subs)
    | Default.is_inst_track title = return []
    | otherwise = do
        -- Note no special treatment for tempo, since signal output shouldn't
        -- be warped.
        track_sigs <- signal_control title track_id
        rest_sigs <- Derive.d_signal_merge =<< mapM _compile_to_signals subs
        return (track_sigs : rest_sigs)

signal_control :: (Monad m) => String -> TrackId
    -> Derive.DeriveT m (TrackId, Signal.Display)
signal_control title track_id = do
    events <- Derive.with_track_warp Control.d_control_track track_id
    sig <- case Default.parse_control_title title of
        (_, Left _) -> Control.d_display_signal events
        (Nothing, Right scale_id) -> Control.d_display_pitch scale_id events
        (Just _, Right scale_id) ->
            Control.d_display_relative_pitch scale_id events
    return (track_id, sig)

-- * parser

-- | A parser figures out a skeleton based on track titles and position.
--
-- Tracks starting with '>' are instrument tracks, the rest are control tracks.
-- The control tracks scope over the next instrument track to the left.
-- A track titled \"tempo\" scopes over all tracks to its right.
--
-- This should take arguments to apply to instrument and control tracks.
--
-- TODO do something special with embedded rulers and dividers
default_parser :: [State.TrackInfo] -> Skeleton.Skeleton
default_parser = Skeleton.make
    . Util.Tree.edges . map (fmap State.track_tracknum) . parse_to_tree

-- | [c0 tempo1 i1 c1 tempo2 c2 i2 c3] ->
-- [c0, tempo1 (c1 . i1), tempo2 (c2 . c3 . i2)]
parse_to_tree :: [State.TrackInfo] -> Tree.Forest State.TrackInfo
parse_to_tree tracks = concatMap parse_tempo_group $
    Seq.split_with (Default.is_tempo_track . State.track_title) tracks

parse_tempo_group :: [State.TrackInfo] -> Tree.Forest State.TrackInfo
parse_tempo_group [] = []
parse_tempo_group (track:tracks)
    | Default.is_tempo_track (State.track_title track) =
        [Tree.Node track (parse_inst_groups tracks)]
    | otherwise = parse_inst_groups (track:tracks)

-- | [c1 i1 c2 c3] -> c1 . c3 . c2 . i1
parse_inst_groups :: [State.TrackInfo] -> Tree.Forest State.TrackInfo
parse_inst_groups tracks = case inst_groups of
        [] -> []
        global : rest -> descend (concatMap parse_inst_group rest) global
    where
    inst_groups = Seq.split_with
        (Default.is_inst_track . State.track_title) tracks

parse_inst_group :: [State.TrackInfo] -> Tree.Forest State.TrackInfo
parse_inst_group [] = []
parse_inst_group (track:tracks) = descend [Tree.Node track []] (reverse tracks)

descend :: Tree.Forest a -> [a] -> Tree.Forest a
descend bottom [] = bottom
descend bottom (track:tracks) = [Tree.Node track (descend bottom tracks)]
