{-# LANGUAGE CPP #-}
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
-}
module Derive.Schema (
    -- Re-export schema types from Cmd, to pretend they're defined here.
    -- * types
    Schema(..), SchemaDeriver
    , CmdContext(..), ContextCmds, SchemaMap

    -- * lookup
    , lookup_deriver, get_cmds

    -- * parser
    , default_parser

    -- * util
    , cmd_context
    , get_track_info, TrackType(..)

#ifdef TESTING
    , get_defaults
    , derive_tree
    , default_schema
#endif
) where
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Tree as Tree

import qualified Util.Seq as Seq
import qualified Util.Tree

import Ui
import qualified Ui.Block as Block
import qualified Ui.Skeleton as Skeleton
import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd
import Cmd.Cmd (Schema(..), SchemaDeriver, CmdContext(..), ContextCmds,
    SchemaMap)
import qualified Cmd.ControlTrack as ControlTrack
import qualified Cmd.Info as Info
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
import qualified Derive.TrackInfo as TrackInfo

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
cmd_context :: State.State
    -> MidiDb.LookupMidiInstrument -> Cmd.EditMode -> Bool -> Maybe TrackNum
    -> State.TrackTree -> CmdContext
cmd_context ustate lookup_midi edit_mode kbd_entry focused_tracknum ttree =
    CmdContext (State.state_default_inst ustate) (State.state_project_scale ustate) inst_addr lookup_midi edit_mode
        kbd_entry focused_tracknum ttree
    where
    -- The thru cmd has to pick a single addr for a give inst, so let's just
    -- pick the lowest one.
    mconfig = State.state_midi_config ustate
    inst_map = Map.fromList [ (inst, minimum addrs)
        | (inst, addrs) <- Map.toList (Instrument.config_alloc mconfig)
        , not (null addrs) ]
    inst_addr = flip Map.lookup inst_map


-- * default schema

-- | The default schema is supposed to be simple but useful, and rather
-- trackerlike.
default_schema :: Schema
default_schema = Schema default_schema_deriver default_cmds

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
        Just (NoteTrack ptrack) -> case edit_mode of
            Cmd.NoEdit -> ([], [])
            Cmd.RawEdit -> ([NoteTrack.cmd_raw_edit maybe_inst scale_id], [])
            Cmd.ValEdit ->
                ([NoteTrack.cmd_val_edit maybe_inst ptrack], [])
            Cmd.MethodEdit ->
                ([], [NoteTrack.cmd_method_edit maybe_inst ptrack])
        Just PitchTrack -> case edit_mode of
            Cmd.NoEdit -> ([], [])
            Cmd.RawEdit -> ([PitchTrack.cmd_raw_edit scale_id], [])
            Cmd.ValEdit -> ([PitchTrack.cmd_val_edit scale_id], [])
            Cmd.MethodEdit -> ([], [PitchTrack.cmd_method_edit])
        Just ControlTrack -> case edit_mode of
            Cmd.NoEdit -> ([], [])
            Cmd.RawEdit -> ([], [ControlTrack.cmd_raw_edit scale_id])
            Cmd.ValEdit -> ([], [ControlTrack.cmd_val_edit])
            Cmd.MethodEdit -> ([], [ControlTrack.cmd_method_edit])
    (cmds2, warns) = case maybe_track_type of
        Just (NoteTrack ptrack) ->
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
    case Info.paths_of track_tree tracknum of
        Nothing -> (Nothing, Nothing, proj_scale)
        Just (track, parents, children) ->
            let inst = find_inst (track : parents ++ children)
                -- Only search parents for scale, since that would make one
                -- set scale override the project scale for higher level
                -- tracks.
                scale_id = maybe proj_scale id $ find_scale (track : parents)
            in (Just (track_type scale_id track parents (null children)),
                inst, scale_id)
    where
    find_inst = msum . map inst_of
    inst_of = TrackInfo.title_to_instrument . State.track_title
    -- TODO if the scale is relative, this won't get the octave from the
    -- enclosing scale
    find_scale = msum . map (TrackInfo.title_to_scale . State.track_title)

-- | Describe the type of a single track.  This is used to figure out what set
-- of cmds should apply to a given track.
data TrackType =
    -- | NoteTrack is paired with the first pitch track found for it.
    NoteTrack NoteTrack.PitchTrack | PitchTrack | ControlTrack
    deriving (Show, Eq)

track_type :: Pitch.ScaleId -> State.TrackInfo -> [State.TrackInfo]
    -> Bool -- ^ no_children
    -> TrackType
track_type scale_id (State.TrackInfo _ _ tracknum) parents True =
    NoteTrack pitch_track
    where
    pitch_track = case msum (map is_pitch parents) of
        Just pair -> pair
        Nothing -> NoteTrack.CreateTrack tracknum scale_id (tracknum+1)
    is_pitch track = case TrackInfo.parse_control (State.track_title track) of
        Right (TrackInfo.Pitch _ _) -> Just $
            NoteTrack.ExistingTrack (State.track_tracknum track) scale_id
        _ -> Nothing
track_type _ (State.TrackInfo title _ _) _ False =
    case TrackInfo.parse_control title of
        Right (TrackInfo.Control _ _) -> ControlTrack
        Right (TrackInfo.Pitch _ _) -> PitchTrack
        -- Default to a control track if it's unparseable.
        _ -> ControlTrack


-- ** default schema deriver

default_schema_deriver :: SchemaDeriver Derive.EventDeriver
default_schema_deriver block_id =
    fmap (derive_tree block_id) (State.get_track_tree_mutes block_id)

-- | Transform a deriver skeleton into a real deriver.
derive_tree :: BlockId -> State.TrackTreeMutes -> Derive.EventDeriver
derive_tree block_id tree = do
    -- d_tempo sets up some stuff that every block needs, so add one if a block
    -- doesn't have at least one top level tempo.
    let with_default_tempo = if has_nontempo_track tree
            then Derive.d_tempo block_id Nothing (Signal.constant 1) else id
    with_default_tempo (derive_tracks block_id tree)

-- | Does this tree have any non-tempo tracks at the top level?
--
-- To ensure that every track is associated with a TrackWarp, I can't have
-- tracks that don't have a tempo track above them.  Those tracks implicitly
-- have an id warp, so this just makes that explicit.
has_nontempo_track :: State.TrackTreeMutes -> Bool
has_nontempo_track = any $ \(Tree.Node (track, _) _) ->
    not $ TrackInfo.is_tempo_track (State.track_title track)

-- | Derive a set of \"top-level\" tracks and merge their results.
derive_tracks :: BlockId -> State.TrackTreeMutes -> Derive.EventDeriver
derive_tracks block_id tree = Derive.d_merge (map with_track tree)
    where
    with_track tree@(Tree.Node (track, _) _) =
        Derive.with_stack_track (State.track_id track)
            (derive_track block_id tree)

-- | Derive a single track and any tracks below it.
derive_track :: BlockId -> Tree.Tree (State.TrackInfo, Bool)
    -> Derive.EventDeriver
derive_track block_id (Tree.Node (_, True) subs)
    | null subs = return []
    | otherwise = derive_tracks block_id subs
derive_track block_id (Tree.Node (State.TrackInfo _ track_id _, False) subs)
    | null subs =
        Derive.track_setup track_id (Note.d_note_track block_id track_id)
    | otherwise = Control.d_control_track block_id track_id
        (derive_tracks block_id subs)


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
    Seq.split_with (TrackInfo.is_tempo_track . State.track_title) tracks

parse_tempo_group :: [State.TrackInfo] -> Tree.Forest State.TrackInfo
parse_tempo_group [] = []
parse_tempo_group (track:tracks)
    | TrackInfo.is_tempo_track (State.track_title track) =
        [Tree.Node track (parse_note_groups tracks)]
    | otherwise = parse_note_groups (track:tracks)

-- | [c1 i1 c2 c3] -> c1 . c3 . c2 . i1
parse_note_groups :: [State.TrackInfo] -> Tree.Forest State.TrackInfo
parse_note_groups tracks = case inst_groups of
        [] -> []
        global : rest -> descend (concatMap parse_note_group rest) global
    where
    inst_groups = Seq.split_with
        (TrackInfo.looks_like_note_track . State.track_title) tracks

parse_note_group :: [State.TrackInfo] -> Tree.Forest State.TrackInfo
parse_note_group [] = []
parse_note_group (track:tracks) = descend [Tree.Node track []] (reverse tracks)

descend :: Tree.Forest a -> [a] -> Tree.Forest a
descend bottom [] = bottom
descend bottom (track:tracks) = [Tree.Node track (descend bottom tracks)]
