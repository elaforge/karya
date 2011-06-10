{-# LANGUAGE CPP #-}
{- | A schema is a transformation from a block to a deriver.  It may intuit
    the deriver solely from the structure of the block, or it may ignore the
    block entirely if it's specialized to one particular shape of block.

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
    Schema(..), SchemaDeriver, SchemaMap

    -- * lookup
    , lookup_deriver

    -- * derive
    , derive_tracks

    -- * parser
    , default_parser, note_bottom_parser

#ifdef TESTING
    , derive_tree
    , default_schema
#endif
) where
import qualified Data.Map as Map
import qualified Data.Tree as Tree

import Util.Control
import qualified Util.Seq as Seq
import qualified Util.Tree

import Ui
import qualified Ui.Block as Block
import qualified Ui.Skeleton as Skeleton
import qualified Ui.State as State
import qualified Ui.Track as Track

import Cmd.Cmd (Schema(..), SchemaDeriver, SchemaMap)
import qualified Derive.Control as Control
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Note as Note
import qualified Derive.TrackInfo as TrackInfo

import qualified Perform.Signal as Signal
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


-- * default schema

-- | The default schema is supposed to be simple but useful, and rather
-- trackerlike.
default_schema :: Schema
default_schema = Schema default_schema_deriver

-- ** default schema deriver

default_schema_deriver :: SchemaDeriver Derive.EventDeriver
default_schema_deriver block_id = do
    block <- State.get_block block_id
    info_tree <- State.get_track_tree block_id
    block_end <- State.block_event_end block_id
    let mutes_tree = State.track_tree_mutes
            (State.muted_tracknums block info_tree) info_tree
    tree <- State.events_tree block_end info_tree
    return $ derive_tree block_end (strip_mutes mutes_tree tree)

-- | Strip the events out of muted tracks.  If the tracks themselves were
-- stripped out it looks like there are orphans.  This way they are just tracks
-- that produce nothing.
--
-- It's ugly how the two trees are zipped up, but otherwise I have yet another
-- type for EventTreeMutes or hairy parameterization just for this one
-- function.
strip_mutes :: State.TrackTreeMutes -> State.EventsTree -> State.EventsTree
strip_mutes mutes tree = zipWith mute_node mutes tree
    where
    mute_node (Tree.Node (_, muted) ms) (Tree.Node track ts) =
        Tree.Node (if muted then mute track else track) (strip_mutes ms ts)
    mute track = track { State.tevents_events = Track.empty_events }

-- | Transform a deriver skeleton into a real deriver.
derive_tree :: ScoreTime -> State.EventsTree -> Derive.EventDeriver
derive_tree block_end tree = do
    -- d_tempo sets up some stuff that every block needs, so add one if a block
    -- doesn't have at least one top level tempo.
    tempo <- State.default_tempo . State.state_default <$> Derive.get_ui_state
    let with_default_tempo = if has_nontempo_track tree
            then Internal.d_tempo block_end Nothing (Signal.constant tempo)
            else id
    with_default_tempo (derive_tracks tree)

-- | Does this tree have any non-tempo tracks at the top level?
--
-- To ensure that every track is associated with a TrackWarp, I can't have
-- tracks that don't have a tempo track above them.  Those tracks implicitly
-- have an id warp, so this just makes that explicit.
has_nontempo_track :: State.EventsTree -> Bool
has_nontempo_track = any $ \(Tree.Node track _) ->
    not $ TrackInfo.is_tempo_track (State.tevents_title track)

-- | Derive an EventsTree.
derive_tracks :: State.EventsTree -> Derive.EventDeriver
derive_tracks tree = Derive.d_merge (map with_track tree)
    where
    with_track tree@(Tree.Node track _) =
        stack (State.tevents_track_id track) (derive_track tree)
    stack (Just track_id) = Internal.with_stack_track track_id
    stack Nothing = id

-- | Derive a single track node and any tracks below it.
derive_track :: State.EventsNode -> Derive.EventDeriver
derive_track node@(Tree.Node track subs)
    | TrackInfo.is_note_track (State.tevents_title track) =
        track_setup (Note.d_note_track node)
    | otherwise = Control.d_control_track node (derive_tracks subs)
    where
    track_setup = maybe id Internal.track_setup (State.tevents_track_id track)


-- * parser

-- | A parser figures out a skeleton based on track titles and position.
--
-- Tracks starting with '>' are instrument tracks, the rest are control tracks.
-- A track titled \"tempo\" scopes over all tracks to its right.
-- Below that, tracks scope left to right.
--
-- This should take arguments to apply to instrument and control tracks.
--
-- TODO do something special with embedded rulers and dividers
default_parser :: [State.TrackInfo] -> Skeleton.Skeleton
default_parser = make_skeleton . parse_to_tree False

-- | The note-bottom parser puts note tracks at the bottom:
--
-- @[tempo c1 i1 c2 i2] -> [tempo1 (c1 i1) (c2 i2)]@
--
-- This is useful when you don't want to invoke slicing.
note_bottom_parser :: [State.TrackInfo] -> Skeleton.Skeleton
note_bottom_parser = make_skeleton . parse_to_tree True

make_skeleton :: Tree.Forest State.TrackInfo -> Skeleton.Skeleton
make_skeleton =
    Skeleton.make . Util.Tree.edges . map (fmap State.track_tracknum)

-- | [c0 tempo1 i1 c1 tempo2 c2 i2 c3] ->
-- [c0, tempo1 (i1 c1), tempo2 (c2 c2 c3)]
parse_to_tree :: Bool -> [State.TrackInfo] -> Tree.Forest State.TrackInfo
parse_to_tree reversed tracks = concatMap parse groups
    where
    groups =
        Seq.split_with (TrackInfo.is_tempo_track . State.track_title) tracks
    parse = if reversed then reverse_tempo_group else parse_tempo_group

parse_tempo_group :: [State.TrackInfo] -> Tree.Forest State.TrackInfo
parse_tempo_group tracks = case groups of
        [] -> []
        non_note : ngroups ->
            descend non_note (concatMap parse_note_group ngroups)
    where
    groups = Seq.split_with (TrackInfo.is_note_track . State.track_title)
        tracks

reverse_tempo_group :: [State.TrackInfo] -> Tree.Forest State.TrackInfo
reverse_tempo_group [] = []
reverse_tempo_group (track:tracks) =
    [Tree.Node track $ concatMap parse_note_group (shift groups)]
    where
    groups = Seq.split_with (TrackInfo.is_note_track . State.track_title)
        tracks
    shift (group : (note : rest) : gs) = (group ++ [note]) : shift (rest : gs)
    shift gs = gs

parse_note_group :: [State.TrackInfo] -> Tree.Forest State.TrackInfo
parse_note_group tracks = descend tracks []

descend :: [a] -> Tree.Forest a -> Tree.Forest a
descend [] bottom = bottom
descend (track:tracks) bottom = [Tree.Node track (descend tracks bottom)]
