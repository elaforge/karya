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
    Schema(..), SchemaDeriver, SchemaMap

    -- * lookup
    , lookup_deriver

    -- * parser
    , default_parser

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

import Cmd.Cmd (Schema(..), SchemaDeriver, SchemaMap)

import qualified Derive.Control as Control
import qualified Derive.Derive as Derive
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
default_schema_deriver block_id =
    fmap (derive_tree block_id) (State.get_track_tree_mutes block_id)

-- | Transform a deriver skeleton into a real deriver.
derive_tree :: BlockId -> State.TrackTreeMutes -> Derive.EventDeriver
derive_tree block_id tree = do
    -- d_tempo sets up some stuff that every block needs, so add one if a block
    -- doesn't have at least one top level tempo.
    tempo <- State.default_tempo . State.state_default <$> Derive.get_ui_state
    let with_default_tempo = if has_nontempo_track tree
            then Derive.d_tempo block_id Nothing (Signal.constant tempo) else id
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
