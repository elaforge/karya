{- | A schema is a transformation from a block to a deriver.  It may intuit the
deriver solely from the structure of the block, or it may ignore the block
entirely if it has a custom deriver.

It's done this way so that each block doesn't need to have its own deriver
ID hardcoded into it.

The SchemaId -> Schema mapping looks in a hardcoded list, a custom list passed
via static configuration, and a dynamically loaded list.  The assumed usage
is that you experiment with new Derivers and make minor changes via dynamic
loading, and later incorporate them into the static configuration.
-}
module Derive.Schema where
import Control.Monad
import qualified Control.Arrow as Arrow
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.List as List

import qualified Util.Seq as Seq
import qualified Ui.Block as Block
import qualified Ui.Track as Track
import qualified Ui.State as State

import qualified Derive.Score as Score
import qualified Derive.Controller as Controller
import qualified Derive.Derive as Derive
import qualified Derive.Twelve as Twelve


-- | A Schema generates a Deriver from a given Block.
type Schema ui d = Block.Block -> ui (Derive.DeriveT d [Score.Event])

get_deriver :: (State.UiStateMonad ui, Monad d) =>
    Block.Block -> ui (Derive.DeriveT d [Score.Event])
get_deriver block = do
    (schema, _) <- State.lookup_id (Block.block_schema block) hardcoded_schemas
    schema block

get_skeleton :: (State.UiStateMonad ui) => Block.Block -> ui Skeleton
get_skeleton block = do
    (_, parser) <- State.lookup_id (Block.block_schema block) hardcoded_schemas
        -- Haskell forces me to pick a random monad for Schema, even though
        -- I don't use the value.  TODO why is this?
        :: (State.UiStateMonad ui) => ui (Schema ui Maybe, Parser)
    fmap parser (block_tracks block)

hardcoded_schemas :: (State.UiStateMonad ui, Monad d) =>
    Map.Map Block.SchemaId (Schema ui d, Parser)
hardcoded_schemas = Map.fromList $ map (Arrow.first Block.SchemaId)
    [ ("default", (default_schema, default_parse))
    ]


-- * default schema

-- | The default schema is supposed to be simple but useful, and rather
-- trackerlike.
--
-- Tracks starting with '>' are instrument tracks, the rest are control tracks.
-- The control tracks scope over the next instrument track to the left.
-- A track titled "tempo" scopes over all tracks to its right.
--
-- This should take arguments to apply to instrument and control tracks.
--
-- A schema is split into two parts: parse the tracks into a skeleton, and
-- then convert the skeleton into a deriver.  The intermediate data structure
-- allows me to compose schemas out of smaller parts, as well as inspect the
-- skeleton for e.g. instrument tracks named, or to create a view layout.
default_schema :: (State.UiStateMonad ui, Monad d) => Schema ui d
default_schema block = do
    tracks <- block_tracks block
    return $ compile_skeleton (default_parse tracks)

type Compiler m = Skeleton -> Derive.DeriveT m [Score.Event]

-- | Transform a deriver skeleton into a real deriver.  The deriver may throw
-- if the skeleton was malformed.
compile_skeleton :: Monad m => Skeleton -> Derive.DeriveT m [Score.Event]
compile_skeleton skel = case skel of
    Controller ctracks Nothing ->
        Derive.throw $ "orphaned controller tracks: " ++ show ctracks
    Controller ctracks (Just sub) ->
        compile_controllers ctracks sub
    Instrument inst (Track _ (Block.TId track_id _)) -> 
        Twelve.twelve =<< Derive.d_instrument inst =<< Derive.d_track track_id
    Instrument _ track ->
        Derive.throw $ "instrument track not an event track: " ++ show track
    Merge subs -> Derive.d_merge =<< mapM compile_skeleton subs

-- | Get the Instruments from a parsed Skeleton.
skeleton_instruments :: Skeleton -> [Score.Instrument]
skeleton_instruments skel = case skel of
    Controller _ (Just sub) -> skeleton_instruments sub
    Controller _ _ -> []
    Instrument inst _ -> [inst]
    Merge subs -> concatMap skeleton_instruments subs

-- | Generate the Deriver for a Controller Skeleton.
compile_controllers tracks sub =
    foldr track_controller (compile_skeleton sub) (event_tracks tracks)
event_tracks tracks =
    [(title, track_id) | Track (Just title) (Block.TId track_id _) <- tracks]

track_controller :: (Monad m) => (String, Track.TrackId)
    -> Derive.DeriveT m [Score.Event] -> Derive.DeriveT m [Score.Event]
track_controller (name, signal_track_id) =
    Controller.d_controller (Score.Controller name)
        (Controller.d_signal =<< Derive.d_track signal_track_id)

block_tracks :: (State.UiStateMonad m) => Block.Block -> m [Track]
block_tracks block = do
    let tracks = map fst (Block.block_tracks block)
    names <- mapM track_name tracks
    return $ zipWith Track names tracks

track_name (Block.TId tid _) =
    fmap (Just . Track.track_title) (State.get_track tid)
track_name _ = return Nothing

-- * parser

type Parser = [Track] -> Skeleton

-- | A parser turns [Track] into a Skeleton, which describes which tracks
-- have scope over which other tracks.  As part of this, it also decides which
-- tracks are instrument tracks, and what Instrument they have.
default_parse :: Parser
default_parse tracks = merge $
    map parse_tempo_group (split (title_matches (=="tempo")) tracks)

data Skeleton =
    -- | A set of controller tracks have scope over a sub-skeleton.
    -- A controller with no "argument" track will have a Nothing sub.
    Controller [Track] (Maybe Skeleton)
    | Instrument Score.Instrument Track
    | Merge [Skeleton]
    deriving (Show)
merge [track] = track
merge tracks = Merge tracks

data Track = Track {
    track_title :: Maybe String
    , track_id :: Block.TracklikeId
    } deriving (Show)


parse_tempo_group tracks = case tracks of
    tempo@(Track { track_title = Just "tempo" }) : rest ->
        Controller [tempo] (Just (parse_inst_groups rest))
    _ -> parse_inst_groups tracks

parse_inst_groups :: [Track] -> Skeleton
parse_inst_groups tracks = merge $
    map parse_inst_group (split (title_matches (">" `List.isPrefixOf`)) tracks)

parse_inst_group tracks = case tracks of
    track@(Track { track_title = Just ('>':inst_name) }) : rest ->
        let inst = Score.Instrument (Seq.lstrip inst_name) in case rest of
            [] -> Instrument inst track
            _ -> Controller rest (Just (Instrument inst track))
    _ -> Controller tracks Nothing

-- ** util

maybe_matches f m = maybe False id (fmap f m)
title_matches f = maybe_matches f . track_title
split f xs = case Seq.split_with f xs of
    [] : rest -> rest
    grps -> grps
