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

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Edit as Edit
import qualified Cmd.NoteEntry as NoteEntry

import qualified Derive.Score as Score
import qualified Derive.Controller as Controller
import qualified Derive.Derive as Derive
import qualified Derive.Twelve as Twelve

import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.InstrumentDb as InstrumentDb


-- TODO convert the others to take SchemaId [Track]

-- | A Schema attaches a number of things to a Block.
data Schema ui d = Schema {
    schema_deriver :: SchemaDeriver ui d
    , schema_parser :: Parser
    , schema_cmds :: CmdContext -> [Track] -> [Cmd.Cmd]
    }

-- | A SchemaDeriver generates a Deriver from a given Block.
type SchemaDeriver ui d = Block.Block -> ui (Derive.DeriveT d [Score.Event])

-- | The parser generates a skeleton from the block, which is a description of
-- how the deriver has is structuring the tracks in the block.  Since the
-- deriver itself is just a function and can't be introspected into, others can
-- use this to determine e.g. what instruments and controls are in the block.
-- Although it's not necessarily complete (e.g. derivers can assign instruments
-- based on event text, not on track title) and a Schema doesn't even have to
-- have a parser, it's still useful to set certain defaults.
type Parser = [Track] -> Skeleton

get_deriver :: (State.UiStateMonad ui, Monad d) =>
    Block.Block -> ui (Derive.DeriveT d [Score.Event])
get_deriver block = do
    schema <- State.lookup_id (Block.block_schema block) hardcoded_schemas
    schema_deriver schema block

-- | A block's Schema also implies a set of Cmds, possibly based on the
-- focused track.  This is so that e.g. control tracks use control editing keys
-- and note tracks use note entry keys, and they can set up the midi thru
-- mapping appropriately.
get_cmds :: CmdContext -> Block.SchemaId -> [Track] -> [Cmd.Cmd]
get_cmds context schema_id tracks =
    maybe [] (\sch -> schema_cmds sch context tracks) schema
    where
    schema = Map.lookup schema_id hardcoded_schemas
        -- "Ambiguous constraint" lossage strikes again.
        :: Maybe (Schema (State.StateT Maybe) Maybe)

get_skeleton :: (State.UiStateMonad ui) => Block.Block -> ui Skeleton
get_skeleton block = do
    schema <- State.lookup_id (Block.block_schema block) hardcoded_schemas
        -- Haskell forces me to pick a random monad for Schema, even though
        -- I don't use the value.  TODO why is this?
        :: (State.UiStateMonad ui) => ui (Schema ui Maybe)
    fmap (schema_parser schema) (block_tracks block)

hardcoded_schemas :: (State.UiStateMonad ui, Monad d) =>
    Map.Map Block.SchemaId (Schema ui d)
hardcoded_schemas = Map.fromList $ map (Arrow.first Block.SchemaId)
    [ ("default", default_schema)
    ]

default_schema :: (State.UiStateMonad ui, Monad d) => Schema ui d
default_schema = Schema (default_schema_deriver default_parser)
    default_parser (default_cmds default_parser)


-- * default schema

-- The default schema is supposed to be simple but useful, and rather
-- trackerlike.

-- | Information needed to decide what cmds should apply.
data CmdContext = CmdContext {
    ctx_midi_config :: Instrument.Config
    , ctx_edit_mode :: Bool
    , ctx_focused_tracknum :: Maybe Block.TrackNum
    } deriving (Show)

default_cmds :: Parser -> CmdContext -> [Track] -> [Cmd.Cmd]
default_cmds parser context tracks = case track_type of
        Just (InstrumentTrack inst) -> midi_thru [inst] ++ inst_edit_cmds
        Just (ControllerTrack insts) ->
            midi_thru insts ++ cont_edit_cmds
        Nothing -> []
    where
    inst_edit_cmds = if (ctx_edit_mode context)
        then [NoteEntry.cmd_midi_entry, NoteEntry.cmd_kbd_note_entry]
        else []
    cont_edit_cmds = if (ctx_edit_mode context)
        then [NoteEntry.cmd_midi_entry, Edit.cmd_controller_entry]
        else []
    track_type = case (ctx_focused_tracknum context) of
        Nothing -> Nothing
        Just tracknum -> track_type_of tracknum (parser tracks)
    config = ctx_midi_config context
    midi_thru insts = case Maybe.catMaybes (map (get_addr config) insts) of
        [] -> []
        (addr:_) -> [Edit.cmd_midi_thru addr]

get_addr :: Instrument.Config -> Score.Instrument -> Maybe Instrument.Addr
get_addr config inst = maybe default_addr Just $ do
    m_inst <- InstrumentDb.lookup inst
    rlookup (Instrument.config_alloc config) m_inst
    where
    default_addr = Instrument.config_default_addr config
    rlookup fm k = lookup k (map (\(x, y) -> (y, x)) (Map.assocs fm))

data TrackType =
    InstrumentTrack Score.Instrument
    | ControllerTrack [Score.Instrument]
    deriving (Eq, Show)

track_type_of :: Block.TrackNum -> Skeleton -> Maybe TrackType
track_type_of tracknum skel = case skel of
        Controller tracks maybe_sub -> case find tracks of
            Nothing -> type_of =<< maybe_sub
            Just _ -> Just $ ControllerTrack (maybe [] instruments_of maybe_sub)
        Instrument inst track
            | track_tracknum track == tracknum -> Just (InstrumentTrack inst)
            | otherwise -> Nothing
        Merge skels -> case Maybe.catMaybes (map type_of skels) of
            [] -> Nothing
            (typ:_) -> Just typ
    where
    type_of = track_type_of tracknum
    has_tracknum track = track_tracknum track == tracknum
    find = List.find has_tracknum

instruments_of (Controller _ maybe_sub) = maybe [] instruments_of maybe_sub
instruments_of (Instrument inst _) = [inst]
instruments_of (Merge subs) = concatMap instruments_of subs


-- | Tracks starting with '>' are instrument tracks, the rest are control
-- tracks.  The control tracks scope over the next instrument track to the
-- left.  A track titled "tempo" scopes over all tracks to its right.
--
-- This should take arguments to apply to instrument and control tracks.
--
-- A schema is split into two parts: parse the tracks into a skeleton, and
-- then convert the skeleton into a deriver.  The intermediate data structure
-- allows me to compose schemas out of smaller parts, as well as inspect the
-- skeleton for e.g. instrument tracks named, or to create a view layout.
default_schema_deriver :: (State.UiStateMonad ui, Monad d) =>
    Parser -> SchemaDeriver ui d
default_schema_deriver parser block =
    fmap (compile_skeleton . parser) (block_tracks block)

type Compiler m = Skeleton -> Derive.DeriveT m [Score.Event]

-- | Transform a deriver skeleton into a real deriver.  The deriver may throw
-- if the skeleton was malformed.
compile_skeleton :: Monad m => Skeleton -> Derive.DeriveT m [Score.Event]
compile_skeleton skel = case skel of
    Controller ctracks Nothing ->
        Derive.throw $ "orphaned controller tracks: " ++ show ctracks
    Controller ctracks (Just sub) ->
        compile_controllers ctracks sub
    Instrument inst (Track { track_id = Block.TId track_id _ }) ->
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
    [(title, track_id) | Track (Just title) (Block.TId track_id _) _ <- tracks]

track_controller :: (Monad m) => (String, Track.TrackId)
    -> Derive.DeriveT m [Score.Event] -> Derive.DeriveT m [Score.Event]
track_controller (name, signal_track_id) =
    Controller.d_controller (Score.Controller name)
        (Controller.d_signal =<< Derive.d_track signal_track_id)

block_tracks :: (State.UiStateMonad m) => Block.Block -> m [Track]
block_tracks block = do
    let tracks = map fst (Block.block_tracks block)
    names <- mapM track_name tracks
    return [Track name track num
        | (num, (name, track)) <- zip [0..] (zip names tracks)]

track_name (Block.TId tid _) =
    fmap (Just . Track.track_title) (State.get_track tid)
track_name _ = return Nothing

-- * parser

-- | A parser turns [Track] into a Skeleton, which describes which tracks
-- have scope over which other tracks.  As part of this, it also decides which
-- tracks are instrument tracks, and what Instrument they have.
default_parser :: Parser
default_parser tracks = merge $
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
    , track_tracknum :: Block.TrackNum
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
