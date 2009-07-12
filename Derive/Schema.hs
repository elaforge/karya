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
    as-is unless I decide global after all.
-}
module Derive.Schema (
    -- Re-export schema types from Cmd, to pretend they're defined here.
    -- * types
    Schema(..), SchemaDeriver, Parser, Skeleton(..), merge
    , Track(..), CmdContext(..), SchemaMap

    -- ** default parser
    , TrackType(..), track_type_of

    -- Just used by testing
    , default_parser
    , compile_to_signals

    -- * lookup
    , lookup_deriver, get_signal_deriver, get_cmds, get_skeleton
    , skeleton_instruments
    -- * util
    , block_tracks
    , cmd_context
    , instrument_to_title, title_to_instrument
) where
-- import qualified Control.Arrow as Arrow
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.List as List

import qualified Util.Seq as Seq
import qualified Ui.Block as Block
import qualified Ui.Track as Track
import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd
import Cmd.Cmd (Schema(..), SchemaDeriver, Parser, Skeleton(..), merge,
    Track(..), CmdContext(..), SchemaMap)
import qualified Cmd.NoteTrack as NoteTrack
import qualified Cmd.ControllerTrack as ControllerTrack

import qualified Derive.Controller as Controller
import qualified Derive.Derive as Derive
import qualified Derive.Note as Note
import qualified Derive.Scale as Scale
import qualified Derive.Score as Score
import qualified Derive.Twelve as Twelve

import qualified Perform.Signal as Signal
import qualified Perform.Midi.Instrument as Instrument

import qualified App.Config as Config


hardcoded_schemas :: SchemaMap
hardcoded_schemas = Map.fromList [(Config.schema, default_schema)]

-- TODO Scale should be stored with the instrument, and pb_range should be
-- looked up.
hardcoded_scale = Twelve.scale
dummy_pb_range = (-2, 2)

merge_schemas :: SchemaMap -> SchemaMap -> SchemaMap
merge_schemas map1 map2 = Map.union map2 map1


-- * look things up in the schema db

-- TODO get_deriver and get_skeleton could also take SchemaId -> [Track]
-- instead of Block... is it worth it?
-- type SchemaContext = (Block.SchemaId, [Track], SchemaMap)

-- | Create a LookupDeriver function.
lookup_deriver :: SchemaMap -> State.State -> Derive.LookupDeriver
lookup_deriver schema_map ui_state block_id = State.eval ui_state $ do
    block <- State.get_block block_id
    schema <- State.lookup_id (Block.block_schema block)
        (merge_schemas hardcoded_schemas schema_map)
    schema_deriver schema block

-- | Get the signal deriver for the given block.  Unlike the event deriver,
-- the signal deriver is only ever local to one block, so it doesn't need
-- a lookup mechanism.
get_signal_deriver :: (State.UiStateMonad m) => SchemaMap -> Block.Block
    -> m Derive.SignalDeriver
get_signal_deriver schema_map block = do
    schema <- State.lookup_id (Block.block_schema block)
        (merge_schemas hardcoded_schemas schema_map)
    state <- State.get
    State.eval_rethrow "get signal deriver" state
        (schema_signal_deriver schema block)

-- | A block's Schema also implies a set of Cmds, possibly based on the
-- focused track.  This is so that e.g. control tracks use control editing keys
-- and note tracks use note entry keys, and they can set up the midi thru
-- mapping appropriately.
get_cmds :: SchemaMap -> CmdContext -> Block.SchemaId -> [Track] -> [Cmd.Cmd]
get_cmds schema_map context schema_id tracks =
    case Map.lookup schema_id (merge_schemas hardcoded_schemas schema_map) of
        Nothing -> []
        Just schema -> schema_cmds schema context tracks

get_skeleton :: (State.UiStateMonad m) => SchemaMap -> Block.Block -> m Skeleton
get_skeleton schema_map block = do
    schema <- State.lookup_id (Block.block_schema block)
        (merge_schemas hardcoded_schemas schema_map)
    fmap (schema_parser schema) (block_tracks block)


block_tracks :: (State.UiStateMonad m) => Block.Block -> m [Track]
block_tracks block = do
    let tracks = Block.block_tracks block
    names <- mapM track_name tracks
    return [Track name track num
        | (num, (name, track)) <- zip [0..] (zip names tracks)]

track_name (Block.TId tid _) =
    fmap (Just . Track.track_title) (State.get_track tid)
track_name _ = return Nothing

-- | Constructor for CmdContext.
cmd_context :: Instrument.Config -> Maybe Cmd.EditMode -> Bool
    -> Maybe Block.TrackNum -> CmdContext
cmd_context midi_config edit_mode kbd_entry focused_tracknum =
    CmdContext default_addr inst_addr edit_mode kbd_entry focused_tracknum
    where
    default_addr = Instrument.config_default_addr midi_config
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
default_schema = Schema
    (default_schema_deriver default_parser)
    (default_schema_signal_deriver default_parser)
    default_parser
    (default_cmds default_parser)

default_cmds :: Parser -> CmdContext -> [Track] -> [Cmd.Cmd]
default_cmds parser context tracks = case track_type of
        Just (NoteTrack _) -> midi_thru ++ kbd_entry ++ inst_edit_cmds
        Just (ControllerTrack _) -> midi_thru ++ cont_edit_cmds
        Nothing -> []
    where
    midi_thru = with_info NoteTrack.cmd_midi_thru

    -- It might be cleaner for kbd_entry to intercept keys and re-emit
    -- them as midi msgs, but that would require cmds to be able to do that,
    -- which is just more complication.  And this way I can disable kbd_entry
    -- when I'm in some other edit mode.
    kbd_entry = case (ctx_kbd_entry context, ctx_edit_mode context) of
        (False, _) -> []
        (_, Nothing) -> with_info NoteTrack.cmd_kbd_note_thru
        (_, Just Cmd.ValEdit) -> with_info NoteTrack.cmd_kbd_note_entry
        _ -> [] -- Don't steal keys from other edit modes.
        -- RawEdit in kbd entry mode doesn't really make sense, since they both
        -- want the entire keyboard.  So let it fall through to non-kbd raw
        -- edit.

    inst_edit_cmds = case ctx_edit_mode context of
        Nothing -> []
        Just Cmd.RawEdit -> [NoteTrack.cmd_raw_edit]
        Just Cmd.ValEdit -> [NoteTrack.cmd_midi_entry hardcoded_scale]
        Just Cmd.MethodEdit -> [NoteTrack.cmd_method_edit]

    cont_edit_cmds = case ctx_edit_mode context of
        Nothing -> []
        -- TODO should be cmd_method_edit
        Just Cmd.MethodEdit -> [ControllerTrack.cmd_raw_edit]
        Just _ -> [ControllerTrack.cmd_raw_edit]

    track_type = case ctx_focused_tracknum context of
        Nothing -> Nothing
        Just tracknum -> track_type_of tracknum (parser tracks)

    -- TODO lookup inst (for pb_range) and inst scale
    maybe_addr = msum $ map (ctx_inst_addr context) insts
        ++ [ctx_default_addr context]
    with_info cmd = case maybe_addr of
        Nothing -> []
        Just addr -> [cmd (NoteTrack.Info hardcoded_scale addr dummy_pb_range)]
    -- TODO use a function to find out the instrument at a given (tracknum, pos)
    -- which looks for a governing instrument track and then the note track
    -- title.  It'll need the skeleton.
    insts = case track_type of
        Just (NoteTrack inst) -> [inst]
        Just (ControllerTrack insts) -> insts
        Nothing -> []

-- | Like Skeleton, but describe the type of a single track.
data TrackType =
    -- | This is just so that I can easily figure out what instruments a block
    -- mentions, e.g. in LanguageCmds, which uses it to deallocate an old
    -- instrument.  This is kinda sketchy anyway since this doesn't account for
    -- the instrument track.
    NoteTrack Score.Instrument
    | ControllerTrack [Score.Instrument]
    deriving (Eq, Show)

track_type_of :: Block.TrackNum -> Skeleton -> Maybe TrackType
track_type_of tracknum skel = case skel of
        Controller tracks maybe_sub -> case find tracks of
            Nothing -> type_of =<< maybe_sub
            Just _ -> Just $ ControllerTrack (maybe [] instruments_of maybe_sub)
        Instrument inst track
            | track_tracknum track == tracknum -> Just (NoteTrack inst)
            | otherwise -> Nothing
        Merge skels -> case Maybe.catMaybes (map type_of skels) of
            [] -> Nothing
            -- TODO this seems arbitrary... how do I justify this?
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
-- left.  A track titled \"tempo\" scopes over all tracks to its right.
--
-- This should take arguments to apply to instrument and control tracks.
--
-- A schema is split into two parts: parse the tracks into a skeleton, and
-- then convert the skeleton into a deriver.  The intermediate data structure
-- allows me to compose schemas out of smaller parts, as well as inspect the
-- skeleton for e.g. instrument tracks named, or to create a view layout.
default_schema_deriver :: Parser -> SchemaDeriver Derive.EventDeriver
default_schema_deriver parser block =
    fmap (compile_skeleton . parser) (block_tracks block)

default_schema_signal_deriver :: Parser -> SchemaDeriver Derive.SignalDeriver
default_schema_signal_deriver parser block =
    fmap (compile_to_signals . parser) (block_tracks block)

-- | Get the Instruments from a parsed Skeleton.
skeleton_instruments :: Skeleton -> [Score.Instrument]
skeleton_instruments skel = case skel of
    Controller _ (Just subskel) -> skeleton_instruments subskel
    Controller _ _ -> []
    Instrument inst _ -> [inst]
    Merge subs -> concatMap skeleton_instruments subs

-- ** compile skeleton

-- | Transform a deriver skeleton into a real deriver.  The deriver may throw
-- if the skeleton was malformed.
compile_skeleton :: Skeleton -> Derive.EventDeriver
compile_skeleton skel = case skel of
    Controller ctracks Nothing ->
        -- TODO or it might be more friendly to just ignore them
        Derive.throw $ "orphaned controller tracks: " ++ show ctracks
    Controller ctracks (Just subskel) ->
        compile_controllers ctracks subskel
    Instrument inst (Track { track_id = Block.TId track_id _ }) ->
        Derive.with_instrument inst $ Derive.with_track_warp
            (Note.d_note_track Scale.scale_map
                (Note.scale_parser hardcoded_scale))
            track_id
    Instrument _ track ->
        Derive.throw $
            "instrument track not an event track, parser is confused: "
            ++ show track
    Merge subs -> Derive.d_merge =<< mapM compile_skeleton subs

-- | Generate the EventDeriver for a Controller Skeleton.
compile_controllers tracks subskel =
    foldr track_controller (compile_skeleton subskel) (event_tracks tracks)
event_tracks tracks =
    [(title, track_id) | Track (Just title) (Block.TId track_id _) _ <- tracks]

track_controller :: (Monad m) => (String, Track.TrackId)
    -> Derive.DeriveT m [Score.Event] -> Derive.DeriveT m [Score.Event]
track_controller (name, track_id) deriver
    | name == tempo_track_title = do
        -- A tempo track is derived like other signals, but gets special
        -- treatment because of the track warps chicanery.
        sig_events <- Derive.with_track_warp_tempo
            Controller.d_controller_track track_id
        Derive.d_tempo track_id (Controller.d_signal sig_events) deriver
    | otherwise = do
        sig_events <- Derive.with_track_warp
            Controller.d_controller_track track_id
        Controller.d_controller (Score.Controller name)
            (Controller.d_signal sig_events) deriver

-- *** compile to signals

-- | Compile a Skeleton to its SignalDeriver.  The SignalDeriver is like the
-- main Deriver except that it derives down to track signals instead of events.
-- While the events go on to performance, the track signals go to the UI so
-- it can draw pretty graphs.
--
-- TODO Think about this some more in light of more complicated derivers.  It
-- seems annoying to have to make a whole separate signal deriver.  Getting the
-- signals from the track could be more hardcoded and less work when writing
-- a new schema.
compile_to_signals :: Skeleton -> Derive.SignalDeriver
compile_to_signals skel = case skel of
    Controller ctracks maybe_subskel -> compile_signals ctracks maybe_subskel
    Instrument _ _ -> return []
    Merge subs -> Derive.d_signal_merge =<< mapM compile_to_signals subs

compile_signals :: [Track] -> Maybe Skeleton -> Derive.SignalDeriver
compile_signals tracks maybe_subskel = do
    track_sigs <- mapM signal_controller (event_tracks tracks)
    rest_sigs <- maybe (return []) compile_to_signals maybe_subskel
    return (track_sigs ++ rest_sigs)

signal_controller :: (Monad m) => (String, Track.TrackId)
    -> Derive.DeriveT m (Track.TrackId, Signal.Signal)
signal_controller (_title, track_id) = do
    sig <- Controller.d_signal
        =<< Derive.with_track_warp Controller.d_controller_track track_id
    return (track_id, sig)

-- | This is the track name that turns a track into a tempo track.
tempo_track_title :: String
tempo_track_title = "tempo"

-- | Convert a track title into its instrument.  This could be per-schema, but
-- I'm going to hardcode it for now and assume all schemas will do the same
-- thing.
--
-- TODO also hardcoded in parse_inst_groups and parse_inst_group
title_to_instrument :: String -> Maybe Score.Instrument
title_to_instrument ('>':inst_name) =
    Just $ Score.Instrument (Seq.strip inst_name)
title_to_instrument _ = Nothing

-- | Convert from an instrument to the title of its instrument track.
instrument_to_title :: Score.Instrument -> String
instrument_to_title (Score.Instrument inst) = '>' : inst

-- * parser

-- | A parser turns [Track] into a Skeleton, which describes which tracks
-- have scope over which other tracks.  As part of this, it also decides which
-- tracks are instrument tracks, and what Instrument they have.
--
-- TODO handle embedded rulers and dividers
default_parser :: Parser
default_parser tracks = merge $
    map parse_tempo_group
        -- The 0th track should be the ruler track, which I ignore.
        (split (title_matches (==tempo_track_title)) (drop 1 tracks))


parse_tempo_group tracks = case tracks of
    tempo@(Track { track_title = Just title }) : rest
        | title == tempo_track_title ->
            Controller [tempo] (Just (parse_inst_groups rest))
        | otherwise -> parse_inst_groups tracks
    _ -> parse_inst_groups tracks

parse_inst_groups :: [Track] -> Skeleton
parse_inst_groups tracks = merge $
    map parse_inst_group (split (title_matches (">" `List.isPrefixOf`)) tracks)

parse_inst_group tracks = case tracks of
    track@(Track { track_title = Just ('>':inst_name) }) : rest ->
        let inst = Score.Instrument (Seq.strip inst_name) in case rest of
            [] -> Instrument inst track
            _ -> Controller rest (Just (Instrument inst track))
    _ -> Controller tracks Nothing

-- ** util

maybe_matches f m = maybe False id (fmap f m)
title_matches f = maybe_matches f . track_title
split f xs = case Seq.split_with f xs of
    [] : rest -> rest
    grps -> grps
