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
    Schema(..), SchemaDeriver, Parser, Skeleton(..), skel_merge
    , Track(..), CmdContext(..), SchemaMap

    -- ** default parser
    , TrackType(..), get_track_type

    -- Just used by testing
    , default_parser
    , compile_skeleton, compile_to_signals

    -- * query
    , is_tempo_track
    , is_pitch_track, scale_of_track, pitch_track_prefix
    , is_inst_track, inst_of_track

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
import Cmd.Cmd (Schema(..), SchemaDeriver, Parser, Skeleton(..), skel_merge,
    Track(..), track_title, CmdContext(..), SchemaMap)
import qualified Cmd.ControlTrack as ControlTrack
import qualified Cmd.NoteTrack as NoteTrack
import qualified Cmd.PitchTrack as PitchTrack
import qualified Cmd.KbdEntry as KbdEntry
import qualified Cmd.MidiThru as MidiThru

import qualified Derive.Controller as Controller
import qualified Derive.Derive as Derive
import qualified Derive.Note as Note
import qualified Derive.Score as Score

import qualified Perform.Signal as Signal
import qualified Perform.Pitch as Pitch
import qualified Perform.Midi.Instrument as Instrument

import qualified Instrument.MidiDb as MidiDb

import qualified App.Config as Config


hardcoded_schemas :: SchemaMap
hardcoded_schemas = Map.fromList [(Config.schema, default_schema)]

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
    let tracks = Block.block_tracklike_ids block
    names <- mapM track_name tracks
    return [Track name track num
        | (num, (name, track)) <- zip [0..] (zip names tracks)]

track_name (Block.TId tid _) =
    fmap (Just . Track.track_title) (State.get_track tid)
track_name _ = return Nothing

-- | Constructor for CmdContext.
cmd_context :: Instrument.Config -> MidiDb.LookupMidiInstrument
    -> Cmd.EditMode -> Bool -> Maybe Block.TrackNum -> CmdContext
cmd_context midi_config lookup_midi edit_mode kbd_entry focused_tracknum =
    CmdContext default_inst inst_addr lookup_midi edit_mode kbd_entry
        focused_tracknum
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
default_schema = Schema
    (default_schema_deriver default_parser)
    (default_schema_signal_deriver default_parser)
    default_parser
    (default_cmds default_parser)

-- ** cmds

-- | This decides what track-specific commands are in scope based on the
-- current focus and other data from the CmdContext.
default_cmds :: Parser -> CmdContext -> [Track] -> [Cmd.Cmd]
default_cmds parser context tracks = wrap $ case maybe_track_type of
        Nothing -> []
        Just (NoteTrack pitch_track) -> case edit_mode of
            Cmd.NoEdit -> []
            Cmd.RawEdit -> [with_note $ NoteTrack.cmd_raw_edit scale_id]
            Cmd.ValEdit ->
                [with_note $ NoteTrack.cmd_val_edit pitch_track scale_id]
            Cmd.MethodEdit -> [NoteTrack.cmd_method_edit pitch_track]
        Just PitchTrack -> case edit_mode of
            Cmd.NoEdit -> []
            Cmd.RawEdit -> [with_note $ PitchTrack.cmd_raw_edit scale_id]
            Cmd.ValEdit -> [with_note $ PitchTrack.cmd_val_edit scale_id]
            Cmd.MethodEdit -> [PitchTrack.cmd_method_edit]
        Just ControlTrack -> case edit_mode of
            Cmd.NoEdit -> []
            Cmd.RawEdit -> [ControlTrack.cmd_raw_edit]
            Cmd.ValEdit -> [ControlTrack.cmd_val_edit]
            Cmd.MethodEdit -> [ControlTrack.cmd_method_edit]
    where
    wrap cmds = universal ++ cmds ++ if kbd_entry
        then [KbdEntry.cmd_eat_keys] else []
    universal =
        with_note (PitchTrack.cmd_record_note_status scale_id) : midi_thru
    with_note = KbdEntry.with_note kbd_entry
    with_midi = if kbd_entry then KbdEntry.with_midi else id
    edit_mode = ctx_edit_mode context
    kbd_entry = ctx_kbd_entry context

    (maybe_track_type, maybe_inst, scale_id, maybe_addr) =
        get_defaults context (parser tracks)
    midi_thru = case (maybe_inst, maybe_addr) of
        (Just inst, Just addr) -> [with_midi $ MidiThru.cmd_midi_thru
            scale_id addr (Instrument.inst_pitch_bend_range inst)]
        _ -> []

get_defaults :: CmdContext -> Skeleton -> (Maybe TrackType,
    Maybe Instrument.Instrument, Pitch.ScaleId, Maybe Instrument.Addr)
get_defaults context skel = (maybe_track_type, inst, scale_id, addr)
    where
    (maybe_track_type, track_inst, track_scale) =
        get_track_type (ctx_focused_tracknum context) skel
    -- Track inst, fall back to default inst.
    score_inst = track_inst `mplus` ctx_default_inst context
    inst = join $ fmap (ctx_lookup_midi context Score.no_attrs) score_inst
    -- Track scale, fall back to track inst scale, then default inst scale,
    -- and then to the global default scale.
    scale_id = maybe Instrument.default_scale id $
        track_scale `mplus` fmap Instrument.inst_scale inst
    addr = join $ fmap (ctx_inst_addr context) score_inst

-- | Describe the type of a single track.  This is used to figure out what set
-- of cmds should apply to a given track.
data TrackType =
    NoteTrack NoteTrack.PitchTrack | PitchTrack | ControlTrack
    deriving (Show)

get_track_type :: Maybe Block.TrackNum -> Skeleton
    -> (Maybe TrackType, Maybe Score.Instrument, Maybe Pitch.ScaleId)
get_track_type Nothing _ = (Nothing, Nothing, Nothing)
get_track_type (Just tracknum) skel =
    case concatMap (_find_track_type tracknum) track_pairs of
        [] -> (Nothing, Nothing, Nothing)
        (ttype, inst, scale_id):_ -> (Just ttype, inst, scale_id)
    where track_pairs = _get_track_type [] skel

_find_track_type :: Block.TrackNum -> ([Track], Track)
    -> [(TrackType, Maybe Score.Instrument, Maybe Pitch.ScaleId)]
_find_track_type tracknum (conts, note)
    | track_tracknum note == tracknum = [(NoteTrack ptrack, inst, scale_id)]
    | Just track <- found_c = if is_pitch_track (track_title track)
        then [(PitchTrack, inst, scale_id)]
        else [(ControlTrack, inst, scale_id)]
    | otherwise = []
    where
    found_c = List.find ((==tracknum) . track_tracknum) conts
    (ptrack, scale_id) = case List.find (is_pitch_track . track_title) conts of
        Nothing -> (NoteTrack.PitchTrack True (track_tracknum note + 1),
            Nothing)
        Just track -> (NoteTrack.PitchTrack False (track_tracknum track),
            Just (scale_of_track (track_title track)))
    inst = let i = inst_of_track (track_title note)
        in if null (Score.inst_name i) then Nothing else Just i

_get_track_type :: [Track] -> Skeleton -> [([Track], Track)]
_get_track_type controllers skel = case skel of
        SkelController tracks (Just sub) ->
            _get_track_type (controllers ++ tracks) sub
        SkelController _ Nothing -> []
        SkelNote track -> [(controllers, track)]
        SkelMerge subs -> concatMap (_get_track_type controllers) subs

-- ** skeleton

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

-- | Get the Instruments from a parsed Skeleton.  See 'TrackType' for why this
-- is sketchy.
skeleton_instruments :: Skeleton -> [Score.Instrument]
skeleton_instruments (SkelController _ maybe_sub) =
    maybe [] skeleton_instruments maybe_sub
skeleton_instruments (SkelNote track) = [title_to_inst (track_title track)]
skeleton_instruments (SkelMerge subs) = concatMap skeleton_instruments subs

-- | TODO should be killed when skeleton_instruments is.
title_to_inst :: String -> Score.Instrument
title_to_inst title
    | is_inst_track title = inst_of_track title
    | otherwise = Score.Instrument ""

-- ** compile skeleton

-- | Transform a deriver skeleton into a real deriver.  The deriver may throw
-- if the skeleton was malformed.
compile_skeleton :: Skeleton -> Derive.EventDeriver
compile_skeleton skel =
    Derive.with_msg "compile_skeleton" (_compile_skeleton skel)
_compile_skeleton skel = case skel of
    SkelController ctracks Nothing ->
        -- TODO or it might be more friendly to just ignore them
        Derive.throw $ "orphaned controller tracks: " ++ show ctracks
    SkelController ctracks (Just subskel) ->
        compile_controllers ctracks subskel
    SkelNote (Track { track_id = Block.TId track_id _ }) ->
        Derive.with_track_warp Note.d_note_track track_id
    SkelNote track -> Derive.throw $
        "instrument track is not an event track, parser is confused: "
        ++ show track
    SkelMerge subs -> Derive.d_merge =<< mapM _compile_skeleton subs

-- | Generate the EventDeriver for a SkelController Skeleton.
compile_controllers :: [Track] -> Skeleton -> Derive.EventDeriver
compile_controllers tracks subskel =
    foldr track_controller (_compile_skeleton subskel) (event_tracks tracks)
event_tracks tracks =
    [(title, track_id) | Track (Just title) (Block.TId track_id _) _ <- tracks]

track_controller :: (Monad m) => (String, Track.TrackId)
    -> Derive.DeriveT m [Score.Event] -> Derive.DeriveT m [Score.Event]
track_controller (title, track_id) deriver
    | is_tempo_track title = do
        -- A tempo track is derived like other signals, but gets special
        -- treatment because of the track warps chicanery.
        sig_events <- Derive.with_track_warp_tempo
            Controller.d_controller_track track_id
        Derive.d_tempo track_id (Controller.d_signal sig_events) deriver
    | otherwise = do
        sig_events <- Derive.with_track_warp
            Controller.d_controller_track track_id
        -- TODO default to inst scale if none is given
        let signal = if is_pitch_track title
                then Controller.d_pitch_signal (scale_of_track title) sig_events
                else Controller.d_signal sig_events
        Controller.d_controller (Score.Controller title) signal deriver

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
compile_to_signals skel =
    Derive.with_msg "compile_to_signals" (_compile_to_signals skel)

_compile_to_signals skel = case skel of
    SkelController ctracks maybe_subskel ->
        compile_signals ctracks maybe_subskel
    SkelNote _ -> return []
    SkelMerge subs -> Derive.d_signal_merge =<< mapM _compile_to_signals subs

compile_signals :: [Track] -> Maybe Skeleton -> Derive.SignalDeriver
compile_signals tracks maybe_subskel = do
    track_sigs <- mapM signal_controller (event_tracks tracks)
    rest_sigs <- maybe (return []) _compile_to_signals maybe_subskel
    return (track_sigs ++ rest_sigs)

signal_controller :: (Monad m) => (String, Track.TrackId)
    -> Derive.DeriveT m (Track.TrackId, Signal.Signal)
signal_controller (title, track_id) = do
    sig_events <- Derive.with_track_warp Controller.d_controller_track track_id
    sig <- if is_pitch_track title
        then Controller.d_pitch_signal (scale_of_track title) sig_events
        else Controller.d_signal sig_events
    return (track_id, sig)

-- | Tracks are treated differently depending on their titles.
is_tempo_track, is_pitch_track, is_inst_track :: String -> Bool
is_tempo_track = (=="tempo")

is_pitch_track = (pitch_track_prefix `List.isPrefixOf`)
scale_of_track = Pitch.ScaleId . Seq.strip . drop 1
track_of_scale :: Pitch.ScaleId -> String
track_of_scale (Pitch.ScaleId scale_id) = pitch_track_prefix ++ scale_id

pitch_track_prefix = "*"
-- | This means use the instrument scale.
default_scale = Pitch.ScaleId ""

is_inst_track = (">" `List.isPrefixOf`)
inst_of_track = Score.Instrument . Seq.strip . drop 1

-- | Convert a track title into its instrument.  This could be per-schema, but
-- I'm going to hardcode it for now and assume all schemas will do the same
-- thing.
title_to_instrument :: String -> Maybe Score.Instrument
title_to_instrument name
    | is_inst_track name = Just $ inst_of_track name
    | otherwise = Nothing

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
default_parser tracks = skel_merge $
    map parse_tempo_group
        -- The 0th track should be the ruler track, which I ignore.
        (split (title_matches is_tempo_track) (drop 1 tracks))


parse_tempo_group tracks = case tracks of
    tempo@(Track { _track_title = Just title }) : rest
        | is_tempo_track title ->
            SkelController [tempo] (Just (parse_inst_groups rest))
        | otherwise -> parse_inst_groups tracks
    _ -> parse_inst_groups tracks

parse_inst_groups :: [Track] -> Skeleton
parse_inst_groups tracks = skel_merge $
    map parse_inst_group (split (title_matches is_inst_track) tracks)

parse_inst_group tracks = case tracks of
    track@(Track { _track_title = Just name }) : rest | is_inst_track name ->
        if (null rest)
            then SkelNote track
            else SkelController rest (Just (SkelNote track))
    _ -> SkelController tracks Nothing

-- ** util

title_matches f = f . track_title
split f xs = case Seq.split_with f xs of
    [] : rest -> rest
    grps -> grps
