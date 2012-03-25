{-# LANGUAGE ViewPatterns #-}
{- | Cmds to add notes to a note track.

    This module is sister to 'Derive.Note' since it edits events that
    Derive.Note parses.

    Notes:

    Note event are usually given a duration of the current time step.  If a
    "trigger only" instrument (e.g. percussion) is in scope, they are created
    with zero duration.  Also, starting a raw edit with space will create a
    zero duration event.  This is useful because some note transformers only
    transform other notes and don't need a duration of their own.
-}
module Cmd.NoteTrack where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Util.Control
import qualified Util.Seq as Seq
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.Key as Key
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Types as Types

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.EditUtil as EditUtil
import qualified Cmd.Info as Info
import qualified Cmd.InputNote as InputNote
import qualified Cmd.Msg as Msg
import qualified Cmd.PitchTrack as PitchTrack
import qualified Cmd.Selection as Selection

import qualified Derive.Score as Score
import qualified Derive.TrackInfo as TrackInfo
import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.MidiDb as MidiDb
import Types


data NoteTrack = NoteTrack {
    track_note :: TrackNum
    , track_pitch :: TrackNum
    } deriving (Show, Eq)

cmd_raw_edit :: Cmd.Cmd
cmd_raw_edit = raw_edit

-- | The val edit for note tracks edits its pitch track (possibly creating it
-- if necessary), and creates a blank event on the note track.  It may also
-- edit multiple pitch tracks for chords, or record velocity in addition to
-- pitch.  TODO not implemented yet
--
-- If I'm in chord mode, try to find the next track.  If there is no
-- appropriate next track, the cmd will throw an error.
cmd_val_edit :: Cmd.Cmd
cmd_val_edit msg = do
    EditUtil.fallthrough msg
    (block_id, sel_tracknum, _, pos) <- Selection.get_insert
    case msg of
        Msg.InputNote input_note -> case input_note of
            InputNote.NoteOn note_id key _vel -> do
                note <- EditUtil.parse_key key
                chord_mode <- get_state Cmd.state_chord
                (ntrack, create) <- if chord_mode
                    then next_note_track block_id sel_tracknum
                    else this_note_track block_id sel_tracknum
                when create $ create_pitch_track block_id ntrack
                associate_note_id block_id (track_pitch ntrack) note_id
                -- TODO if I can find a vel track, put the vel there
                PitchTrack.val_edit_at (block_id, track_pitch ntrack, pos) note
                ensure_note_event (block_id, track_note ntrack, pos)
                advance_mode <- get_state Cmd.state_advance
                when (advance_mode && not chord_mode) Selection.advance
            InputNote.PitchChange note_id key -> do
                (pitch_tracknum, track_id) <- Cmd.require_msg
                    ("no track for note_id " ++ show note_id)
                    =<< find_pitch_track note_id
                note <- EditUtil.parse_key key
                -- If advance is set, the selection will have advanced past
                -- the pitch's position, so look for a previous event.
                pos <- event_at_or_before track_id pos
                PitchTrack.val_edit_at (block_id, pitch_tracknum, pos) note
            InputNote.NoteOff note_id _vel -> do
                dissociate_note_id note_id
                advance <- andM [get_state Cmd.state_advance,
                    get_state Cmd.state_chord, all_keys_up]
                when advance Selection.advance
            InputNote.Control _ _ _ -> return ()
        (Msg.key_down -> Just Key.Backspace) -> do
            remove_event (block_id, sel_tracknum, pos)
            -- clear out the pitch track too
            maybe_pitch <- Info.pitch_of_note block_id sel_tracknum
            when_just maybe_pitch $ \pitch ->
                remove_event (block_id, State.track_tracknum pitch, pos)
            Selection.advance
        _ -> Cmd.abort
    return Cmd.Done
    where
    dissociate_note_id note_id = Cmd.modify_wdev_state $ \st -> st
        { Cmd.wdev_pitch_track = Map.delete note_id (Cmd.wdev_pitch_track st) }

associate_note_id :: (Cmd.M m) => BlockId -> TrackNum -> InputNote.NoteId
    -> m ()
associate_note_id block_id tracknum note_id = Cmd.modify_wdev_state $ \st ->
    st { Cmd.wdev_pitch_track =
        Map.insert note_id (block_id, tracknum) (Cmd.wdev_pitch_track st) }

-- | Find the next available note track.  An available pitch track is one that
-- is or is on the right of the given track, has either the same instrument or
-- has the default instrument, and doesn't already have a note_id associated
-- with it.
--
-- TODO instead of checking for Score.default_inst I should use the environ
-- to make sure it's actually the same inst.
next_note_track :: (Cmd.M m) => BlockId -> TrackNum -> m (NoteTrack, Bool)
next_note_track block_id tracknum = do
    wdev <- Cmd.get_wdev_state
    let associated =
            [tracknum | (_, tracknum) <- Map.elems (Cmd.wdev_pitch_track wdev)]
    tracks <- Info.block_tracks block_id
    inst <- Info.get_instrument_of block_id tracknum
    case List.find (candidate inst associated) tracks of
        Nothing -> Cmd.throw $ "no next note track in " ++ show tracks
        Just track -> should_create_pitch block_id track
    where
    candidate inst associated (Info.Track track ttype) =
        State.track_tracknum track >= tracknum
        -- Either no pitch track, or an unassociated one.
        && maybe True (`notElem` associated) pitch_tracknum
        && maybe False (`elem` [inst, Score.default_inst])
            (TrackInfo.title_to_instrument (State.track_title track))
        where
        pitch_tracknum = case ttype of
            Info.Note (Just pitch) -> Just $ State.track_tracknum pitch
            _ -> Nothing

-- | The given track should be a note track.  Figure out if it has a pitch
-- track, or if one should be created.
this_note_track :: (Cmd.M m) => BlockId -> TrackNum -> m (NoteTrack, Bool)
this_note_track block_id tracknum = do
    track <- Info.get_track_type block_id tracknum
    should_create_pitch block_id track

should_create_pitch :: (Cmd.M m) => BlockId -> Info.Track -> m (NoteTrack, Bool)
should_create_pitch block_id track = case Info.track_type track of
    Info.Note Nothing -> return (NoteTrack tracknum (tracknum+1), True)
    Info.Note (Just pitch) ->
        return (NoteTrack tracknum (State.track_tracknum pitch), False)
    ttype -> Cmd.throw $ "expected a note track for "
        ++ show (block_id, tracknum) ++ " but got " ++ show ttype
    where tracknum = State.track_tracknum (Info.track_info track)

--

event_at_or_before :: (Cmd.M m) => TrackId -> ScoreTime -> m ScoreTime
event_at_or_before track_id pos = do
    track <- State.get_track track_id
    let (pre, post) = Events.split pos (Track.track_events track)
    return $ case (pre, post) of
        (_, (p, _) : _) | p == pos -> pos
        ((prev, _) : _, _) -> prev
        _ -> pos

all_keys_up :: (Cmd.M m) => m Bool
all_keys_up = do
    st <- Cmd.get_wdev_state
    return (Map.null (Cmd.wdev_pitch_track st))

-- | Find the pitch track associated with the given NoteId, if one exists.
find_pitch_track :: (Cmd.M m) => InputNote.NoteId
    -> m (Maybe (TrackNum, TrackId))
find_pitch_track note_id = do
    st <- Cmd.get_wdev_state
    let maybe_track = Map.lookup note_id (Cmd.wdev_pitch_track st)
    case maybe_track of
        Nothing -> return Nothing
        Just (block_id, tracknum) -> do
            track_id <- State.get_event_track_at "NoteTrack.track_of"
                block_id tracknum
            return $ Just (tracknum, track_id)


-- * cmd_method_edit

cmd_method_edit :: Cmd.Cmd
cmd_method_edit msg = do
    EditUtil.fallthrough msg
    case msg of
        (EditUtil.method_key -> Just key) -> do
            (block_id, tracknum, _, pos) <- Selection.get_insert
            (ntrack, create) <- this_note_track block_id tracknum
            when create $ create_pitch_track block_id ntrack
            PitchTrack.method_edit_at (block_id, track_pitch ntrack, pos) key
            ensure_note_event (block_id, track_note ntrack, pos)
        _ -> Cmd.abort
    return Cmd.Done

-- * parsing

-- | Try to to figure out the BlockId of a block call in a note track.
-- It's not guaranteed to be a real block because it might not even be
-- a block call.
--
-- This doesn't use the full Derive.Parse machinery, but is simple and doesn't
-- require the text to be fully parseable.
block_call :: Id.Namespace -> String -> Maybe BlockId
block_call ns expr = Types.BlockId <$> Id.make ns call
    where call = generator_of expr

generator_of :: String -> String
generator_of = Seq.strip . last . Seq.split "|"

-- * implementation

-- | Create a pitch track.
create_pitch_track :: (Cmd.M m) => BlockId -> NoteTrack -> m ()
create_pitch_track block_id (NoteTrack note pitch) = do
    scale_id <- EditUtil.get_scale_id
    tid <- Create.track block_id pitch
    -- Link note track underneath newly created pitch track.
    State.splice_skeleton_below block_id pitch note
    State.set_track_title tid (TrackInfo.scale_to_title scale_id)

-- | Ensure that a note event exists at the given spot.  An existing event is
-- left alone, but if there is no existing event a new one will be created.
ensure_note_event :: (Cmd.M m) => EditUtil.SelPos -> m ()
ensure_note_event pos = do
    txt <- Cmd.gets (Cmd.state_note_text . Cmd.state_edit)
    modify_event_at pos False False $
        maybe (Just txt, False) (\old -> (Just old, False))

remove_event :: (Cmd.M m) => EditUtil.SelPos -> m ()
remove_event selpos =
    EditUtil.modify_event_at selpos False False (const (Nothing, False))

raw_edit :: Cmd.Cmd
raw_edit msg = do
    EditUtil.fallthrough msg
    sel <- EditUtil.get_sel_pos
    case msg of
        Msg.InputNote (InputNote.NoteOn _ key _) -> do
            note <- EditUtil.parse_key key
            modify_event_at sel False False $ \txt ->
                (EditUtil.modify_text_note note (Maybe.fromMaybe "" txt),
                    False)
        (EditUtil.raw_key -> Just key) ->
            -- Create a zero length event on a space.  'modify_text_key' will
            -- eat a lone space, so this is an easy way to create
            -- a zero-length note.
            modify_event_at sel (key == Key.Char ' ') False $ \txt ->
                (EditUtil.modify_text_key key (Maybe.fromMaybe "" txt), False)
        _ -> Cmd.abort
    return Cmd.Done

-- | Instruments with the triggered flag set don't pay attention to note off,
-- so I can make the duration 0.
triggered_inst :: (Cmd.M m) => Maybe Score.Instrument -> m Bool
triggered_inst Nothing = return False -- don't know, but guess it's not
triggered_inst (Just inst) =
    maybe False (Instrument.has_flag Instrument.Triggered . MidiDb.info_patch)
        <$> Cmd.lookup_instrument_info inst

modify_event_at :: (Cmd.M m) => EditUtil.SelPos -> Bool -> Bool
    -> EditUtil.Modify -> m ()
modify_event_at selpos zero_dur modify_dur f = do
    trigger_inst <- triggered_inst =<< EditUtil.lookup_instrument
    EditUtil.modify_event_at selpos (zero_dur || trigger_inst) modify_dur f

get_state :: (Cmd.M m) => (Cmd.EditState -> a) -> m a
get_state f = Cmd.gets (f . Cmd.state_edit)
