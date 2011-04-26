{-# LANGUAGE ViewPatterns #-}
{- | Cmds to add notes to a note track.

    This module is sister to 'Derive.Note' since it edits events that
    Derive.Note parses.
-}
module Cmd.NoteTrack where
import qualified Data.Map as Map
import qualified Util.Control as Control
import qualified Util.Seq as Seq

import Ui
import qualified Ui.Id as Id
import qualified Ui.Key as Key
import qualified Ui.State as State
import qualified Ui.Types as Types

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.EditUtil as EditUtil
import qualified Cmd.InputNote as InputNote
import qualified Cmd.Msg as Msg
import qualified Cmd.PitchTrack as PitchTrack
import qualified Cmd.Selection as Selection

import qualified Derive.TrackInfo as TrackInfo
import qualified Derive.Score as Score

import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.MidiDb as MidiDb


-- | Indicate the pitch track of a note track, or how to create one if
-- necessary.
data PitchTrack =
    -- | Create a pitch track with (note_tracknum, scale_id, pitch_tracknum).
    CreateTrack TrackNum TrackNum
    | ExistingTrack TrackNum TrackId
    deriving (Show, Eq)

cmd_raw_edit :: Cmd.Cmd
cmd_raw_edit = raw_edit

-- | The val edit for note tracks edits its pitch track (possibly creating it
-- if necessary), and creates a blank event on the note track.  It may also
-- edit multiple pitch tracks for chords, or record velocity in addition to
-- pitch.  TODO not implemented yet
cmd_val_edit :: PitchTrack -> Cmd.Cmd
cmd_val_edit pitch_track msg = do
    EditUtil.fallthrough msg
    (block_id, tracknum, track_id, pos) <- Selection.get_insert
    case msg of
        Msg.InputNote input_note -> case input_note of
            InputNote.NoteOn note_id key _vel -> do
                -- TODO if I can find a vel track, put the vel there
                (pitch_tracknum, track_id) <-
                    make_pitch_track (Just note_id) pitch_track
                note <- EditUtil.parse_key key
                PitchTrack.val_edit_at (pitch_tracknum, track_id, pos) note
                -- TODO if I do chords, this will have to be the chosen note
                -- track
                create_event
            InputNote.PitchChange note_id key -> do
                (tracknum, track_id) <- track_of note_id
                note <- EditUtil.parse_key key
                PitchTrack.val_edit_at (tracknum, track_id, pos) note
            InputNote.NoteOff note_id _vel -> do
                delete_note_id note_id
                Control.whenM all_keys_up Selection.advance
            InputNote.Control _ _ _ -> return ()
        (Msg.key_down -> Just Key.Backspace) -> do
            remove (tracknum, track_id, pos)
            -- clear out the pitch track too
            case pitch_track of
                ExistingTrack tracknum _ -> do
                    track_id <- State.get_event_track_at
                        "NoteTrack.cmd_val_edit" block_id tracknum
                    remove (tracknum, track_id, pos)
                _ -> return ()
            Selection.advance
        _ -> Cmd.abort
    return Cmd.Done
    where
    delete_note_id note_id = do
        st <- Cmd.get_wdev_state
        Cmd.set_wdev_state $ st { Cmd.wdev_note_track =
            Map.delete note_id (Cmd.wdev_note_track st) }

cmd_method_edit :: PitchTrack -> Cmd.Cmd
cmd_method_edit pitch_track msg = do
    EditUtil.fallthrough msg
    case msg of
        (EditUtil.method_key -> Just key) -> do
            (_, _, pos) <- EditUtil.get_sel_pos
            (tracknum, track_id) <- make_pitch_track Nothing pitch_track
            PitchTrack.method_edit_at (tracknum, track_id, pos) key
            create_event
        _ -> Cmd.abort
    return Cmd.Done

all_keys_up :: (Cmd.M m) => m Bool
all_keys_up = do
    st <- Cmd.get_wdev_state
    return (Map.null (Cmd.wdev_note_track st))

-- | Find existing tracknum or throw.
track_of :: (Cmd.M m) => InputNote.NoteId -> m (TrackNum, TrackId)
track_of note_id = do
    st <- Cmd.get_wdev_state
    (block_id, tracknum) <- maybe
        (Cmd.throw $ "no tracknum for " ++ show note_id) return
        (Map.lookup note_id (Cmd.wdev_note_track st))
    track_id <- State.get_event_track_at "NoteTrack.track_of" block_id tracknum
    return (tracknum, track_id)

-- | Turn the given PitchTrack into a TrackId, creating a new track if it's
-- a CreateTrack.  If a NoteId is given, associate that ID with the track.
make_pitch_track :: (Cmd.M m) => Maybe InputNote.NoteId -> PitchTrack
    -> m (TrackNum, TrackId)
make_pitch_track maybe_note_id pitch_track = do
    block_id <- Cmd.get_focused_block
    (tracknum, tid) <- case pitch_track of
        CreateTrack note_tracknum pitch_tracknum -> do
            scale_id <- EditUtil.get_scale_id
            tid <- create_pitch_track block_id note_tracknum
                (TrackInfo.scale_to_title scale_id) pitch_tracknum
            return (pitch_tracknum, tid)
        ExistingTrack tracknum _ -> do
            tid <- State.get_event_track_at "NoteTrack.make_pitch_track"
                block_id tracknum
            return (tracknum, tid)
    st <- Cmd.get_wdev_state
    case maybe_note_id of
        Just note_id -> Cmd.set_wdev_state $ st { Cmd.wdev_note_track =
            Map.insert note_id (block_id, tracknum) (Cmd.wdev_note_track st) }
        _ -> return ()
    return (tracknum, tid)

-- | Create a pitch track for a note track.
create_pitch_track :: (State.M m) => BlockId
    -> TrackNum -- ^ tracknum of corresponding note track
    -> String -- ^ created track has this title
    -> TrackNum -> m TrackId
create_pitch_track block_id note_tracknum title tracknum = do
    tid <- Create.track block_id tracknum
    -- Link note track underneath newly created pitch track.
    State.splice_skeleton block_id (tracknum, note_tracknum)
    State.set_track_title tid title
    return tid

-- * parsing

-- | Try to to figure out the BlockId of a block call in a note track.
-- It's not guaranteed to be a real block because it might not even be
-- a block call.
--
-- This doesn't use the full Derive.Parse machinery, but is simple and doesn't
-- require the text to be fully parseable.
block_call :: Id.Namespace -> String -> Maybe BlockId
block_call ns expr
    | null call = Nothing
    | otherwise = Just $ Types.BlockId (Id.make ns call)
    where call = generator_of expr

generator_of :: String -> String
generator_of = Seq.strip . last . Seq.split "|"

-- * implementation

create_event :: (Cmd.M m) => m ()
create_event = do
    txt <- Cmd.gets (Cmd.state_note_text . Cmd.state_edit)
    modify_event True (const (Just txt, False))

remove :: (Cmd.M m) => EditUtil.SelPos -> m ()
remove selpos =
    EditUtil.modify_event_at selpos False False (const (Nothing, False))

raw_edit :: Cmd.Cmd
raw_edit msg = do
    EditUtil.fallthrough msg
    case msg of
        Msg.InputNote (InputNote.NoteOn _ key _) -> do
            note <- EditUtil.parse_key key
            modify_event False $ \txt ->
                (EditUtil.modify_text_note note txt, False)
        (EditUtil.raw_key -> Just key) -> do
            modify_event False $ \txt ->
                (EditUtil.modify_text_key key txt, False)
        _ -> Cmd.abort
    return Cmd.Done

-- | Instruments with the triggered flag set don't pay attention to note off,
-- so I can make the duration 0.
triggered_inst :: (Cmd.M m) => Maybe Score.Instrument -> m Bool
triggered_inst Nothing = return False -- don't know, but guess it's not
triggered_inst (Just inst) = do
    maybe_info <- Cmd.lookup_instrument_info inst
    return $ maybe False (Instrument.patch_triggered . MidiDb.info_patch)
            maybe_info

modify_event :: (Cmd.M m) => Bool -> (String -> (Maybe String, Bool)) -> m ()
modify_event modify_dur f = do
    zero_dur <- triggered_inst =<< EditUtil.lookup_instrument
    EditUtil.modify_event zero_dur modify_dur f
