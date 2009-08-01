{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{- | Cmds to add notes to a note track and send midi thru.

    This module is sister to 'Derive.Note' since it edits events that
    Derive.Note parses.

    A note track event has three fields: the interpolation method, the pitch,
    and the call.  The method and pitch are just as an indexed control track
    except that the pitch is interpreted relative to a given scale.  The call
    is used in sub-derivation, as documented in Derive.Note.

    - Midi keys send midi thru, and enter the scale degree (based on the octave
    offset) if edit mode is on.  Later this will go through a scale mapping
    layer, but for now it's hardcoded to Twelve.

    - In kbd_entry mode, letter keys do the same as midi keys, according to an
    organ-like layout.

    - But in non kbd_entry mode, letter keys edit the call text.
-}
module Cmd.NoteTrack where
import Control.Monad
import qualified Control.Monad.Identity as Identity
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified Midi.Midi as Midi

import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.UiMsg as UiMsg

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Edit as Edit
import qualified Cmd.EditUtil as EditUtil
import qualified Cmd.Keymap as Keymap
import qualified Cmd.Msg as Msg
import qualified Cmd.PitchTrack as PitchTrack
import qualified Cmd.Selection as Selection

import qualified Derive.Note as Note

import qualified Perform.Pitch as Pitch
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Controller as Controller

import qualified App.Config as Config


-- | Indicate the pitch track of a note track.  If the Bool is True,
-- the track doesn't exist and should be created at the given TrackNum.
data PitchTrack = PitchTrack Bool Block.TrackNum deriving (Show)

cmd_raw_edit :: Pitch.ScaleId -> Cmd.Cmd
cmd_raw_edit scale_id msg = do
    key <- EditUtil.edit_key scale_id msg
    EditUtil.modify_event (EditUtil.modify_text key)
    return Cmd.Done

cmd_val_edit :: PitchTrack -> Pitch.ScaleId -> Cmd.Cmd
cmd_val_edit pitch_track scale_id msg = do
    note <- EditUtil.note_key scale_id msg
    track_id <- get_pitch_track pitch_track
    (_, tracknum, pos) <- Selection.get_insert_track
    PitchTrack.cmd_val_edit_at (track_id, tracknum, pos) note
    if (Maybe.isJust note)
        then ensure_exists
        else remove
    return Cmd.Done

cmd_method_edit :: PitchTrack -> Cmd.Cmd
cmd_method_edit pitch_track msg = do
    key <- EditUtil.alpha_key msg
    track_id <- get_pitch_track pitch_track
    (_, tracknum, pos) <- Selection.get_insert_track
    PitchTrack.cmd_method_edit_at (track_id, tracknum, pos) key
    ensure_exists
    return Cmd.Done

get_pitch_track :: (Monad m) => PitchTrack -> Cmd.CmdT m Track.TrackId
get_pitch_track (PitchTrack create tracknum) = do
    block_id <- Cmd.get_focused_block
    if create
        then create_pitch_track block_id tracknum
        else Cmd.require_msg
            ("get_pitch_track: invalid tracknum " ++ show tracknum)
            =<< State.event_track_at block_id tracknum

create_pitch_track block_id tracknum = do
    tid <- Create.track block_id tracknum
    -- TODO if I really wanted to make this schema-independent, the title would
    -- have to be passed in, but this is all pretty dependent on the default
    -- schema anyway.
    State.set_track_title tid "*"
    return tid

-- * implementation

ensure_exists :: (Monad m) => Cmd.CmdT m ()
ensure_exists = EditUtil.modify_event Just

remove :: (Monad m) => Cmd.CmdT m ()
remove = EditUtil.modify_event (const Nothing)
