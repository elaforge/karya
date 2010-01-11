-- | Cmds meant to be used from the REPL, for dealing with pitch tracks.
--
-- TODO It seems a little arbitrary to divide these cmds out like this.
-- However, they are distinct from Cmd.PitchTrack, so a separate module is
-- good.  I suppose if I need these functions elsewhere I can more them to more
-- generic places.
module Cmd.Lang.LPitch where
import Control.Monad
import qualified Util.Seq as Seq

import Ui
import qualified Ui.Event as Event
import qualified Ui.State as State
import qualified Ui.Track as Track

import qualified Cmd.Cmd as Cmd
import qualified Cmd.ModifyEvents as ModifyEvents
import qualified Cmd.PitchTrack as PitchTrack
import qualified Derive.Schema.Default as Default
import qualified Derive.Control as Control

import qualified Perform.Pitch as Pitch


-- * invert

-- TODO these should invert position of control events too
to_negative, to_positive :: Cmd.CmdL ()
to_negative = ModifyEvents.events_sorted $ \evt ->
    Just $ if Track.event_negative evt then evt else negate_event evt
to_positive = ModifyEvents.events_sorted $ \evt ->
    Just $ if Track.event_positive evt then evt else negate_event evt

negate_event (pos, evt) =
    (pos + Event.event_duration evt, Event.modify_duration negate evt)

-- * to_relative

to_relative :: String -> Cmd.CmdL ()
to_relative note_s = ModifyEvents.tracks_sorted $ \track_id events -> do
    title <- fmap Track.track_title (State.get_track track_id)
    let maybe_scale = Default.title_to_scale title
    case maybe_scale of
        Just scale_id | not (Default.is_relative_track title) ->
            track_to_degree (Pitch.Note note_s) track_id scale_id events
        _ -> return events

track_to_degree :: (Monad m) => Pitch.Note -> TrackId -> Pitch.ScaleId
    -> [Track.PosEvent] -> Cmd.CmdT m [Track.PosEvent]
track_to_degree base_note track_id scale_id events = do
    let name = "LPitch.track_to_degree"
    scale <- Cmd.get_scale name scale_id
    base <- maybe
        (Cmd.throw $ name ++ ": unknown base note: " ++ show base_note)
        return (Pitch.scale_note_to_degree scale base_note)
    let (bad_notes, degrees) = Seq.partition_either
            (map (event_to_degree scale . snd) events)
    unless (null bad_notes) $
        Cmd.throw $ name ++ ": notes not in scale: " ++ show bad_notes
    let degrees2 = map (subtract base) degrees
    State.modify_track_title track_id $ \title ->
        Default.unparse_control_title (Just "+") $ snd $
            Default.parse_control_title title
    return [(pos, set_note (degree_to_relative scale degree) event)
            | ((pos, event), degree) <- zip events degrees2]

set_note :: Pitch.Note -> Event.Event -> Event.Event
set_note note = PitchTrack.modify f
    where f (meth, _) = (meth, Pitch.note_text note)

degree_to_relative :: Pitch.Scale -> Pitch.Degree -> Pitch.Note
degree_to_relative scale (Pitch.Degree n) =
    Control.unparse_relative (oct, fromIntegral nn + frac)
    where
    (d, frac) = properFraction n
    (oct, nn) = d `quotRem` Pitch.scale_octave scale

event_to_degree :: Pitch.Scale -> Event.Event -> Either Pitch.Note Pitch.Degree
event_to_degree scale event =
    maybe (Left note) Right (Pitch.scale_note_to_degree scale note)
    where note = Pitch.Note (snd (PitchTrack.parse (Event.event_string event)))
