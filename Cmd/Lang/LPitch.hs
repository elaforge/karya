-- | Cmds meant to be used from the REPL, for dealing with pitch tracks.
--
-- TODO It seems a little arbitrary to divide these cmds out like this.
-- However, they are distinct from Cmd.PitchTrack, so a separate module is
-- good.  I suppose if I need these functions elsewhere I can more them to more
-- generic places.
module Cmd.Lang.LPitch where
import Util.Control
import Ui
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.State as State
import qualified Ui.Track as Track

import qualified Cmd.Cmd as Cmd
import qualified Cmd.ModifyEvents as ModifyEvents
import qualified Cmd.Perf as Perf
import qualified Cmd.PitchTrack as PitchTrack

import qualified Derive.Scale.Relative as Relative
import qualified Derive.Score as Score
import qualified Derive.TrackInfo as TrackInfo
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.PitchSignal as PitchSignal


-- * invert

-- TODO these should invert position of control events too
to_negative, to_positive :: Cmd.CmdL ()
to_negative = ModifyEvents.events_sorted $ \evt ->
    Just $ if Events.negative evt then evt else negate_event evt
to_positive = ModifyEvents.events_sorted $ \evt ->
    Just $ if Events.positive evt then evt else negate_event evt

negate_event (pos, evt) =
    (pos + Event.event_duration evt, Event.modify_duration negate evt)

-- * transpose

transpose :: Pitch.Octave -> Integer -> Cmd.CmdL ()
transpose = PitchTrack.transpose_selection

-- * to_relative

-- | Convert the selected absolute pitch track into a relative one by
-- subtracting all the notes from the given base note.
to_relative :: String -> Cmd.CmdL ()
to_relative note_s = ModifyEvents.tracks_sorted $ \track_id events -> do
    title <- Track.track_title <$> State.get_track track_id
    case TrackInfo.parse_control title of
        Right (TrackInfo.Pitch (TrackInfo.PitchAbsolute (Just scale_id))
                name) -> do
            State.modify_track_title track_id $ const $ name_to_relative name
            degree <- lookup_degree scale_id (Pitch.Note note_s)
            Just <$> relative_events track_id degree events
        val -> Cmd.throw $ "not an absolute pitch track: " ++ show val

lookup_degree :: (Cmd.M m) => Pitch.ScaleId -> Pitch.Note -> m Pitch.Degree
lookup_degree scale_id note = Cmd.require_right ("Unknown base note: "++)
    =<< Perf.note_to_degree scale_id note

-- | Make the given absolute pitch events relative to the given base degree.
relative_events :: (Cmd.M m) => TrackId -> Pitch.Degree -> [Events.PosEvent]
    -> m [Events.PosEvent]
relative_events track_id base events = do
    degrees <- map PitchSignal.y_to_degree <$>
        Perf.get_pitch_at track_id (map fst events)
    let degrees2 = map (subtract base) degrees
    return [(pos, set_note (Relative.degree_to_note degree) event)
            | ((pos, event), degree) <- zip events degrees2]

-- | Make a relative pitch title from an optional pitch track name.
name_to_relative :: Maybe Score.Control -> String
name_to_relative = TrackInfo.unparse_control
    . TrackInfo.Pitch (TrackInfo.PitchRelative (TrackLang.Symbol "add"))

set_note :: Pitch.Note -> Event.Event -> Event.Event
set_note note = PitchTrack.modify f
    where f (meth, _) = (meth, Pitch.note_text note)
