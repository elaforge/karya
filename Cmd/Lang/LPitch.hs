-- | Cmds meant to be used from the REPL, for dealing with pitch tracks.
--
-- TODO It seems a little arbitrary to divide these cmds out like this.
-- However, they are distinct from Cmd.PitchTrack, so a separate module is
-- good.  I suppose if I need these functions elsewhere I can more them to more
-- generic places.
module Cmd.Lang.LPitch where
import Control.Monad
import Util.Control
import qualified Util.Seq as Seq

import Ui
import qualified Ui.Event as Event
import qualified Ui.State as State
import qualified Ui.Track as Track

import qualified Cmd.Cmd as Cmd
import qualified Cmd.ModifyEvents as ModifyEvents
import qualified Cmd.PitchTrack as PitchTrack

import qualified Derive.Scale as Scale
import qualified Derive.Scale.Relative as Relative
import qualified Derive.TrackInfo as TrackInfo
import qualified Derive.TrackLang as TrackLang

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

-- * transpose

transpose :: Pitch.Octave -> Integer -> Cmd.CmdL ()
transpose = PitchTrack.transpose_selection

-- * to_relative

to_relative :: String -> Cmd.CmdL ()
to_relative note_s = ModifyEvents.tracks_sorted $ \track_id events -> do
    title <- Track.track_title <$> State.get_track track_id
    case TrackInfo.parse_control title of
        Right (TrackInfo.Pitch (TrackInfo.PitchAbsolute (Just scale_id)) _) ->
            Just <$> track_to_degree (Pitch.Note note_s) track_id scale_id
                events
        val -> Cmd.throw $ "not an absolute pitch track: " ++ show val

track_to_degree :: (Cmd.M m) => Pitch.Note -> TrackId -> Pitch.ScaleId
    -> [Track.PosEvent] -> m [Track.PosEvent]
track_to_degree base_note track_id scale_id events = do
    let name = "LPitch.track_to_degree"
    scale <- Cmd.get_scale name scale_id
    base <- maybe
        (Cmd.throw $ name ++ ": unknown base note: " ++ show base_note)
        return (note_to_degree scale base_note)
    let (bad_notes, degrees) = Seq.partition_either
            (map (event_to_degree scale . snd) events)
    unless (null bad_notes) $
        Cmd.throw $ name ++ ": notes not in scale: " ++ show bad_notes
    title <- Track.track_title <$> State.get_track track_id
    track_type <- case TrackInfo.parse_control title of
        Right (TrackInfo.Pitch (TrackInfo.PitchAbsolute _) pname) ->
            return $ TrackInfo.Pitch
                (TrackInfo.PitchRelative (TrackLang.Symbol "add")) pname
        val -> Cmd.throw $ "not an absolute pitch track: " ++ show val
    State.modify_track_title track_id $
        const $ TrackInfo.unparse_control track_type
    let degrees2 = map (subtract base) degrees
    return [(pos, set_note (Relative.degree_to_note degree) event)
            | ((pos, event), degree) <- zip events degrees2]

set_note :: Pitch.Note -> Event.Event -> Event.Event
set_note note = PitchTrack.modify f
    where f (meth, _) = (meth, Pitch.note_text note)

event_to_degree :: Scale.Scale -> Event.Event -> Either Pitch.Note Pitch.Degree
event_to_degree scale event =
    maybe (Left note) Right (note_to_degree scale note)
    where note = Pitch.Note (snd (PitchTrack.parse (Event.event_string event)))

-- | TODO I have a problem here.  I need to evaluate the note expression to
-- know its degree, but there's no machinery for running derivation from Cmds.
-- Technically the degree may be different depending on context anyway.
-- It seems like there's still a use for a context-independent Note->Degree
-- though, so maybe I should put it back in...
note_to_degree :: Scale.Scale -> Pitch.Note -> Maybe Pitch.Degree
note_to_degree = undefined
