-- | Cmds meant to be used from the REPL, for dealing with pitch tracks.
--
-- TODO It seems a little arbitrary to divide these cmds out like this.
-- However, they are distinct from Cmd.PitchTrack, so a separate module is
-- good.  I suppose if I need these functions elsewhere I can more them to more
-- generic places.
module Cmd.Lang.LPitch where
import Control.Monad
import qualified Util.Log as Log
import qualified Util.Seq as Seq

import Ui
import qualified Ui.Event as Event
import qualified Ui.State as State
import qualified Ui.Track as Track

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Selection as Selection
import qualified Cmd.PitchTrack as PitchTrack
import qualified Derive.Schema.Default as Default
import qualified Derive.Control as Control

import qualified Perform.Pitch as Pitch


-- | Given a base note, convert absolute pitch events to relative.
to_relative :: String -> Cmd.CmdL ()
to_relative note_s = do
    track_events <- Selection.events True
    let track_ids = map (\(a, _, _) -> a) track_events
    titles <- fmap (map Track.track_title) (mapM State.get_track track_ids)
    let scales = map Default.title_to_scale titles
    let tracks = [(track_id, scale, events)
            | (title, Just scale, (track_id, _, events)) <-
                zip3 titles scales track_events,
            not (Default.is_relative_track title)]
    if (null tracks)
        then Log.warn $ "LPitch.to_relative: no tracks to process"
        else mapM_ (track_to_degree (Pitch.Note note_s)) tracks

track_to_degree :: (Monad m) => Pitch.Note
    -> (TrackId, Pitch.ScaleId, [Track.PosEvent]) -> Cmd.CmdT m ()
track_to_degree base_note (track_id, scale_id, events) = do
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
    let events2 = [(pos, set_note (degree_to_relative scale degree) event)
            | ((pos, event), degree) <- zip events degrees2]
    unless (null events2) $ do
        -- This is kinda grody.  TODO there should be some higher level way
        -- to modify events.  Selection.modify_events?
        let (start, end) = (fst (head events), Track.event_end (last events))
        State.remove_events track_id start end
        State.insert_sorted_events track_id events2
        State.modify_track_title track_id $ \title ->
            case Default.parse_control_title title of
                (_, cont) -> Default.unparse_control_title (Just "+") cont

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
