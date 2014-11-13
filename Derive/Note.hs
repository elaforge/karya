-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Derive events on a note track.  This is the Derive equivalent of
    "Cmd.NoteTrack", but has a different name to avoid clashes.
-}
module Derive.Note (d_note_track, stash_signal_if_wanted) where
import qualified Data.Char as Char
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Data.Tree as Tree

import qualified Util.Seq as Seq
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree
import qualified Derive.Control as Control
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Eval as Eval
import qualified Derive.EvalTrack as EvalTrack
import qualified Derive.LEvent as LEvent
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Score as Score

import qualified Perform.Signal as Signal
import Global
import Types


-- * note track

-- | Top level deriver for note tracks.
d_note_track :: ([TrackTree.EventsNode] -> Derive.NoteDeriver)
    -- ^ This is used to derive orphans, as documented by
    -- 'EvalTrack.derive_note_track'.
    -> TrackTree.EventsNode -> Derive.NoteDeriver
d_note_track derive_tracks (Tree.Node track subs) =
    title $ derive_notes derive_tracks (track_info track subs)
    where
    title = with_title subs (TrackTree.track_end track)
        (TrackTree.track_title track)

-- | Note tracks can also have track signals, extracted from the events they
-- produce.
stash_signal_if_wanted :: Derive.Events -> TrackTree.Track
    -> Derive.Deriver ()
stash_signal_if_wanted events track =
    whenJustM (Control.render_of track) $ \(block_id, track_id, maybe_source) ->
        whenJust maybe_source $ \source ->
            stash_signal block_id track_id source events

stash_signal :: BlockId -> TrackId -> Track.RenderSource -> Derive.Events
    -> Derive.Deriver ()
stash_signal block_id track_id source events =
    Control.stash_signal block_id track_id signal
    where signal = extract_track_signal source (LEvent.events_of events)

extract_track_signal :: Track.RenderSource -> [Score.Event] -> Signal.Control
extract_track_signal source events = mconcat $ case source of
    Track.Control control -> mapMaybe (extract_control control) events
    Track.Pitch control -> mapMaybe (extract_pitch control) events
    where
    extract_control control = fmap Score.typed_val . Score.event_control control
    extract_pitch Nothing event = Just $ convert event $
        Score.event_transformed_pitch event
    extract_pitch (Just control) event =
        convert event <$> Score.event_named_pitch control event
    convert event psig = Signal.coerce $ fst $ PitchSignal.to_nn $
        -- Since these signals will be mconcatted into one signal, I don't
        -- want one event's control at 0 to wipe out the previous events.
        PitchSignal.drop_before_strict (Score.event_min event) $
        PitchSignal.apply_controls (Score.event_environ event)
            (Score.event_transformed_controls event) psig

with_title :: TrackTree.EventsTree -> ScoreTime -> Text -> Derive.NoteDeriver
    -> Derive.NoteDeriver
with_title subs end title deriver
    | Text.all Char.isSpace title = deriver
    | otherwise = do
        track_expr <- either (Derive.throw . ("track title: "++)) return
            (ParseTitle.parse_note title)
        Eval.eval_transformers cinfo (NonEmpty.toList track_expr) deriver
    where
    cinfo = (Derive.dummy_call_info 0 end "note track")
        { Derive.info_sub_tracks = subs }

derive_notes :: ([TrackTree.EventsNode] -> Derive.NoteDeriver)
    -> EvalTrack.TrackInfo Score.Event -> Derive.NoteDeriver
derive_notes derive_tracks tinfo = do
    state <- Derive.get
    let (event_groups, threaded, collect) =
            EvalTrack.derive_note_track derive_tracks state tinfo
    Internal.merge_collect collect
    Internal.set_threaded threaded
    return $ Derive.merge_asc_events event_groups

track_info :: TrackTree.Track -> [TrackTree.EventsNode]
    -> EvalTrack.TrackInfo Score.Event
track_info track subs = EvalTrack.TrackInfo
    { EvalTrack.tinfo_track = track
    , EvalTrack.tinfo_sub_tracks = subs
    , EvalTrack.tinfo_type = ParseTitle.NoteTrack
    , EvalTrack.tinfo_get_last_val = Seq.last
    }
