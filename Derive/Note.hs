-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Derive events on a note track.  This is the Derive equivalent of
    "Cmd.NoteTrack", but has a different name to avoid clashes.

    Note tracks have a \"function call\" abstraction mechanism.  An event in
    one block is expanded into the events derived from the block it names, and
    so on recursively.

    The sub-block is passed its parent's tempo map (along with all the other
    controls in the environment) to interpret as it will, so that it may,
    for example, set absolute tempo.  The generated events should begin with
    the given start time, but are not guaranteed to last for the given
    duration.

    The parse section parses note track events as manipulated by
    "Cmd.NoteTrack".

    Note track language:

    Notes are created from a combination of the event text and the track title.
    The track title is simply prepended to the contents of each event in that
    track.  There are several kinds of words:

    - A word starting with @--@ is a comment, and that word and every
    subsequent word is ignored.  At the least this is useful for tests to
    keep track of events.

    - As a special case, an event that is entirely @--@ is ignored entirely.
    It would be more elegant to implement this simply as a call that emits
    no events, but -- looks nicer.  At the moment, this is convenient to extend
    the length of a block past the last note.

    - Attributes are prepended by @+@, @-@, or @=@ are collected into a set,
    which is placed in the environment.  @+@ adds to the set, @-@ subtracts,
    and @=@ sets it.

    - Instruments are prepended by @>@ and the textually last one is placed in
    the environment, replacing what was previously there.

    - The call is the first word that remains after the above is stripped out.

    - Args are the words after the call.

    Args can be:

    [@ 1.23 @]              floating point number

    [@ letters @]           string

    [@ \@var @]              variable from the environment (TODO implement)

    [@ *note @]             note from a scale (TODO which one?)

    Usage:

        >>inst      t *4c- *4d- *4e-    triplet played with inst
        >>inst      g -2                grace from pitch-2 to pitch
        >>inst      ns/b1               subderive block "ns/b1", which
        >                               inherits >inst if it doesn't set its own
        >>inst1     >inst2 t a b        set >inst2 and call "t a b"
        >>inst      seed 123            random seed set to 123
        >>drums     snare               play "drums" inst with "snare" arg
        >>          ""                  inherit inst from calling block

    The call can be several things:

    [@ func @] @func@ will be looked up in a set of hardcoded derives, and if
    that fails, as block proj/func to be subderived.

    [@ns/block@ or @/block@] As the above, only it's always a block.

    Edit:

    - In raw mode, note inserts the relevant degree, backspace deletes a char
    or a degree.

    - In note mode, a note replaces the last 'note' event after splitting ';',
    or adds one if there isn't one already.  Backspace removes the last note
    event.  If it was the only note expression, remove the event.

    - In method mode, it edits the last note event or adds one, just like note
    mode.
-}
module Derive.Note (d_note_track, stash_signal_if_wanted) where
import qualified Data.Char as Char
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Tree as Tree

import Util.Control
import qualified Ui.Event as Event
import qualified Ui.Events as Events
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
import Types


-- * note track

-- | Top level deriver for note tracks.
d_note_track :: ([TrackTree.EventsNode] -> Derive.NoteDeriver)
    -- ^ This is used to derive orphans, as documented by
    -- 'EvalTrack.derive_note_track'.
    -> TrackTree.EventsNode -> Derive.NoteDeriver
d_note_track derive_tracks (Tree.Node track subs) =
    title $ derive_notes derive_tracks (track_info track subs) events
    where
    events = Events.ascending (TrackTree.track_events track)
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
    Control.put_unwarped_signal block_id track_id signal
    where signal = extract_track_signal source (LEvent.events_of events)

extract_track_signal :: Track.RenderSource -> [Score.Event] -> Signal.Control
extract_track_signal source events = mconcat $ case source of
    Track.Control control -> mapMaybe (extract_control control) events
    Track.Pitch control -> mapMaybe (extract_pitch control) events
    where
    extract_control control = fmap Score.typed_val . Map.lookup control
        . Score.event_controls
    extract_pitch Nothing event = Just $ convert event $ Score.event_pitch event
    extract_pitch (Just control) event =
        fmap (convert event) $ Map.lookup control $ Score.event_pitches event
    convert event psig = Signal.coerce $ fst $ PitchSignal.to_nn $
        -- Since these signals will be mconcatted into one signal, I don't
        -- want one event's control at 0 to wipe out the previous events.
        PitchSignal.drop_before_strict (Score.event_min event) $
        PitchSignal.apply_controls (Score.event_environ event)
            (Score.event_controls event) psig

with_title :: TrackTree.EventsTree -> ScoreTime -> Text -> Derive.NoteDeriver
    -> Derive.NoteDeriver
with_title subs end title deriver
    | Text.all Char.isSpace title = deriver
    | otherwise = do
        track_expr <- either (Derive.throw . ("track title: "++)) return
            (ParseTitle.parse_note title)
        Eval.apply_transformers cinfo (NonEmpty.toList track_expr) deriver
    where
    cinfo = (Derive.dummy_call_info 0 end "note track")
        { Derive.info_sub_tracks = subs }

derive_notes :: ([TrackTree.EventsNode] -> Derive.NoteDeriver)
    -> EvalTrack.TrackInfo -> [Event.Event] -> Derive.NoteDeriver
derive_notes derive_tracks tinfo events = do
    state <- Derive.get
    let (event_groups, collect) =
            EvalTrack.derive_note_track derive_tracks state tinfo events
    Internal.merge_collect collect
    return $ Derive.merge_asc_events event_groups

track_info :: TrackTree.Track -> [TrackTree.EventsNode] -> EvalTrack.TrackInfo
track_info track subs = EvalTrack.TrackInfo
    { EvalTrack.tinfo_track = track
    , EvalTrack.tinfo_sub_tracks = subs
    , EvalTrack.tinfo_type = ParseTitle.NoteTrack
    }
