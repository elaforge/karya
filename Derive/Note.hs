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
module Derive.Note where
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Tree as Tree

import Util.Control
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.TrackTree as TrackTree

import qualified Derive.Call as Call
import qualified Derive.Control as Control
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Score as Score
import qualified Derive.TrackInfo as TrackInfo
import qualified Derive.TrackLang as TrackLang

import Types


-- * note track

-- | Top level deriver for note tracks.
d_note_track :: TrackTree.EventsNode -> Derive.EventDeriver
d_note_track (Tree.Node track subs) = do
    stash_sub_signals subs
    with_title (TrackTree.tevents_title track) $ derive_notes
        (TrackTree.tevents_end track) (TrackTree.tevents_range track)
        (TrackTree.tevents_shifted track) subs (TrackTree.tevents_around track)
        (Events.ascending (TrackTree.tevents_events track))

with_title :: String -> Derive.EventDeriver -> Derive.EventDeriver
with_title title deriver
    | null title = return mempty
    | otherwise = do
        track_expr <- either (Derive.throw . ("track title: "++)) return
            (TrackInfo.parse_note title)
        let transform = if is_empty_title track_expr then id
                else Call.apply_transformer info (NonEmpty.toList track_expr)
        transform deriver
    where info = Derive.dummy_call_info 0 1 "note track"

is_empty_title :: TrackLang.Expr -> Bool
is_empty_title (TrackLang.Call sym
        [TrackLang.Literal (TrackLang.VInstrument inst)] :| []) =
    inst == Score.Instrument "" && sym == TrackLang.Symbol "note-track"
is_empty_title _ = False

stash_sub_signals :: TrackTree.EventsTree -> Derive.Deriver ()
stash_sub_signals subs = do
    let tracks = concatMap Tree.flatten subs
    sigs <- mapM Control.track_signal tracks
    Control.put_track_signals
        [(track_id, tsig) | (Just track_id, tsig)
            <- zip (map TrackTree.tevents_track_id tracks) sigs]

-- | It could just pass the 'TrackTree.TrackInfo', but this way
-- "Derive.Lazy_test" can pass an infinite events list.
derive_notes :: ScoreTime -> (ScoreTime, ScoreTime) -> ScoreTime
    -> TrackTree.EventsTree -> ([Event.Event], [Event.Event])
    -> [Event.Event] -> Derive.EventDeriver
derive_notes events_end track_range shifted subs events_around events = do
    -- You'd think 'd_note_track' should just pass TrackEvents, but then I
    -- can't test for laziness by passing an infinite events list.
    state <- Derive.get
    let (event_groups, collect) = Call.derive_track state tinfo
            (\_ _ -> Nothing) events
    Internal.merge_collect collect
    return $ Derive.merge_asc_events event_groups
    where
    tinfo = Call.TrackInfo
        { Call.tinfo_events_end = events_end
        , Call.tinfo_track_range = track_range
        , Call.tinfo_shifted = shifted
        , Call.tinfo_sub_tracks = subs
        , Call.tinfo_events_around = events_around
        , Call.tinfo_type = TrackInfo.NoteTrack
        }
