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


    Negative duration:

    Arriving beats are implemented with negative duration.  An arriving beat is
    a beat which arrives at the end of a unit of time, instead of departing
    from the beginning.  This concept is usually associated with Javanese and
    Balinese music, but Western music includes arriving patterns such as
    cadences and grace notes, while Indonesian music includes departing
    patterns such as the \"bouncing ball note\".

    An arriving note is not a change in the sounding duration of the note, but
    is a higher level concept.  The harmonic scope of of a departing note
    extends after the note in the pitch context established by that note, while
    the scope of an arriving note precedes the sounding of the note, preparing
    for its arriving.  A note whose harmonic scope precedes the sounding of the
    note itself is naturally represented as an event with negative duration.

    However, since the physical sounding duration (which of course must always
    be positive) and harmonic scope of a note no longer coincide, the sounding
    duration of an arriving note is somewhat more implicit.  The rule is such:

    If the event lines up with the following event then the note will sound
    until the next note begins:

    @
        --|--|
          |++|++...
    @

    If the event doesn't line up, then the empty space is taken as a rest,
    and the note is played up until the arrival of the rest:

    @
        --|   --|
          |+++  |++...
    @

    An arriving note followed by a departing note will sound until the
    beginning of the departing note, and a departing note followed by an
    arriving note is unchanged.  This logic is implemented by the
    'Derive.process_negative_durations' function.

    Note that final note, if it is an arriving note, has an ambiguous duration
    (@...@).  This defaults to 'Derive.negative_duration_default', but you can
    specify the duration of the final note easily enough by making it
    a departing note.
-}
module Derive.Note where
import Util.Control

import Ui
import qualified Ui.Track as Track

import qualified Derive.Call as Call
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang


-- * note track

-- | Top level deriver for note tracks.
d_note_track :: BlockId -> TrackId -> Derive.EventDeriver
d_note_track block_id track_id = do
    track <- Derive.get_track track_id
    track_expr <- case TrackLang.parse (Track.track_title track) of
        Left err -> Derive.throw $ "track title: " ++ err
        Right expr -> return (preprocess_title expr)
    -- TODO event calls are evaluated in normalized time, but track calls
    -- aren't.  Should they be?
    let pos_events = Track.event_list (Track.track_events track)
    block_end <- Derive.get_block_dur block_id
    -- Unlike event evaluation, if the title evaluation throws, the whole block
    -- will abort.  This seems reasonable to me.
    result <- Call.apply_transformer info track_expr
        (derive_notes block_end pos_events)
    Derive.insert_event_damage =<< Derive.take_local_damage track_id
    return result
    where info = (derive_info, Derive.dummy_call_info "note track")

derive_notes :: ScoreTime -> [Track.PosEvent] -> Derive.EventDeriver
derive_notes block_end events = Derive.with_msg "note" $
    Derive.merge_asc_events <$>
        Call.derive_track block_end derive_info id (\_ _ -> Nothing) events

derive_info :: Call.DeriveInfo Derive.Events
derive_info = Call.DeriveInfo Derive.no_events Call.lookup_note_call

-- | It's convenient to tag a note track with @>inst@ to set its instrument.
-- Unfortunately, this is parsed as a call to @>inst@
preprocess_title :: Call.PreProcess
preprocess_title (TrackLang.Call (TrackLang.Symbol ('>':inst)) args : calls) =
    TrackLang.Call (TrackLang.Symbol "n") (mkinst inst : args) : calls
    where
    mkinst = TrackLang.Literal . TrackLang.VInstrument . Score.Instrument
preprocess_title expr = expr
