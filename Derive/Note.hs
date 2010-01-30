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
    'process_negative_duration' function.

    Note that final note, if it is an arriving note, has an ambiguous duration
    (@...@).  This defaults to 'negative_duration_default', but you can specify
    the duration of the final note easily enough by making it a departing note.
-}
module Derive.Note where
import qualified Util.Seq as Seq

import Ui
import qualified Ui.Event as Event
import qualified Ui.Track as Track

import qualified Derive.Derive as Derive
import qualified Derive.TrackLang as TrackLang
import qualified Derive.Call.Basic as Call.Basic


-- | Notes with negative duration have an implicit sounding duration which
-- depends on the following note.  Meanwhile (and for the last note of the
-- score), they have this sounding duration.
negative_duration_default :: TrackPos
negative_duration_default = 1

-- * note track

d_note_track :: TrackId -> Derive.EventDeriver
d_note_track track_id = do
    track <- Derive.get_track track_id
    let pos_events = Track.event_list (Track.track_events track)
    title_expr <- case TrackLang.parse (Track.track_title track) of
        Left err -> Derive.throw $ "track title: " ++ err
        Right expr -> return expr
    note_derivers <- derive_notes pos_events
    note_derivers <- eval "title" title_expr (Derive.Transformer note_derivers)
    sequence_notes note_derivers

sequence_notes :: Derive.OrderedNoteDerivers -> Derive.EventDeriver
    -- TODO concat here assumes they are non-overlapping.  I suppose I should
    -- use merge?
sequence_notes = fmap concat . mapM place . Seq.zip_next
    where
    place (Derive.NoteDeriver pos dur deriver, maybe_next)
        | dur == 0 = do
            Derive.with_stack_pos pos dur $
                Derive.warn "omitting note with 0 duration"
            return []
        | otherwise = with_dur (calculate_duration pos dur maybe_next) deriver
        where
        with_dur real_dur = Derive.d_at pos . Derive.d_stretch real_dur
            . Derive.with_stack_pos pos dur

calculate_duration :: TrackPos -> TrackPos -> Maybe Derive.NoteDeriver
    -> TrackPos
calculate_duration cur_pos cur_dur (Just next)
    -- TODO this doesn't handle the duration of notes at the end of a block.
    -- Unfortunately it seems hard to do that, because I need to know the
    -- start time of the next *event* which is first hard because that's global
    -- time and secondly hard because I only know the next deriver.  However,
    -- it's hard to run this as a post processor because by the time that
    -- events are produced I've lost the info about who follows who on a track.
    -- Though I suppose I could look at the stack for each event...
        -- Departing notes are not changed.
    | Event.is_positive_duration cur_dur = cur_dur
        -- Arriving followed by arriving with a rest in between extends to
        -- the arrival of the rest.
    | Event.is_negative_duration next_dur && rest > 0 = rest
        -- Arriving followed by arriving with no rest, or an arriving note
        -- followed by a departing note will sound until the next note.
    | otherwise = next_pos - cur_pos
    where
    next_pos = Derive.n_start next
    next_dur = Derive.n_duration next
    rest = next_pos + next_dur - cur_pos
calculate_duration _ cur_dur Nothing
    | Event.is_positive_duration cur_dur = cur_dur
    | otherwise = negative_duration_default

-- * parse

-- ** directive

{-
is_directive :: String -> Bool
is_directive (';':_) = True
is_directive _ = False

parse_directive :: Score.Event -> Maybe (Either String Call)
parse_directive event = case Score.event_string event of
    c : rest | c == directive_prefix -> Just (parse_directive_text rest)
    _ -> Nothing

parse_directive_text :: String -> Either String Call
parse_directive_text text = do
    (tokens, call_tokens) <- TrackLang.parse text
    when (not (null call_tokens)) $
        Left $ "calls not supported for directives: " ++ show call_tokens
    parse_call tokens

directive_prefix :: Char
directive_prefix = ';'
-}

-- * derive

derive_notes :: [Track.PosEvent] -> Derive.CallResult
derive_notes = fmap concat . mapM note . Seq.zip_next
    where
    note ((pos, event), next) = Derive.with_stack_pos pos
        (Event.event_duration event) (derive_note pos event (fmap fst next))

derive_note :: TrackPos -> Event.Event -> Maybe TrackPos -> Derive.CallResult
derive_note pos event next_note
        -- Events that are entirely comment are skipped entirely, see
        -- module comment.
    | Event.event_string event == "--" = return []
    | otherwise = case TrackLang.parse (Event.event_string event) of
        Left err -> Derive.warn err >> return []
        Right expr -> eval "note" expr (Derive.Generator (pos, event) next_note)

eval :: String -> TrackLang.Expr -> Derive.CallType -> Derive.CallResult
eval caller expr call_type = do
    val <- go call_type (reverse expr)
    case val of
        Left err -> do
            Derive.warn $ "eval " ++ caller ++ " "
                ++ TrackLang.show_type_error err
            return []
        Right derivers -> return derivers
    where
    go call_type (TrackLang.Call call_id args : rest) = do
        call <- Call.Basic.lookup_note_call call_id
        case call args call_type of
            Left err -> return $ Left err
            Right d -> do
                note_derivers <- d
                go (Derive.Transformer note_derivers) rest
    go (Derive.Transformer note_derivers) [] = return $ Right note_derivers
    go (Derive.Generator event _) [] = Derive.throw $
        "event with no calls at all (this shouldn't happen): " ++ show event

{-
d_eq :: TrackLang.ValName -> TrackLang.Val -> (Maybe Derive.EventDeriver)
    -> Derive.EventDeriver
d_eq sym val maybe_deriver = do
    pos <- Derive.local_to_global 0
    case maybe_deriver of
        Nothing -> Derive.insert_val sym pos val >> return []
        Just deriver -> Derive.with_val sym pos val deriver

d_equal :: TrackLang.ValName -> TrackLang.Val -> Derive.Deriver ()
d_equal sym val = do
    pos <- Derive.local_to_global 0
    Derive.insert_val sym pos val

d_equal_m :: TrackLang.ValName -> TrackLang.Val -> Derive.EventDeriver
    -> Derive.EventDeriver
d_equal_m sym val deriver = do
    pos <- Derive.local_to_global 0
    Derive.with_val sym pos val deriver
-}
