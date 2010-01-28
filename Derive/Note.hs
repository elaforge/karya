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
import Control.Monad
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Util.Seq as Seq

import Ui
import qualified Ui.Event as Event
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Types as Types

import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.PitchSignal as PitchSignal


-- | Notes with negative duration have an implicit sounding duration which
-- depends on the following note.  Meanwhile (and for the last note of the
-- score), they have this sounding duration.
negative_duration_default :: TrackPos
negative_duration_default = 1

data NoteDesc = NoteDesc
    { note_args :: [TrackLang.Val]
    , note_inst :: Maybe Score.Instrument
    , note_attrs :: Score.Attributes
    } deriving (Eq, Show)
empty_note = NoteDesc [] Nothing Score.no_attrs

data Call = Call { call_id :: TrackLang.CallId, call_args :: [TrackLang.Val] }
    deriving (Eq, Show)

-- * note track

d_note_track :: TrackId -> Derive.EventDeriver
d_note_track track_id = do
    track <- Derive.get_track track_id
    let pos_events = Track.event_list (Track.track_events track)
    (ndesc, calls) <- parse_track_title (Track.track_title track)
    note_derivers <- derive_notes =<< parse_events ndesc pos_events
    -- foldM call events calls
    sequence_notes note_derivers

sequence_notes :: Derive.OrderedNoteDerivers -> Derive.EventDeriver
    -- TODO concat here assumes they are non-overlapping.  I suppose I should
    -- use merge?
sequence_notes = fmap concat . mapM place . Seq.zip_next
    where
    place (Derive.NoteDeriver pos dur deriver, maybe_next) =
        with_dur (calculate_duration pos dur maybe_next) deriver
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
    dur = Event.event_duration
calculate_duration _ cur_dur Nothing
    | Event.is_positive_duration cur_dur = cur_dur
    | otherwise = negative_duration_default

call :: [Score.Event] -> Call -> Derive.EventDeriver
call events (Call call_id args) = do
    call <- Derive.lookup_note_call call_id
    let msg (TrackLang.CallId c) = "note call " ++ show c
    case call args events of
        Left err -> Derive.throw $
            "parse error: " ++ TrackLang.show_type_error err
        Right d -> Derive.with_msg (msg call_id) d

-- * parse

-- ** track title

parse_track_title :: (Monad m) => String -> Derive.DeriveT m (NoteDesc, [Call])
parse_track_title title = do
    (inst_tokens, call_tokens) <-
        either (Derive.throw . ("parsing track title: " ++)) return
        (TrackLang.parse title)
    inst <- Derive.gets Derive.state_instrument
    attrs <- Derive.gets Derive.state_attributes
    let ndesc = parse_note_desc (NoteDesc [] inst attrs) inst_tokens
    calls <- mapM do_parse_call call_tokens
    return (ndesc, calls)
    where
    do_parse_call args = either (Derive.throw . ("parse_track_title: " ++))
        return (parse_call args)

-- | Filter the inst and attr declarations out of the parsed Vals and put the
-- rest in 'note_args'.
parse_note_desc :: NoteDesc -> [TrackLang.Val] -> NoteDesc
parse_note_desc ndesc tokens = rev $ List.foldl' collect (rev ndesc) tokens
    where
    collect ndesc val = case val of
        TrackLang.VInstrument (Just inst) -> ndesc { note_inst = Just inst }
        TrackLang.VInstrument Nothing -> ndesc
        TrackLang.VAttr (TrackLang.Attr (mode, attr)) -> ndesc
            { note_attrs = TrackLang.set_attr mode attr (note_attrs ndesc) }
        _ -> ndesc { note_args = val : note_args ndesc }
    rev ndesc = ndesc { note_args = List.reverse (note_args ndesc) }

-- ** directive

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

-- ** events

-- | Parse each Event and pair it with a NoteDesc, filtering out the ones that
-- can't be parsed.
parse_events :: (Monad m) => NoteDesc -> [Track.PosEvent]
    -> Derive.DeriveT m [(Track.PosEvent, NoteDesc)]
parse_events ndesc pos_events = do
    maybe_ndescs <- mapM (Derive.catch_warn . (parse_event ndesc)) pos_events
    return [(pos_event, ndesc)
        | (pos_event, Just ndesc) <- zip pos_events maybe_ndescs]

parse_event :: Monad m => NoteDesc -> Track.PosEvent
    -> Derive.DeriveT m NoteDesc
parse_event ndesc (pos, event)
    | is_directive (Event.event_string event) = return empty_note
    | otherwise = do
        let err msg = Derive.with_stack_pos pos (Event.event_duration event) $
                Derive.throw ("parsing note: " ++ msg)
        (inst_tokens, call_tokens) <- either err return $
            TrackLang.parse (Event.event_string event)
        when (not (null call_tokens)) $
            -- TODO support pipes in notes
            Derive.throw $ "note calls not yet supported: " ++ show call_tokens
        return $ parse_note_desc ndesc inst_tokens

-- | Split the call and args from a list of Vals.
parse_call :: [TrackLang.Val] -> Either String Call
parse_call args = do
    (call_id, args) <- case args of
        [] -> Left "empty expression"
        arg:args -> case arg of
            TrackLang.VCall call_id -> Right (call_id, args)
            _ -> Left $ "non-function in function position: " ++ show arg
    let bad_calls = [cid | TrackLang.VCall cid <- args]
    when (not (null bad_calls)) $
        Left $ "functions in non-function position: " ++ show bad_calls
    return $ Call call_id args

-- * derive

derive_notes :: (Monad m) => [(Track.PosEvent, NoteDesc)]
    -> Derive.DeriveT m Derive.OrderedNoteDerivers
derive_notes = fmap Maybe.catMaybes . mapM note . Seq.zip_next
    where
    note (((pos, event), ndesc), next) =
        Derive.with_stack_pos pos (Event.event_duration event)
            (derive_note pos event ndesc (fmap (fst . fst) next))

derive_note :: (Monad m) => TrackPos -> Event.Event -> NoteDesc
    -> Maybe TrackPos -> Derive.DeriveT m (Maybe Derive.NoteDeriver)
derive_note pos event (NoteDesc args inst attrs) next_note
    | Event.event_duration event == 0 = do
        Derive.warn "omitting note with 0 duration"
        return Nothing
        -- Events that are entirely comment are skipped entirely, see
        -- module comment.
    | Event.event_string event == "--" = return Nothing
    | null args = do -- TODO lookup null call
        -- eventually this gets subsumed into call below, but it still has
        -- special treatment wrt negative durations
        return $ Just (note_deriver (d_note event (fmap to_local next_note)))
    | otherwise = do
        Call call_id args <- either (Derive.throw . ("derive_note: " ++))
            return (parse_call args)
        -- TODO look in namespace other than blocks for call_id
        -- TODO pass args
        when (not (null args)) $
            Derive.throw $ "args not supported yet: " ++ show args
        -- Derivation happens according to the extent of the note, not the
        -- duration.  This is how negative duration events begin deriving
        -- before arriving at the trigger.
        let (start, end) = (Track.event_min (pos, event),
                Track.event_max (pos, event))
        deriver <- d_call_block start (end-start) call_id
        return $ Just (note_deriver deriver)
    where
    note_deriver d = Derive.NoteDeriver pos (Event.event_duration event)
        (Derive.with_instrument inst attrs d)
    -- All notes are 0--1 and rely on NoteDeriver to place and stretch them.
    -- So I need to reverse the transformation for the next note pos so it is
    -- in that local time.
    to_local p = (p - pos) / Event.event_duration event

d_note :: Event.Event -> Maybe TrackPos -> Derive.EventDeriver
d_note event next_note = do
    start <- Derive.local_to_global 0
    end <- Derive.local_to_global 1
    next <- case next_note of
        Nothing -> return Nothing
        Just next -> fmap Just (Derive.local_to_global next)
    st <- Derive.get
    let inst = Derive.state_instrument st
        attrs = Derive.state_attributes st
        pitch_sig = trimmed_pitch next (Derive.state_pitch st)
    return [Score.Event start (end - start)
        (Event.event_text event) (Derive.state_controls st)
        pitch_sig (Derive.state_stack st) inst attrs]

d_call_block :: (Monad m) => TrackPos -> TrackPos -> TrackLang.CallId
    -> Derive.DeriveT m Derive.EventDeriver
d_call_block start dur call_id = do
    default_ns <- Derive.gets (State.state_project . Derive.state_ui)
    let block_id = Types.BlockId (make_id default_ns call_id)
    return $ Derive.d_sub_derive [] (Derive.d_block block_id)

-- | In a note track, the pitch signal for each note is constant as soon as the
-- next note begins.  Otherwise, it looks like each note changes pitch during
-- its decay.
-- TODO when signals are lazy I should drop the heads of the control
-- signals so they can be gced.
trimmed_pitch :: Maybe TrackPos -> PitchSignal.PitchSignal
    -> PitchSignal.PitchSignal
trimmed_pitch (Just next) sig = PitchSignal.truncate next sig
trimmed_pitch Nothing sig = sig


-- * util

-- | Make an Id from a string, relative to the current ns if it doesn't already
-- have one.
--
-- TODO move this to a more generic place since LanguageCmds may want it to?

make_id :: String -> TrackLang.CallId -> Id.Id
make_id default_ns (TrackLang.CallId ident_str) = Id.id ns ident
    where
    (w0, w1) = break (=='/') ident_str
    (ns, ident) = if null w1 then (default_ns, w0) else (w0, drop 1 w1)
