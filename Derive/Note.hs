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
import Control.Monad
import qualified Data.List as List
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


data NoteDesc = NoteDesc
    { note_args :: [TrackLang.Val]
    , note_inst :: Maybe Score.Instrument
    , note_attrs :: Score.Attributes
    } deriving (Eq, Show)
empty_note = NoteDesc [] Nothing Score.no_attrs

data Call = Call TrackLang.CallId [TrackLang.Val]
    deriving (Eq, Show)

-- * note track

d_note_track :: TrackId -> Derive.EventDeriver
d_note_track track_id = do
    track <- Derive.get_track track_id
    let pos_events = Track.event_list (Track.track_events track)
    (ndesc, calls) <- parse_track_title (Track.track_title track)
    events <- derive_notes =<< parse_events ndesc pos_events
    foldM call events calls

call :: [Score.Event] -> Call -> Derive.EventDeriver
call events (Call call_id args) = do
    call <- Derive.lookup_note_call call_id
    let left err = Derive.throw $
            "parse error: " ++ TrackLang.show_type_error err
    join $ either left return (call args events)

parse_track_title :: (Monad m) => String -> Derive.DeriveT m (NoteDesc, [Call])
parse_track_title title = do
    (inst_tokens, call_tokens) <-
        either (Derive.throw . ("parsing track title: " ++)) return
        (TrackLang.parse title)
    inst <- Derive.gets Derive.state_instrument
    attrs <- Derive.gets Derive.state_attributes
    let ndesc = parse_note_desc (NoteDesc [] inst attrs) inst_tokens
    calls <- mapM parse_call call_tokens
    return (ndesc, calls)

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

-- | Split the call and args from a list of Vals.
parse_call :: (Monad m) => [TrackLang.Val] -> Derive.DeriveT m Call
parse_call args = either (Derive.throw . ("parse_call: "++)) return $ do
    (call_id, args) <- case args of
        [] -> Left "empty expression"
        arg:args -> case arg of
            TrackLang.VCall call_id -> Right (call_id, args)
            _ -> Left $ "non-function in function position: " ++ show arg
    let bad_calls = [cid | TrackLang.VCall cid <- args]
    when (not (null bad_calls)) $
        Left $ "functions in non-function position: " ++ show bad_calls
    return $ Call call_id args

-- ** parse

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
parse_event ndesc (pos, event) = do
    let err msg = Derive.with_stack_pos pos (Event.event_duration event) $
            Derive.throw ("parsing note: " ++ msg)
    (inst_tokens, call_tokens) <- either err return $
        TrackLang.parse (Event.event_string event)
    when (not (null call_tokens)) $
        -- TODO support pipes in notes
        Derive.throw $ "note calls not yet supported: " ++ show call_tokens
    return $ parse_note_desc ndesc inst_tokens

-- ** derive

derive_notes :: (Monad m) => [(Track.PosEvent, NoteDesc)]
    -> Derive.DeriveT m [Score.Event]
derive_notes = fmap (trim_pitches . concat) . mapM note
    where
    note ((pos, event), ndesc) = Derive.with_stack_pos
        pos (Event.event_duration event) (derive_note pos event ndesc)

derive_note :: (Monad m) => TrackPos -> Event.Event -> NoteDesc
    -> Derive.DeriveT m [Score.Event]
derive_note pos event (NoteDesc args inst attrs) = do
    -- TODO when signals are lazy this will be inefficient.  I need to come
    -- up with a way to guarantee such accesses are increasing and let me gc
    -- the head.
    start <- Derive.local_to_global pos
    end <- Derive.local_to_global (pos + Event.event_duration event)
    -- Log.debug $ show (Event.event_string event) ++ ": local global "
    --     ++ show ((pos, start), (pos + Event.event_duration event, end))
    -- Derive.Warp wp stretch shift <- Derive.gets Derive.state_warp
    -- Log.debug $ "warp " ++ (show wp)
    st <- Derive.get

    case args of
        -- TODO when signals are lazy I should drop the heads of the control
        -- signals so they can be gced.
        [] -> return [Score.Event start (end-start) (Event.event_text event)
            (Derive.state_controls st) (Derive.state_pitch st)
            (Derive.state_stack st) inst attrs]
        _ -> do
            Call sub args <- parse_call args
            -- d_sub will set shift and stretch which is in local time, so pass
            -- local rather than global.
            -- TODO look in namespace other than blocks for sub_id
            -- TODO pass args
            when (not (null args)) $
                Derive.throw $ "args not supported yet: " ++ show args
            Derive.d_sub_derive [] $ Derive.with_instrument inst attrs $
                d_sub pos (Event.event_duration event) sub

-- | In a note track, the pitch signal for each note ends when the next note
-- begins.  Otherwise, it looks like each note changes pitch when the next note
-- begins.  Of course, the note is already \"done\" at this point, but the
-- decay time means it may still be audible.
trim_pitches :: [Score.Event] -> [Score.Event]
trim_pitches events = map trim_event (Seq.zip_next events)
    where
    trim_event (event, Nothing) = event
    trim_event (event, Just next) =
        event { Score.event_pitch = PitchSignal.truncate p psig }
        where
        psig = Score.event_pitch event
        p = Score.event_start next

d_sub :: TrackPos -> TrackPos -> TrackLang.CallId -> Derive.EventDeriver
d_sub start dur call_id = do
    -- TODO also I'll want to support generic calls
    default_ns <- Derive.gets (State.state_project . Derive.state_ui)
    let block_id = Types.BlockId (make_id default_ns call_id)
    stack <- Derive.gets Derive.state_stack
    -- Since there is no branching, any recursion will be endless.
    when (block_id `elem` [bid | (bid, _, _) <- stack]) $
        Derive.throw $ "recursive block derivation: " ++ show block_id
    -- Stretch call to fit in duration, based on the block length.
    -- An alternate approach would be no stretch, but just clip, but I'm
    -- not sure how to indicate that kind of derivation.
    -- This is actually the only thing that requires that block_id name a real
    -- block.
    ui_state <- Derive.gets Derive.state_ui
    -- This is the first time I look up block_id so if the block does't exist
    -- this will throw.
    block_dur <- either (Derive.throw . ("d_sub: "++) . show) return
        (State.eval ui_state (get_block_dur block_id))
    if block_dur > TrackPos 0
        then Derive.d_at start (Derive.d_stretch (dur/block_dur)
            (Derive.d_block block_id))
        else do
            Derive.warn $ "block with zero duration: " ++ show block_id
            return []

-- | Sub-derived blocks are stretched according to their length, and this
-- function defines the length of a block.  'event_end' seems the most
-- intuitive, but then you can't make blocks with trailing space.  You can
-- work around it though by appending a comment dummy event.
get_block_dur :: (State.UiStateMonad m) => BlockId -> m TrackPos
get_block_dur = State.event_end


-- | Make an Id from a string, relative to the current ns if it doesn't already
-- have one.
--
-- TODO move this to a more generic place since LanguageCmds may want it to?

make_id :: String -> TrackLang.CallId -> Id.Id
make_id default_ns (TrackLang.CallId ident_str) = Id.id ns ident
    where
    (w0, w1) = break (=='/') ident_str
    (ns, ident) = if null w1 then (default_ns, w0) else (w0, drop 1 w1)


-- * old crap kept alive by Cmd.NoteTrack

-- | (method, note)
type NoteTokens = (String, String)

tokenize_note :: String -> Either String NoteTokens
tokenize_note text = case Seq.split_commas text of
    [] -> Right ("", "")
    [w0] -> Right ("", w0)
    [w0, w1] -> Right (w0, w1)
    ws -> Left $ "too many words in note: " ++ show ws

untokenize_note :: NoteTokens -> String
untokenize_note toks = case toks of
    ("", "") -> ""
    ("", note) -> note
    (meth, note) -> meth ++ "," ++ note
