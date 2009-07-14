{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- Control.Monad
{- | Derive events on a note track.  This is the Derive equivalent of
    'Cmd.NoteTrack', but has a different name to avoid clashes.

    Note tracks have a \"function call\" abstraction mechanism.  An event in
    one block is expanded into the events derived from the block it names, and
    so on recursively.

    The sub-block is passed its parent's tempo map (along with all the other
    controllers in the environment) to interpret as it will, so that it may,
    for example, set absolute tempo.  The generated events should begin with
    the given start time, but are not guaranteed to last for the given
    duration.

    The parse section parses note track events as manipulated by
    'Cmd.NoteTrack'.

    Note track language:
    There are two kinds of statements: pitch, and call.  Pitch is separate
    because it's so common and should be minimal, and it consists of (Method,
    ScaleDegree).  Call is for the rest, and is distinguished by starting with
    a symbol (i.e. non-alphanumeric printable character).  Calls are looked up
    in a Deriver namespace based on the symbol, and the Deriver is run.  For
    instance, the &xyz deriver can call xyz, and the < deriver can subderive
    the named block.  I'll probably have a simple value system with numbers,
    strings (perhaps barewords?), variables (come from the controller
    environ?), and scale degrees (specially marked so it doesn't look like
    a bareword).

    More than one statement can be put in an event by separating them with
    semicolons.

    Editing, use cases:
    just enter notes: "5a-"
    edit the method of a note: "i,5a-"
    call a function that takes notes: "&t *5a- *5b- *5c-"
    sub-block: "<ns/block" "<block"

    Examples:
    5a-     <-- simple note
    i, 5a-  <-- linear approach to 5a-
    3e, 5a- <-- exp**3 approach
    &t *5a- *5b- *5c-   <-- call "&t" (tuple) with three Notes, triplet

    5a- pizz            <-- pizz should actually go in the instrument track
    5a- +trem +sfz      <-- so instrument has a set of attributes, as sibelius
    &grace *5a- *5c-    <-- two note args to function "&grace"
    <pattern *a- *5c-   <-- pass two args to block deriver?

    It seems bogus to differentiate between block and function derivation,
    but I do want to be explicit about which namespace I'm looking in for the
    function.

    Edit:
    In raw mode, note inserts the relevant degree, backspace deletes a char or
    a degree.
    In note mode, a note replaces the last 'note' event after splitting ';',
    or adds one if there isn't one already.  Backspace removes the last note
    event.  If it was the only note expression, remove the event.
    In method mode, it edits the last note event or adds one, just like note
    mode.
-}
module Derive.Note where
import Control.Monad
import qualified Control.Monad.Identity as Identity
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Text.ParserCombinators.Parsec as P

import Util.Control ((#>>))
import qualified Util.Log as Log
import Util.Seq as Seq

import Ui.Types
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Ui.Track as Track

import qualified Derive.Controller as Controller
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import Util.Debug


type NoteParser m = Track.PosEvent -> Derive.DeriveT m [Parsed]

data Parsed =
    ParsedNote (Maybe Signal.Method) Pitch.Pitch
    | ParsedCall String
    deriving (Eq, Show)

-- * instrument track

d_note_track :: (Monad m) => Pitch.ScaleMap -> NoteParser m
    -> Derive.TrackDeriver m
d_note_track scales note_parser track_id = do
    track <- Derive.get_track track_id
    let events = Track.event_list (Track.track_events track)
    parsed_psig <- fmap splice_notes (parse_events note_parser events)
    pitch_signal <- extract_pitch_signal scales parsed_psig
    Derive.with_controller Score.pitch pitch_signal
        (derive_notes (map fst parsed_psig))

data ParsedEvent = ParsedEvent {
    parsed_text :: String
    , parsed_val :: Parsed
    , parsed_start :: TrackPos
    , parsed_dur :: TrackPos
    } deriving (Eq, Show)

type PitchSignal = [(TrackPos, Signal.Method, Pitch.Pitch)]

derive_notes :: (Monad m) => [ParsedEvent] -> Derive.DeriveT m [Score.Event]
derive_notes parsed = fmap concat (mapM note parsed)
    where
    note parsed = Derive.with_stack_pos
        (parsed_start parsed) (parsed_dur parsed) (derive_note parsed)

-- | A note is spliced with all following notes that have methods set.  This is
-- so they will describe a pitch curve and not retrigger a new note at each
-- point in the curve.
--
-- This will happen even if there's a lot of silence in between the notes.
-- I'm not sure if that's a good thing or not, but it's simpler to splice
-- unconditionally.
splice_notes :: [ParsedEvent] -> [(ParsedEvent, PitchSignal)]
splice_notes [] = []
splice_notes (parsed:rest) =
    (parsed { parsed_dur = dur }, psig) : splice_notes post
    where
    (splice, post) = span has_method rest
    dur = if null splice then parsed_dur parsed else let final = last splice
        in parsed_start final + parsed_dur final - parsed_start parsed
    psig = case parsed of
        ParsedEvent { parsed_val = ParsedNote maybe_method pitch } ->
            (parsed_start parsed, from_meth maybe_method, pitch)
                : [(pos, meth, pitch) | ParsedEvent { parsed_start = pos,
                    parsed_val = ParsedNote (Just meth) pitch } <- splice]
        -- Uknonwn initial pitch, so don't bother creating any signal.
        _ -> []
    from_meth = Maybe.fromMaybe Signal.Set
    has_method (ParsedEvent { parsed_val = ParsedNote (Just _) _ }) = True
    has_method _ = False

derive_note :: (Monad m) => ParsedEvent -> Derive.DeriveT m [Score.Event]
derive_note parsed = do
    -- TODO when signals are lazy this will be inefficient.  I need to come
    -- up with a way to guarantee such accesses are increasing and let me gc
    -- the head.
    start <- Derive.local_to_global (parsed_start parsed)
    -- Log.debug $ show (parsed_text parsed) ++ ": local global "
    --     ++ show (parsed_start parsed, start)
    -- warp <- fmap Derive.state_warp Derive.get
    -- Log.debug $ "warp " ++ show warp

    end <- Derive.local_to_global (parsed_start parsed + parsed_dur parsed)
    st <- Derive.get
    case parsed_val parsed of
            -- The (meth, pitch) is ignored here because it's already in the
            -- pitch signal.
        ParsedNote _ _ -> return [Score.Event start (end-start)
            (parsed_text parsed) (Derive.state_controllers st)
            (Derive.state_stack st) (Derive.state_instrument st)]
        -- d_call will set shift and stretch which is in local time, so pass
        -- local rather than global.
        -- TODO look in namespace for calls other than subderive
        ParsedCall call -> Derive.d_sub_derive []
            (d_call (parsed_start parsed) (parsed_dur parsed) call)

d_call :: TrackPos -> TrackPos -> String -> Derive.EventDeriver
d_call start dur ident = do
    -- TODO also I'll want to support generic calls
    default_ns <- fmap (State.state_project . Derive.state_ui) Derive.get
    let block_id = Block.BlockId (make_id default_ns ident)
    stack <- fmap Derive.state_stack Derive.get
    -- Since there is no branching, any recursion will be endless.
    when (block_id `elem` [bid | (bid, _, _) <- stack]) $
        Derive.throw $ "recursive block derivation: " ++ show block_id
    -- Stretch call to fit in duration, based on the block length.
    -- An alternate approach would be no stretch, but just clip, but I'm
    -- not sure how to indicate that kind of derivation.
    -- This is actually the only thing that requires block_id name a real
    -- block.
    ui_state <- fmap Derive.state_ui Derive.get
    block_dur <- either (Derive.throw . ("getting block end: "++) . show)
        return (State.eval ui_state (State.ruler_end block_id))
    if block_dur > TrackPos 0
        then Derive.d_at start (Derive.d_stretch (dur/block_dur)
            (Derive.d_block block_id))
        else do
            Log.warn $ "block with zero duration: " ++ show block_id
            return []

-- | Make an Id from a string, relative to the current ns if it doesn't already
-- have one.
--
-- TODO move this to a more generic place since LanguageCmds may want it to?

make_id :: String -> String -> Id.Id
make_id default_ns ident_str = Id.id ns ident
    where
    (w0, w1) = break (=='/') ident_str
    (ns, ident) = if null w1 then (default_ns, w0) else (w0, drop 1 w1)

parse_events :: (Monad m) => NoteParser m -> [Track.PosEvent]
    -> Derive.DeriveT m [ParsedEvent]
parse_events note_parser events = do
    maybe_parsed <- mapM Derive.catch_warn (map note_parser events)
    return
        [ ParsedEvent (Event.event_text event) parsed start
            (Event.event_duration event)
        | ((start, event), Just event_parsed) <- zip events maybe_parsed
        , parsed <- event_parsed
        ]

-- Turn a sequence of events and their 'PitchSignal's into a plain Signal
-- representing the note number of the combined sequence.  This is where the
-- Scale abstraction is eliminated.
-- TODO: If I'm going to look up the scale in the instrument, I'll also need
-- the instrument here.
-- TODO: This currently happens too early.  Since I store the pitch signal as
-- nns, a sub-block with a relative pitch signal can only be nns, so non
-- tempered scales will not transpose properly.
extract_pitch_signal :: (Monad m) => Pitch.ScaleMap
    -> [(ParsedEvent, PitchSignal)] -> Derive.DeriveT m Signal.Signal
extract_pitch_signal scales parsed_psig = do
    let pitch_points = [ (pos, meth, pitch, pitch_to_val scales pitch)
            | (_, psig) <- parsed_psig, (pos, meth, pitch) <- psig ]
        errors = [(pos, pitch) | (pos, _, pitch, Nothing) <- pitch_points]
        pos_list = [pos | (pos, _, _, Just _) <- pitch_points]
    unless (null errors) $
        -- This should never happen.
        Log.warn $ "notes not part of their scales: " ++ show errors
    -- TODO this won't be efficient with a lazy signal because I would need to
    -- compute it incrementally.
    warped <- mapM Derive.local_to_global pos_list
    return $ Signal.track_signal Signal.default_srate
        [ (pos, method, val)
        | (pos, (_, method, _, Just val)) <- zip warped pitch_points ]

-- | Convert the Pitch to a signal val.  This loses information, such as scale.
-- I think I'll need to get it back to e.g. transpose by scale degree, but
-- I'll worry about that later.
-- TODO
pitch_to_val :: Pitch.ScaleMap -> Pitch.Pitch -> Maybe Signal.Val
pitch_to_val scales pitch = do
    scale <-  Map.lookup (Pitch.pitch_scale pitch) scales
    Pitch.NoteNumber nn <- Pitch.scale_to_nn scale (Pitch.pitch_note pitch)
    return nn


-- * parser

-- | Parse scale degrees for a given scale.  This one just matches the scale
-- degrees directly, but a more complicated one may get scale values out of the
-- environment or something.
scale_parser :: (Monad m) => Pitch.Scale -> NoteParser m
scale_parser scale (_pos, event) = case either_parsed of
        Left err -> Derive.throw $ "parsing note: " ++ err
        Right parsed -> return [parsed]
    where
    either_parsed = default_parse_note scale (Event.event_text event)

default_parse_note:: Pitch.Scale -> String -> Either String Parsed
default_parse_note scale text = default_parse_note_val scale (Seq.strip text)

-- | Try to parse a note track event.  It's a little finicky because
-- I want to be unambiguous but also not ugly.  I think I can manage that by
-- using \< to disambiguate a call when it occurs alone, and always insert the
-- commas otherwise.
--
-- > \<call -> ParsedCall "<call"
-- > note -> ParsedNote (Just Set) note
-- > i, note -> ParsedNote (Just Linear) note
-- > i, -> Left error
--
-- TODO relative pitches: +4, -1/0 (oct/degree), +1/2+3 +1/2-3 (oct/degree+hz)
default_parse_note_val :: Pitch.Scale -> String -> Either String Parsed
default_parse_note_val scale text
    | is_call text = Right (ParsedCall text)
    | otherwise = do
        (method_s, pitch_s) <- tokenize_note text
        method <- if null method_s
            then Right Nothing else fmap Just (parse_method method_s)
        pitch <- if null pitch_s then Left "no pitch" else
            maybe (Left ("note not in scale: " ++ show pitch_s)) Right
                (Pitch.pitch scale pitch_s)
        return (ParsedNote method pitch)
    where
    parse_method s = case P.parse (Controller.p_method #>> P.eof) "" s of
        Left _ -> Left $ "couldn't parse method: " ++ show s
        Right v -> Right v

is_call (c:_) =
    Char.isPrint c && not (Char.isSpace c) && not (Char.isAlphaNum c)
is_call "" = False

-- | Tokenization is separate from parsing so Cmd.NoteTrack can edit incomplete
-- and unparseable note events.

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
