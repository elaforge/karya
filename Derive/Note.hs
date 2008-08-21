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
-}
module Derive.Note where
import Control.Monad
import qualified Control.Monad.Identity as Identity
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


type NoteParser m = Track.PosEvent
    -> Derive.DeriveT m (Maybe Signal.Method, Maybe Pitch.Pitch, Maybe String)

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
    , parsed_start :: TrackPos
    , parsed_dur :: TrackPos
    , parsed_method :: Maybe Signal.Method
    , parsed_pitch :: Maybe Pitch.Pitch
    , parsed_call :: Maybe String
    } deriving (Show)

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
        ParsedEvent { parsed_start = start, parsed_pitch = Just pitch } ->
            (start, Maybe.fromMaybe Signal.Set (parsed_method parsed), pitch)
                : [(pos, meth, pitch) | (ParsedEvent { parsed_start = pos,
                    parsed_method = Just meth, parsed_pitch = Just pitch })
                        <- splice]
            -- Uknonwn initial pitch, so don't bother creating any signal.
        _ -> []
    has_method (ParsedEvent { parsed_method = Just _ }) = True
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
    case parsed_call parsed of
        Nothing -> return [Score.Event start (end-start) (parsed_text parsed)
            (Derive.state_controllers st) (Derive.state_stack st)
            (Derive.state_instrument st)]
        -- d_call will set shift and stretch which is in local time, so pass
        -- local rather than global.
        Just call -> Derive.d_sub_derive []
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
    maybe_parsed <- mapM derive_event (map note_parser events)
    return
        [ ParsedEvent (Event.event_text event) start
            (Event.event_duration event) meth pitch call
        | ((start, event), Just (meth, pitch, call)) <- zip events maybe_parsed]

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

derive_event :: (Monad m) => Derive.DeriveT m a -> Derive.DeriveT m (Maybe a)
derive_event deriver = Derive.catch_warn deriver


-- * parser

-- | The idea is that a more complicated note parser may get scale values out
-- of the environment or something.
scale_parser :: (Monad m) => Pitch.Scale -> NoteParser m
scale_parser scale (_pos, event) =
    case default_parse_note scale (Event.event_text event) of
        Left err -> Derive.throw err
        Right parsed -> return parsed

-- | Try to parse a note track event.  It's a little finicky because
-- I want to be unambiguous but also not ugly.  I think I can manage that by
-- using \< to disambiguate a call when it occurs alone, and always insert the
-- commas otherwise.
--
-- TODO Unfortunately I still have a problem with shift since I can't type _,
-- but I'll deal with that later if it becomes a problem.
--
-- > note -> ((Set, note), Nothing)
-- > \<call -> (Nothing, block)
-- > i, note, \<call -> ((i, note), call))
-- > i, note -> ((i, note), Nothing)
-- > i, , \<call -> (Nothing, call)
default_parse_note :: Pitch.Scale -> String
    -> Either String (Maybe Signal.Method, Maybe Pitch.Pitch, Maybe String)
default_parse_note scale text = do
    (method_s, pitch_s, call) <- tokenize_note text
    method <- if null method_s
        then Right Nothing else fmap Just (parse_method method_s)
    pitch <- if null pitch_s then Right Nothing else
        maybe (Left ("note not in scale: " ++ show pitch_s)) (Right . Just)
            (Pitch.pitch scale pitch_s)
    return (method, pitch, to_maybe call)
    where
    to_maybe s = if null s then Nothing else Just s
    parse_method s = case P.parse (Controller.p_method #>> P.eof) "" s of
        Left _ -> Left $ "couldn't parse method: " ++ show s
        Right v -> Right v


-- | Tokenization is separate from parsing so Cmd.NoteTrack can edit incomplete
-- and unparseable note events.
--
-- (method, pitch, call)
type NoteTokens = (String, String, String)

tokenize_note :: String -> Either String NoteTokens
tokenize_note text = fmap drop_third $ case Seq.split ", " text of
    [] -> Right ("", "", "")
    [w0]
        | is_call w0 -> Right ("", "", w0)
        | otherwise -> Right ("", w0, "")
    [w0, w1] -> Right (w0, w1, "")
    [w0, w1, w2] -> Right (w0, w1, w2)
    ws -> Left $ "too many words in note: " ++ show ws
    where
    is_call = (=="<") . take 1
    drop_third (a, b, c) = (a, b, drop 1 c) -- drop off the '<'

untokenize_note :: NoteTokens -> String
untokenize_note (a, b, c) = case (a, b, if null c then c else '<':c) of
    ("", "", "") -> ""
    ("", "", call) -> call
    ("", note, "") -> note
    (meth, pitch, call) -> let toks = Seq.rdrop_while null [meth, pitch, call]
        in Seq.join ", " (if length toks == 1 then toks ++ [""] else toks)


-- One of the early proponents of this style during the renaissance was
-- Johannes Fux, who was surpassed in unfortunateness only by the much-loved
-- Count Fux Dux.
