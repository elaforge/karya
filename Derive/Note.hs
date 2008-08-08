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
import qualified Ui.Event as Event
import qualified Ui.Track as Track

import qualified Derive.Controller as Controller
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal


type NoteParser m = Track.PosEvent
    -> Derive.DeriveT m (Maybe (Signal.Method, Pitch.Pitch), Maybe String)

-- * instrument track

d_note_track :: (Monad m) => Pitch.ScaleMap -> NoteParser m
    -> Derive.TrackDeriver m
d_note_track scales note_parser track_id = do
    track <- Derive.get_track track_id
    let events = Track.event_list (Track.track_events track)
    parsed <- parse_events note_parser events
    pitch_signal <- extract_pitch_signal scales parsed
    Derive.with_controller Score.pitch pitch_signal (derive_notes parsed)

data ParsedEvent = ParsedEvent {
    parsed_text :: String
    , parsed_start :: TrackPos
    , parsed_dur :: TrackPos
    , parsed_pitch :: Maybe (Signal.Method, Pitch.Pitch)
    , parsed_sub :: Maybe String
    } deriving (Show)

derive_notes :: (Monad m) => [ParsedEvent] -> Derive.DeriveT m [Score.Event]
derive_notes parsed = fmap concat (mapM derive_note parsed)

derive_note :: (Monad m) => ParsedEvent -> Derive.DeriveT m [Score.Event]
derive_note parsed = do
    -- TODO when signals are lazy this will be inefficient.  I need to come
    -- up with a way to guarantee such accesses are increasing and let me gc
    -- the head.
    start <- Derive.local_to_global (parsed_start parsed)
    end <- Derive.local_to_global (parsed_start parsed + parsed_dur parsed)
    st <- Derive.get
    case parsed_sub parsed of
        Nothing -> return [Score.Event start (end-start) (parsed_text parsed)
            (Derive.state_controllers st) (Derive.state_stack st)
            (Derive.state_instrument st)]
        Just ('<':block_name) -> error "block subderive unimplemented"
        Just sub -> error $ "call subderive unimplemented for " ++ show sub

parse_events :: (Monad m) => NoteParser m -> [Track.PosEvent]
    -> Derive.DeriveT m [ParsedEvent]
parse_events note_parser events = do
    maybe_parsed <- mapM derive_event (map note_parser events)
    return
        [ ParsedEvent (Event.event_text event) start
            (Event.event_duration event) pitch sub
        | ((start, event), Just (pitch, sub)) <- zip events maybe_parsed]

extract_pitch_signal :: (Monad m) => Pitch.ScaleMap -> [ParsedEvent]
    -> Derive.DeriveT m Signal.Signal
extract_pitch_signal scales parsed = do
    let pitch_points =
            [ (start, method, pitch, pitch_to_val scales pitch)
            | ParsedEvent { parsed_start = start,
                parsed_pitch = Just (method, pitch) } <- parsed ]
        pos_list = map parsed_start parsed
        errors = [(pos, pitch) | (pos, _, pitch, Nothing) <- pitch_points]
    unless (null errors) $
        -- This should never happen.
        Log.warn $ "notes not part of their scales: " ++ show errors
    -- TODO this won't be efficient with a lazy signal because I need to
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
derive_event deriver = Derive.catch_event deriver

-- * parser

-- | The idea is that a more complicated note parser may get scale values out
-- of the environment or something.
scale_parser :: (Monad m) => Pitch.Scale -> NoteParser m
scale_parser scale (_pos, event) =
    case parse_note scale (Event.event_text event) of
        Left err -> Derive.throw_event err
        Right parsed -> return parsed

-- | Try to parse a note track event.  It's a little finicky because
-- I want to be unambiguous but also not ugly.  I think I can manage that by
-- using < to disambiguate a call and not allowing (method, "", call).
--
-- TODO Unfortunately I still have a problem with shift since I can't type _,
-- but I'll deal with that later if it becomes a problem.
--
-- > i, scl, block -> ((i, scl), block))
-- > i, scl -> ((i, scl), Nothing)
-- > 2.4e, 7c#
-- > scl -> ((Set, scl), Nothing)
-- > \<block -> (Nothing, block)
-- > , , block -> (Nothing, block)
parse_note :: Pitch.Scale -> String
    -> Either String (Maybe (Signal.Method, Pitch.Pitch), Maybe String)
parse_note scale text = do
    (method_s, pitch_s, call) <- tokenize_note text
    if null pitch_s then return (Nothing, to_maybe call) else do
    method <- if null method_s
        then Right Signal.Set else parse_method method_s
    pitch <- maybe (Left ("note not in scale: " ++ show pitch_s)) Right
        (Pitch.pitch scale pitch_s)
    return (Just (method, pitch), to_maybe call)
    where
    to_maybe s = if null s then Nothing else Just s
    parse_method s = case P.parse (Controller.p_method #>> P.eof) "" s of
        Left _ -> Left $ "couldn't parse method: " ++ show s
        Right v -> Right v

tokenize_note :: String -> Either String (String, String, String)
tokenize_note text = fmap drop_third $ case Seq.split ", " text of
    [w0]
        | is_call w0 -> Right ("", "", w0)
        | otherwise -> Right ("", w0, "")
    [w0, w1]
        | is_call w1 -> Right ("", w0, w1)
        | otherwise -> Right (w0, w1, "")
    [w0, w1, w2] -> Right (w0, w1, w2)
    _ -> Left "too many words in note"
    where
    is_call = (=="<") . take 1
    drop_third (a, b, c) = (a, b, drop 1 c) -- drop off the '<'

untokenize_note ("", "", "") = ""
untokenize_note ("", "", call) = '<':call
untokenize_note (note_s, pitch_s, "") = join_note [note_s, pitch_s]
untokenize_note (note_s, pitch_s, call) = join_note [note_s, pitch_s, '<':call]
join_note = Seq.join ", " . filter (not . null)


-- * sub-derive

sub_derive :: (Monad m) => Derive.EventDeriver -> Derive.DeriveT m [Score.Event]
sub_derive deriver = do
    state <- Derive.get
    let (res, state2, logs) = Identity.runIdentity $ Derive.run state deriver
    case res of
            -- TODO Maybe I should save exceptions as-is.
        Left err -> do
            Log.warn $ "error sub-deriving: " ++ show err
            return []
        Right events -> do
            let pos_range = get_track_pos_range events
            Derive.modify (merge_states pos_range state2)
            return events

get_track_pos_range [] = (TrackPos 0, TrackPos 0)
get_track_pos_range events =
    (Score.event_start (head events), Score.event_end (last events))

merge_states :: (TrackPos, TrackPos) -> Derive.State -> Derive.State
    -> Derive.State
merge_states pos_range src dest = dest
    -- TODO use pos_range to insert into the WarpMap
    { Derive.state_track_warps = Derive.state_track_warps src
    }


-- One of the early proponents of this style during the renaissance was
-- Johannes Fux, who was surpassed in unfortunateness only by the much-loved
-- Count Fux Dux.
