{- | Derive a block \"called\" from another block.

    This is the GUI level \"function call\" abstraction mechanism.  An event in
    one block is expanded into the events derived from the block it names, and
    so on recursively.  The event may pass arguments which will be substituted
    into the sub-block.

    The sub-block is passed its parent's tempo map (along with all the other
    controllers in the environment) to interpret as it will, so that it may,
    for example, set absolute tempo.  However, the notes should be within the
    calling range... how does this work in nyquist?

    Note abstraction:

-}
module Derive.Note where
import qualified Control.Monad.Identity as Identity
import qualified Data.Maybe as Maybe
import qualified Text.ParserCombinators.Parsec as P

import qualified Util.Log as Log
import Util.Control ((#>>))

import Ui.Types
import qualified Ui.Event as Event
import qualified Ui.Track as Track

import qualified Derive.Controller as Controller
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import qualified Derive.Twelve as Twelve


type NoteParser m = Track.PosEvent
    -> Derive.DeriveT m (Maybe (Signal.Method, Pitch.Pitch), Maybe String)

-- * instrument track

d_note_track :: (Monad m) => NoteParser m -> Derive.TrackDeriver m
d_note_track note_parser track_id = do
    track <- Derive.get_track track_id
    let events = Track.event_list (Track.track_events track)
    parsed <- parse_events note_parser events
    pitch_signal <- extract_pitch_signal parsed
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

extract_pitch_signal :: (Monad m) => [ParsedEvent]
    -> Derive.DeriveT m Signal.Signal
extract_pitch_signal parsed = do
    let pitch_points =
            [(start, method, pitch) | ParsedEvent { parsed_start = start,
                parsed_pitch = Just (method, pitch) } <- parsed]
        pos_list = map parsed_start parsed
    -- TODO this won't be efficient with a lazy signal because I need to
    -- compute it incrementally.
    warped <- mapM Derive.local_to_global pos_list
    return $ Signal.track_signal Signal.default_srate
        [(pos, method, pitch_to_val pitch)
            | (pos, (_, method, pitch)) <- zip warped pitch_points]

-- | Convert the Pitch to a signal val.  This loses information, such as scale.
-- I think I'll need to get it back to e.g. transpose by scale degree, but
-- I'll worry about that later.
pitch_to_val :: Pitch.Pitch -> Signal.Val
pitch_to_val (Pitch.Pitch _ (Pitch.NoteNumber nn)) = nn

derive_event :: (Monad m) => Derive.DeriveT m a -> Derive.DeriveT m (Maybe a)
derive_event deriver = Derive.catch_event deriver

-- * parser

-- | The idea is that a more complicated note parser may get scale values out
-- of the environment or something.
scale_parser :: (Monad m) => Twelve.Scale -> NoteParser m
scale_parser scale (_pos, event) =
    case parse_note scale (Event.event_text event) of
        Left err -> Derive.throw_event err
        Right parsed -> return parsed

-- | Try to parse a note track event.  It's a little finicky because
-- interpolation methods can look like scale degrees, which can look like
-- calls, so I just see what matches.
--
-- - i scl \<block -> ((i, scl), block))
--
-- - i scl -> ((i, scl), Nothing)
--
-- - 2.4e 7c#
--
-- - scl -> ((Set, scl), Nothing)
--
-- - block -> (Nothing, block)
parse_note :: Twelve.Scale -> String
    -> Either String (Maybe (Signal.Method, Pitch.Pitch), Maybe String)
parse_note scale text = case words text of
    [w0] -> case pitch w0 of
        Nothing -> Right (Nothing, Just w0)
        Just p -> Right (Just (Signal.Set, p), Nothing)
    [w0, w1] -> case method w0 of
        Nothing -> case pitch w0 of
            Nothing -> Left $ "expected scale degree, got " ++ show w0
            Just p -> Right (Just (Signal.Set, p), Just w1)
        Just meth -> case pitch w1 of
            Nothing -> Left $ "expected scale degree, got " ++ show w1
            Just p -> Right (Just (meth, p), Nothing)
    [w0, w1, w2] -> case (method w0, pitch w1) of
        (Just meth, Just p) -> Right (Just (meth, p), Just w2)
        _ -> Left $ "expected [method, scale], got " ++ show [w0, w1]
    ws -> Left $ "can't parse note event from " ++ show ws
    where
    pitch = Twelve.scale_to_pitch scale
    method = either (const Nothing) Just
        . P.parse (Controller.p_method #>> P.eof) ""


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
