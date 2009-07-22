{-# LANGUAGE PatternGuards #-}
{- | Derivers for controller tracks.

    Interpolation methods:

    - s - Set value at the point.  This is the default if there is no method.

    - i - Approach with linear interpolation.

    - #e - Approach with exponential interpolation with #.  # defaults to 2.

    TODO
    - #a - Take # TrackPos to reach the value, starting at the point.  It's
    like @[(p, "v"), (p+#, "iv")]@.

    - method;val - Approach val with method, then jump to val.
-}
module Derive.Controller where
import Prelude hiding (lex)
import Control.Monad
import qualified Data.Char as Char
import qualified Data.Map as Map

import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<|>), (<?>))

import Util.Control ((#>>))

import qualified Ui.Event as Event
import qualified Ui.Track as Track
import Ui.Types

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import qualified Perform.Warning as Warning

import qualified Derive.Derive as Derive
import qualified Derive.Parse as Parse
import qualified Derive.Scale as Scale
import qualified Derive.Score as Score


-- | It's necessary for @eventsm@ to be monadic, since it needs to be run
-- inside the environment modified by d_controller.
d_controller :: (Monad m) => Score.Controller -> Derive.DeriveT m Signal.Signal
    -> Derive.DeriveT m [Score.Event] -> Derive.DeriveT m [Score.Event]
d_controller cont signalm eventsm = do
    signal <- signalm
    Derive.with_controller cont signal eventsm

-- | Get the signal events from a controller track.  They are meant to be
-- fed to 'd_signal' or 'd_pitch_signal', which will convert them into
-- a signal, and the whole thing passed as the signal deriver to
-- 'd_controller'.
d_controller_track :: (Monad m) => Derive.TrackDeriver m
d_controller_track track_id = do
    track <- Derive.get_track track_id
    stack <- fmap Derive.state_stack Derive.get
    let (pos_list, events) = unzip $ Track.event_list (Track.track_events track)
    -- TODO when signals are lazy this will be inefficient.  See TODO in
    -- Note.hs.
    -- Unlike the note deriver, I can do the warping all in one go here,
    -- because control events never trigger subderivation.
    warped <- mapM Derive.local_to_global pos_list
    return $ map (uncurry (control_event stack)) (zip warped events)

-- * implementation

-- | Construct a control event from warped position.
control_event :: Warning.Stack -> TrackPos -> Event.Event -> Score.Event
control_event stack pos event = Score.Event pos 0 (Event.event_text event)
        Map.empty evt_stack Nothing Score.no_attrs
    where
    evt_stack = case stack of
        (block_id, track_id, _) : rest ->
            (block_id, track_id, Just (pos, Event.event_duration event)) : rest
        [] -> []

-- ** number signals

-- | Derive a Signal from Events.
d_signal :: (Monad m) => [Score.Event] -> Derive.DeriveT m Signal.Signal
    -- TODO Should the srate be controllable?
d_signal events = fmap (Signal.track_signal Signal.default_srate)
    (Derive.map_events parse_event () id events)

parse_event :: (Monad m) => () -> Score.Event
    -> Derive.DeriveT m (TrackPos, Signal.Method, Signal.Val)
parse_event _ event = do
    (method, val) <- Parse.parse p_segment (Score.event_text event)
    return (Score.event_start event, method, val)

p_segment :: P.CharParser st (Signal.Method, Signal.Val)
p_segment = do
    meth <- P.option Signal.Set (P.try p_method)
    P.spaces
    val <- Parse.p_float
    return (meth, val)

-- ** pitch signals

d_pitch_signal :: (Monad m) => Pitch.ScaleId -> [Score.Event]
    -> Derive.DeriveT m Signal.PitchSignal
d_pitch_signal scale_id events = do
    scale <- maybe (Derive.throw ("unknown scale " ++ show scale_id)) return
        (Map.lookup scale_id Scale.scale_map)
    fmap (Signal.track_signal Signal.default_srate)
        (Derive.map_events (parse_pitch_event scale) () id events)

parse_pitch_event :: (Monad m) => Pitch.Scale -> () -> Score.Event
    -> Derive.DeriveT m (TrackPos, Signal.Method, Signal.Val)
parse_pitch_event scale _ event = do
    (method, note) <- Parse.parse p_note_segment (Score.event_text event)
    val <- parse_note scale note
    return (Score.event_start event, method, val)

-- | Parse scale notes for a given scale.  This one just matches the scale
-- notes directly, but a more complicated one may get scale values out of the
-- environment or something.
--
-- This converts from Pitch to Signal.Val, which loses scale information.
parse_note :: (Monad m) => Pitch.Scale -> Pitch.Note
    -> Derive.DeriveT m Signal.Val
parse_note scale note = case Pitch.scale_to_nn scale note of
    Nothing -> Derive.throw $
        "note " ++ show note ++ " not in scale " ++ show (Pitch.scale_id scale)
    Just (Pitch.NoteNumber nn) -> return nn

p_note_segment :: P.CharParser st (Signal.Method, Pitch.Note)
p_note_segment = do
    meth <- P.option Signal.Set (P.try (p_method #>> P.char ','))
    P.spaces
    note <- p_scale_note
    return (meth, note)

p_scale_note :: P.CharParser st Pitch.Note
p_scale_note = fmap Pitch.Note (P.many (P.satisfy (not . Char.isSpace)))

-- ** generic

p_method = (P.char 'i' >> return Signal.Linear)
    <|> (P.char 's' >> return Signal.Set)
    <|> (P.try
        (P.option 2 (Parse.p_float) #>> P.char 'e' >>= return . Signal.Exp))
    <?> "method: 'i' 's', or '#e'"

unparse_method :: Signal.Method -> String
unparse_method meth = case meth of
    Signal.Set -> ""
    Signal.Linear -> "i"
    Signal.Exp n -> show n ++ "e"
