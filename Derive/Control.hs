{-# LANGUAGE PatternGuards #-}
{- | Derivers for control tracks.

    Interpolation methods:

    - s - Set value at the point.  This is the default if there is no method.

    - i - Approach with linear interpolation.

    - #e - Approach with exponential interpolation with #.  # defaults to 2.

    - method;val - Approach val with method, then jump to val.
-}
module Derive.Control where
import Prelude hiding (lex)
import Control.Monad
import qualified Data.Char as Char
import qualified Data.Map as Map

import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<|>), (<?>))

import Util.Control ((#>>))
import qualified Util.Parse as Parse

import Ui
import qualified Ui.Event as Event
import qualified Ui.Track as Track

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import qualified Perform.Warning as Warning

import qualified Derive.Derive as Derive
import qualified Derive.Parse as Derive.Parse
import qualified Derive.Scale as Scale
import qualified Derive.Score as Score


-- | It's necessary for @eventsm@ to be monadic, since it needs to be run
-- inside the environment modified by d_control.
d_control :: (Monad m) => Score.Control -> Derive.DeriveT m Signal.Signal
    -> Derive.DeriveT m [Score.Event] -> Derive.DeriveT m [Score.Event]
d_control cont signalm eventsm = do
    signal <- signalm
    Derive.with_control cont signal eventsm

d_relative_control :: (Monad m) =>
    Score.Control -> Derive.ControlOp -> Derive.DeriveT m Signal.Signal
    -> Derive.DeriveT m [Score.Event] -> Derive.DeriveT m [Score.Event]
d_relative_control cont c_op signalm eventsm = do
    signal <- signalm
    Derive.with_relative_control cont c_op signal eventsm

-- | Get the signal events from a control track.  They are meant to be
-- fed to 'd_signal' or 'd_pitch_signal', which will convert them into
-- a signal, and the whole thing passed as the signal deriver to
-- 'd_control'.
d_control_track :: (Monad m) => Derive.TrackDeriver m
d_control_track track_id = do
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

-- It would be nicer to use a Text implementation of parsec so I don't have to
-- unpack the event text.

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
    (Derive.map_events parse_event Derive.NoState id events)

parse_event :: (Monad m) => Derive.NoState -> Score.Event
    -> Derive.DeriveT m ((TrackPos, Signal.Method, Signal.Y), Derive.NoState)
parse_event _ event = do
    (method, val) <- Derive.Parse.parse (liftM2 (,) p_opt_method Parse.p_float)
        (Score.event_string event)
    return ((Score.event_start event, method, val), Derive.NoState)

-- ** pitch signals

d_pitch_signal :: (Monad m) => Pitch.ScaleId -> [Score.Event]
    -> Derive.DeriveT m Signal.PitchSignal
d_pitch_signal scale_id events = do
    scale <- maybe (Derive.throw ("unknown " ++ show scale_id)) return
        (Map.lookup scale_id Scale.scale_map)
    fmap (Signal.track_signal Signal.default_srate)
        (Derive.map_events (parse_pitch_event scale) Nothing id events)

parse_pitch_event :: (Monad m) => Pitch.Scale -> Maybe Signal.Y
    -> Score.Event -> Derive.DeriveT m
        ((TrackPos, Signal.Method, Signal.Y), Maybe Signal.Y)
parse_pitch_event scale previous_nn event = do
    (method, note) <- Derive.Parse.parse
        (liftM2 (,) p_opt_method p_scale_note) (Score.event_string event)
    val <- parse_note scale previous_nn note
    return ((Score.event_start event, method, val), Just val)

d_relative_pitch_signal :: (Monad m) => [Score.Event]
    -> Derive.DeriveT m Signal.PitchSignal
d_relative_pitch_signal events =
    fmap (Signal.track_signal Signal.default_srate)
        (Derive.map_events parse_relative_pitch_event () id events)

parse_relative_pitch_event :: (Monad m) => () -> Score.Event
    -> Derive.DeriveT m ((TrackPos, Signal.Method, Signal.Y), ())
parse_relative_pitch_event _ event = do
    (method, val) <- Derive.Parse.parse
        (liftM2 (,) p_opt_method p_relative_pitch) (Score.event_string event)
    return ((Score.event_start event, method, val), ())

p_relative_pitch :: P.CharParser st Signal.Y
p_relative_pitch = do
    s <- P.many P.anyToken
    Pitch.Generic oct nn <-
        maybe P.pzero return (Pitch.to_relative (Pitch.Note s))
    -- TODO this means relative pitches won't work correctly on non tempered
    -- scales.  To make this work, PitchSignal has to be a Signal Generic.
    return (fromIntegral oct * 12 + nn)

-- | Parse scale notes for a given scale.  This one just matches the scale
-- notes directly, but a more complicated one may get scale values out of the
-- environment or something.
--
-- This converts from Pitch to Signal.Y, which loses scale information.
-- TODO so if I want to have a pitch signal be GenericPitch, here is the place
-- to change
parse_note :: (Monad m) => Pitch.Scale -> Maybe Signal.Y -> Pitch.Note
    -> Derive.DeriveT m Signal.Y
parse_note scale previous_nn note
    | note == empty_note, Just nn <- previous_nn = return nn
    | otherwise = case Pitch.scale_note_to_nn scale note of
        -- TODO If I made the signal go to an invalid value here here it would
        -- prevent the notes from playing, which seems desirable, otherwise they
        -- just keep playing the last parseable pitch.
        Nothing -> Derive.throw $
            show note ++ " not in " ++ show (Pitch.scale_id scale)
        Just (Pitch.NoteNumber nn) -> return nn
    where empty_note = Pitch.Note ""

p_scale_note :: P.CharParser st Pitch.Note
p_scale_note = fmap Pitch.Note (P.many (P.satisfy (not . Char.isSpace)))

-- ** generic

-- The method is separated from the value with a comma.  The cmds have to parse
-- incomplete event text and the comma is important to disambiguate.

p_opt_method :: P.CharParser st Signal.Method
p_opt_method = P.option Signal.Set (P.try (p_meth #>> P.char ',')) #>> P.spaces

p_meth :: P.CharParser st Signal.Method
p_meth = (P.char 'i' >> return Signal.Linear)
    <|> (P.char 's' >> return Signal.Set)
    <|> (P.try
        (P.option 2 (Parse.p_float) #>> P.char 'e' >>= return . Signal.Exp))
    <?> "method: 'i' 's', or '#e'"

unparse_method :: Signal.Method -> String
unparse_method meth = case meth of
    Signal.Set -> ""
    Signal.Linear -> "i"
    Signal.Exp n -> show n ++ "e"
