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
import qualified Perform.PitchSignal as PitchSignal
import qualified Perform.Signal as Signal
import qualified Perform.Warning as Warning

import qualified Derive.Derive as Derive
import qualified Derive.Parse as Derive.Parse
import qualified Derive.Score as Score


-- | It's necessary for @eventsm@ to be monadic, since it needs to be run
-- inside the environment modified by d_control.
d_control :: (Monad m) => Score.Control -> Derive.DeriveT m Signal.Control
    -> Derive.DeriveT m [Score.Event] -> Derive.DeriveT m [Score.Event]
d_control cont signalm eventsm = do
    signal <- signalm
    Derive.with_control cont signal eventsm

d_relative :: (Monad m) =>
    Score.Control -> Derive.ControlOp -> Derive.DeriveT m Signal.Control
    -> Derive.DeriveT m [Score.Event] -> Derive.DeriveT m [Score.Event]
d_relative cont c_op signalm eventsm = do
    signal <- signalm
    Derive.with_relative_control cont c_op signal eventsm

d_pitch :: (Monad m) => Derive.DeriveT m PitchSignal.PitchSignal
    -> Derive.DeriveT m [Score.Event] -> Derive.DeriveT m [Score.Event]
d_pitch signalm eventsm = do
    signal <- signalm
    Derive.with_pitch signal eventsm

d_relative_pitch :: (Monad m) => Derive.ControlOp
    -> Derive.DeriveT m PitchSignal.Relative
    -> Derive.DeriveT m [Score.Event] -> Derive.DeriveT m [Score.Event]
d_relative_pitch c_op signalm eventsm = do
    signal <- signalm
    Derive.with_relative_pitch c_op signal eventsm

-- | Get the signal events from a control track.  They are meant to be
-- fed to 'd_signal', which will convert them into a signal, and the whole
-- thing passed as the signal deriver to 'd_control'.
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

-- TODO It would be nicer to use a Text implementation of parsec so I don't
-- have to unpack the event text.

-- | Construct a control event from warped position.
--
-- TODO using Score.Event for control events seems a bit silly since they're
-- empty except pos, text, and stack.
control_event :: Warning.Stack -> TrackPos -> Event.Event -> Score.Event
control_event stack pos event = Score.Event pos 0 (Event.event_text event)
        Map.empty PitchSignal.empty evt_stack Nothing Score.no_attrs
    where
    evt_stack = case stack of
        (block_id, track_id, _) : rest ->
            (block_id, track_id, Just (pos, Event.event_duration event)) : rest
        [] -> []

-- ** number signals

-- | Derive a Signal from Events.
d_signal :: (Monad m) => [Score.Event] -> Derive.DeriveT m Signal.Control
    -- TODO Should the srate be controllable?
d_signal events = fmap (Signal.track_signal Signal.default_srate)
    (Derive.map_events parse_event Derive.NoState id events)

parse_event :: (Monad m) => Derive.NoState -> Score.Event
    -> Derive.DeriveT m (Signal.Segment, Derive.NoState)
parse_event _ event = do
    (method, val) <- Derive.Parse.parse (liftM2 (,) p_opt_method Parse.p_float)
        (Score.event_string event)
    return ((Score.event_start event, method, val), Derive.NoState)

-- ** pitch signals

d_pitch_signal :: (Monad m) => Pitch.ScaleId -> [Score.Event]
    -> Derive.DeriveT m PitchSignal.PitchSignal
d_pitch_signal scale_id events = do
    scale <- Derive.get_scale "d_pitch_signal" scale_id
    fmap (PitchSignal.track_signal scale_id PitchSignal.default_srate)
        (Derive.map_events (parse_pitch_event scale) Nothing id events)

-- | Produce a plain signal from an absolute pitch track to render on the UI.
d_display_pitch :: (Monad m) => Pitch.ScaleId -> [Score.Event]
    -> Derive.DeriveT m Signal.Signal
d_display_pitch scale_id _events = do
    scale <- Derive.get_scale "d_display_pitch" scale_id
    -- TODO implement when I implement pitch signal rendering
    return (Signal.constant 0)

-- | As 'd_display_pitch' but parse a relative pitch track.
d_display_relative_pitch :: (Monad m) => Pitch.ScaleId -> [Score.Event]
    -> Derive.DeriveT m Signal.Signal
d_display_relative_pitch scale_id _events = do
    scale <- Derive.get_scale "d_display_relative_pitch" scale_id
    -- TODO implement when I implement pitch signal rendering
    return (Signal.constant 0)

parse_pitch_event :: (Monad m) => Pitch.Scale -> Maybe Pitch.Generic
    -> Score.Event -> Derive.DeriveT m
        (PitchSignal.Segment, Maybe Pitch.Generic)
parse_pitch_event scale prev event = do
    (method, note) <- Derive.Parse.parse
        (liftM2 (,) p_opt_method p_scale_note) (Score.event_string event)
    generic <- parse_note scale prev note
    return ((Score.event_start event, method, generic), Just generic)

d_relative_pitch_signal :: (Monad m) => Pitch.ScaleId -> [Score.Event]
    -> Derive.DeriveT m PitchSignal.Relative
d_relative_pitch_signal scale_id events = do
    scale <- Derive.get_scale "d_relative_pitch_signal" scale_id
    let per_oct = Pitch.scale_octave scale
    segs <- Derive.map_events (parse_relative_pitch_event per_oct) () id events
    return $ PitchSignal.track_signal scale_id Signal.default_srate segs

parse_relative_pitch_event :: (Monad m) => Pitch.Octave -> () -> Score.Event
    -> Derive.DeriveT m (PitchSignal.Segment, ())
parse_relative_pitch_event per_oct _ event = do
    (method, (oct, nn)) <- Derive.Parse.parse
        (liftM2 (,) p_opt_method parse_relative)
        (Score.event_string event)
    let val = Pitch.Generic (fromIntegral (oct * per_oct) + nn)
    return ((Score.event_start event, method, val), ())

-- | Relative notes look like \"+4/3.2\" or \"+3.2\" (octave omitted)
-- or \"+3/\" (only octave).
parse_relative :: P.CharParser st (Pitch.Octave, Double)
parse_relative = do
    sign <- fmap (\c -> if c == '-' then -1 else 1) (P.oneOf "-+")
    (oct, nn) <- P.try oct_nn <|> just_nn
    return (sign*oct, nn)
    where
    oct_nn = do
        oct <- Parse.p_int
        P.char '/'
        nn <- P.option 0 Parse.p_float
        return (oct, nn)
    just_nn = Parse.p_float >>= \nn -> return (0, nn)

unparse_relative :: (Pitch.Octave, Double) -> Pitch.Note
unparse_relative (oct, nn)
    | oct == 0 = Pitch.Note (sign ++ Parse.show_float (Just 2) (abs nn))
    | otherwise = Pitch.Note (sign ++ show (abs oct) ++ degree)
    where
    sign = if (if oct /= 0 then fromIntegral oct else nn) >= 0 then "+" else "-"
    degree = (if oct == 0 then "" else "/")
        ++ (if nn == 0 then "" else Parse.show_float (Just 2) nn)

-- | Parse scale notes for a given scale.  This one just matches the scale
-- notes directly, but a more complicated one may get scale values out of the
-- environment or something.
--
-- An empty Note gets the same pitch as the previous one which gives an easy
-- way to shorten the length of a subsequent slide, e.g. @["4c", "i", "i, 4d"]@.
parse_note :: (Monad m) => Pitch.Scale -> Maybe Pitch.Generic -> Pitch.Note
    -> Derive.DeriveT m Pitch.Generic
parse_note scale prev note
    | note == empty_note, Just generic <- prev = return generic
    | otherwise = maybe
        (Derive.throw $ show note ++ " not in " ++ show (Pitch.scale_id scale))
        return (Pitch.scale_note_to_generic scale note)
        -- TODO If I made the signal go to an invalid value on error it would
        -- prevent the notes from playing, which seems desirable, otherwise they
        -- just keep playing the last parseable pitch.
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
