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
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<|>), (<?>))

import Util.Control ((#>>))

import qualified Ui.Event as Event
import qualified Ui.Track as Track

import qualified Perform.Signal as Signal

import qualified Derive.Score as Score
import qualified Derive.Parse as Parse
import qualified Derive.Derive as Derive

import qualified Data.Maybe as Maybe


-- | It's necessary for @eventsm@ to be monadic, since it needs to be run
-- inside the environment modified by d_controller.
d_controller :: (Monad m) => Score.Controller -> Derive.DeriveT m Signal.Signal
    -> Derive.DeriveT m [Score.Event] -> Derive.DeriveT m [Score.Event]
d_controller cont signalm eventsm = do
    signal <- signalm
    Derive.with_controller cont signal eventsm

-- | Get the signal events from a controller track.  They are meant to be
-- fed to 'd_signal', which will convert them into a signal, and the whole
-- thing passed as the signal deriver to 'd_controller'.
d_controller_track :: (Monad m) => Derive.TrackDeriver m
d_controller_track track_id = do
    track <- Derive.get_track track_id
    -- TODO set the stack per event.
    stack <- fmap Derive.state_stack Derive.get
    let (pos_list, events) = unzip $ Track.event_list (Track.track_events track)
    warped <- mapM Derive.local_to_global pos_list
    return $ map (uncurry (control_event stack)) (zip warped events)

control_event stack pos event = Score.Event pos 0 (Event.event_text event)
    Map.empty stack Nothing

d_signal :: (Monad m) => [Score.Event] -> Derive.DeriveT m Signal.Signal
    -- TODO Should the srate be controllable?
d_signal events = fmap (Signal.track_signal Signal.default_srate)
    (Derive.map_events parse_event Nothing id events)

parse_event _ event = do
    (method, val) <- Parse.parse (p_segment Parse.p_float)
        (Score.event_text event)
    return (Score.event_start event, method, val)

p_segment :: P.CharParser st a -> P.CharParser st (Signal.Method, a)
p_segment p_val = do
    meth <- P.option Signal.Set (P.try p_method)
    P.spaces
    val <- p_val
    return (meth, val)

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
