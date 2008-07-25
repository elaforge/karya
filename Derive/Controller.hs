{-# LANGUAGE PatternGuards #-}
{- |
TODO support jumps, e.g. "i.4;1" -> (Linear 0.4, 1)
-}
module Derive.Controller where
import Prelude hiding (lex)
import Control.Monad
import qualified Data.Maybe as Maybe

import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<|>), (<?>))

import Util.Control ((#>>))

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
    Derive.with_env cont signal eventsm

d_signal :: (Monad m) => [Score.Event] -> Derive.DeriveT m Signal.Signal
    -- TODO Should the srate be controllable?
d_signal events = fmap (Signal.track_signal Signal.default_srate)
    (Derive.map_events parse_event Nothing id events)

parse_event _ event = do
    (method, val) <- Parse.parse p_text event
    return (Score.event_start event, method, val)

p_text :: P.CharParser st (Signal.Method, Double)
p_text = do
    meth <- P.option Signal.Set (P.try p_method)
    val <- Parse.p_float
    return (meth, val)

p_method = (P.char 'i' >> return Signal.Linear)
    <|> (P.char 's' >> return Signal.Set)
    <|> (P.option 2 (Parse.p_float) #>> P.char 'e' >>= return . Signal.Exp)
    <?> "method: 'i' 's', or '#e'"
