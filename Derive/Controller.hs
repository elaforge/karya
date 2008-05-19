module Derive.Controller where
import Prelude hiding (lex)
import Control.Monad
import qualified Data.Maybe as Maybe

import Ui.Types
import qualified Perform.Signal as Signal

import qualified Derive.Score as Score
import qualified Derive.Parse as Parse
import qualified Derive.Derive as Derive

import qualified Data.Maybe as Maybe

type D = Derive.DeriveT

controller :: (Monad m) => String -> D m Signal.Signal -> [D m [Score.Event]]
    -> D m [Score.Event]
controller name signalm tracksm = do
    signal <- signalm
    Derive.with_env (Score.Controller name) signal $ do
        eventses <- sequence tracksm
        return (Derive.merge_events eventses)

signal :: (Monad m) => [Score.Event] -> Derive.DeriveT m Signal.Signal
signal events = fmap Signal.signal
    (Derive.map_events Nothing realize_signal fst (Parse.lex events))

realize_signal _ (event, args) = do
    [meth_str, val_str] <- require_args "realize_signal" 2 args
    method <- parse_method meth_str
    val <- parse_val val_str
    return (Score.event_start event, method, val)

require_args caller n args
    | length args /= n = Derive.throw_event $
        caller ++ " expected " ++ show n ++ " args, but got " ++ show args
    | otherwise = return args

parse_val s = Parse.warn_float "signal value" s

parse_method "i" = return Signal.Linear
parse_method "s" = return Signal.Set
parse_method ('e':exp_str) = do
    exp <- Parse.warn_float "Exp degree" exp_str
    return (Signal.Exp exp)
parse_method s = Derive.throw_event $ "unknown signal spec: " ++ show s
