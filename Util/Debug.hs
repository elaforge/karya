module Util.Debug where

import qualified Debug.Trace as Trace

trace = Trace.trace
tracev x = Trace.trace ("**trace: " ++ show x) x

