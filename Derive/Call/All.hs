-- | Collect the various calls into one place.
module Derive.Call.All where
import qualified Data.Map as Map

import qualified Derive.Derive as Derive

import qualified Derive.Call.Basic as Basic
import qualified Derive.Call.Echo as Echo
import qualified Derive.Call.Rambat as Rambat


call_map :: Derive.CallEnv
call_map = Derive.CallEnv note_calls control_calls

note_calls :: Derive.CallMap
note_calls = Map.unions [Basic.note_calls, Echo.note_calls, Rambat.note_calls]

control_calls :: Derive.CallMap
control_calls = Map.unions [Basic.control_calls]
