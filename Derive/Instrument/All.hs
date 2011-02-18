-- | Gather together the built-in instrument derive modules.
module Derive.Instrument.All where
import qualified Data.Map as Map
import qualified Derive.Derive as Derive

import qualified Derive.Instrument.Drums

import qualified App.Link as Link


note :: Map.Map Link.ModuleId (Derive.LookupCall Derive.NoteCall)
note = Map.fromList $ map make_module
    [ ("Derive.Instrument.Drums.traps", Derive.Instrument.Drums.traps)
    , ("Derive.Instrument.Drums.hang", Derive.Instrument.Drums.hang)
    ]

val :: Map.Map Link.ModuleId (Derive.LookupCall Derive.ValCall)
val = Map.fromList $ map make_module
    [
    ]

make_module (k, v) = (Link.ModuleId k, Derive.make_lookup v)
