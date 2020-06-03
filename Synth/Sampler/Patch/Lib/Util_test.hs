module Synth.Sampler.Patch.Lib.Util_test where
import qualified Synth.Sampler.Patch.Lib.Util as Util
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import           Util.Test


test_dynEnvelope = do
    let f dyn = Signal.to_pairs $ Util.dynEnvelope 0.25 1 $
            Note.withControl Control.dynamic (Signal.from_pairs dyn) $
            Note.note "patch" "inst" 4 4
    equal (f []) [(8, 0), (9, 0)]
    equal (f [(0, 1), (100, 1)]) [(0, 1), (8, 1), (9, 0)]
    equal (f [(4, 0), (8, 1)]) [(4, 0.25), (8, 1), (9, 0)]
    equal (f [(4, 0), (8, 0.5)]) [(4, 0.25), (8, 0.625), (9, 0)]
