module Perform.Lilypond.Convert_test where
import Util.Test
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score

import qualified Perform.Lilypond.Convert as Convert
import qualified Perform.Lilypond.Lilypond as Lilypond


test_convert = do
    let f quarter = map (fmap extract) . Convert.convert quarter
            . map (LEvent.Event . mkevent)
        mkevent (start, dur, pitch) =
            DeriveTest.mkevent (start, dur, pitch, [], Score.default_inst)
        extract (Lilypond.Event start dur pitch _ _) = (start, dur, pitch)
    equal (f 0.05 [(0, 0.05, "c"), (0.05, 0.1, "d")])
        [LEvent.Event (0, 32, "d'"), LEvent.Event (32, 64, "ds'")]
