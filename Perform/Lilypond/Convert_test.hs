-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Lilypond.Convert_test where
import Util.Test
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score

import qualified Perform.Lilypond.Convert as Convert
import qualified Perform.Lilypond.Lilypond as Lilypond


test_convert = do
    let f quarter = map (fmap extract) . Convert.convert quarter . map mkevent
        mkevent (start, dur, pitch) =
            DeriveTest.mkevent (start, dur, pitch, [], Score.empty_inst)
        extract e = (Lilypond.event_start e, Lilypond.event_duration e,
            Lilypond.event_pitch e)
    equal (f 0.05 [(0, 0.05, "3b"), (0.05, 0.1, "4c#")])
        [LEvent.Event (0, 32, "b"), LEvent.Event (32, 64, "cs'")]
