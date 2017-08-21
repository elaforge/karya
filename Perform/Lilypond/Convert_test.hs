-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Lilypond.Convert_test where
import Util.Test
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score

import qualified Perform.Lilypond.Convert as Convert
import qualified Perform.Lilypond.Types as Types


test_convert = do
    let f = map (fmap extract) . Convert.convert config . map mkevent
        config = Types.default_config { Types.config_quarter_duration = 0.05 }
        mkevent (start, dur, pitch) =
            DeriveTest.mkevent (start, dur, pitch, [], Score.empty_instrument)
        extract e = (Types.event_start e, Types.event_duration e,
            maybe "" Types.to_lily (Types.event_pitch e))
    equal (f [(0, 0.05, "3b"), (0.05, 0.1, "4c#")])
        [LEvent.Event (0, 32, "b"), LEvent.Event (32, 64, "cs'")]
