-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Lilypond.Convert_test where
import Util.Test
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.LEvent as LEvent
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Score as Score

import qualified Perform.Lilypond.Convert as Convert
import qualified Perform.Lilypond.Types as Types


test_convert = do
    let f scale key = map (fmap extract) . Convert.convert config . map mkevent
            where
            mkevent (start, dur, pitch) = DeriveTest.mkevent_scale_key scale key
                (start, dur, pitch, [], Score.empty_instrument)
        config = Types.default_config { Types.config_quarter_duration = 0.05 }
        extract e = (Types.event_start e, Types.event_duration e,
            maybe "" Types.to_lily (Types.event_pitch e))
    equal (f Twelve.scale Nothing [(0, 0.05, "3b"), (0.05, 0.1, "4c#")])
        [LEvent.Event (0, 32, "b"), LEvent.Event (32, 64, "cs'")]
    -- Non-twelve scales also work, and accidentals are preserved.
    equal (f Twelve.relative_scale Nothing
            [(0, 0.05, "3s"), (0.05, 0.05, "3rb")])
        [LEvent.Event (0, 32, "c"), LEvent.Event (32, 32, "df")]
    equal (f Twelve.relative_scale (Just "b-min") [(0, 0.05, "3s")])
        [LEvent.Event (0, 32, "b")]
