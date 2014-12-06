-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.Wayang_test where
import qualified Util.Seq as Seq
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Scale as Scale
import qualified Derive.Scale.ScaleTest as ScaleTest
import qualified Derive.Scale.Wayang as Wayang
import qualified Derive.Score as Score

import qualified Perform.Pitch as Pitch
import Global


test_read = do
    let f scale_id pitch = read_scale
            (ScaleTest.get_scale Wayang.scales scale_id) pitch
    -- The same pitch also winds up with the same Pitch and same frequency.
    equal (f "wayang" "5i") (Right "5-0")
    equal (f "wayang-pemade" "i^") (Right "5-0")
    equal (f "wayang-kantilan" "i-") (Right "5-0")
    let run scale pitch = DeriveTest.extract Score.initial_nn $
            DeriveTest.derive_tracks "" $ scale_track scale [pitch]
    equal (run "wayang" "5i") (run "wayang-pemade" "i^")
    equal (run "wayang" "5i") (run "wayang-kantilan" "i-")

read_scale :: Scale.Scale -> Pitch.Note -> Either String String
read_scale scale note = (pretty *** pretty) $
    Scale.scale_read scale mempty note

scale_track :: String -> [String] -> [UiTest.TrackSpec]
scale_track scale_id pitches =
    [ (">", [(n, 1, "") | n <- map fst events])
    , ('*' : scale_id, [(n, 0, p) | (n, p) <- events])
    ]
    where events = zip (Seq.range_ 0 1) pitches
