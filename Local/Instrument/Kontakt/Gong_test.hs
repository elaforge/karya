-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Local.Instrument.Kontakt.Gong_test where
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified Local.Instrument.Kontakt.Gong as Gong
import qualified Local.Instrument.Kontakt.KontaktTest as KontaktTest


test_resolve = do
    equal Gong.kajar_resolve_errors []

test_doubled = do
    let run tracks = DeriveTest.extract extract $ derive "" tracks
        extract e = (Score.event_start e, Score.initial_dynamic e,
            DeriveTest.e_attributes e)
    equal (run [(">k", [(1, 0, "oo-time=.5 | oo-dyn=.75 | oo")])])
        ([(1, 1, "+center+closed"), (1.5, 0.75, "+center+closed")], [])
    equal (run
            [ (">k", [(1, 0, "oo-time=.5 | oo-dyn=.75 | oo")])
            , ("dyn", [(0, 0, ".5")])
            ])
        ([(1, 0.5, "+center+closed"), (1.5, 0.75 * 0.5, "+center+closed")], [])

test_nruk = do
    let run notes = DeriveTest.extract extract $ derive "" [(">k", notes)]
        extract e = (Score.event_start e, DeriveTest.e_attributes e)
        o = "+center+closed"
    equal (run [(4, 0, "o.. 2 2"), (6, 0, "+")])
        ([(4, o), (4.5, o), (5, o), (5.5, o), (6, "+open+rim")], [])

derive :: String -> [UiTest.TrackSpec] -> Derive.Result
derive = KontaktTest.derive allocs
    where allocs = UiTest.allocations [("k", "kontakt/kajar")]
