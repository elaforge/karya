-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Local.Instrument.Vsl_test where
import Util.Test
import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal

import qualified Local.Instrument.Vsl as Vsl


test_strip_attrs = do
    let f = map ShowVal.show_val . Vsl.strip_attrs . map Score.attrs
    -- Strip them.
    equal (f [["sus"], ["vib", "marcato"]]) ["+", "+marcato"]
    -- Can't strip if it would be non-unique.
    equal (f [["a", "sus"], ["a"]]) ["+a+sus", "+a"]
    -- Or if it's non-unique with an already stripped one.
    equal (f [["sus"], ["vib"]]) ["+", "+vib"]

test_natural_harmonic = do
    let run inst attrs pitch = DeriveTest.extract extract $
            DeriveTest.derive_tracks_setup with "" $
                [ (inst, [(0, 1, attrs)])
                , ("*", [(0, 0, pitch)])
                ]
        extract = Midi.to_key . maybe 0 round . Score.initial_nn
        with = DeriveTest.with_synths [("v", "vsl/violin")] [Vsl.synth]
    equal (run ">v" "+harm+nat" "4c")
        ([], ["Error: c4 unplayable on [+g, +d, +a, +e]"])
    equal (run ">v" "+harm+nat" "3g") ([Key.c3], [])
    equal (run ">v" "+harm+nat" "4g") ([Key.d3], [])
    equal (run ">v" "+harm+nat" "6d") ([Key.gs3], [])
    equal (run ">v" "+harm+nat+d" "6d") ([Key.f4], [])
