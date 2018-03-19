-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module User.Elaforge.Instrument.Vsl_test where
import qualified Data.List as List

import Util.Test
import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Derive.Attrs as Attrs
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal

import qualified Perform.Midi.Patch as Patch
import qualified Instrument.Common as Common
import qualified User.Elaforge.Instrument.Vsl as Vsl
import qualified User.Elaforge.Instrument.VslInst as VslInst
import Global


test_attr_priority = do
    let Just violin = List.find
            ((=="violin") . Patch.patch_name)
            (map MidiInst.patch_patch Vsl.patches)
        lookup attrs = fst <$> Common.lookup_attributes attrs
            (Patch.patch_attribute_map violin)
    -- +pizz wins over +spiccato
    equal (lookup (VslInst.pizz <> VslInst.spiccato)) (lookup VslInst.pizz)
    -- +harsh wins over +stac
    equal (lookup (VslInst.harsh <> VslInst.staccato)) (lookup VslInst.harsh)
    -- Everything wins over +nv.
    equal (lookup (VslInst.nv <> VslInst.legato)) (lookup VslInst.legato)

test_strip_attrs = do
    let f = map ShowVal.show_val . Vsl.strip_attrs . map Attrs.attrs
    -- Strip them.
    equal (f [["sus"], ["vib", "marcato"]]) ["+", "+marcato"]
    -- Can't strip if it would be non-unique.
    equal (f [["a", "sus"], ["a"]]) ["+a+sus", "+a"]
    -- Or if it's non-unique with an already stripped one.
    equal (f [["sus"], ["vib"]]) ["+", "+vib"]

test_natural_harmonic = do
    let run attrs pitch = DeriveTest.extract extract $
            DeriveTest.derive_tracks_setup with ""
                [ (">v", [(0, 1, attrs)])
                , ("*", [(0, 0, pitch)])
                ]
        extract = Midi.to_key . maybe 0 round . Score.initial_nn
        with = DeriveTest.with_synths_simple [("v", "vsl/violin")] [Vsl.synth]
    equal (run "+harm+nat" "4c")
        ([], ["Error: c4(60) unplayable on [+g, +d, +a, +e]"])
    equal (run "+harm+nat" "3g") ([Key.c3], [])
    equal (run "+harm+nat" "4g") ([Key.d3], [])
    equal (run "+harm+nat" "6d") ([Key.gs3], [])
    equal (run "+harm+nat+d" "6d") ([Key.f4], [])

    -- Artificial harmonics are one octave too high.
    equal (run "+harm" "3g") ([Key.g2], [])
    equal (run "+harm+stac" "3g") ([Key.g2], [])
    -- Even though I ask for +nat, I won't get it, so use +harm+stac.
    equal (run "+harm+stac+nat" "3g") ([Key.g2], [])
    -- Gliss is treated specially.
    equal (run "+harm+nat+gliss+up" "4g") ([Key.c3], [])
    equal (run "+harm+nat+gliss+down" "4g") ([Key.c3], [])

test_infer_seconds = do
    let run call = DeriveTest.extract DeriveTest.e_attributes $
            DeriveTest.derive_tracks_setup with "" [(">v", [(0, 2, call)])]
        with = DeriveTest.with_synths_simple [("v", "vsl/violin")] [Vsl.synth]
    equal (run "sec +cresc") (["+cresc+sec1-5"], [])
    equal (run "sec +cresc u") (["+cresc+sec3"], [])
    equal (run "sec +cresc d") (["+cresc+sec1-5"], [])

test_parse_sec = do
    let f = VslInst.parse_sec
    equal (f $ VslInst.cresc <> VslInst.sec 1.5) (Just (1.5, VslInst.cresc))
    equal (f VslInst.cresc) Nothing
