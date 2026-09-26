-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Image-Line's Drumaxx softsynth.
module User.Elaforge.Instrument.Drumaxx where
import qualified Data.Set as Set

import Midi.Key
import qualified Midi.Midi as Midi
import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Cmd.Instrument.MidiInst as MidiInst

import Derive.Attrs
import qualified Derive.Instrument.DUtil as DUtil
import Global


synth :: MidiInst.Synth
synth = MidiInst.synth "drumaxx" "Imagine-Line Drumaxx" patches

patches :: [MidiInst.Patch]
patches =
    [ MidiInst.code #= CUtil.drumCode_ CUtil.MidiThru (map fst strokeKeys) $
        CUtil.drumPatch strokeKeys $ MidiInst.defaultPatch pbRange []
    , MidiInst.code #= compositeCode $
        MidiInst.doc #= compositeDoc $
        MidiInst.dummy "should have been redirected to other instruments" $
        MidiInst.namedPatch pbRange "comb" []
    ]
    where
    compositeCode = MidiInst.noteGenerators
        [(call, composite call) | call <- map (Drums._name . fst) strokeKeys]
    composite call = DUtil.redirectPitch "comb"
        "" (Set.fromList ["mix", "fbk"]) call mempty
    compositeDoc = "This drum takes a pitch signal, which is then sent\
        \ to the `composite-pitched` instrument, which is a tuned comb filter.\
        \ The audio routing has to be set up in the VST host."
    pbRange = (-24, 24)

-- | The octave numbers on the drumaxx are one greater than the standard
-- usage.  This is for \"Acoustic 2 FG\".  I'll have to come up with
-- a standard mapping later.
strokeKeys :: [(Drums.Stroke, Midi.Key)]
strokeKeys =
    [ (Drums.cBd, c2)
    , (Drums.cBd2, b1)
    , (Drums.cSn, d2)
    , (Drums.cSn2, e2)
    , (Drums.stroke 'c' "sn3" (snare <> v3), ds2)
    , (Drums.cRim, cs2)
    , (Drums.cLtom, g2)
    , (Drums.cMtom, b2)
    , (Drums.cHtom, c3)
    , (Drums.cHh, fs2)
    , (Drums.cOhh, as2)
    , (Drums.cRide, ds3)
    , (Drums.cCrash, cs3)
    ]
