-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Sampler.Patch_test where
import qualified Data.Map as Map

import qualified Cmd.Instrument.ImInst as ImInst
import qualified Perform.Im.Patch as Im.Patch
import qualified Synth.Faust.Effect as Effect
import qualified Synth.Faust.EffectC as EffectC
import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Shared.Control as Control

import           Util.Test


test_checkControls :: Test
test_checkControls = do
    effect <- EffectC.get "test-delay"
    let f controls renames = Patch.checkControls
            (mkPatch controls)
            (Map.keysSet (Effect._controls effect))
            ((Patch.effect "effect")
                { Patch._toEffectControl = Map.fromList renames })
    equal (f ["blah"] []) []
    strings_like (f ["delay"] []) ["*controls overlap*"]
    strings_like (f [] [("delay", "blah")]) ["*not in effect: {blah}"]
    -- e-delay is now mapped to delay, so a delay control on the patch is fine.
    strings_like (f ["delay"] [("e-delay", "delay")]) []

mkPatch :: [Control.Control] -> Patch.Patch
mkPatch controls = (Patch.patch "patch")
    { Patch._karyaPatch = ImInst.make_patch $ Im.Patch.patch
        { Im.Patch.patch_controls = Map.fromList [(c, "") | c <- controls]
        }
    }
