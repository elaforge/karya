-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Deriver code common to sampler patches.
module Synth.Sampler.Patch.Lib.Code where
import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.EnvKey as EnvKey

import qualified Perform.Pitch as Pitch

import Global


-- | Modify a deriver to add 'Controls.variation', for patches that use it.
withVariation :: Derive.Deriver a -> Derive.Deriver a
withVariation deriver =
    ifM (Derive.is_control_set Controls.variation) deriver $ do
        n <- Call.random
        Derive.with_constant_control Controls.variation n deriver

-- | Like 'withVariation', but use a normal distribution.
withVariationNormal :: Double -> Derive.Deriver a -> Derive.Deriver a
withVariationNormal stddev deriver =
    ifM (Derive.is_control_set Controls.variation) deriver $ do
        n <- Call.normal stddev
        Derive.with_constant_control Controls.variation n deriver

addSymbolicPitch :: Derive.PassedArgs x -> Derive.Deriver a -> Derive.Deriver a
addSymbolicPitch args deriver = do
    note <- Call.get_symbolic_pitch =<< Args.real_start args
    Derive.with_val EnvKey.element (Pitch.note_text note) deriver
