-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Predefined meters.
--
-- Defined separate from "Cmd.Meter" so you can redefine them without reloading
-- everything that depends on Cmd.Meter.
module Cmd.Meters where
import qualified Cmd.Meter as Meter
import Cmd.Meter (AbstractMeter(..), regular_subdivision)


-- half/measure, quarter/half, eighth/quarter, ...
-- These use 1s to help keep the timestep mnemonics in sync with staff notation
-- durations, as documented in the module haddock.
m64, m54, m44, m34 :: AbstractMeter
m64 = regular_subdivision [2, 3, 2, 2, 2]
m54 = regular_subdivision [1, 5, 2, 2, 2]
m44 = regular_subdivision [2, 2, 2, 2, 2]
m34 = regular_subdivision [1, 3, 2, 2, 2]
m24 = regular_subdivision [1, 2, 2, 2, 2]

-- | A section of 4 4/4 bars.
m44_4 :: AbstractMeter
m44_4 = Meter.repeat 4 m44

m3p3p2_8 :: AbstractMeter
m3p3p2_8 = Meter.repeat 1 $ Meter.subdivides [2, 2, 2, 2] $
    D [D [T, T, T], D [T, T, T], D [T, T]]

-- | 2+2+2 / 8, 4 quarters per measure
m2p2p2_8 :: AbstractMeter
m2p2p2_8 = regular_subdivision [1, 3, 2, 2, 2, 2]

-- | 3+3 / 8, 2 dotted quarters per measure
m3p3_8 :: AbstractMeter
m3p3_8 = regular_subdivision [1, 2, 3, 2, 2, 2]
