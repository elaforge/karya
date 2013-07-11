-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Define a few inhabitants of Environ which are used by the built-in set of
-- calls.
module Derive.Environ where
import Derive.BaseTypes (ValName)


-- | Default set of attrs.
attributes :: ValName
attributes = "attr"

-- | Set to the control that is being derived, inside of a control track.
control :: ValName
control = "control"

-- | Default instrument.
instrument :: ValName
instrument = "inst"

-- | Diatonic transposition often requires a key.  The interpretation of the
-- value depends on the scale.
key :: ValName
key = "key"

-- | Default scale, used by pitch tracks with a @*@ title.
scale :: ValName
scale = "scale"

-- | Random seed used by randomization functions.  Can be explicitly
-- initialized to capture a certain \"random\" variation.
--
-- This is rounded to an integer, so only integral values make sense.
seed :: ValName
seed = "seed"

-- | Sampling rate used by signal interpolators.
srate :: ValName
srate = "srate"

-- | Kind of tuning for the scale in scope.  The meaning is dependent on the
-- scale, e.g. ngumbang ngisep for Balinese scales.
tuning :: ValName
tuning = "tuning"

-- | Separate notes into different voices.  This is used by integrate to put
-- them on their own tracks, and by the lilypond backend to split them into
-- their own voices.  Should be an integer from 1 to 4.
voice :: ValName
voice = "v"
