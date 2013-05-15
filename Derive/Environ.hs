-- | Define a few inhabitants of Environ which are used by the built-in set of
-- calls.
module Derive.Environ where
import Derive.BaseTypes (ValName, Symbol(..))


-- | Default set of attrs.
attributes :: ValName
attributes = Symbol "attr"

-- | Default instrument.
instrument :: ValName
instrument = Symbol "inst"

-- | Diatonic transposition often requires a key.  The interpretation of the
-- value depends on the scale.
key :: ValName
key = Symbol "key"

-- | Default scale, used by pitch tracks with a @*@ title.
scale :: ValName
scale = Symbol "scale"

-- | Random seed used by randomization functions.  Can be explicitly
-- initialized to capture a certain \"random\" variation.
--
-- This is rounded to an integer, so only integral values make sense.
seed :: ValName
seed = Symbol "seed"

-- | Sampling rate used by signal interpolators.
srate :: ValName
srate = Symbol "srate"

-- | Kind of tuning for the scale in scope.  The meaning is dependent on the
-- scale, e.g. ngumbang ngisep for Balinese scales.
tuning :: ValName
tuning = Symbol "tuning"

-- | Separate notes into different voices.  This is used by integrate to put
-- them on their own tracks, and by the lilypond backend to split them into
-- their own voices.  Should be an integer from 1 to 4.
voice :: ValName
voice = Symbol "v"
