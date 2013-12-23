-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Define a few inhabitants of Environ which are used by the built-in set of
-- calls.  Expected types are in 'Derive.TrackLang.hardcoded_types'.
module Derive.Environ where
import Derive.BaseTypes (ValName)


-- | VAttributes: Default set of attrs.
attributes :: ValName
attributes = "attr"

-- | VSymbol: Set to the control that is being derived, inside of a control
-- track.
control :: ValName
control = "control"

-- | VInstrument: Default instrument.
instrument :: ValName
instrument = "inst"

-- | VSymbol: Diatonic transposition often requires a key.  The interpretation
-- of the value depends on the scale.
key :: ValName
key = "key"

-- | VSymbol: Default scale, used by pitch tracks with a @*@ title.
scale :: ValName
scale = "scale"

-- | VNum: Random seed used by randomization functions.  Can be explicitly
-- initialized to capture a certain \"random\" variation.
--
-- This is rounded to an integer, so only integral values make sense.
seed :: ValName
seed = "seed"

-- | VNum: Sampling rate used by signal interpolators.
srate :: ValName
srate = "srate"

-- | VNum: Set the default tempo, overriding 'Ui.State.default_tempo'.  This
-- only applies if there is no toplevel tempo track, and generally only has an
-- effect if the block is played as a toplevel block since it's a constant
-- tempo.
--
-- Previously I would directly set the tempo warp in the equal call, but tempo
-- setting has to be at the toplevel tempo track for it to interact properly
-- with block call stretching.
tempo :: ValName
tempo = "tempo"

-- | VSymbol: Kind of tuning for the scale in scope.  The meaning is dependent
-- on the scale, e.g. ngumbang ngisep for Balinese scales.
tuning :: ValName
tuning = "tuning"

-- | VNum: Separate notes into different voices.  This is used by integrate to
-- put them on their own tracks, and by the lilypond backend to split them into
-- their own voices.  Should be an integer from 1 to 4.
voice :: ValName
voice = "v"

-- | VString: @right@, @r@,  @left@, or @l@.  Used by the lilypond backend, and
-- also by any call that relies on an instrument's parts being divided by hand.
hand :: ValName
hand = "hand"

-- | VNum: hold the start of a call for a certain amount of ScoreTime or
-- RealTime.
hold :: ValName
hold = "hold"
