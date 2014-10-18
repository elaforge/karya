-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Define a few inhabitants of Environ which are used by the built-in set of
-- calls.  Expected types are in 'Derive.TrackLang.hardcoded_types'.
module Derive.Environ where
import Data.Text (Text)

import Derive.BaseTypes (ValName)


-- * directly supported by core derivers

-- | VList: arguments for a 'Derive.Call.Tags.requires_postproc' call.
-- Also see 'Derive.Call.Post.make_delayed'.
args :: ValName
args = "args"

-- | VAttributes: Default set of attrs.
attributes :: ValName
attributes = "attr"

-- | ScoreTime: the block deriver sets it to the ScoreTime end of the block.
-- Blocks always start at zero, but this is the only way for a call to know if
-- an event is at the end of the block.
block_end :: ValName
block_end = "block-end"

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

-- | VSymbol: Set along with 'control' to the 'Derive.Derive.Merge' function
-- which will be used for this control track.  Calls can use this to subvert
-- the merge function and emit an absolute value.
--
-- Values are @compose@ for tempo tracks, @set@, or any of the names from
-- 'Derive.Derive.ControlOp'.
merge :: ValName
merge = "merge"

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

-- | VNum: this is the count of the tracks with the same instrument, starting
-- from 0 on the left.  So three tracks named @>pno@ would be 0, 1, and 2,
-- respectively.  Used with "Derive.Call.InferTrackVoice".
track_voice :: ValName
track_voice = "track-voice"

-- * internal

-- | VNum: This is a bit of a hack for the dynamic to velocity conversion in
-- "Perform.Midi.Convert".  The default note deriver stashes the control
-- function output here, so if it turns out to not be a Pressure instrument
-- it can use this value.
--
-- Details in 'Perform.Midi.Convert.convert_dynamic'.
dynamic_val :: ValName
dynamic_val = "dyn-val"

-- | RealTime: This stores the RealTime sum of 'Derive.Controls.start_s' and
-- 'Derive.Controls.start_t', and is later applied by the @apply-start-offset@
-- postproc.
start_offset_val :: ValName
start_offset_val = "start-offset-val"

-- * supported by not so core derivers

-- | VNotePitch or VNum (NN): The top of the instrument's range.
--
-- It's a VNotePitch for instruments that are tied to a particular family of
-- scales, and have an upper note that is independent of any particular
-- frequency. For instance, a kantilan's top note will have a different
-- NoteNumber depending on its scale, or even within a single scale, depending
-- if it is pengumbang or pengisep.
--
-- For other instruments without such complicated scale requirements,
-- NoteNumber is simpler.
instrument_top :: ValName
instrument_top = "inst-top"

instrument_bottom :: ValName
instrument_bottom = "inst-bottom"

-- | List VPitch: tuning of open strings for this instrument.  The pitches
-- should be probably absolute NNs, not in any scale, so they work regardless
-- of which scale you happen to be in.
--
-- TODO maybe it should be VNotePitch as with 'instrument_top'?
open_strings :: ValName
open_strings = "open-strings"

-- | VSymbol: Kind of tuning for the scale in scope.  The meaning is dependent
-- on the scale, e.g. ngumbang ngisep for Balinese scales.
tuning :: ValName
tuning = "tuning"

-- | VNum: Separate notes into different voices.  This is used by integrate to
-- put them on their own tracks, and by the lilypond backend to split them into
-- their own voices.  Should be an integer from 1 to 4.
voice :: ValName
voice = "v"

-- | VSymbol: @right@, @r@,  @left@, or @l@.  Used by the lilypond backend, and
-- also by any call that relies on an instrument's parts being divided by hand.
hand :: ValName
hand = "hand"

-- | VNum: hold the start of a call for a certain amount of ScoreTime or
-- RealTime.
hold :: ValName
hold = "hold"


-- * values

umbang, isep :: Text
umbang = "umbang"
isep = "isep"
