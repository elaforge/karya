-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Define a few inhabitants of Environ which are used by the built-in set of
-- calls.  Expected types are in 'Derive.TrackLang.hardcoded_types'.
module Derive.EnvKey where
import Data.Text (Text)

import Derive.BaseTypes (Key)


-- * directly supported by core derivers

-- | VList: arguments for a 'Derive.Call.Tags.requires_postproc' call.
-- Also see 'Derive.Call.Post.make_delayed'.
args :: Key
args = "args"

-- | VAttributes: Default set of attrs.
attributes :: Key
attributes = "attr"

-- | ScoreTime: the block deriver sets it to the ScoreTime end of the block.
-- Blocks always start at zero, but this is the only way for a call to know if
-- an event is at the end of the block.
block_end :: Key
block_end = "block-end"

-- | VSymbol: Set to the control that is being derived, inside of a control
-- track.
control :: Key
control = "control"

-- | VInstrument: Default instrument.
instrument :: Key
instrument = "inst"

-- | VSymbol: Many scales use this for scale-specific configuration.
-- Some scales conceptually have a separate key and mode, e.g. C minor, in
-- which case they're combined into @key@.
key :: Key
key = "key"

-- | VNum (ScoreTime): End time of the note event.  This is set when evaluating
-- note events so that inverted control tracks know when their parent event
-- ends.
note_end :: Key
note_end = "note-end"

-- | This is just like 'note_end', except, you know, the other end.
note_start :: Key
note_start = "note-start"

-- | VSymbol: Set along with 'control' to the 'Derive.Derive.Merge' function
-- which will be used for this control track.  Calls can use this to subvert
-- the merge function and emit an absolute value.
--
-- Values are @compose@ for tempo tracks, @set@, or any of the names from
-- 'Derive.Derive.ControlOp'.
merge :: Key
merge = "merge"

-- | VSymbol: Default scale, used by pitch tracks with a @*@ title.
scale :: Key
scale = "scale"

-- | VNum: Random seed used by randomization functions.  Can be explicitly
-- initialized to capture a certain \"random\" variation.
--
-- This is rounded to an integer, so only integral values make sense.
seed :: Key
seed = "seed"

-- | VNum: Sampling rate used by signal interpolators.
srate :: Key
srate = "srate"

-- | VNum: Set the default tempo, overriding 'Ui.State.default_tempo'.  This
-- only applies if there is no toplevel tempo track, and generally only has an
-- effect if the block is played as a toplevel block since it's a constant
-- tempo.
--
-- Previously I would directly set the tempo warp in the equal call, but tempo
-- setting has to be at the toplevel tempo track for it to interact properly
-- with block call stretching.
tempo :: Key
tempo = "tempo"

-- | VNum: this is the count of the tracks with the same instrument, starting
-- from 0 on the left.  So three tracks named @>pno@ would be 0, 1, and 2,
-- respectively.  Used with "Derive.Call.InferTrackVoice".
track_voice :: Key
track_voice = "track-voice"

-- * internal

-- | RealTime: suppress other notes until this time, inclusive.  Only events
-- without a suppress-until will be retained.  Applied by @infer-duration@, see
-- "Derive.Call.Post.Move".
suppress_until :: Key
suppress_until = "suppress-until"

-- | VNum: This is a bit of a hack for the dynamic to velocity conversion in
-- "Perform.Midi.Convert".  The default note deriver stashes the control
-- function output here so it can use it for note on velocity.  Otherwise I
-- couldn't use ControlFunctions (e.g. randomization) for note on velocity
-- because control functions don't exist after MIDI conversion.
--
-- Details in NOTE [EnvKey.dynamic_val].
dynamic_val :: Key
dynamic_val = "dyn-val"

-- | Like 'dynamic_val', but for the release velocity.  Set from
-- 'Derive.Controls.release_velocity'.
release_val :: Key
release_val = "release-val"

-- | VNum: Set from 'Derive.Controls.attack_velocity' in the same way as
-- 'release_val'.
attack_val :: Key
attack_val = "attack-val"

-- | RealTime: This stores the RealTime sum of 'Derive.Controls.start_s' and
-- 'Derive.Controls.start_t', and is later applied by the @apply-start-offset@
-- postproc.
start_offset_val :: Key
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
-- For instruments with less complicated scale requirements, NoteNumber is
-- simpler.
instrument_top :: Key
instrument_top = "inst-top"

instrument_bottom :: Key
instrument_bottom = "inst-bottom"

-- | List VPitch: tuning of open strings for this instrument.  The pitches
-- should be probably absolute NNs, not in any scale, so they work regardless
-- of which scale you happen to be in.
--
-- TODO maybe it should be VNotePitch as with 'instrument_top'?
open_strings :: Key
open_strings = "open-strings"

-- | VSymbol: Instrument role, e.g. 'polos' or 'sangsih'.
role :: Key
role = "role"

-- | VSymbol: Kind of tuning for the scale in scope.  The meaning is dependent
-- on the scale, e.g. ngumbang ngisep for Balinese scales.
tuning :: Key
tuning = "tuning"

-- | VNum: Separate notes into different voices.  This is used by integrate to
-- put them on their own tracks, and by the lilypond backend to split them into
-- their own voices.  Should be an integer from 1 to 4.
voice :: Key
voice = "v"

-- | VSymbol: @right@, @r@,  @left@, or @l@.  Used by the lilypond backend, and
-- also by any call that relies on an instrument's parts being divided by hand.
hand :: Key
hand = "hand"

-- | VNum: hold the start of a call for a certain amount of ScoreTime or
-- RealTime.
hold :: Key
hold = "hold"


-- * values

-- | Instrument role.
polos, sangsih :: Text
polos = "polos"
sangsih = "sangsih"


{- NOTE [EnvKey.dynamic_val]
    I originally intended to handle 'Instrument.Pressure' with a note call
    override.  But it turns out that Pressure is also used by Cmd, so I still
    need it.  But to get velocity from control functions I have to include the
    ControlValMap in Score.Event.

    I waffled for a long time about whether it was better to handle in the note
    call or in conversion, and initially favored the note call because it feels
    like complexity should go in Derive, which is configurable, and not in
    Convert.  But it turns out doing dyn mapping in Derive then breaks
    integration, so I'd have to undo it for integration.  That made me think
    that this is really a low level MIDI detail and perhaps it's best handled
    by Convert after all.

    One problem with doing the dyn conversion here is that for a control
    function on dyn to have any effect I need the value from the control
    function, which means I need a scalar value.  But by the time I get here
    the control functions are already gone.  The note call can't know
    which of the dyn signal or control function is wanted, because that
    decision is made here.  One solution was to put a ControlValMap in
    Score.Event so they get here, but that means that anything that modifies
    controls also has to remember to modify the ControlValMap.  I can do that
    by updating 'Score.modify_control', but it seems like overkill when all
    I really want is to communicate the dyn value.  So instead I stash the
    control function value in 'Controls.dynamic_function'.  Unfortunately this
    brings it's own complications since now I need to remember to modify it
    when I modify an event's dynamic, and filter it out of integration so it
    doesn't create a track for it.

    So neither way is very satisfying, but at least this way doesn't require
    a whole new field in Score.Event.  Perhaps I'll come up with something
    better someday.
-}
