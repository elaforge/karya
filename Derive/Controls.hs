-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | This module defines control names understood by the default calls, or by
-- the MIDI deriver.  There are also conventional names for certain controls
-- that instruments can use.  Although each instrument as its own independent
-- set of controls, they're easier to remember if they reuse some conventional
-- names.
module Derive.Controls where
import Prelude hiding (null)

import Util.Control
import qualified Derive.Score as Score
import Derive.Score (Control)
import qualified Perform.Pitch as Pitch


-- | These controls should be combined with addition by default instead of
-- multiplication.
additive_controls :: [Control]
additive_controls = [diatonic, chromatic, nn, hz]

-- | Used as the default control by control block calls.  This is because
-- a ControlCall produces a Signal, but for it to be derived in a block it
-- needs a temporary name.
null :: Control
null = ""

-- | The tempo track is handled differently than other controls, and winds up
-- in the warp rather than the 'ControlMap'.
tempo :: Control
tempo = "tempo"

-- | Converted into velocity or breath depending on the instrument.
dynamic :: Control
dynamic = Score.c_dynamic

-- ** generally understood by the note deriver

-- | Scale note duration.  This is multiplicative, so 1 is no change.
--
-- Note duration is documented in 'Derive.Call.Note.duration_attributes'.
sustain :: Control
sustain = "sus"

-- | Add an absolute amount of real time to the duration of each note.
sustain_abs :: Control
sustain_abs = "sus-abs"

-- | Start offset, in RealTime.  This is added to event start times.  The end
-- times stay the same, so it changes note duration, and is limited to not
-- force a note to 0 duration.
start_s :: Control
start_s = "start-s"

-- | Start offset, in ScoreTime.
start_t :: Control
start_t = "start-t"

-- ** specific to instruments

-- | Variable mute control, where 1 is fully muted.
mute :: Control
mute = "mute"

-- ** understood by MIDI performer

-- | MIDI velocity and breath.  Generally you should use 'dynamic', which
-- will emit velocity or breath depending on the instrument.
velocity, breath :: Control
velocity = "vel"
breath = "breath"

-- | Channel pressure.
pressure :: Control
pressure = "pressure"

-- | Polyphonic aftertouch.  Unlike other controls, this one can share
-- channels.
aftertouch :: Control
aftertouch = "at"

-- | Modulation wheel.
mod :: Control
mod = "mod"

-- ** transposition

transpose_control :: Pitch.Transpose -> (Double, Control)
transpose_control t = case t of
    Pitch.Diatonic d -> (d, diatonic)
    Pitch.Chromatic d -> (d, chromatic)
    Pitch.Nn d -> (d, nn)

transpose_type :: Score.Type -> Maybe Control
transpose_type t = case t of
    Score.Diatonic -> Just diatonic
    Score.Chromatic -> Just chromatic
    Score.Nn -> Just nn
    _ -> Nothing

-- | Pitches respond to this with diatonic transposition, which generally
-- requires a key.  This is stepwise transposition for scales with no
-- distinction between chromatic and diatonic.
diatonic :: Control
diatonic = "t-dia"

-- | Pitches respond to this with chromatic transposition.  This is stepwise
-- transposition for scales with no distinction between chromatic and diatonic.
chromatic :: Control
chromatic = "t-chrom"

-- | Transpose by NoteNumber, which is cents \/ 100.
nn :: Control
nn = "t-nn"

-- | Transpose in absolute hz.
hz :: Control
hz = "t-hz"

-- * conventional control names

-- | Volume control, conventionally mapped to CC 7.
vol :: Control
vol = "vol"

-- | Sustain pedal.
pedal :: Control
pedal = "pedal"

-- * standard names for patches

-- | Low and high pass filter cutoff.
lpf, hpf :: Control
lpf = "lpf"
hpf = "hpf"

-- | Filter resonance.
q :: Control
q = "q"

-- | Often 'mod' is mapped to vibrato, but when it's more specifically vibrato
-- depth rather than general modulation, whatever that is, it's nicer to use
-- a more specific name.
vib, vib_speed :: Control
vib = "vib"
vib_speed = "vib-speed"

mc :: Int -> Control
mc = Score.control . ("mc"<>) . showt

-- | Macro controls.  Many synths have general-purpose "change the timbre"
-- knobs.
mc1, mc2, mc3, mc4 :: Control
mc1 = mc 1
mc2 = mc 2
mc3 = mc 3
mc4 = mc 4
