-- | This module defines control names understood by the default calls, or by
-- the MIDI deriver.  There are also conventional names for certain controls
-- that instruments can use.  Although each instrument as its own independent
-- set of controls, they're easier to remember if they reuse some conventional
-- names.
module Derive.Controls where
import Prelude hiding (null)

import Util.Control
import qualified Derive.Score as Score
import Derive.Score (Control(..))


-- | Used as the default control by control block calls.  This is because
-- a ControlCall produces a Signal, but for it to be derived in a block it
-- needs a temporary name.
null :: Control
null = Control ""

-- | The tempo track is handled differently than other controls, and winds up
-- in the warp rather than the 'ControlMap'.
tempo :: Control
tempo = Control "tempo"

-- | Converted into velocity or breath depending on the instrument.
dynamic :: Control
dynamic = Score.c_dynamic

-- ** generally understood by the note deriver

-- | Scale note duration.  This is multiplicative, so 1 is no change.
--
-- Note duration is documented in 'Derive.Call.Note.duration_attributes'.
sustain :: Control
sustain = Control "sus"

-- | Add an absolute amount of real time to the duration of each note.
sustain_abs :: Control
sustain_abs = Control "sus-abs"

-- | Offset the start time of each note by a value between - start-rnd/2 and
-- start-rnd/2.  The start and end are shifted by the same amount, so the
-- duration of the note is unaffected.
start_rnd :: Control
start_rnd = Control "start-rnd"

-- | Shorten each note by a value between 0 and dur-rnd.  Shortening makes it
-- less likely that this will cause notes to become overlapping, which MIDI
-- doesn't like when they have the same pitch.
dur_rnd :: Control
dur_rnd = Control "dur-rnd"

-- ** understood by MIDI performer

-- | MIDI velocity and breath.  Generally you should use 'dynamic', which
-- will emit velocity or breath depending on the instrument.
velocity, breath :: Control
velocity = Control "vel"
breath = Control "breath"

-- | Channel pressure.
pressure :: Control
pressure = Control "pressure"

-- | Polyphonic aftertouch.  Unlike other controls, this one can share
-- channels.
aftertouch :: Control
aftertouch = Control "at"

-- | Modulation wheel.
mod :: Control
mod = Control "mod"

-- ** transposition

-- | Pitches respond to this with chromatic transposition.  This is stepwise
-- transposition for scales with no distinction between chromatic and diatonic.
chromatic :: Control
chromatic = Control "t-chromatic"

-- | Pitches respond to this with diatonic transposition, which generally
-- requires a key.  This is stepwise transposition for scales with no
-- distinction between chromatic and diatonic.
diatonic :: Control
diatonic = Control "t-diatonic"

-- | Transpose by NoteNumber, which is cents \/ 100.
nn :: Control
nn = Control "t-nn"

-- | Transpose in absolute hz.
hz :: Control
hz = Control "t-hz"

-- * conventional control names

-- | Volume control, conventionally mapped to CC 7.
vol :: Control
vol = Control "vol"

-- | Sustain pedal.
pedal :: Control
pedal = Control "pedal"

-- | Filter cutoff.
fc :: Control
fc = Control "fc"

-- | Filter resonance.
q :: Control
q = Control "q"

-- | Often 'mod' is mapped to vibrato, but when it's more specifically vibrato
-- depth rather than general modulation, whatever that is, it's nicer to use
-- a more specific name.
vib, vib_speed :: Control
vib = Control "vib"
vib_speed = Control "vib-speed"

mc :: Int -> Control
mc = Control . ("mc"<>) . showt

-- | Macro controls.  Many synths have general-purpose "change the timbre"
-- knobs.
mc1, mc2, mc3, mc4 :: Control
mc1 = mc 1
mc2 = mc 2
mc3 = mc 3
mc4 = mc 4
