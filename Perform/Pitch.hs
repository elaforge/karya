{- |

Pitch handling may have to be a little more fancy than the usual controller
signals, which are just Vals.  This is because I want to represent them in
a higher level form, like (pitch_class, octave), so derivers can transpose by
scale degree or octave in a generic way.

I think I should make Signal polymorphic.

-}
module Perform.Pitch where
import Util.Pretty
import qualified Midi.Midi as Midi

{-
-- | Name for logging and whatnot, and Octave PitchClass are so
data Pitch = Pitch Scale String Octave PitchClass deriving (Eq, Ord, Show)
type Octave = Int
type PitchClass = Double

Then I have to turn it into hz...
- Typeclass, but then I can't have different scales in one Signal.  I'd like
to be able to mix scales freely.
- existential, but I don't think I can make that storable
- a Scale enumeration, but then I have to edit the Pitch module to add another
- a ScaleId and some way to look them up.  It's more complicated but seems to
be the only one that works.

I think efficient signals of pitches are important for good pitch handling, but
I'm not sure they should be in the scale.  Maybe I should convert pitches to hz
and then interpolate?

The thing is, while I want to be able to slide between two scale degrees,
I usually will want to do it "evenly" which means logarithmically.  If I do it
linear by the scale degrees, the slide will sound funny since it'll go faster
where the scale is wider.  However, it's also important to be able to go
between scale notes in a set time.  That's still linear, actually.

I also want to be able to

data Scale = forall a. Scale a (a -> (Int, Double)) ((Int, Double) -> a)
Signal should be [(

But really, what I want to transpose is the high level points, the actual
sampled points should be from final pitch to final pitch.

3+10hz +1 should become 4+10hz

+5 - i+0 plus transpose +1: it should become just like +6 - i+1
but if it gets sampled to
5, 4.5, etc.
then I would have to be able to add an interval to an arbitrary frequency, and
I can't do that if the intervals are irregular.

this implies that I have to keep the high level signal as written by the user,
I can't sample out curves.

Say vibrato, it should be in logarithmic but absolute units.

3 kinds of units: absolute hz, absolute log (ratios), scale (irregular distance)

hz and log (equal tempered) is easy because they are both regular, so I can
add intervals, but I still need to keep them separate, so 64+10hz can stay at
+10hz.

-}

-- frequency = 440 * 2^((pitch - 69)/12)
data Pitch = Pitch String NoteNumber deriving (Eq, Ord, Show)

-- newtype Hz = Hz Double deriving (Eq, Ord, Show)

-- | This is the same note range as MIDI, so MIDI note 0 is NoteNumber 0,
-- at 8.176 Hz.  Middle C is octave NoteNumber 60, octave 5.
newtype NoteNumber = NoteNumber Double deriving (Eq, Ord, Show)

-- | Convert NoteNumber to a MIDI key number.  Rounds down.
midi_nn :: Pitch -> Midi.Key
midi_nn (Pitch _ (NoteNumber nn)) = floor nn

-- | Number of cents the given Pitch is above its equal tempered pitch.
cents :: Pitch -> Int
cents (Pitch _ (NoteNumber nn)) = floor (nn * 100) `mod` 100

from_midi_nn :: Integral a => String -> a -> Pitch
from_midi_nn name nn = Pitch name (NoteNumber (fromIntegral nn))

instance Pretty Pitch where
    pretty (Pitch s _) = show s
