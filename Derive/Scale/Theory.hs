-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{- | Functions to manipulate pitches in scales according to the rules of
    standard western music theory.

    Pitches are represented as 'Pitch.Pitch'es and 'Pitch.Degree's.  They're
    generalized to work with any number of 'Pitch.PitchClass'es, but since each
    scale carries an implied key 'Layout' it can only handle pitch classes in
    a certain range.  Internally there are no checks that the pitch class is in
    range, so the range has to be checked on parse or show.  Parsing and
    showing is handled in "Derive.Scale.TheoryFormat".

    @
        db      eb  fb      gb      ab      bb  cb
        c#      d#      e#  f#      g#      a#      b#
    c       d       e   f       g       a       b   c
    |   |   |   |   |   |   |   |   |   |   |   |   |
    @
-}
module Derive.Scale.Theory (
    -- * constants
    pianoIntervals, pianoLayout, diatonicLayout
    -- * NoteNumber diatonic transposition
    , diatonicToChromatic
    -- * symbolic transposition
    , transposeDiatonic, transposeChromatic
    -- * input
    , enharmonicsOf
    , pitchToSemis, degreeToSemis
    , semisToPitch, pickEnharmonic, semisToPitchSharps
    , semisToNn, fsemisToNn, nnToSemis
    -- ** key
    , Key(keyTonic, keyName, keyIntervals, keySignature, keyLayout), key
    , accidentalsAtPc
    , Signature, Intervals
    , layout
    , layoutPcPerOctave, layoutSemisPerOctave
    , containsDegree
#ifndef TESTING
    , Layout(layoutIntervals)
#else
    , Layout(..)
    , calculateSignature, stepOf
#endif
) where
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Vector as Boxed
import qualified Data.Vector as Vector

import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Lists as Lists
import qualified Util.Vector as Vector

import qualified Perform.Pitch as Pitch

import           Global


-- * constants

pianoIntervals :: [Pitch.Semi]
pianoIntervals = [2, 2, 1, 2, 2, 2, 1]

-- | The layout of keys on everyone's favorite boxed harp.
pianoLayout :: Layout
pianoLayout = layout pianoIntervals

diatonicLayout :: Pitch.PitchClass -> Layout
diatonicLayout perOct = layout $ replicate perOct 1


-- * NoteNumber diatonic transposition

-- | Convert a fractional number of diatonic steps to chromatic steps.
diatonicToChromatic :: Key -> Pitch.Degree -> Double -> Pitch.FSemi
diatonicToChromatic key degree steps
    | steps == 0 = 0
    | steps > 0 = Num.scale (transpose isteps) (transpose (isteps+1)) frac
    | otherwise =
        Num.scale (transpose (isteps-1)) (transpose isteps) (1 - abs frac)
    where
    (isteps, frac) = properFraction steps
    transpose = fromIntegral . chromaticSteps key degree

-- | Convert diatonic steps to chromatic steps.
chromaticSteps :: Key -> Pitch.Degree -> Int -> Pitch.Semi
chromaticSteps key degree steps =
    case table Vector.!? (middle + step + steps2) of
        Just val -> octSemis + val - table Vector.! (middle + step)
        -- 'make_table' should build a table big enough that this can't happen.
        Nothing -> error $ "ran out of transpose table for "
            ++ show (middle, step, steps2) ++ ": " ++ show table
    where
    step = stepOf key degree
    (octaves, steps2) = steps `divMod` keyStepsPerOctave key
    octSemis = if octaves == 0 then 0
        else octaves * layoutSemisPerOctave (keyLayout key)
    middle = Vector.length table `div` 2
    table = keyTransposeTable key

-- * symbolic transposition

-- | Transpose a pitch diatonically.  If the key is diatonic (i.e. there is
-- a 1:1 relationship between note letters and scale degrees), then this will
-- increment or decrement the note letters by the number of steps and adjust
-- the accidentals based on the key signature.  Otherwise (i.e. for scales like
-- whole-tone or octatonic), it will figure out the number of chromatic steps
-- to transpose and act like 'transpose_chromatic'.
transposeDiatonic :: Key -> Step -> Pitch.Pitch -> Pitch.Pitch
transposeDiatonic key steps
        pitch@(Pitch.Pitch oct degree@(Pitch.Degree pc accs))
    | steps == 0 = pitch
    | otherwise = case keySignature key of
        Just _ -> Pitch.Pitch (oct + oct2) $ Pitch.Degree pc2 $
            accs - accidentalsAtPc key pc + accidentalsAtPc key pc2
        Nothing -> transposeChromatic
            key (chromaticSteps key degree steps) pitch
    where
    (oct2, pc2) = (pc + steps) `divMod` keyStepsPerOctave key

-- | Chromatic transposition.  Try to pick a spelling that makes sense for the
-- given key.
transposeChromatic :: Key -> Pitch.Semi -> Pitch.Pitch -> Pitch.Pitch
transposeChromatic key steps pitch
    | steps == 0 = pitch
    | otherwise = semisToPitch key $ pitchToSemis layout pitch + steps
    where layout = keyLayout key

pitchToSemis :: Layout -> Pitch.Pitch -> Pitch.Semi
pitchToSemis layout (Pitch.Pitch oct degree) =
    oct * layoutSemisPerOctave layout + degreeToSemis layout degree

degreeToSemis :: Layout -> Pitch.Degree -> Pitch.Semi
degreeToSemis layout (Pitch.Degree pc_ accs) =
    Vector.sum (Vector.take pc (layoutIntervals layout)) + accs
        + oct * layoutSemisPerOctave layout
    where (oct, pc) = pc_ `divMod` layoutPcPerOctave layout

-- | Pick the most sensible enharmonic for the given pitch.
--
-- TODO I reduce to semis and then pick an enharmonic, so 5b# becomes 6c.  But
-- if they asked for 5b# they should get it.
pickEnharmonic :: Key -> Pitch.Pitch -> Pitch.Pitch
pickEnharmonic key = semisToPitch key . pitchToSemis (keyLayout key)

-- | Convert an absolute semitones value to a pitch.  This is a bit
-- complicated because it wants to find the best spelling for the given key.
semisToPitch :: Key -> Pitch.Semi -> Pitch.Pitch
semisToPitch key semis = mkpitch $ case keySignature key of
    Just sig -> case List.find (inScale sig) enharmonics of
        Nothing -> pickEnharmonic (sharpSignature sig) enharmonics
        Just pitch -> pitch
    Nothing -> pickEnharmonic (sharpTonic key) enharmonics
    where
    mkpitch (oct, degree) = Pitch.Pitch (octave + oct) degree
    -- The (Pitch.Degree (-1) 0) error value is icky, but here's why it should
    -- never happen: It happens when enharmonics is empty.  Since the values of
    -- layout_enharmonics are never [] as per the definition of 'layout', it
    -- means the mod of semis is out of range for the array, which means the
    -- sum of the intervals is larger than the length of layout_enharmonics.
    -- That shouldn't happen because layout_enharmonics is initialized to
    -- [..  | i <- intervals, a <- [0..i-1]].
    pickEnharmonic useSharps notes = fromMaybe (0, Pitch.Degree (-1) 0) $
        Lists.minimumOn (key . Pitch.degreeAccidentals . snd) notes
        where key accs = (if useSharps then accs < 0 else accs > 0, abs accs)
    inScale sig (_, degree) =
        sig Vector.!? stepOf key degree
            == Just (Pitch.degreeAccidentals degree)
    enharmonics = fromMaybe [] $ layoutEnharmonics layout Boxed.!? steps
    (octave, steps) = semis `divMod` layoutSemisPerOctave layout
    layout = keyLayout key
    -- Sharpish looking key signatures favor sharps.
    sharpSignature sig = Vector.count (>0) sig >= Vector.count (<0) sig
    sharpTonic = (>=0) . Pitch.degreeAccidentals . keyTonic

-- | Like 'semis_to_pitch', but only emits sharps, so it doesn't require a key.
semisToPitchSharps :: Layout -> Pitch.Semi -> Pitch.Pitch
semisToPitchSharps layout semis = Pitch.Pitch (octave + oct) degree
    where
    (octave, steps) = semis `divMod` layoutSemisPerOctave layout
    (oct, degree) = head $ enharmonics Boxed.! steps
    enharmonics = layoutEnharmonics layout

-- | Convert Semis to integral NNs.  This is only valid for 12TET, which is the
-- only scale where Semis correspond directly to NNs.
--
-- It doesn't return 'Pitch.NoteNumber' because these values are specifically
-- integral.
--
-- NOTE [middle-c] Middle C is 5 octaves above NN 0, but is conventially called
-- 4c.  Therefore, a 'Pitch' with octave 0 actually starts at NN 12 (in 12TET),
-- and I have to add an octave when converting from NNs and subtract an octave
-- when converting from NNs.
--
-- Previously I considered the octave offset a part of formatting, and added
-- an octave in 'TheoryFormat.p_octave' and subtracted an octave in
-- 'TheoryFormat.show_octave'.  But I was unsatisfied because it applied to
-- all scales, and it seemed confusing to ask for a Pitch with octave 4 and get
-- a note with octave 3.  TODO maybe the add/subtract octave should just go in
-- TheoryFormat.absolute_c?
semisToNn :: Pitch.Semi -> Int
semisToNn = (+12)

fsemisToNn :: Pitch.FSemi -> Pitch.NoteNumber
fsemisToNn = Pitch.NoteNumber . (+12)

nnToSemis :: Int -> Pitch.Semi
nnToSemis = subtract 12

-- * input

-- | Enharmonics of a pitch.
--
-- This choses the next highest enharmonic until it wraps around, so if you
-- repeatedly pick the first one you'll cycle through them all.
enharmonicsOf :: Layout -> Pitch.Pitch -> [Pitch.Pitch]
enharmonicsOf layout pitch =
    [ Pitch.Pitch (Pitch.pitchOctave pitch + oct) n
    | (oct, n) <-
        getEnharmonics (layoutIntervals layout) (Pitch.pitchDegree pitch)
    ]

-- * step

{- | A degree is one step of a scale.  Unlike 'Pitch.PitchClass' it's relative
    to the tonic of the key, but also may have a different range.  This is
    because some scales, such as whole-tone or octatonic, have fewer or more
    degrees than 7, even though the underlying notation system uses only
    7 letters.  This means that not every Degree will map to a PitchClass.

    Another approach is to change the number of PitchClasses, which would
    result in a--h for octatonic, but it would not admit easy modulation from
    an octatonic scale to a septatonic one.

    'Key' has more documentation about the PitchClass and Step distinction.
-}
type Step = Int

-- * Key

{- | A Key is a scale along with a tonic Pitch.

    There's a distinction between \"diatonic\" and \"chromatic\" keys.  It's
    not really standard terminology, but within this module I call scales with
    a 1:1 'Pitch.PitchClass' to 'Degree' mapping \"diatonic\", and the ones
    without \"chromatic\".  That's because diatonic transposition for the
    former kind of scale is defined in terms of pitch classes, regardless of
    what accidentals the 'Pitch.Degree' may have, but the latter kind of scale
    must resort to chromatic transposition, losing the spelling of the original
    note.  Ultimately there is a tension between diatonic and chromatic
    systems.
-}
data Key = Key
    { keyTonic :: !Pitch.Degree
    -- | This is the name of the key without reference to its tonic, e.g.
    -- \"dorian\" or \"major\".
    , keyName :: !Text
    -- | Semitones between each scale degree.  This should have at least two
    -- octaves of intervals, as needed by 'chromatic_steps'.  If this is a
    -- diatonic key, each interval is one pitch class.
    , keyIntervals :: Intervals
    -- | Nothing for a chromatic key.
    , keySignature :: Maybe Signature
    -- | Table to speed up diatonic transposition, see 'make_table'.
    , keyTransposeTable :: Intervals
    -- | White and black keys, this is constant for keys
    , keyLayout :: Layout
    } deriving (Eq, Show)

-- | Map from a Step to the number of sharps or flats at that Step.
type Signature = Vector.Vector Pitch.Accidentals
-- | Semitones between each scale degree.
type Intervals = Vector.Vector Pitch.Semi

-- | Make a Key given intervals and a layout.  If the number of intervals are
-- equal to the number of intervals in the layout, the scale is considered
-- diatonic and will get a 'Signature'.
key :: Pitch.Degree -> Text -> [Pitch.Semi] -> Layout -> Key
key tonic name intervals layout = Key
    { keyTonic = tonic
    , keyName = name
    , keyIntervals = ints
    , keySignature = generateSignature tonic layout ints
    , keyTransposeTable = makeTable intervals
    , keyLayout = layout
    }
    where ints = Vector.fromList intervals

-- | Precalculated transpositions so I can figure out a transposition with
-- a single table lookup.  This goes out to two octaves on either direction
-- so I can start at any degree and go up to an octave of transposition.
-- Everything past an octave is chopped off by divMod and transposed with
-- multiplication.
makeTable :: [Pitch.Semi] -> Intervals
makeTable intervals = Vector.fromList $
    reverse (drop 1 (make (-) (reverse intervals))) ++ make (+) intervals
    where make f = take (length intervals * 2) . scanl f 0 . cycle

generateSignature :: Pitch.Degree -> Layout -> Intervals -> Maybe Signature
generateSignature tonic layout intervals
    | Vector.length (layoutIntervals layout) /= Vector.length intervals =
        Nothing
    | otherwise = Just $
        calculateSignature tonic (layoutIntervals layout) intervals

calculateSignature :: Pitch.Degree -> Intervals -> Intervals -> Intervals
calculateSignature (Pitch.Degree pc accs) layout intervals =
    Vector.take (Vector.length intervals) $
        Vector.zipWith subtract (Vector.scanl (+) 0 (rotate pc layout))
            (Vector.scanl (+) accs intervals)
    where
    rotate n xs = post <> pre
        where (pre, post) = Vector.splitAt n xs

keyIsDiatonic :: Key -> Bool
keyIsDiatonic = Maybe.isJust . keySignature

instance Pretty Key where
    format key@(Key tonic name ints sig _table _layout) = Pretty.record title
        [ ("intervals",
            Pretty.format (Vector.take (keyStepsPerOctave key) ints))
        , ("signature", Pretty.format sig)
        ]
        where
        title = Pretty.text "Key" Pretty.<+> Pretty.format tonic
            Pretty.<+> Pretty.text name

-- | The number of accidentals in the key signature at the given pitch class.
accidentalsAtPc :: Key -> Pitch.PitchClass -> Pitch.Accidentals
accidentalsAtPc key pc = fromMaybe 0 $ do
    sig <- keySignature key
    sig Vector.!? diatonicStepOf key pc

-- | Number of degrees in an octave for this scale.
--
-- This is different from the number of PCs per octave, because scales like
-- octatonic or whole tone don't have a mapping from PCs to scale degrees.
keyStepsPerOctave :: Key -> Step
keyStepsPerOctave = Vector.length . keyIntervals

-- | Figure out the relative scale step of a degree in the given key.
stepOf :: Key -> Pitch.Degree -> Step
stepOf key degree
    | keyIsDiatonic key = diatonicStepOf key (Pitch.degreePc degree)
    | otherwise = Vector.findBefore semis (keyIntervals key)
    where semis = degreeToSemis (keyLayout key) degree

-- | Figure out the (relative) scale step of an absolute PitchClass in
-- a diatonic key.  In a diatonic key, the step and pitch class are relative
-- and absolute versions of the same thing.
diatonicStepOf :: Key -> Pitch.PitchClass -> Step
diatonicStepOf key pc =
    (pc - Pitch.degreePc (keyTonic key)) `mod` keyStepsPerOctave key

-- ** Layout

-- | A Layout represents the configuration of white and black keys.
data Layout = Layout
    -- | Map PitchClass to the number of sharps above it.
    { layoutIntervals :: !Intervals
    -- | Map Pitch.Semis to the enharmonic Notes at that PitchClass.
    , layoutEnharmonics :: !(Boxed.Vector [(Pitch.Octave, Pitch.Degree)])
    } deriving (Eq, Show)

layoutSemisPerOctave :: Layout -> Pitch.Semi
layoutSemisPerOctave = Vector.sum . layoutIntervals

layoutPcPerOctave :: Layout -> Pitch.PitchClass
layoutPcPerOctave = Vector.length . layoutIntervals

layout :: [Pitch.Semi] -> Layout
layout intervals = Layout
    { layoutIntervals = vec
    , layoutEnharmonics = Boxed.fromList $
        map (\n -> (0, n) : getEnharmonics vec n) notes
    }
    where
    vec = Vector.fromList intervals
    notes =
        [ Pitch.Degree pc accs
        | (pc, int) <- zip [0..] intervals, accs <- [0..int-1]
        ]

-- | Enharmonics of a note, along with an octave offset if the enharmonic
-- wrapped an octave boundary.
--
-- This choses the next highest enharmonic until it wraps around, so if you
-- repeatedly pick the first one you'll cycle through them all.
getEnharmonics :: Intervals -> Pitch.Degree -> [(Pitch.Octave, Pitch.Degree)]
getEnharmonics intervals (Pitch.Degree notePc noteAccs) =
    [ mknote intervals (notePc + pc) (noteAccs + accs)
    | (pc, accs) <- pcs, abs (noteAccs + accs) < 3
    ]
    where
    -- Find the distance in semitones from neighbor pcs.
    pcs =
        [ (1, -diffs [0])
        , (2, -diffs [0, 1])
        , (-2, diffs [-2, -1])
        , (-1, diffs [-1])
        ]
    diffs = Num.sum . map (layoutAt intervals . (notePc+))
    mknote intervals pc accs = (oct, Pitch.Degree pc2 accs)
        where (oct, pc2) = pc `divMod` Vector.length intervals

layoutAt :: Intervals -> Pitch.PitchClass -> Pitch.Accidentals
layoutAt intervals pc =
    fromMaybe 0 $ intervals Vector.!? (pc `mod` Vector.length intervals)

-- | True if the degree exists as its own key in the layout.
--
-- For a relative scale, the Intervals should be from 'key_intervals', which
-- considers that the tonic is shifted to PC 0.  For an absolute scale, the
-- keyboard never shifts, so use 'layout_intervals'.
containsDegree :: Intervals -> Pitch.Degree -> Bool
containsDegree intervals (Pitch.Degree pc acc)
    | acc >= 0 = acc < layoutAt intervals pc
    | otherwise = layoutAt intervals (pc - 1) + acc > 0
