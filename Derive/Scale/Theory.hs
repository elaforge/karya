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
    piano_intervals, piano_layout, diatonic_layout
    -- * NoteNumber diatonic transposition
    , diatonic_to_chromatic
    , simple_diatonic_to_chromatic
    , simple_chromatic_steps
    -- * symbolic transposition
    , transpose_diatonic, transpose_chromatic
    -- * input
    , enharmonics_of
    , pitch_to_semis, degree_to_semis
    , semis_to_pitch, pick_enharmonic, semis_to_pitch_sharps
    , semis_to_nn, fsemis_to_nn, nn_to_semis
    -- ** key
    , Key(key_tonic, key_name, key_intervals, key_signature, key_layout), key
    , accidentals_at_pc
    , Signature, Intervals
    , layout
    , layout_pc_per_octave, layout_semis_per_octave
    , contains_degree
#ifndef TESTING
    , Layout(layout_intervals)
#else
    , Layout(..)
    , calculate_signature, step_of
#endif
) where
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Vector as Boxed
import qualified Data.Vector.Unboxed as Vector

import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Lists as Lists
import qualified Util.Vector as Vector

import qualified Perform.Pitch as Pitch

import           Global


-- * constants

piano_intervals :: [Pitch.Semi]
piano_intervals = [2, 2, 1, 2, 2, 2, 1]

-- | The layout of keys on everyone's favorite boxed harp.
piano_layout :: Layout
piano_layout = layout piano_intervals

diatonic_layout :: Pitch.PitchClass -> Layout
diatonic_layout per_oct = layout $ replicate per_oct 1


-- * NoteNumber diatonic transposition

-- | Convert a fractional number of diatonic steps to chromatic steps.
diatonic_to_chromatic :: Key -> Pitch.Degree -> Double -> Pitch.FSemi
diatonic_to_chromatic key degree steps
    | steps == 0 = 0
    | steps > 0 = Num.scale (transpose isteps) (transpose (isteps+1)) frac
    | otherwise =
        Num.scale (transpose (isteps-1)) (transpose isteps) (1 - abs frac)
    where
    (isteps, frac) = properFraction steps
    transpose = fromIntegral . chromatic_steps key degree

-- | Convert diatonic steps to chromatic steps.
chromatic_steps :: Key -> Pitch.Degree -> Int -> Pitch.Semi
chromatic_steps key degree steps =
    case table Vector.!? (middle + step + steps2) of
        Just val -> oct_semis + val - table Vector.! (middle + step)
        -- 'make_table' should build a table big enough that this can't happen.
        Nothing -> error $ "ran out of transpose table for "
            ++ show (middle, step, steps2) ++ ": " ++ show table
    where
    step = step_of key degree
    (octaves, steps2) = steps `divMod` key_steps_per_octave key
    oct_semis = if octaves == 0 then 0
        else octaves * layout_semis_per_octave (key_layout key)
    middle = Vector.length table `div` 2
    table = key_transpose_table key

simple_diatonic_to_chromatic :: Intervals -> Pitch.PitchClass -> Double
    -> Pitch.FSemi
simple_diatonic_to_chromatic intervals pc steps
    | steps == 0 = 0
    | steps > 0 = Num.scale (transpose isteps) (transpose (isteps+1)) frac
    | otherwise =
        Num.scale (transpose (isteps-1)) (transpose isteps) (1 - abs frac)
    where
    (isteps, frac) = properFraction steps
    transpose = fromIntegral . simple_chromatic_steps intervals pc

simple_chromatic_steps :: Intervals -> Pitch.PitchClass -> Int -> Pitch.Semi
simple_chromatic_steps intervals pc steps =
    Num.sum $ take steps $ drop pc $ cycle $ Vector.toList intervals

-- * symbolic transposition

-- | Transpose a pitch diatonically.  If the key is diatonic (i.e. there is
-- a 1:1 relationship between note letters and scale degrees), then this will
-- increment or decrement the note letters by the number of steps and adjust
-- the accidentals based on the key signature.  Otherwise (i.e. for scales like
-- whole-tone or octatonic), it will figure out the number of chromatic steps
-- to transpose and act like 'transpose_chromatic'.
transpose_diatonic :: Key -> Step -> Pitch.Pitch -> Pitch.Pitch
transpose_diatonic key steps
        pitch@(Pitch.Pitch oct degree@(Pitch.Degree pc accs))
    | steps == 0 = pitch
    | otherwise = case key_signature key of
        Just _ -> Pitch.Pitch (oct + oct2) $ Pitch.Degree pc2 $
            accs - accidentals_at_pc key pc + accidentals_at_pc key pc2
        Nothing -> transpose_chromatic
            key (chromatic_steps key degree steps) pitch
    where
    (oct2, pc2) = (pc + steps) `divMod` key_steps_per_octave key

-- | Chromatic transposition.  Try to pick a spelling that makes sense for the
-- given key.
transpose_chromatic :: Key -> Pitch.Semi -> Pitch.Pitch -> Pitch.Pitch
transpose_chromatic key steps pitch
    | steps == 0 = pitch
    | otherwise = semis_to_pitch key $ pitch_to_semis layout pitch + steps
    where layout = key_layout key

pitch_to_semis :: Layout -> Pitch.Pitch -> Pitch.Semi
pitch_to_semis layout (Pitch.Pitch oct degree) =
    oct * layout_semis_per_octave layout + degree_to_semis layout degree

degree_to_semis :: Layout -> Pitch.Degree -> Pitch.Semi
degree_to_semis layout (Pitch.Degree pc_ accs) =
    Vector.sum (Vector.take pc (layout_intervals layout)) + accs
        + oct * layout_semis_per_octave layout
    where (oct, pc) = pc_ `divMod` layout_pc_per_octave layout

-- | Pick the most sensible enharmonic for the given pitch.
--
-- TODO I reduce to semis and then pick an enharmonic, so 5b# becomes 6c.  But
-- if they asked for 5b# they should get it.
pick_enharmonic :: Key -> Pitch.Pitch -> Pitch.Pitch
pick_enharmonic key = semis_to_pitch key . pitch_to_semis (key_layout key)

-- | Convert an absolute semitones value to a pitch.  This is a bit
-- complicated because it wants to find the best spelling for the given key.
semis_to_pitch :: Key -> Pitch.Semi -> Pitch.Pitch
semis_to_pitch key semis = mkpitch $ case key_signature key of
    Just sig -> case List.find (in_scale sig) enharmonics of
        Nothing -> pick_enharmonic (sharp_signature sig) enharmonics
        Just pitch -> pitch
    Nothing -> pick_enharmonic (sharp_tonic key) enharmonics
    where
    mkpitch (oct, degree) = Pitch.Pitch (octave + oct) degree
    -- The (Pitch.Degree (-1) 0) error value is icky, but here's why it should
    -- never happen: It happens when enharmonics is empty.  Since the values of
    -- layout_enharmonics are never [] as per the definition of 'layout', it
    -- means the mod of semis is out of range for the array, which means the
    -- sum of the intervals is larger than the length of layout_enharmonics.
    -- That shouldn't happen because layout_enharmonics is initialized to
    -- [..  | i <- intervals, a <- [0..i-1]].
    pick_enharmonic use_sharps notes = fromMaybe (0, Pitch.Degree (-1) 0) $
        Lists.minimumOn (key . Pitch.degree_accidentals . snd) notes
        where key accs = (if use_sharps then accs < 0 else accs > 0, abs accs)
    in_scale sig (_, degree) =
        sig Vector.!? step_of key degree
            == Just (Pitch.degree_accidentals degree)
    enharmonics = fromMaybe [] $ layout_enharmonics layout Boxed.!? steps
    (octave, steps) = semis `divMod` layout_semis_per_octave layout
    layout = key_layout key
    -- Sharpish looking key signatures favor sharps.
    sharp_signature sig = Vector.count (>0) sig >= Vector.count (<0) sig
    sharp_tonic = (>=0) . Pitch.degree_accidentals . key_tonic

-- | Like 'semis_to_pitch', but only emits sharps, so it doesn't require a key.
semis_to_pitch_sharps :: Layout -> Pitch.Semi -> Pitch.Pitch
semis_to_pitch_sharps layout semis = Pitch.Pitch (octave + oct) degree
    where
    (octave, steps) = semis `divMod` layout_semis_per_octave layout
    (oct, degree) = head $ enharmonics Boxed.! steps
    enharmonics = layout_enharmonics layout

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
semis_to_nn :: Pitch.Semi -> Int
semis_to_nn = (+12)

fsemis_to_nn :: Pitch.FSemi -> Pitch.NoteNumber
fsemis_to_nn = Pitch.NoteNumber . (+12)

nn_to_semis :: Int -> Pitch.Semi
nn_to_semis = subtract 12

-- * input

-- | Enharmonics of a pitch.
--
-- This choses the next highest enharmonic until it wraps around, so if you
-- repeatedly pick the first one you'll cycle through them all.
enharmonics_of :: Layout -> Pitch.Pitch -> [Pitch.Pitch]
enharmonics_of layout pitch =
    [ Pitch.Pitch (Pitch.pitch_octave pitch + oct) n
    | (oct, n) <-
        get_enharmonics (layout_intervals layout) (Pitch.pitch_degree pitch)
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
data Key = Key {
    key_tonic :: !Pitch.Degree
    -- | This is the name of the key without reference to its tonic, e.g.
    -- \"dorian\" or \"major\".
    , key_name :: !Text
    -- | Semitones between each scale degree.  This should have at least two
    -- octaves of intervals, as needed by 'chromatic_steps'.  If this is a
    -- diatonic key, each interval is one pitch class.
    , key_intervals :: Intervals
    -- | Nothing for a chromatic key.
    , key_signature :: Maybe Signature
    -- | Table to speed up diatonic transposition, see 'make_table'.
    , key_transpose_table :: Intervals
    -- | White and black keys, this is constant for keys
    , key_layout :: Layout
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
    { key_tonic = tonic
    , key_name = name
    , key_intervals = ints
    , key_signature = generate_signature tonic layout ints
    , key_transpose_table = make_table intervals
    , key_layout = layout
    }
    where ints = Vector.fromList intervals

-- | Precalculated transpositions so I can figure out a transposition with
-- a single table lookup.  This goes out to two octaves on either direction
-- so I can start at any degree and go up to an octave of transposition.
-- Everything past an octave is chopped off by divMod and transposed with
-- multiplication.
make_table :: [Pitch.Semi] -> Intervals
make_table intervals = Vector.fromList $
    reverse (drop 1 (make (-) (reverse intervals))) ++ make (+) intervals
    where make f = take (length intervals * 2) . scanl f 0 . cycle

generate_signature :: Pitch.Degree -> Layout -> Intervals -> Maybe Signature
generate_signature tonic layout intervals
    | Vector.length (layout_intervals layout) /= Vector.length intervals =
        Nothing
    | otherwise = Just $
        calculate_signature tonic (layout_intervals layout) intervals

calculate_signature :: Pitch.Degree -> Intervals -> Intervals -> Intervals
calculate_signature (Pitch.Degree pc accs) layout intervals =
    Vector.take (Vector.length intervals) $
        Vector.zipWith subtract (Vector.scanl (+) 0 (rotate pc layout))
            (Vector.scanl (+) accs intervals)
    where
    rotate n xs = post <> pre
        where (pre, post) = Vector.splitAt n xs

key_is_diatonic :: Key -> Bool
key_is_diatonic = Maybe.isJust . key_signature

instance Pretty Key where
    format key@(Key tonic name ints sig _table _layout) = Pretty.record title
        [ ("intervals",
            Pretty.format (Vector.take (key_steps_per_octave key) ints))
        , ("signature", Pretty.format sig)
        ]
        where
        title = Pretty.text "Key" Pretty.<+> Pretty.format tonic
            Pretty.<+> Pretty.text name

-- | The number of accidentals in the key signature at the given pitch class.
accidentals_at_pc :: Key -> Pitch.PitchClass -> Pitch.Accidentals
accidentals_at_pc key pc = fromMaybe 0 $ do
    sig <- key_signature key
    sig Vector.!? diatonic_step_of key pc

-- | Number of degrees in an octave for this scale.
--
-- This is different from the number of PCs per octave, because scales like
-- octatonic or whole tone don't have a mapping from PCs to scale degrees.
key_steps_per_octave :: Key -> Step
key_steps_per_octave = Vector.length . key_intervals

-- | Figure out the relative scale step of a degree in the given key.
step_of :: Key -> Pitch.Degree -> Step
step_of key degree
    | key_is_diatonic key = diatonic_step_of key (Pitch.degree_pc degree)
    | otherwise = Vector.find_before semis (key_intervals key)
    where semis = degree_to_semis (key_layout key) degree

-- | Figure out the (relative) scale step of an absolute PitchClass in
-- a diatonic key.  In a diatonic key, the step and pitch class are relative
-- and absolute versions of the same thing.
diatonic_step_of :: Key -> Pitch.PitchClass -> Step
diatonic_step_of key pc =
    (pc - Pitch.degree_pc (key_tonic key)) `mod` key_steps_per_octave key

-- ** Layout

-- | A Layout represents the configuration of white and black keys.
data Layout = Layout {
    -- | Map PitchClass to the number of sharps above it.
    layout_intervals :: !Intervals
    -- | Map Pitch.Semis to the enharmonic Notes at that PitchClass.
    , layout_enharmonics :: !(Boxed.Vector [(Pitch.Octave, Pitch.Degree)])
    } deriving (Eq, Show)

layout_semis_per_octave :: Layout -> Pitch.Semi
layout_semis_per_octave = Vector.sum . layout_intervals

layout_pc_per_octave :: Layout -> Pitch.PitchClass
layout_pc_per_octave = Vector.length . layout_intervals

layout :: [Pitch.Semi] -> Layout
layout intervals = Layout
    { layout_intervals = vec
    , layout_enharmonics = Boxed.fromList $
        map (\n -> (0, n) : get_enharmonics vec n) notes
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
get_enharmonics :: Intervals -> Pitch.Degree -> [(Pitch.Octave, Pitch.Degree)]
get_enharmonics intervals (Pitch.Degree note_pc note_accs) =
    [ mknote intervals (note_pc + pc) (note_accs + accs)
    | (pc, accs) <- pcs, abs (note_accs + accs) < 3
    ]
    where
    -- Find the distance in semitones from neighbor pcs.
    pcs =
        [ (1, -diffs [0])
        , (2, -diffs [0, 1])
        , (-2, diffs [-2, -1])
        , (-1, diffs [-1])
        ]
    diffs = Num.sum . map (layout_at intervals . (note_pc+))
    mknote intervals pc accs = (oct, Pitch.Degree pc2 accs)
        where (oct, pc2) = pc `divMod` Vector.length intervals

layout_at :: Intervals -> Pitch.PitchClass -> Pitch.Accidentals
layout_at intervals pc =
    fromMaybe 0 $ intervals Vector.!? (pc `mod` Vector.length intervals)

-- | True if the degree exists as its own key in the layout.
--
-- For a relative scale, the Intervals should be from 'key_intervals', which
-- considers that the tonic is shifted to PC 0.  For an absolute scale, the
-- keyboard never shifts, so use 'layout_intervals'.
contains_degree :: Intervals -> Pitch.Degree -> Bool
contains_degree intervals (Pitch.Degree pc acc)
    | acc >= 0 = acc < layout_at intervals pc
    | otherwise = layout_at intervals (pc - 1) + acc > 0
