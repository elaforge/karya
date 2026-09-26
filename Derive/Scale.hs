-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Scale is actually defined in "Derive.Deriver.Monad" to avoid circular
-- imports.  But you should refer to it from here.
--
-- The difference between this and "Derive.Scale.Scales" is that this is
-- intended for using scales, while Scales is intended for implementing them.
module Derive.Scale (module Derive.Deriver.Monad, module Derive.Scale) where
import qualified Data.Map as Map
import qualified Data.Vector as Vector

import qualified Derive.Deriver.Monad as Derive
-- TODO remove re-exports
import           Derive.Deriver.Monad
    (LookupScale(..), Scale(..), Transposition(..))
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Env as Env
import qualified Derive.Eval as Eval
import qualified Derive.PSignal as PSignal
import qualified Derive.Sig as Sig

import qualified Midi.Midi as Midi
import qualified Perform.Midi.Patch as Patch
import qualified Perform.Pitch as Pitch

import           Global


-- | Lookup a scale or throw.
get :: Derive.CallName -> [DeriveT.Val] -> Derive.Deriver Derive.Scale
get name args = Derive.require ("Scale.get: unknown scale: " <> pretty name)
    =<< lookupScale name args

lookupScale :: Derive.CallName -> [DeriveT.Val]
    -> Derive.Deriver (Maybe Derive.Scale)
lookupScale name args = do
    scaleCalls <- Derive.gets $
        Derive.stateScaleCalls . Derive.stateConstant
    case Map.lookup name scaleCalls of
        Nothing -> pure Nothing
        Just scall -> Just <$> Derive.scallCall scall args

-- | Scale calls always use Sig.Unprefixed.  This makes them inconsistent with
-- other kinds of calls, but I think is the better default, since I try to make
-- them respond to some standard env vals, such as key or scale-inst.
-- Especially scale-inst relies on it.
call :: Sig.Parser a -> (a -> Derive.Deriver Derive.Scale)
    -> Derive.WithArgDoc Derive.ScaleF
call parser f = (go, Sig.parserDocs parser)
    where
    go args =
        f =<< Sig.requireRight =<< Sig.parseVals parser ctx callName args
    -- This will make Sig.Prefixed act like Sig.Unprefixed.
    callName = ""
    -- TODO Sig.parse insists on having a ctx, can I remove it?
    -- used for Typecheck.Derive -> Sig.quoted_to_deriver -> Eval.eval_quoted
    ctx = Derive.dummyContext 0 1 "scale-call"

-- * old Definition

data Definition
    -- | Fancy scales can configure themselves.  Since you can't just look at
    -- the Scale directly, it has the ScaleId (pattern, doc) extracted.
    = Make !Pitch.ScaleId !(Text, Derive.DocumentedCall)
        !(Env.Environ -> LookupScale -> Either DeriveT.PitchError Scale)
    | Simple !Scale

scaleIdOf :: Definition -> Pitch.ScaleId
scaleIdOf (Make scaleId _ _) = scaleId
scaleIdOf (Simple scale) = scaleId scale

-- * util

-- | I would much rather pass a more specific value than Environ.
-- Unfortunately, ChromaticScales.SemisToNoteNumber needs a per-scale value
-- (e.g. Environ.key or Environ.tuning).  So pitch_nn needs to be parameterized
-- with a "get_key" function, but it also needs Environ.key.  I think it's
-- doable by parameterizing pitch_nn and hence note_to_call and moving
-- smap_semis_to_nn into note_to_call, but it seems complicated.
type PitchNn = PSignal.PitchConfig -> Either PSignal.PitchError Pitch.NoteNumber
type PitchNote = PSignal.PitchConfig -> Either PSignal.PitchError Pitch.Note

layout :: [Pitch.Semi] -> Derive.Layout
layout = Vector.fromList

noOctaves :: Derive.Layout
noOctaves = Vector.empty

diatonicLayout :: Pitch.PitchClass -> Derive.Layout
diatonicLayout perOct = layout $ replicate perOct 1

-- | Number of chromatic steps in an octave.  Nothing if this scale doesn't
-- have octaves.
semisPerOctave :: Derive.Layout -> Pitch.Semi
semisPerOctave = Vector.sum

semisAtPc :: Derive.Layout -> Pitch.PitchClass -> Pitch.Semi
semisAtPc layout pc = case pcPerOctave layout of
    Nothing -> pc
    Just perOct -> oct * Vector.sum layout + Vector.sum (Vector.take i layout)
        where (oct, i) = pc `divMod` perOct

-- | Number of diatonic steps in an octave.  Nothing if this scale doesn't have
-- octaves.  This is the same as 'semis_per_octave' for scales without
-- a diatonic\/chromatic distinction.
pcPerOctave :: Derive.Layout -> Maybe Pitch.PitchClass
pcPerOctave layout
    | Vector.null layout = Nothing
    | otherwise = Just $ Vector.length layout

diatonicDifference :: Derive.Layout -> Pitch.Pitch -> Pitch.Pitch
    -> Pitch.PitchClass
diatonicDifference layout (Pitch.Pitch oct1 (Pitch.Degree pc1 _))
        (Pitch.Pitch oct2 (Pitch.Degree pc2 _)) =
    octDiff + (pc1 - pc2)
    where octDiff = maybe 0 (* (oct1-oct2)) (pcPerOctave layout)

chromaticDifference :: Derive.Layout -> Pitch.Pitch -> Pitch.Pitch
    -> Pitch.Semi
chromaticDifference layout (Pitch.Pitch oct1 (Pitch.Degree pc1 acc1))
        (Pitch.Pitch oct2 (Pitch.Degree pc2 acc2)) =
    octDiff + (semisAtPc layout pc1 - semisAtPc layout pc2) + (acc1 - acc2)
    where octDiff = semisPerOctave layout * (oct1 - oct2)

transpose :: Transposition -> Scale -> Env.Environ -> Pitch.Octave
    -> Pitch.Step -> Pitch.Note -> Either DeriveT.PitchError Pitch.Note
transpose transposition scale environ octaves steps =
    scaleShow scale environ
    <=< scaleTranspose scale transposition environ steps
    . Pitch.addOctave octaves <=< scaleRead scale environ

transposePitch :: Transposition -> Scale -> Env.Environ -> Pitch.Octave
    -> Pitch.Step -> Pitch.Pitch -> Either DeriveT.PitchError Pitch.Pitch
transposePitch transposition scale environ octaves steps =
    scaleTranspose scale transposition environ steps
    . Pitch.addOctave octaves

-- * Range

-- | This is an inclusive pitch range, intended for instrument ranges.
data Range = Range
    { rangeBottom :: !Pitch.Pitch
    , rangeTop :: !Pitch.Pitch
    } deriving (Show, Eq)

inRange :: Range -> Pitch.Pitch -> Bool
inRange (Range bottom top) pitch = bottom <= pitch && pitch <= top

instance Pretty Range where
    pretty (Range bottom top) = pretty bottom <> "--" <> pretty top

-- * pitches

-- | Return the pitches in the scale.  If the scale has an unbounded range,
-- this may go on forever, so zip with 'note_numbers' if you want the usable
-- range.  Also, not all scales actually have defined degrees.
pitches :: Scale -> Env.Environ -> [Pitch.Pitch]
pitches scale environ = go (scaleBottom scale)
    where
    go pitch = pitch : either (const []) go (step pitch)
    step = scaleTranspose scale Chromatic environ 1

-- | Return the notes in the scale.  As with 'pitches', it may be unbounded.
notes :: Scale -> Env.Environ -> [Pitch.Note]
notes scale environ = go (pitches scale environ)
    where
    go (p:ps) = case scaleShow scale environ p of
        Right n -> n : go ps
        Left _ -> []
    go [] = []

-- | Return pitches of the scale's degrees.
noteNumbers :: Scale -> Env.Environ -> Derive.Deriver [Pitch.NoteNumber]
noteNumbers scale environ = go (notes scale environ)
    where
    go [] = pure []
    go (note : notes) = do
        pitch <- Eval.evalNote scale note
        case PSignal.pitchNn pitch of
            Right nn -> (nn:) <$> go notes
            Left (DeriveT.OutOfRangeError {}) -> pure []
            Left err -> Derive.throw $ "note_numbers: " <> pretty err

-- | Make a patch scale from the NoteNumbers.
patchScale :: Pitch.ScaleId -> [Pitch.NoteNumber] -> Patch.Scale
patchScale scaleId nns = Patch.makeScale (pretty scaleId) $
    map (first Midi.toKey) $ assignKeys 128 nns

-- | Try to assign MIDI keys that correspond to the NoteNumbers, but
-- they won't line up if there are too many NoteNumbers.
assignKeys :: Int -> [Pitch.NoteNumber] -> [(Int, Pitch.NoteNumber)]
assignKeys topKey nns = go 0 (topKey - length nns) nns
    where
    go _ _ [] = []
    go key extra (nn:nns)
        | key >= topKey = []
        | otherwise =
            (assigned, nn) : go (assigned+1) (extra - (assigned-key)) nns
        where assigned = key + max 0 (min (floor nn - key) extra)
