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
    =<< lookup_scale name args

lookup_scale :: Derive.CallName -> [DeriveT.Val]
    -> Derive.Deriver (Maybe Derive.Scale)
lookup_scale name args = do
    scale_calls <- Derive.gets $
        Derive.state_scale_calls . Derive.state_constant
    case Map.lookup name scale_calls of
        Nothing -> return Nothing
        Just scall -> Just <$> Derive.scall_call scall args

-- | Scale calls always use Sig.Unprefixed.  This makes them inconsistent with
-- other kinds of calls, but I think is the better default, since I try to make
-- them respond to some standard env vals, such as key or scale-inst.
-- Especially scale-inst relies on it.
call :: Sig.Parser a -> (a -> Derive.Deriver Derive.Scale)
    -> Derive.WithArgDoc Derive.ScaleF
call parser f = (go, Sig.parser_docs parser)
    where
    go args =
        f =<< Sig.require_right =<< Sig.parse_vals parser ctx call_name args
    -- This will make Sig.Prefixed act like Sig.Unprefixed.
    call_name = ""
    -- TODO Sig.parse insists on having a ctx, can I remove it?
    -- used for Typecheck.Derive -> Sig.quoted_to_deriver -> Eval.eval_quoted
    ctx = Derive.dummy_context 0 1 "scale-call"

-- * old Definition

data Definition =
    -- | Fancy scales can configure themselves.  Since you can't just look at
    -- the Scale directly, it has the ScaleId (pattern, doc) extracted.
    Make !Pitch.ScaleId !(Text, Derive.DocumentedCall)
        !(Env.Environ -> LookupScale -> Either DeriveT.PitchError Scale)
    | Simple !Scale

scale_id_of :: Definition -> Pitch.ScaleId
scale_id_of (Make scale_id _ _) = scale_id
scale_id_of (Simple scale) = scale_id scale

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

no_octaves :: Derive.Layout
no_octaves = Vector.empty

diatonic_layout :: Pitch.PitchClass -> Derive.Layout
diatonic_layout per_oct = layout $ replicate per_oct 1

-- | Number of chromatic steps in an octave.  Nothing if this scale doesn't
-- have octaves.
semis_per_octave :: Derive.Layout -> Pitch.Semi
semis_per_octave = Vector.sum

semis_at_pc :: Derive.Layout -> Pitch.PitchClass -> Pitch.Semi
semis_at_pc layout pc = case pc_per_octave layout of
    Nothing -> pc
    Just per_oct -> oct * Vector.sum layout + Vector.sum (Vector.take i layout)
        where (oct, i) = pc `divMod` per_oct

-- | Number of diatonic steps in an octave.  Nothing if this scale doesn't have
-- octaves.  This is the same as 'semis_per_octave' for scales without
-- a diatonic\/chromatic distinction.
pc_per_octave :: Derive.Layout -> Maybe Pitch.PitchClass
pc_per_octave layout
    | Vector.null layout = Nothing
    | otherwise = Just $ Vector.length layout

diatonic_difference :: Derive.Layout -> Pitch.Pitch -> Pitch.Pitch
    -> Pitch.PitchClass
diatonic_difference layout (Pitch.Pitch oct1 (Pitch.Degree pc1 _))
        (Pitch.Pitch oct2 (Pitch.Degree pc2 _)) =
    oct_diff + (pc1 - pc2)
    where oct_diff = maybe 0 (* (oct1-oct2)) (pc_per_octave layout)

chromatic_difference :: Derive.Layout -> Pitch.Pitch -> Pitch.Pitch
    -> Pitch.Semi
chromatic_difference layout (Pitch.Pitch oct1 (Pitch.Degree pc1 acc1))
        (Pitch.Pitch oct2 (Pitch.Degree pc2 acc2)) =
    oct_diff + (semis_at_pc layout pc1 - semis_at_pc layout pc2) + (acc1 - acc2)
    where oct_diff = semis_per_octave layout * (oct1 - oct2)

transpose :: Transposition -> Scale -> Env.Environ -> Pitch.Octave
    -> Pitch.Step -> Pitch.Note -> Either DeriveT.PitchError Pitch.Note
transpose transposition scale environ octaves steps =
    scale_show scale environ
    <=< scale_transpose scale transposition environ steps
    . Pitch.add_octave octaves <=< scale_read scale environ

transpose_pitch :: Transposition -> Scale -> Env.Environ -> Pitch.Octave
    -> Pitch.Step -> Pitch.Pitch -> Either DeriveT.PitchError Pitch.Pitch
transpose_pitch transposition scale environ octaves steps =
    scale_transpose scale transposition environ steps
    . Pitch.add_octave octaves

-- * Range

-- | This is an inclusive pitch range, intended for instrument ranges.
data Range = Range {
    range_bottom :: !Pitch.Pitch
    , range_top :: !Pitch.Pitch
    } deriving (Show, Eq)

in_range :: Range -> Pitch.Pitch -> Bool
in_range (Range bottom top) pitch = bottom <= pitch && pitch <= top

instance Pretty Range where
    pretty (Range bottom top) = pretty bottom <> "--" <> pretty top

-- * pitches

-- | Return the pitches in the scale.  If the scale has an unbounded range,
-- this may go on forever, so zip with 'note_numbers' if you want the usable
-- range.  Also, not all scales actually have defined degrees.
pitches :: Scale -> Env.Environ -> [Pitch.Pitch]
pitches scale environ = go (scale_bottom scale)
    where
    go pitch = pitch : either (const []) go (step pitch)
    step = scale_transpose scale Chromatic environ 1

-- | Return the notes in the scale.  As with 'pitches', it may be unbounded.
notes :: Scale -> Env.Environ -> [Pitch.Note]
notes scale environ = go (pitches scale environ)
    where
    go (p:ps) = case scale_show scale environ p of
        Right n -> n : go ps
        Left _ -> []
    go [] = []

-- | Return pitches of the scale's degrees.
note_numbers :: Scale -> Env.Environ -> Derive.Deriver [Pitch.NoteNumber]
note_numbers scale environ = go (notes scale environ)
    where
    go [] = return []
    go (note : notes) = do
        pitch <- Eval.eval_note scale note
        case PSignal.pitch_nn pitch of
            Right nn -> (nn:) <$> go notes
            Left (DeriveT.OutOfRangeError {}) -> return []
            Left err -> Derive.throw $ "note_numbers: " <> pretty err

-- | Make a patch scale from the NoteNumbers.
patch_scale :: Pitch.ScaleId -> [Pitch.NoteNumber] -> Patch.Scale
patch_scale scale_id nns = Patch.make_scale (pretty scale_id) $
    map (first Midi.to_key) $ assign_keys 128 nns

-- | Try to assign MIDI keys that correspond to the NoteNumbers, but
-- they won't line up if there are too many NoteNumbers.
assign_keys :: Int -> [Pitch.NoteNumber] -> [(Int, Pitch.NoteNumber)]
assign_keys top_key nns = go 0 (top_key - length nns) nns
    where
    go _ _ [] = []
    go key extra (nn:nns)
        | key >= top_key = []
        | otherwise =
            (assigned, nn) : go (assigned+1) (extra - (assigned-key)) nns
        where assigned = key + max 0 (min (floor nn - key) extra)
