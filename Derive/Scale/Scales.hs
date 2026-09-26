-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for simple scales, which simply map pitch names to frequencies.
-- Ok, so they also have octave structure, used by the input mechanism and to
-- parse to 'Pitch.Pitch'es, but it can be set to the number of degrees in the
-- scale if you don't have octaves.
module Derive.Scale.Scales where
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import           Data.Vector ((!?))

import qualified Util.Doc as Doc
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Texts as Texts

import qualified Derive.Call.ScaleDegree as ScaleDegree
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Eval as Eval
import qualified Derive.PSignal as PSignal
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Theory as Theory
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch

import           Global
import           Types


-- | Make a simple scale where there is a direct mapping from input to note to
-- nn.
makeScale :: Pitch.ScaleId -> DegreeMap -> Text -> Doc.Doc -> Scale.Scale
makeScale scaleId dmap pattern doc = Scale.Scale
    { scaleId = scaleId
    , scalePattern = pattern
    , scaleSymbols = []
    , scaleTransposers = standardTransposers
    , scaleRead = const $ readNote dmap
    , scaleShow = const $ showPitch dmap
    , scaleBottom = Pitch.pitch (dmStartOctave dmap) (dmStartPc dmap)
    , scaleLayout = Scale.diatonicLayout (dmPerOctave dmap)
    , scaleTranspose = transpose dmap
    , scaleEnharmonics = noEnharmonics
    , scaleNoteToCall = mappedNoteToCall dmap scale
    , scaleInputToNote = inputToNote dmap
    , scaleInputToNn = mappedInputToNn dmap
    , scaleCallDoc = callDoc standardTransposers dmap doc
    }
    where scale = PSignal.Scale scaleId standardTransposers

-- | An empty scale that doesn't do anything.
emptyScale :: Pitch.ScaleId -> Text -> Derive.DocumentedCall -> Scale.Scale
emptyScale scaleId pattern doc = Scale.Scale
    { scaleId = scaleId
    , scalePattern = pattern
    , scaleSymbols = []
    , scaleTransposers = standardTransposers
    , scaleRead = \_ _ -> Left DeriveT.NotImplemented
    , scaleShow = \_ _ -> Left DeriveT.NotImplemented
    , scaleBottom = Pitch.pitch 1 0
    , scaleLayout = Scale.layout []
    , scaleTranspose = \_ _ _ _ -> Left DeriveT.NotImplemented
    , scaleEnharmonics = noEnharmonics
    , scaleNoteToCall = const Nothing
    , scaleInputToNote = \_ _ -> Left DeriveT.NotImplemented
    , scaleInputToNn = \ _ _ -> pure $ Left DeriveT.NotImplemented
    , scaleCallDoc = doc
    }

-- * types

-- TODO this is no longer used.  But still an example of a simple scale.
-- Move it some place like SimpleScales?
data DegreeMap = DegreeMap
    { dmToSemis :: Map Pitch.Note Pitch.Semi
    , dmToNote :: Vector.Vector Pitch.Note
    , dmToNn :: Vector.Vector Pitch.NoteNumber
    -- | Number of scale steps per octave.  Actually, simple scales are just
    -- a collection of frequencies and don't need to have a notion of an
    -- octave.  But since the input mechanism wants to orient around octaves,
    -- it needs to know how many keys to assign to each octave.  So if your
    -- scale has no octaves, then just set this to 7, that way it lines up with
    -- the piano keyboard.
    , dmPerOctave :: Pitch.Semi
    , dmStartOctave :: Pitch.Octave
    , dmStartPc :: Pitch.PitchClass
    } deriving (Show)

instance Pretty DegreeMap where
    format dmap = Pretty.format $ Map.fromList $ do
        (note, semis) <- Map.toList (dmToSemis dmap)
        pure (semis, (note, dmToNn dmap !? semis))

degreeMap :: Pitch.PitchClass
    -> Pitch.Octave -- ^ The first Note is this Octave and PitchClass.
    -> Pitch.PitchClass
    -> [Pitch.Note] -> [Pitch.NoteNumber] -> DegreeMap
degreeMap perOctave startOctave startPc notes_ nns_ = DegreeMap
    { dmToSemis = Map.fromList (zip notes [0..])
    , dmToNote = Vector.fromList notes
    , dmToNn = Vector.fromList nns
    , dmPerOctave = perOctave
    , dmStartOctave = startOctave
    , dmStartPc = startPc
    }
    where
    -- Guard against infinite notes or nns.
    (notes, nns) = unzip $ zip notes_ nns_

type SemisToNoteNumber = PSignal.PitchConfig -> Pitch.Semi
    -> Either DeriveT.PitchError Pitch.NoteNumber

-- * scale functions

readNote :: DegreeMap -> Pitch.Note -> Either DeriveT.PitchError Pitch.Pitch
readNote dmap note = semisToPitch dmap <$>
    justErr (DeriveT.UnparseableNote note) (Map.lookup note (dmToSemis dmap))

showPitch :: DegreeMap -> Pitch.Pitch -> Either DeriveT.PitchError Pitch.Note
showPitch dmap pitch = justErr
    (DeriveT.PitchError ("invalid pitch: " <> pretty pitch)) $
    dmToNote dmap !? pitchToSemis dmap pitch

-- ** transpose

transpose :: DegreeMap -> Derive.Transpose
transpose dmap _transposition _environ steps pitch
    | Maybe.isJust $ dmToNote dmap !? transposed =
        Right $ semisToPitch dmap transposed
    | otherwise = Left $ DeriveT.OutOfRangeError DeriveT.outOfRange
    where transposed = pitchToSemis dmap pitch + steps

-- | Transpose function for a non-transposing scale.
nonTransposing :: Derive.Transpose
nonTransposing _ _ _ _ = Left DeriveT.NotImplemented

-- | Indicate that this scale responds to the standard set of transpose
-- signals.  It still has to implement the support in its
-- 'Scale.scale_note_to_call'.
standardTransposers :: Set ScoreT.Control
standardTransposers = Set.fromList
    [ Controls.octave, Controls.chromatic, Controls.diatonic
    , Controls.nn, Controls.hz
    ]

-- ** note_to_call

-- | A specialization of 'note_to_call' that operates on scales with
-- a 'DegreeMap', i.e. a static map from notes to degrees, and from degrees to
-- NNs.
mappedNoteToCall :: DegreeMap -> PSignal.Scale
    -> Pitch.Note -> Maybe Derive.ValCall
mappedNoteToCall dmap scale note = do
    semis <- Map.lookup note (dmToSemis dmap)
    Just $ noteToCall (dmPerOctave dmap) (Just maxSemi)
        scale (semisToNn semis) (semisToNote semis)
    where
    maxSemi = Vector.length (dmToNn dmap)
    semisToNn semis _config transpose =
        justErr err $ dmToNn dmap !? (semis + transpose)
        where
        err = DeriveT.outOfRangeError (semis + transpose) (0, maxSemi)
    semisToNote semis transpose = dmToNote dmap !? (semis + transpose)

-- | Create a note call that respects chromatic and diatonic transposition.
-- However, diatonic transposition is mapped to chromatic transposition,
-- so this is for scales that don't distinguish.
noteToCall :: Pitch.Semi -> Maybe Pitch.Semi -> PSignal.Scale
    -> SemisToNoteNumber -> (Pitch.Semi -> Maybe Pitch.Note) -> Derive.ValCall
noteToCall perOctave maxSemi scale semisToNn semisToNote =
    ScaleDegree.scaleDegree scale pitchNn pitchNote
    where
    pitchNn :: Scale.PitchNn
    pitchNn config = toNn transposeSteps frac config
        where (transposeSteps, frac) = properFraction $ transposition config
    toNn semis frac config
        | frac == 0 = semisToNn config semis
        | otherwise = Num.scale
            <$> semisToNn config semis
            <*> semisToNn config (semis + 1)
            <*> pure (Pitch.NoteNumber frac)
    pitchNote :: Scale.PitchNote
    pitchNote config = justErr err $ semisToNote semis
        where
        semis = floor $ transposition config
        err = DeriveT.OutOfRangeError $ DeriveT.outOfRange
            { DeriveT.oorDegree = Just (fromIntegral semis)
            , DeriveT.oorValid = (0,) <$> maxSemi
            }
    transposition config =
        get Controls.octave * fromIntegral perOctave
            + get Controls.chromatic + get Controls.diatonic
        where get c = Map.findWithDefault 0 c (PSignal.pitchControls config)

addPc :: DegreeMap -> Pitch.PitchClass -> Pitch.Pitch -> Pitch.Pitch
addPc dmap = Pitch.addPc (dmPerOctave dmap)

-- ** input

type InputToNote = Env.Environ -> Pitch.Input
    -> Either DeriveT.PitchError Pitch.Note

-- | Input to note for simple scales without keys.
inputToNote :: DegreeMap -> InputToNote
inputToNote dmap _environ (Pitch.Input kbd pitch frac) = do
    steps <- simpleKbdToScale dmap kbd pitch
    note <- justErr DeriveT.InvalidInput $ dmToNote dmap !? steps
    pure $ ScaleDegree.pitchExpr frac note

type InputToNn = ScoreTime -> Pitch.Input
    -> Derive.Deriver (Either DeriveT.PitchError Pitch.NoteNumber)

-- | Input to NoteNumber for scales that have a direct relationship between
-- Degree and NoteNumber.
mappedInputToNn :: DegreeMap -> InputToNn
mappedInputToNn dmap = \_pos (Pitch.Input kbd pitch frac) -> pure $ do
    semis <- simpleKbdToScale dmap kbd pitch
    justErr (DeriveT.OutOfRangeError DeriveT.outOfRange) $
        toNn semis frac
    where
    toNn semis frac
        | frac == 0 = lookup semis
        | frac > 0 = do
            nn <- lookup semis
            next <- lookup (semis + 1)
            pure $ Num.scale nn next (Pitch.NoteNumber frac)
        | otherwise = do
            nn <- lookup semis
            prev <- lookup (semis - 1)
            pure $ Num.scale prev nn (Pitch.NoteNumber (frac + 1))
    lookup semis = dmToNn dmap !? semis

setDirectInputToNn :: Scale.Scale -> Scale.Scale
setDirectInputToNn scale = scale
    { Scale.scaleInputToNn = directInputToNn }

-- | An Input maps directly to a NoteNumber.  This is an efficient
-- implementation for scales tuned to 12TET.
--
-- This obeys 'Controls.octave' but none of the other transposer controls.
-- This is inconsistent with 'computed_input_to_nn', but trying to implement
-- diatonic transposition would make this not so direct any more.  And in
-- any case, "Cmd.MidiThru" shouldn't let through any transposers other than
-- octave.
directInputToNn :: InputToNn
directInputToNn pos (Pitch.Input _ pitch frac) = do
    controls <- Derive.controlsAt =<< Derive.real pos
    let octaves = Map.findWithDefault 0 Controls.octave controls
    pure $ Right $ nn + Pitch.nn (frac + octaves * 12)
    where
    nn = fromIntegral $ Theory.semisToNn $
        Theory.pitchToSemis Theory.pianoLayout pitch

-- | Convert input to nn by going through note_to_call.  This works for
-- complicated scales that retune based on the environment but is more work.
computedInputToNn :: InputToNote -> (Pitch.Note -> Maybe Derive.ValCall)
    -> InputToNn
computedInputToNn inputToNote noteToCall pos input = do
    env <- Derive.getEnviron
    case getCall env of
        Left err -> pure $ Left err
        Right (note, call) -> do
            pitch <- Eval.applyPitch pos call
            controls <- Derive.controlsAt =<< Derive.real pos
            nn <- Derive.requireRight
                    ((("evaluating note " <> pretty note <> ": ")<>) . pretty) $
                PSignal.pitchNn $
                PSignal.applyConfig (PSignal.PitchConfig env controls) pitch
            pure $ Right nn
    where
    getCall env = do
        note <- inputToNote env input
        (note,) <$> justErr (DeriveT.UnparseableNote note) (noteToCall note)

makeNn :: Maybe Pitch.NoteNumber -> Pitch.NoteNumber -> Maybe Pitch.NoteNumber
    -> Pitch.Frac -> Maybe Pitch.NoteNumber
makeNn mprev nn mnext frac
    | frac == 0 = Just nn
    | frac > 0 = fmap (\next -> interpolate nn next) mnext
    | otherwise = fmap (\prev -> interpolate prev nn) mprev
    where
    interpolate low high = Num.scale low high (Pitch.NoteNumber frac)

-- *** diatonic

simpleKbdToScale :: DegreeMap -> Pitch.KbdType -> Pitch.Pitch
    -> Either DeriveT.PitchError Pitch.Semi
simpleKbdToScale dmap kbd pitch =
    pitchToSemis dmap <$> kbdToScale kbd (dmPerOctave dmap) 0 pitch

pitchToSemis :: DegreeMap -> Pitch.Pitch -> Pitch.Semi
pitchToSemis dmap (Pitch.Pitch oct (Pitch.Degree pc accs)) =
    (oct - dmStartOctave dmap) * dmPerOctave dmap
        + pc - dmStartPc dmap + accs

semisToPitch :: DegreeMap -> Pitch.Semi -> Pitch.Pitch
semisToPitch dmap semis =
    Pitch.addPc perOct (dmStartPc dmap) $
        Pitch.Pitch (oct + dmStartOctave dmap) (Pitch.Degree pc 0)
    where
    (oct, pc) = semis `divMod` perOct
    perOct = dmPerOctave dmap

kbdToScale :: Pitch.KbdType -> Pitch.PitchClass -> Pitch.PitchClass
    -> Pitch.Pitch -> Either DeriveT.PitchError Pitch.Pitch
kbdToScale kbd pcPerOctave tonic =
    justErr DeriveT.InvalidInput . lookupKbdToScale kbd pcPerOctave tonic

-- | Convert an absolute Pitch in the input keyboard's layout to a relative
-- Pitch within a scale with the given number of diatonic steps per octave, or
-- Nothing if that key should have no pitch.
lookupKbdToScale :: Pitch.KbdType -> Pitch.PitchClass -> Pitch.PitchClass
    -> Pitch.Pitch -> Maybe Pitch.Pitch
lookupKbdToScale kbd pcPerOctave tonic pitch = case kbd of
    Pitch.PianoKbd -> pianoKbdPitch tonic pcPerOctave pitch
    Pitch.AsciiKbd -> Just $ asciiKbdPitch pcPerOctave pitch

{- Scale octave doesn't match the kbd octave, but is absolute:

       C D E F G A B|C D E F G A B
    C  1 2 3 4 5 - - 1 2 3 4 5 - -
    D  - 1 2 3 4 5 - - 1 2 3 4 5 -
    E  - - 1 2 3 4 5 - - 1 2 3 4 5

    Piano:
       0 1 2 3 4 5 6 0 1 2 3 4 5 6 0
       C D E F G A B|C D E F G A B|C
       0 1 2 3 4 - - 0
       0 1 2 3 4 5 6 7 8 - - - - - 0
-}

-- | The MIDI kbd is absolute.  This means that relative scales start on
-- different keys rather than all starting on C.  For example, in C major
-- C produces the first scale degree, while in D major D produces the first
-- scale degree.
--
-- In addition, if the scale octave is not an even multiple of the kbd octave
-- (7), the extra notes produce Nothing.  This check has to be done to
-- the relative PitchClass.  That way, a D on a 6 note scale starting on D is
-- 1, and a C is Nothing.  Thus, the returned Pitch is relative to the given
-- tonic, so it should be formatted as-is, without the key.
pianoKbdPitch :: Pitch.PitchClass -> Pitch.PitchClass -> Pitch.Pitch
    -> Maybe Pitch.Pitch
pianoKbdPitch tonic pcPerOctave (Pitch.Pitch oct (Pitch.Degree pc accs))
    | relativePc >= pcPerOctave = Nothing
    | otherwise =
        Just $ Pitch.Pitch (oct1 + octDiff) (Pitch.Degree relativePc accs)
    where
    (oct1, pc1) = adjustOctave pcPerOctave 7 oct pc
    (octDiff, relativePc) = (pc1 - tonic) `divMod` maxPc
    maxPc = ceiling (fromIntegral pcPerOctave / 7) * 7

-- | The ASCII kbd is relative.  This means that relative scales always start
-- on \"C\".  So the tonic note of a key in a relative scale is irrelevant,
-- C major and D major both start in the same place.  Of course, they produce
-- different frequencies, but that's the responsibility of
-- 'Scale.scale_note_to_call'.
--
-- Unlike 'absolute_to_pitch', if the scale octave is not an even multiple of
-- the kbd octave (10), the extra notes wrap to the next highest octave.
asciiKbdPitch :: Pitch.PitchClass -> Pitch.Pitch -> Pitch.Pitch
asciiKbdPitch pcPerOctave (Pitch.Pitch oct (Pitch.Degree pc accs)) =
    Pitch.Pitch (addOct + oct1) (Pitch.Degree pc2 accs)
    where
    (oct1, pc1) = adjustOctave pcPerOctave 10 oct pc
    -- If the scale is shorter than the kbd, go up to the next octave on
    -- the same row.
    (addOct, pc2) = pc1 `divMod` pcPerOctave

-- | Try to fit a note from a keyboard into a scale.  Round the note up to the
-- nearest multiple of the keyboard octave and adjust the octave accordingly.
adjustOctave :: Pitch.PitchClass -> Pitch.PitchClass -> Pitch.Octave
    -> Pitch.PitchClass -> (Pitch.Octave, Pitch.PitchClass)
adjustOctave pcPerOctave kbdPerOctave oct pc =
    (oct2, offset * kbdPerOctave + pc)
    where
    rows = ceiling $ fromIntegral pcPerOctave / fromIntegral kbdPerOctave
    (oct2, offset) = oct `divMod` rows


-- ** call_doc

callDoc :: Set ScoreT.Control -> DegreeMap -> Doc.Doc -> Derive.DocumentedCall
callDoc transposers dmap doc =
    annotateCallDoc transposers doc fields defaultScaleDegreeDoc
    where
    fields
        | Vector.null notes = []
        | otherwise = [("range", Doc.pretty bottom <> " to " <> Doc.pretty top)]
        where
        bottom = notes Vector.! 0
        top = notes Vector.! (Vector.length notes - 1)
        notes = dmToNote dmap

-- | Documentation of the standard 'Call.Pitch.scale_degree'.
defaultScaleDegreeDoc :: Derive.DocumentedCall
defaultScaleDegreeDoc = scaleDegreeDoc ScaleDegree.scaleDegree

scaleDegreeDoc ::
    (PSignal.Scale -> Scale.PitchNn -> Scale.PitchNote -> Derive.ValCall)
    -> Derive.DocumentedCall
scaleDegreeDoc scaleDegree =
    Derive.extractDoc $ scaleDegree PSignal.noScale err err
    where err _ = Left $ PSignal.PitchError "it was just an example!"

annotateCallDoc :: Set ScoreT.Control -> Doc.Doc -> [(Doc.Doc, Doc.Doc)]
    -> Derive.DocumentedCall -> Derive.DocumentedCall
annotateCallDoc transposers doc fields = prependDoc extraDoc
    where
    extraDoc = doc <> "\n\n" <> join (transposersField ++ fields)
    transposersField =
        [("transposers", Doc.Doc $ pretty transposers) |
            not (Set.null transposers)]
    join = Texts.enumeration
        . map (\(k, v) -> k <> ": " <> v) . filter ((/="") . snd)

addDoc :: Doc.Doc -> Scale.Scale -> Scale.Scale
addDoc doc scale = scale
    { Scale.scaleCallDoc = prependDoc doc (Scale.scaleCallDoc scale) }

-- *** DocumentedCall

-- | Prepend a bit of text to the documentation.
prependDoc :: Doc.Doc -> Derive.DocumentedCall -> Derive.DocumentedCall
prependDoc text = modifyDoc ((text <> "\n") <>)

modifyDoc :: (Doc.Doc -> Doc.Doc) -> Derive.DocumentedCall
    -> Derive.DocumentedCall
modifyDoc modify (Derive.DocumentedCall name doc) =
    Derive.DocumentedCall name (annotate doc)
    where
    annotate (Derive.CallDoc module_ tags cdoc args) =
        Derive.CallDoc module_ tags (modify cdoc) args

-- * util

noEnharmonics :: Derive.Enharmonics
noEnharmonics _ _ = Right []

-- | Like 'read_environ_', but with a simpler parser.
readEnviron :: (Typecheck.Typecheck a, ShowVal.ShowVal a)
    => (a -> Maybe val)
    -> Maybe val -- ^ if Just, a missing value gets this, otherwise error
    -> Env.Key -> Env.Environ -> Either DeriveT.PitchError val
readEnviron parse maybeDeflt =
    readEnviron_ (maybe (Left Nothing) Right . parse) (Right <$> maybeDeflt)

-- | Like 'read_environ', except the default is given to the parse function.
readEnvironDefault :: (Typecheck.Typecheck a, ShowVal.ShowVal a) =>
    (a -> Maybe val) -> Maybe a
    -> Env.Key -> Env.Environ -> Either DeriveT.PitchError val
readEnvironDefault parse maybeDeflt name =
    readEnviron_ (maybe (Left Nothing) Right . parse)
        (parseDefault <$> maybeDeflt) name
    where
    parseDefault val = case parse val of
        Just a -> Right a
        Nothing -> environError $
            "can't parse default: " <> ShowVal.showVal val
    environError = Left . DeriveT.EnvironError name . Just

-- | Read and parse an environ value, or throw a ScaleError.
-- This takes a parse function in addition to the usual Typecheck, because
-- scales and keys don't use use Typecheck, beyond ensuring it's a string.
readEnviron_ :: (Typecheck.Typecheck a, ShowVal.ShowVal a)
    => (a -> Either (Maybe Text) val) -> Maybe (Either PSignal.PitchError val)
    -> Env.Key -> Env.Environ -> Either DeriveT.PitchError val
readEnviron_ parse maybeDeflt name env = case Env.getVal name env of
    Left (Env.WrongType expected) ->
        environError ("expected type " <> pretty expected)
    Left Env.NotFound -> case maybeDeflt of
        Nothing -> Left $ DeriveT.EnvironError name Nothing
        Just deflt -> deflt
    Right val -> parseVal val
    where
    parseVal val = case parse val of
        Right a -> Right a
        Left msg -> environError $ ShowVal.showVal val <> ": "
            <> fromMaybe "can't parse" msg
    environError = Left . DeriveT.EnvironError name . Just


-- ** keys

environKey :: Env.Environ -> Maybe Pitch.Key
environKey = fmap Pitch.Key . Env.maybeVal EnvKey.key

-- | Find a key in a map, or throw a ScaleError.
getKey :: key -> Map Pitch.Key key -> Maybe Pitch.Key
    -> Either DeriveT.PitchError key
getKey deflt _ Nothing = Right deflt
getKey _ keys (Just key) = justErr (keyError key) (Map.lookup key keys)

lookupKey :: key -> Map Pitch.Key key -> Maybe Pitch.Key -> Maybe key
lookupKey deflt _ Nothing = Just deflt
lookupKey _ keys (Just key) = Map.lookup key keys

keyError :: Pitch.Key -> DeriveT.PitchError
keyError (Pitch.Key key) =
    DeriveT.EnvironError EnvKey.key (Just $ "unknown key: " <> key)
