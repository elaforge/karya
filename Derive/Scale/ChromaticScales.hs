-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for equal-tempered chromatic scales with keys and modes.
module Derive.Scale.ChromaticScales (
    ScaleMap(..)
    , SemisToNoteNumber
    , twelveDoc
    , scaleMap
    , Keys
    , makeKeys
    , makeScale
    , transpose
    , pitchNote
    , pitchNn
    , inputToNote
    , groupTonicMode
    , relativeFmt

    -- * util
    , showPitch, readPitch
    , readKey
) where
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Doc as Doc
import qualified Util.Lists as Lists
import qualified Derive.Call.ScaleDegree as ScaleDegree
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Env as Env
import qualified Derive.PSignal as PSignal
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Scales as Scales
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.TheoryFormat as TheoryFormat
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal

import qualified Perform.Pitch as Pitch

import           Global


-- | This contains all that is needed to define a modal key system.  It fills
-- a similar role to 'Scales.DegreeMap' for non-keyed scales.
data ScaleMap = ScaleMap
    { smapFmt :: !TheoryFormat.Format
    , smapKeys :: !Keys
    , smapDefaultKey :: !Theory.Key
    , smapLayout :: !Theory.Layout
    -- | Configure how the scale converts 'Pitch.Semi's to frequency.
    , smapSemisToNn :: SemisToNoteNumber
    -- | Inclusive (bottom, top) of scale.
    , smapRange :: !(Pitch.Semi, Pitch.Semi)
    }

type SemisToNoteNumber = PSignal.PitchConfig -> Pitch.FSemi
    -> Either DeriveT.PitchError Pitch.NoteNumber

twelveDoc :: Doc.Doc
twelveDoc = "Scales in the \"twelve\" family use European style note naming.\
    \ That is, note names look like octave-letter-accidentals like \"4c#\".\
    \ They have a notion of a \"layout\", which is a pattern of half and\
    \ whole steps, e.g. the piano layout, and a key, which is a subset of\
    \ notes from the scale along with a preferred spelling for them. The\
    \ rules of how enharmonic spelling works are complicated, and documented\
    \ in 'Derive.Scale.Theory'. The key is read from the `key` env var, and\
    \ each scale has a list of keys it will accept."

scaleMap :: Theory.Layout -> TheoryFormat.Format -> Keys -> Theory.Key
    -> ScaleMap
scaleMap layout fmt keys defaultKey = ScaleMap
    { smapFmt = fmt
    , smapKeys = keys
    , smapDefaultKey = defaultKey
    , smapLayout = layout
    , smapSemisToNn = \_config -> pure . Theory.fsemisToNn
    , smapRange = range
    }
    where range = (Theory.nnToSemis 0, Theory.nnToSemis 127)

type Keys = Map Pitch.Key Theory.Key

makeKeys :: TheoryFormat.Format -> [Theory.Key] -> Keys
makeKeys fmt keys =
    Map.fromList $ zip (map (TheoryFormat.showKey fmt) keys) keys

makeScale :: Pitch.ScaleId -> ScaleMap -> Doc.Doc -> Scale.Scale
makeScale scaleId smap doc = Scale.Scale
    { scaleId = scaleId
    , scalePattern = TheoryFormat.fmtPattern (smapFmt smap)
    , scaleSymbols = []
    , scaleTransposers = Scales.standardTransposers
    , scaleRead = readPitch smap . Scales.environKey
    , scaleShow = showPitch smap . Scales.environKey
    , scaleBottom = Theory.semisToPitchSharps (smapLayout smap)
        (fst (smapRange smap))
    , scaleLayout = Theory.layoutIntervals (smapLayout smap)
    , scaleTranspose = transpose smap
    , scaleEnharmonics = enharmonics smap
    , scaleNoteToCall = noteToCall scale smap
    , scaleInputToNote = inputToNote smap
    , scaleInputToNn = Scales.computedInputToNn
        (inputToNote smap) (noteToCall scale smap)
    , scaleCallDoc = callDoc Scales.standardTransposers smap doc
    }
    where scale = PSignal.Scale scaleId Scales.standardTransposers

-- * functions

transpose :: ScaleMap -> Derive.Transpose
transpose smap transposition env steps pitch = do
    key <- readEnvironKey smap env
    pure $ trans key steps pitch
    where
    trans = case transposition of
        Scale.Chromatic -> Theory.transposeChromatic
        Scale.Diatonic -> Theory.transposeDiatonic

enharmonics :: ScaleMap -> Derive.Enharmonics
enharmonics smap env note = do
    pitch <- readPitch smap (Scales.environKey env) note
    key <- readEnvironKey smap env
    pure $ Either.rights $ map (showPitch smap (Scales.environKey env)) $
        Theory.enharmonicsOf (Theory.keyLayout key) pitch

noteToCall :: PSignal.Scale -> ScaleMap -> Pitch.Note -> Maybe Derive.ValCall
noteToCall scale smap note =
    case TheoryFormat.readRelativePitch (smapFmt smap) note of
        Left _ -> Nothing
        Right relative -> Just $ ScaleDegree.scaleDegree scale
            (pitchNn smap relative) (pitchNote smap relative)

-- | Create a PitchNote for 'ScaleDegree.scale_degree'.
pitchNote :: ScaleMap -> TheoryFormat.RelativePitch -> Scale.PitchNote
pitchNote smap relative (PSignal.PitchConfig env controls) = do
    -- Adjustment to absolute is only necessary for 'pitch_nn', since
    -- NoteNumbers are absolute.
    -- TODO I should leave as relative to preserve naturals, otherwise twelve-k
    -- doesn't preserve it through a pitch->note.
    let pitch = TheoryFormat.relativeToAbsolute relative
    let maybeKey = Scales.environKey env
    let c = round chromatic
        o = round octave
        d = round diatonic
    -- Symbolic pitch is still relative, so while I transpose with the key,
    -- I don't give it to 'show_pitch'.
    showPitch smap Nothing =<< if o == 0 && d == 0 && c == 0
        then pure pitch
        else do
            key <- readKey smap maybeKey
            pure $ Pitch.addOctave o $ Theory.transposeChromatic key c $
                Theory.transposeDiatonic key d pitch
    where
    octave = get Controls.octave
    chromatic = get Controls.chromatic
    diatonic = get Controls.diatonic
    get m = Map.findWithDefault 0 m controls

-- | Create a PitchNn for 'ScaleDegree.scale_degree'.
pitchNn :: ScaleMap -> TheoryFormat.RelativePitch -> Scale.PitchNn
pitchNn smap relative config@(PSignal.PitchConfig env controls) = do
    let maybeKey = Scales.environKey env
    pitch <- TheoryFormat.fmtToAbsolute (smapFmt smap) maybeKey relative
    dsteps <- if diatonic == 0 then Right 0 else do
        key <- readKey smap maybeKey
        pure $ Theory.diatonicToChromatic key
            (Pitch.pitchDegree pitch) diatonic
    let semis = Theory.pitchToSemis (smapLayout smap) pitch
        degree = octave * fromIntegral perOctave
            + fromIntegral semis + chromatic + dsteps
    smapSemisToNn smap config degree
    where
    octave = get Controls.octave
    chromatic = get Controls.chromatic
    diatonic = get Controls.diatonic
    get m = Map.findWithDefault 0 m controls
    perOctave = Theory.layoutSemisPerOctave (smapLayout smap)

inputToNote :: ScaleMap -> Scales.InputToNote
inputToNote smap env (Pitch.Input kbdType pitch frac) = do
    pitch <- Scales.kbdToScale kbdType pcPerOctave (keyTonic key) pitch
    let intervals = if isRelative
            then Theory.keyIntervals key
            else Theory.layoutIntervals (smapLayout smap)
    unless (Theory.containsDegree intervals (Pitch.pitchDegree pitch)
            && inRange smap pitch) $
        Left DeriveT.InvalidInput
    -- Relative scales don't need to figure out enharmonic spelling, and
    -- besides it would be wrong since it assumes Pitch 0 0 is C.
    let pickEnharmonic = if isRelative then id else Theory.pickEnharmonic key
    -- Don't pass the key, because I want the Input to also be relative, i.e.
    -- Pitch 0 0 should be scale degree 0 no matter the key.
    note <- invalidInput $ showPitch smap
        (if isRelative then Nothing else (Scales.environKey env))
        (pickEnharmonic pitch)
    pure $ ScaleDegree.pitchExpr frac note
    where
    isRelative = TheoryFormat.fmtRelative (smapFmt smap)
    invalidInput (Left (DeriveT.OutOfRangeError {})) =
        Left DeriveT.InvalidInput
    invalidInput x = x
    pcPerOctave = Theory.layoutPcPerOctave (smapLayout smap)
    -- Default to a key because otherwise you couldn't enter notes in an
    -- empty score!
    key = fromMaybe (smapDefaultKey smap) $
        flip Map.lookup (smapKeys smap) =<< Scales.environKey env

inRange :: ScaleMap -> Pitch.Pitch -> Bool
inRange smap pitch = bottom <= semis && semis <= top
    where
    (bottom, top) = smapRange smap
    semis = Theory.pitchToSemis (smapLayout smap) pitch

callDoc :: Set ScoreT.Control -> ScaleMap -> Doc.Doc -> Derive.DocumentedCall
callDoc transposers smap doc =
    Scales.annotateCallDoc transposers doc fields callDoc
    where
    callDoc = Scales.scaleDegreeDoc ScaleDegree.scaleDegree
    -- Not efficient, but shouldn't matter for docs.
    defaultKey = fst <$> List.find ((== smapDefaultKey smap) . snd)
        (Map.toList (smapKeys smap))
    (bottom, top) = smapRange smap
    showP = either Doc.pretty Doc.pretty . showPitch smap Nothing
        . Theory.semisToPitchSharps (smapLayout smap)
    fields = concat
        [ [("range", showP bottom <> " to " <> showP top)]
        , maybe [] (\n -> [("default key", ShowVal.doc n)]) defaultKey
        , [ ("keys", formatKeys $ Map.keys (smapKeys smap)) ]
        ]

formatKeys :: [Pitch.Key] -> Doc.Doc
formatKeys keys
    | all (("-" `Text.isInfixOf`) . Pitch.keyText) keys = Doc.commas $
        map (Doc.literal . fst) $ groupTonicMode $ map (, ()) keys
    | otherwise = Doc.commas (map Doc.pretty keys)

-- | Assuming keys are formatted @tonic-mode@, group keys by mode and replace
-- the tonics with a pattern.
groupTonicMode :: [(Pitch.Key, a)] -> [(Text, a)]
groupTonicMode = map extract . Lists.keyedGroupSort key . map (first split)
    where
    extract (mode, group) = (fmt mode (map (fst . fst) group), snd (head group))
    key ((_, mode), _) = mode
    split (Pitch.Key t) = (pre, Text.drop 1 post)
        where (pre, post) = Text.break (=='-') t
    fmt mode keys = "(" <> Text.intercalate "|" keys <> ")-" <> mode

-- * format

relativeFmt :: Theory.Key -> Keys -> TheoryFormat.RelativeFormat Theory.Key
relativeFmt defaultKey allKeys  = TheoryFormat.RelativeFormat
    { relConfig = TheoryFormat.defaultConfig
    , relKeyConfig = TheoryFormat.KeyConfig
        { keyParse = Scales.getKey defaultKey allKeys
        , keyDefault = defaultKey
        }
    , relShowDegree = TheoryFormat.showDegreeChromatic
    , relToAbsolute = TheoryFormat.chromaticToAbsolute
    }

-- * util

keyTonic :: Theory.Key -> Pitch.PitchClass
keyTonic = Pitch.degreePc . Theory.keyTonic

showPitch :: ScaleMap -> Maybe Pitch.Key -> Pitch.Pitch
    -> Either DeriveT.PitchError Pitch.Note
showPitch smap key = Right . TheoryFormat.showPitch (smapFmt smap) key
    -- Previously this would check for OutOfRange, but it meant I couldn't
    -- transpose a pitch to out of range even if I was going to later transpose
    -- it back in range (say via octave wrapping).  Since the range is
    -- ultimately which degrees can be mapped to frequencies, it seems it
    -- doesn't hurt anything for the symbolic pitch to be theoretically
    -- boundless.

readPitch :: ScaleMap -> Maybe Pitch.Key -> Pitch.Note
    -> Either DeriveT.PitchError Pitch.Pitch
readPitch smap = TheoryFormat.readPitch (smapFmt smap)

readEnvironKey :: ScaleMap -> Env.Environ
    -> Either DeriveT.PitchError Theory.Key
readEnvironKey smap = Scales.getKey (smapDefaultKey smap) (smapKeys smap)
    . Scales.environKey

readKey :: ScaleMap -> Maybe Pitch.Key
    -> Either DeriveT.PitchError Theory.Key
readKey smap = Scales.getKey (smapDefaultKey smap) (smapKeys smap)
