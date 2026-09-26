-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for Balinese scales.  Mostly that means dealing with umbang and
-- isep.
--
-- They're implemented as a modification of "ChromaticScales" because a saih
-- pitu or pelog scale requires a key or pathet, which winds up being similar
-- to a simplified chromatic scale.
module Derive.Scale.BaliScales where
import qualified Data.Attoparsec.Text as A
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import           Data.Vector ((!?))

import qualified Util.Doc as Doc
import qualified Util.Lists as Lists
import qualified Util.Num as Num
import qualified Util.Texts as Texts

import qualified Derive.DeriveT as DeriveT
import qualified Derive.EnvKey as EnvKey
import qualified Derive.PSignal as PSignal
import qualified Derive.REnv as REnv
import qualified Derive.Scale as Scale
import qualified Derive.Scale.ChromaticScales as ChromaticScales
import qualified Derive.Scale.Scales as Scales
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.TheoryFormat as TheoryFormat
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch

import           Global


-- | Top level scale constructor.
makeScale :: Pitch.ScaleId -> ScaleMap -> Scale.Scale
makeScale scaleId smap =
    (ChromaticScales.makeScale scaleId (smapChromatic smap) doc)
    { Scale.scaleEnharmonics = Scales.noEnharmonics }
    where
    doc = "Balinese scales come in detuned pairs. They use the "
        <> ShowVal.doc EnvKey.tuning <> " env var to select between pengumbang\
        \ and pengisep. The env var should be set to either `umbang` or `isep`,\
        \ defaulting to `umbang`. Normally the umbang and isep\
        \ frequencies are hardcoded according to the scale, but if the "
        <> ShowVal.doc cOmbak
        <> " control is present, they will be tuned that many hz apart.\
        \\nThe " <> ShowVal.doc larasKey <> " env var chooses between\
        \ different tunings.  It defaults to "
        <> ShowVal.doc (larasName (smapDefaultLaras smap))
        <> ". Laras:\n"
        <> Texts.enumeration
            [ ShowVal.doc name <> " - " <> larasDoc laras
            | (name, laras) <- Map.toList (smapLaras smap)
            ]

data ScaleMap = ScaleMap
    { smapChromatic :: !ChromaticScales.ScaleMap
    , smapLaras :: !LarasMap
    , smapDefaultLaras :: !Laras
    }

type LarasMap = Map Text Laras

scaleMap :: Config -> TheoryFormat.Format -> Maybe (Pitch.Semi, Pitch.Semi)
    -- ^ If not given, use the complete range of the saih.
    -> ScaleMap
scaleMap (Config layout allKeys defaultKey laras defaultLaras)
        fmt maybeRange =
    ScaleMap
        { smapChromatic =
            (ChromaticScales.scaleMap layout fmt allKeys defaultKey)
            { ChromaticScales.smapSemisToNn =
                semisToNn layout laras defaultLaras
            -- Convert range to absolute.
            , ChromaticScales.smapRange = bimap (+offset) (+offset) range
            }
        , smapLaras = laras
        , smapDefaultLaras = defaultLaras
        }
    where
    -- Each laras can start on a different note, but I use the default one
    -- for the range as a whole.
    offset = larasOffset layout defaultLaras
    range = fromMaybe (0, top) maybeRange
    top = maybe 0 (subtract 1 . Vector.length . larasUmbang) $
        Lists.head (Map.elems laras)

data Config = Config
    { configLayout :: !Theory.Layout
    , configKeys :: !ChromaticScales.Keys
    , configDefaultKey :: !Theory.Key
    , configLaras :: !LarasMap
    , configDefaultLaras :: !Laras
    } deriving (Show)

-- | This is a specialized version of 'scale_map' that uses base octave and
-- low and high pitches to compute the range.
instrumentScaleMap :: Config -> Instrument -> ScaleMap
instrumentScaleMap config
        (Instrument degrees relativeOctaves centerOct low high) =
    scaleMap config fmt (Just (toPc low, toPc high))
    where
    toPc p = Pitch.diffPc perOct p basePitch
    basePitch = larasBase (configDefaultLaras config)
    fmt = relativeArrow degrees relativeOctaves centerOct True
        (configDefaultKey config) (configKeys config)
    perOct = Theory.layoutSemisPerOctave (configLayout config)

-- | Describe an instrument-relative scale.
data Instrument = Instrument
    { instDegrees :: !TheoryFormat.Degrees
    , instRelativeOctaves :: !RelativeOctaves
    , instCenter :: !Pitch.Octave
    , instLow :: !Pitch.Pitch
    , instHigh :: !Pitch.Pitch
    } deriving (Eq, Show)

instrumentRange :: Instrument -> Scale.Range
instrumentRange inst = Scale.Range (instLow inst) (instHigh inst)

-- * Laras

-- | Describe the frequencies in a saih.  This doesn't say what the range is,
-- since that's in the 'ScaleMap', and all saihs in one scale should have the
-- same range.
data Laras = Laras
    { larasName :: Text
    , larasDoc :: Doc.Doc
    -- | The pitch where the laras starts.  It should be such that octave 4 is
    -- close to middle C.
    , larasBase :: Pitch.Pitch
    , larasUmbang :: Vector.Vector Pitch.NoteNumber
    , larasIsep :: Vector.Vector Pitch.NoteNumber
    } deriving (Eq, Show)

larasMap :: [Laras] -> Map Text Laras
larasMap = Map.fromList . Lists.keyOn larasName

laras :: Text -> Pitch.Pitch -> ([Pitch.NoteNumber] -> [Pitch.NoteNumber])
    -> Doc.Doc -> [(Pitch.NoteNumber, Pitch.NoteNumber)] -> Laras
laras name basePitch extend doc nns = Laras
    { larasName = name
    , larasDoc = doc
    , larasBase = basePitch
    , larasUmbang = Vector.fromList (extend umbang)
    , larasIsep = Vector.fromList (extend isep)
    }
    where (umbang, isep) = unzip nns

larasOffset :: Theory.Layout -> Laras -> Pitch.Semi
larasOffset layout laras =
    Theory.layoutSemisPerOctave layout * Pitch.pitchOctave basePitch
        + Pitch.pitchPc basePitch
    where
    basePitch = larasBase laras

larasNns :: Laras -> [(Pitch.NoteNumber, Pitch.NoteNumber)]
larasNns laras =
    zip (Vector.toList (larasUmbang laras)) (Vector.toList (larasIsep laras))

-- * Format

-- | This can't use backtick symbols because then the combining octave
-- characters don't combine.
balinese :: TheoryFormat.Degrees
balinese = TheoryFormat.makeDegrees ["᭦", "᭡", "᭢", "᭣", "᭤"]

ioeua :: TheoryFormat.Degrees
ioeua = TheoryFormat.makeDegrees ["i", "o", "e", "u", "a"]

digitOctaveRelative :: TheoryFormat.Degrees -> Bool -> Theory.Key
    -> ChromaticScales.Keys -> TheoryFormat.Format
digitOctaveRelative degrees chromatic defaultKey keys =
    TheoryFormat.makeRelativeFormat
        ("[1-9]" <> degreesDoc degrees <> if chromatic then "#?" else "")
        degrees fmt
    where fmt = ChromaticScales.relativeFmt defaultKey keys

ioeuaRelative :: Bool -> Theory.Key -> ChromaticScales.Keys
    -> TheoryFormat.Format
ioeuaRelative = digitOctaveRelative ioeua

-- | (high, middle, low)
type RelativeOctaves = (Char, Maybe Char, Char)

-- | Use ascii-art arrows for octaves.
arrowOctaves :: RelativeOctaves
arrowOctaves = ('^', Just '-', '_')

-- | Use combining marks for octaves.
balineseOctaves :: RelativeOctaves
balineseOctaves =
    ( '\x1b6b' -- balinese musical symbol combining tegeh
    , Nothing
    , '\x1b6c' -- balinese musical symbol combining endep
    )

degreesDoc :: TheoryFormat.Degrees -> Text
degreesDoc degrees = "[" <> mconcat (Vector.toList degrees) <> "]"

relativeArrow :: TheoryFormat.Degrees -> RelativeOctaves
    -> Pitch.Octave -> Bool -> Theory.Key
    -> ChromaticScales.Keys -> TheoryFormat.Format
relativeArrow degrees relativeOctaves center chromatic defaultKey keys =
    TheoryFormat.makeRelativeFormat
        (degreesDoc degrees <> (if chromatic then "#?" else "") <> "[_^-]")
        degrees fmt
    where
    fmt = modifyConfig (setRelativeOctaves relativeOctaves center) $
        ChromaticScales.relativeFmt defaultKey keys

ioeuaRelativeArrow :: Pitch.Octave -> Bool -> Theory.Key
    -> ChromaticScales.Keys -> TheoryFormat.Format
ioeuaRelativeArrow = relativeArrow ioeua arrowOctaves

ioeuaAbsolute :: TheoryFormat.Format
ioeuaAbsolute = TheoryFormat.makeAbsoluteFormat "[1-9][ioeua]" ioeua

modifyConfig :: (TheoryFormat.Config -> TheoryFormat.Config)
    -> TheoryFormat.RelativeFormat key -> TheoryFormat.RelativeFormat key
modifyConfig f fmt = fmt
    { TheoryFormat.relConfig = f (TheoryFormat.relConfig fmt) }

setRelativeOctaves :: RelativeOctaves -> Pitch.Octave
    -> TheoryFormat.Config -> TheoryFormat.Config
setRelativeOctaves (high, middle, low) center =
    TheoryFormat.setOctave showOctave parseOctave
    where
    showOctave oct
        | oct > center = (<> Text.replicate (oct-center) (t high))
        | oct < center = (<> Text.replicate (center-oct) (t low))
        | otherwise = (<> maybe "" t middle)
    parseOctave pDegree = do
        (pc, acc) <- pDegree
        octStr <- A.many' $ A.satisfy $ \c ->
            c == high || c == low || maybe True (==c) middle
        let octValue c
                | c == high = 1
                | c == low = -1
                | otherwise = 0
        let oct = Num.sum $ map octValue octStr
        pure $ TheoryFormat.RelativePitch (center + oct) pc acc
    t = Text.singleton

cipherRelativeDotted :: Pitch.Octave -> Theory.Key -> ChromaticScales.Keys
    -> TheoryFormat.Format
cipherRelativeDotted center defaultKey keys =
    TheoryFormat.makeRelativeFormat "[12356]|`[12356][.^]*`" cipher5 fmt
    where
    fmt = modifyConfig (dottedOctaves center) $
        ChromaticScales.relativeFmt defaultKey keys

cipher5 :: TheoryFormat.Degrees
cipher5 = TheoryFormat.makeDegrees ["1", "2", "3", "5", "6"]

dottedOctaves :: Pitch.Octave -> TheoryFormat.Config -> TheoryFormat.Config
dottedOctaves center = TheoryFormat.setOctave showOctave parseOctave
    where
    showOctave oct d
        | oct == center = d
        | otherwise = "`" <> d
            <> (if oct >= center then Text.replicate (oct-center) "^"
                else Text.replicate (center-oct) ".")
            <> "`"
    parseOctave pDegree =
        uncurry (TheoryFormat.RelativePitch center) <$> pDegree
            <|> withOctave pDegree
    withOctave pDegree = do
        A.char '`'
        (pc, acc) <- pDegree
        octs <- A.many' $ A.satisfy $ \c -> c == '.' || c == '^'
        A.char '`'
        let oct = Lists.count (=='^') octs - Lists.count (=='.') octs
        pure $ TheoryFormat.RelativePitch (center + oct) pc acc

-- * tuning

data Tuning = Umbang | Isep deriving (Eq, Ord, Enum, Bounded, Show)

instance Pretty Tuning where pretty = showt
instance Typecheck.Typecheck Tuning
instance REnv.ToVal Tuning
instance ShowVal.ShowVal Tuning

-- | If ombak is unset, use the hardcoded tunings.  Otherwise, create new
-- umbang and isep tunings based on the given number.
cOmbak :: ScoreT.Control
cOmbak = "ombak"

-- | Convert 'Pitch.FSemi' to 'Pitch.NoteNumber'.
semisToNn :: Theory.Layout -> LarasMap -> Laras
    -> ChromaticScales.SemisToNoteNumber
semisToNn layout larasMap defaultLaras =
    \(PSignal.PitchConfig env controls) fsemis_ -> do
        laras <- Scales.readEnviron (\v -> Map.lookup v larasMap)
            (Just defaultLaras) larasKey env
        let fsemis = fsemis_ - fromIntegral offset
            offset = larasOffset layout laras
        tuning <- Scales.readEnviron Just (Just Umbang) EnvKey.tuning env
        let err = DeriveT.outOfRangeError fsemis
                (0, Vector.length (larasUmbang laras))
        justErr err $ case Map.lookup cOmbak controls of
            Nothing -> case tuning of
                Umbang -> getNn (larasUmbang laras) fsemis
                Isep -> getNn (larasIsep laras) fsemis
            Just ombak -> do
                umbang <- getNn (larasUmbang laras) fsemis
                isep <- getNn (larasIsep laras) fsemis
                let avg = (Pitch.nnToHz umbang + Pitch.nnToHz isep) / 2
                pure $ Pitch.hzToNn $ case tuning of
                    Umbang -> avg - ombak / 2
                    Isep -> avg + ombak / 2

-- | VStr: Select saih tuning.
larasKey :: EnvKey.Key
larasKey = "laras"

getNn :: Vector.Vector Pitch.NoteNumber -> Pitch.FSemi
    -> Maybe Pitch.NoteNumber
getNn nns fsemis
    | frac == 0 = nns !? semis
    | otherwise = do
        low <- nns !? semis
        high <- nns !? (semis + 1)
        pure $ Num.scale low high (Pitch.nn frac)
    where (semis, frac) = properFraction fsemis
