-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE StrictData #-}
-- | Javanese gamelan instruments.
module Synth.Sampler.Patch.Java (patches) where
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import           System.FilePath ((</>))

import qualified Util.Lists as Lists
import qualified Util.Maps as Maps
import qualified Util.Num as Num

import qualified Cmd.Instrument.ImInst as ImInst
import qualified Derive.Attrs as Attrs
import qualified Derive.Instrument.DUtil as DUtil
import qualified Derive.Scale as Scale

import qualified Instrument.Common as Common
import qualified Perform.Pitch as Pitch
import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.Patch.Lib.Bali as Lib.Bali
import qualified Synth.Sampler.Patch.Lib.Prepare as Prepare
import qualified Synth.Sampler.Patch.Lib.Util as Util
import           Synth.Sampler.Patch.Lib.Util (Dynamic(..))
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import           Global


sampleFormat :: Util.SampleFormat
sampleFormat = Util.Wav

patches :: [Patch.Patch]
patches = map makePatch [slenthem, peking]

data Instrument = Instrument {
    name :: Text
    , variations :: Variations
    , tuning :: Tuning
    , dynamic :: Dynamic -> (Util.DynVal, (Util.Db, Util.Db))
    , dynamicTweaks :: Map Sample.SamplePath Util.Db
    , articulations :: Set Articulation
    }

type Variations = Articulation -> (Pitch, Util.Dynamic) -> Util.Variation
type Tuning = Map Pitch Pitch.NoteNumber

slenthem :: Instrument
slenthem = Instrument
    { name = "slenthem"
    , variations
    , tuning = Map.fromList $ zip (map (Pitch 2) [P1 ..])
        -- TODO copy pasted from Scale.Java
        [ 50.18 -- 21
        , 51.65 -- 22
        , 53    -- 23
        , 56    -- 24
        , 57.05 -- 25
        , 58.68 -- 26
        , 60.13 -- 27
        ]
    , dynamic = standardDyns $ \case
        PP -> (-9, 1)
        MP -> (-6, 1)
        MF -> (-7, 1)
        FF -> (-7, 0)
    , dynamicTweaks = Map.fromList
        [ ("open/21-pp-v3.wav", -3)
        , ("open/21-mf-v4.wav", -2)
        , ("open/21-ff-v4.wav", 4)
        , ("open/22-pp-v3.wav", 1)
        , ("open/22-mp-v1.wav", -3)
        , ("open/22-mp-v2.wav", -3)
        , ("open/24-pp-v2.wav", -5)
        , ("open/24-mp-v1.wav", -3)
        , ("open/24-mf-v1.wav", 2)
        , ("open/24-mf-v2.wav", 2)
        , ("open/24-mf-v3.wav", -3)
        , ("open/24-mf-v4.wav", -3)
        , ("open/24-ff-v3.wav", -2)
        , ("open/24-ff-v4.wav", -3)
        , ("open/25-pp-v1.wav", 2)
        , ("open/25-mf-v1.wav", -3)
        , ("open/25-ff-v1.wav", 2)
        , ("open/26-mp-v2.wav", 2)
        , ("open/26-ff-v4.wav", -2)
        , ("open/27-pp-v4.wav", 2)
        ]
    , articulations = Set.fromList [Open, Mute]
    }
    where
    variations Open = \case
        (Pitch 2 P6, PP) -> 3
        (Pitch 2 P6, MF) -> 5
        (Pitch 2 P7, FF) -> 3
        _ -> 4
    variations Mute = \case
        (Pitch 2 P5, PP) -> 5
        _ -> 6
    variations Character = const 0

peking :: Instrument
peking = Instrument
    { name = "peking"
    , variations
    , tuning = Map.fromList $ zip (map (Pitch 5) [P1 ..])
        -- TODO copy pasted from Scale.Java
        [ 86.4  -- 51
        , 87.7  -- 52
        , 88.98 -- 53
        , 80 + 12    -- 44
        , 81.03 + 12 -- 45
        , 82.48 + 12 -- 46
        , 84.14 + 12 -- 47
        ]
    -- TODO
    , dynamic = standardDyns $ \case
        _ -> (0, 0)
        -- PP -> (0.25, (-8, 4))
        -- MP -> (0.5, (-4, 4))
        -- MF -> (0.75, (-4, 6))
        -- FF -> (1, (-4, 2))
    , dynamicTweaks = mempty
    , articulations = Set.fromList [Open, Mute, Character]
    }
    where
    variations Open = const 4
    variations Mute = \case
        (Pitch 5 P3, MF) -> 5
        _ -> 6
    variations Character = \(Pitch _ p, dyn) ->
        fromMaybe 0 $ Map.lookup dyn =<< Map.lookup p byPitch
        where
        -- Character works with +character, calls have to add it or not.
        byPitch = Map.fromList $ zip [P1 ..] $ map Map.fromList
            [ [(MP, 2), (MF, 2), (FF, 2)]
            , [(MP, 1), (MF, 4)]
            , [(MF, 4)]
            , [(MP, 2), (MF, 4)]
            , [(MP, 2), (MF, 2)]
            , [(MF, 5)]
            , [(MP, 4), (MF, 4)]
            ]

standardDyns :: (Dynamic -> range) -> Dynamic -> (Util.DynVal, range)
standardDyns f = \case
    PP -> (0.25, f PP)
    MP -> (0.5, f MP)
    MF -> (0.75, f MF)
    FF -> (1, f FF)

makePatch :: Instrument -> Patch.Patch
makePatch inst = (Patch.patch name)
    { Patch._dir = dir
    , Patch._convert = convert inst attributeMap
    , Patch._karyaPatch =
        ImInst.code #= code $
        ImInst.range (makeRange tuning) $
        ImInst.make_patch $
        Lib.Bali.supportVariableMute $
        Util.patchPitchDynVar attributeMap
    , Patch._allFilenames = Set.fromList $ allFilenames inst
    }
    where
    dir = "java" </> untxt name
    -- TODO copy paste with Rambat
    code = Lib.Bali.zeroDurMute 0.65
        <> Util.thru dir (convert inst attributeMap)
        <> ImInst.postproc DUtil.with_symbolic_pitch
    -- %character at 0 never adds +character, at 1 always adds it
    attributeMap = Common.attribute_map $
        filter ((`Set.member` articulations) . snd)
            [ (Attrs.mute, Mute)
            , (Attrs.attr "character", Character)
            , (mempty, Open)
            ]
    Instrument { name, tuning, articulations } = inst

-- * implementation

allFilenames :: Instrument -> [Sample.SamplePath]
allFilenames (Instrument { tuning, variations }) =
    [ fname
    | art <- Util.enumAll
    , pitch <- Map.keys tuning
    , dyn <- Util.enumAll
    , fname <- toFilenames variations art pitch dyn
    ]

makeRange :: Map Pitch a -> Scale.Range
makeRange tuning = Scale.Range (toPitch bottom) (toPitch top)
    where
    Just bottom = fst . fst <$> Map.minViewWithKey tuning
    Just top = fst . fst <$> Map.maxViewWithKey tuning

data Articulation = Open | Mute | Character -- ^ peking have character
    deriving (Show, Eq, Ord, Enum, Bounded)

convert :: Instrument -> Common.AttributeMap Articulation -> Note.Note
    -> Patch.ConvertM Sample.Sample
convert inst attrMap note = do
    let art = Util.articulationDefault Open attrMap $ Note.attributes note
    let (dyn, dynVal) = Util.dynamic dynamic note
    symPitch <- Util.symbolicPitch note
    (pitch, (noteNn, sampleNn)) <- tryRight $ findPitch tuning symPitch
    let fname = Util.chooseVariation (findFilenames variations art pitch dyn)
            note
    dynVal <- return $ dynVal
        + Util.dbToDyn (Map.findWithDefault 0 fname dynamicTweaks)
    return $ (Sample.make fname)
        { Sample.envelope = if
            | art == Mute -> Signal.constant dynVal
            | otherwise -> Lib.Bali.variableMuteEnv dynVal note
        , Sample.ratios = Signal.constant $ Sample.pitchToRatio sampleNn noteNn
        }
    where
    Instrument { tuning, variations, dynamic, dynamicTweaks } = inst

-- TODO similar to Rambat.findPitch, except no umbang/isep
findPitch :: Tuning -> Either Pitch.Note Pitch.NoteNumber
    -> Either Text (Pitch, (Pitch.NoteNumber, Pitch.NoteNumber))
findPitch tuning = either findSymPitch findNnPitch
    where
    findSymPitch (Pitch.Note pitch) = do
        pitch <- tryJust ("can't parse symbolic pitch: " <> pitch) $
            parsePitch pitch
        -- Symbolic pitch doesn't actually need the nn, but can at least check
        -- range.
        nn <- tryJust ("pitch out of range: " <> txt (showPitch pitch)) $
            Map.lookup pitch tuning
        return (pitch, (nn, nn))
    findNnPitch nn = do
        (sampleNn, pitch) <- tryJust "no pitches" $
            Maps.lookupClosest nn nnToPitch
        return (pitch, (nn, sampleNn))
    nnToPitch = Maps.invert tuning

-- If 0 variations, choose from next dyn up
findFilenames :: Variations -> Articulation -> Pitch -> Util.Dynamic
    -> [Sample.SamplePath]
findFilenames variation art pitch dyn = nonNull
    [ toFilenames variation art pitch dyn
    , if dyn < FF then findFilenames variation art pitch (succ dyn) else []
    , if art == Character then findFilenames variation Open pitch dyn else []
    ]
    where
    nonNull ([] : xs) = nonNull xs
    nonNull (x : _) = x
    nonNull [] = []

-- | Find variation samples: {open,mute}/2{1..7}-{pp,mp,mf,ff}-v{1..n}
toFilenames :: Variations -> Articulation -> Pitch -> Util.Dynamic
    -> [Sample.SamplePath]
toFilenames variations art pitch dyn =
    map (unparseFilename pitch art dyn) [1 .. variations art (pitch, dyn)]

unparseFilename :: Pitch -> Articulation -> Util.Dynamic -> Util.Variation
    -> Sample.SamplePath
unparseFilename pitch art dyn var =
    articulationDir art
        </> Lists.join "-" [showPitch pitch, Util.showLower dyn, 'v' : show var]
        ++ Util.extension sampleFormat

articulationDir :: Articulation -> String
articulationDir = \case
    Mute -> "mute"
    Open -> "open"
    Character -> "character"

-- * prepare

_relink :: String -> [FilePath] -> IO ()
_relink inst filenames =
    Prepare.relink (Prepare.baseDir </> "java" </> inst) "raw" "samples"
        =<< Prepare.renames (Prepare.baseDir </> "java" </> inst </> "raw")
            filenames

_pekingRelink :: IO ()
_pekingRelink = _relink "peking" (allFilenames peking)


-- * pitch

data Pitch = Pitch Pitch.Octave PitchClass
    deriving (Show, Eq, Ord)

data PitchClass = P1 | P2 | P3 | P4 | P5 | P6 | P7
    deriving (Show, Eq, Ord, Enum)

toPitch :: Pitch -> Pitch.Pitch
toPitch (Pitch oct pc) = Pitch.pitch oct (fromEnum pc)

showPitch :: Pitch -> String
showPitch (Pitch oct pc) = show oct <> drop 1 (show pc)

parsePitch :: Text -> Maybe Pitch
parsePitch t = do
    [o, p] <- return $ Text.unpack t
    oct <- Num.readDigit o
    guard $ Num.inRange 0 8 oct
    pc <- parsePitchClass =<< Num.readDigit p
    Just $ Pitch oct pc

parsePitchClass :: Int -> Maybe PitchClass
parsePitchClass = \case
    1 -> Just P1
    2 -> Just P2
    3 -> Just P3
    4 -> Just P4
    5 -> Just P5
    6 -> Just P6
    7 -> Just P7
    _ -> Nothing
