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
import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.Patch.Lib.Bali as Lib.Bali
import qualified Synth.Sampler.Patch.Lib.Drum as Drum
import qualified Synth.Sampler.Patch.Lib.Prepare as Prepare
import qualified Synth.Sampler.Patch.Lib.Util as Util
import           Synth.Sampler.Patch.Lib.Util (Dynamic(..))
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import           Global


sampleFormat :: Util.SampleFormat
sampleFormat = Util.Wav

patches :: [Patch.Patch]
patches = map makePatch [genderPanerus, slenthem, peking, kenong]

data Instrument = Instrument {
    name :: Text
    , variations :: Variations
    , tuning :: Tuning
    , dynamicTweaks :: Map Sample.SamplePath Util.Db
    , articulations :: Set Articulation
    }

data Variations =
    FixedDyns FixedDyns (Dynamic -> (Util.Dyn, (Util.Db, Util.Db)))
    | VarDyns VarDyns
type FixedDyns = Articulation -> (Pitch, Util.Dynamic) -> Util.Variation

-- TODO convert from Dyn to Db
type VarDyns = Map Pitch (Map Articulation (Map Util.Dyn [Sample.SamplePath]))

-- | dB from dBFS numbers, which should be negative, where 0 is full volume.
-- via 'normalize --no-adjust *', pick 'peak' numbers.
makeVarDyns :: [(Pitch, [(Articulation, [Util.Db])])] -> VarDyns
makeVarDyns pitchArtDbs = Map.fromList
    [ (pitch, Map.fromList [(art, make pitch art dbs) | (art, dbs) <- arts])
    | (pitch, arts) <- pitchArtDbs
    ]
    where
    make pitch art dbs = Maps.multimap $ zip (map AUtil.dbToDyn dbs)
        (map (unparseFilenameVarDyn pitch art) [1 .. length dbs])

type Tuning = Map Pitch Pitch.NoteNumber

genderPanerus :: Instrument
genderPanerus = Instrument
    { name = "gender-panerus"
    , variations = FixedDyns fixedDyns dynamic
    , tuning = Map.fromList $ zip (filter (not . is4) [Pitch 2 P6 ..])
        [ 58.68 -- 26
        , 60.13 -- 27
        , 62.18 -- 31 TODO approx, no resonator
        , 63.65 -- 32
        , 65    -- 33
        , 69.05 -- 35
        , 70.5  -- 36
        , 72.14 -- 37
        , 74.25 -- 41 TODO approx
        , 75.68 -- 42
        , 77    -- 43
        , 81.03 -- 45
        , 82.48 -- 46
        , 84.14 -- 47
        , 86.4  -- 51 TODO approx
        , 87.7  -- 52
        , 88.98 -- 53
        ]
    , dynamicTweaks = Map.fromList
        [ ("open/26-mf-v4.wav", -2)
        , ("open/27-mf-v2.wav", 2)
        , ("open/27-mp-v2.wav", 3)
        , ("open/27-pp-v1.wav", 5)
        , ("open/27-pp-v2.wav", 7)
        , ("open/33-mf-v3.wav", -1)
        , ("open/33-pp-v3.wav", -3)
        , ("open/35-ff-v4.wav", -3)
        , ("open/35-mp-v3.wav", 2)
        , ("open/35-mp-v4.wav", 4)
        , ("open/35-pp-v3.wav", -2)
        , ("open/37-ff-v3.wav", -2)
        , ("open/37-ff-v4.wav", -4)
        , ("open/37-mp-v1.wav", 5)
        , ("open/41-mf-v1.wav", 2)
        , ("open/42-mp-v4.wav", -3)
        , ("open/42-mp-v5.wav", -4)
        , ("open/42-pp-v3.wav", 3)
        , ("open/45-mf-v1.wav", 3)
        , ("open/45-mf-v2.wav", 3)
        , ("open/45-mf-v3.wav", 3)
        , ("open/47-ff-v4.wav", -3)
        , ("open/51-pp-v1.wav", -4)
        , ("open/52-ff-v2.wav", 4)
        , ("open/52-mf-v6.wav", -4)
        ]
    , articulations = Set.fromList [Open, Mute]
    }
    where
    fixedDyns Open = \case
        (Pitch 2 P6, MP) -> 3
        (Pitch 3 P2, MF) -> 3
        (Pitch 3 P2, FF) -> 3
        (Pitch 3 P3, MP) -> 3
        (Pitch 3 P3, MF) -> 3
        (Pitch 3 P3, FF) -> 2
        (Pitch 3 P5, MF) -> 5
        (Pitch 3 P7, MP) -> 5
        (Pitch 3 P7, MF) -> 5
        (Pitch 4 P1, MP) -> 5
        (Pitch 4 P1, MF) -> 5
        (Pitch 4 P2, PP) -> 5
        (Pitch 4 P2, MP) -> 5
        (Pitch 4 P2, MF) -> 5
        (Pitch 4 P3, MF) -> 3
        (Pitch 4 P5, MF) -> 5
        (Pitch 4 P6, MF) -> 5
        (Pitch 4 P7, PP) -> 3
        (Pitch 4 P7, MF) -> 5
        (Pitch 5 P1, MP) -> 5
        (Pitch 5 P1, MF) -> 5
        (Pitch 5 P2, MF) -> 6
        (Pitch 5 P3, MF) -> 5
        (Pitch 5 P3, FF) -> 5
        _ -> 4
    fixedDyns Mute = \case
        -- TODO
        _ -> 0
        -- _ -> 6
    fixedDyns Character = const 0
    fixedDyns MuteLoose = const 0
    dynamic = standardDyns $ \case -- TODO
        _ -> (0, 0)

slenthem :: Instrument
slenthem = Instrument
    { name = "slenthem"
    , variations = FixedDyns fixedDyns dynamic
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
    -- TODO: 22-ff-v[1-4].wav are distorted
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
    fixedDyns Open = \case
        (Pitch 2 P6, PP) -> 3
        (Pitch 2 P6, MF) -> 5
        (Pitch 2 P7, FF) -> 3
        _ -> 4
    fixedDyns Mute = \case
        (Pitch 2 P5, PP) -> 5
        _ -> 6
    fixedDyns Character = const 0
    fixedDyns MuteLoose = const 0
    dynamic = standardDyns $ \case
        PP -> (-9, 1)
        MP -> (-6, 1)
        MF -> (-7, 1)
        FF -> (-7, 0)

peking :: Instrument
peking = Instrument
    { name = "peking"
    , variations = FixedDyns fixedDyns dynamic
    , tuning = Map.fromList $ zip (map (Pitch 5) [P1 ..])
        -- TODO copy pasted from Scale.Java, retune from samples
        [ 86.4  -- 51
        , 87.7  -- 52
        , 88.98 -- 53
        , 80 + 12    -- 44
        , 81.03 + 12 -- 45
        , 82.48 + 12 -- 46
        , 84.14 + 12 -- 47
        ]
    , dynamicTweaks = mempty
    , articulations = Set.fromList [Open, Mute, Character]
    }
    where
    fixedDyns Open = const 4
    fixedDyns Mute = \case
        (Pitch 5 P3, MF) -> 5
        _ -> 6
    fixedDyns Character = \(Pitch _ p, dyn) ->
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
    fixedDyns MuteLoose = const 0
    -- TODO
    dynamic = standardDyns $ \case
        _ -> (0, 0)
        -- PP -> (0.25, (-8, 4))
        -- MP -> (0.5, (-4, 4))
        -- MF -> (0.75, (-4, 6))
        -- FF -> (1, (-4, 2))

{-
calibrateSine :: Instrument
calibrateSine = Instrument
    { name = "sine"
    , variations = VarDyns varDyns
    , tuning = Map.fromList [(Pitch 3 P1, 60)]
    , dynamicTweaks = mempty
    , articulations = Set.fromList [Open]
    }
    where
    varDyns Open _ = evenDyns 25
    varDyns _ _ = mempty

t0 = Drum.makeFileListWeighted (Prepare.baseDir </> "java/kenong/raw")
    ["Open", "MuteTight", "MuteLoose"]
    "kenongSamples"

renames :: IO ()
renames =
    -- Prepare.relink "data/sampler" "sine" "java/sine" $
    Prepare.relink "data" "im/calibrate/sine" "sampler/java/sine" $
        zip olds (allFilenames calibrateSine)
    where
    olds = map untxt ["60nn-" <> Num.zeroPad 3 dyn <> ".wav" | dyn <- dyns]
    -- pitch = Pitch 3 P1
    -- unparse = unparseFilenameVarDyn pitch Open
    dyns = [4, 8 .. 100]

    -- fnames = Maps.multimap $ zip (Unboxed.toList dyns)
    --     (map (unparseFilenameVarDyn pitch art) [1 .. Unboxed.length dyns])
-}

kenong :: Instrument
kenong = Instrument
    { name = "kenong"
    , variations = VarDyns varDyns
    , tuning = Map.fromList $ zip (filter (not . is4) [Pitch 3 P2 ..])
        -- TODO copy pasted from Scale.Java, retune from samples
        [ 63.65 -- 32
        , 65    -- 33
        , 69.05 -- 35
        , 70.5  -- 36
        , 72.14 -- 37
        , 74.25 -- 41 1 approx
        ]
    , dynamicTweaks = mempty
    , articulations = Set.fromList [Open, Mute, MuteLoose]
    }
    where
    varDyns = makeVarDyns
        [ (Pitch 3 P7,
            [ (Open,
                [ -36.6839
                , -31.3895
                , -30.0015
                , -24.4858
                , -18.1833
                , -19.8991
                , -18.0129
                , -15.8150
                , -10.2637
                , -10.2784
                , -11.2703
                , -7.5212
                , -5.3331
                , -5.0327
                , -1.3736
                , -2.4583
                , -0.9238
                , 0
                , 0
                ])
            , (MuteLoose,
                [ -49.4809
                , -44.9185
                , -41.0908
                , -39.0391
                , -34.4190
                , -30.7726
                , -27.6380
                , -30.5912
                , -18.4120
                , -12.0102
                , -10.1257
                , -6.5167
                , -5.5805
                , -2.4116
                , -0.7634
                , -0.0027
                , 0.0000
                ])
            , (Mute,
                [ -50.3090
                , -48.3705
                , -38.4874
                , -39.3042
                , -28.8344
                , -28.4965
                , -27.9170
                , -23.5516
                , -18.3966
                , -16.8538
                , -17.1638
                , -17.3493
                , -8.7410
                , -4.7008
                , -7.9078
                , -6.9082
                , -2.2821
                , -0.0029
                , -0.0003
                , 0.0000
                , 0.0000
                , 0.0000
                ])
            ])
        , (Pitch 4 P1,
            [])
        ]

standardDyns :: (Dynamic -> range) -> Dynamic -> (Util.Dyn, range)
standardDyns f = \case
    PP -> (0.25, f PP)
    MP -> (0.5, f MP)
    -- 0.85 instead of 0.75 because FF samples are more extreme, and should be
    -- uncommon.
    MF -> (0.85, f MF)
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
            [ (Attrs.mute <> Attrs.loose, MuteLoose)
            , (Attrs.mute, Mute)
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
    , fname <- case variations of
        FixedDyns fixedDyns _ -> toFilenames fixedDyns art pitch dyn
        VarDyns varDyns -> toFilenamesVarDyn varDyns art pitch
    ]

makeRange :: Map Pitch a -> Scale.Range
makeRange tuning = Scale.Range (toPitch bottom) (toPitch top)
    where
    Just bottom = fst . fst <$> Map.minViewWithKey tuning
    Just top = fst . fst <$> Map.maxViewWithKey tuning

data Articulation = Open
    | Mute -- ^ If there is MuteLoose, this is MuteTight
    | MuteLoose
    | Character -- ^ peking have character
    deriving (Show, Eq, Ord, Enum, Bounded)

isMute :: Articulation -> Bool
isMute = \case
    Mute -> True
    MuteLoose -> True
    _ -> False

convert :: Instrument -> Common.AttributeMap Articulation -> Note.Note
    -> Patch.ConvertM Sample.Sample
convert inst attrMap note = do
    symPitch <- Util.symbolicPitch note
    (pitch, (noteNn, sampleNn)) <- tryRight $ findPitch tuning symPitch
    (fname, dynVal) <- tryJust "no samples" $ case variations of
        FixedDyns fixedDyns dynamic ->
            convertFixedDyn fixedDyns dynamic pitch art note
        VarDyns varDyns -> convertVarDyn varDyns pitch art note
    dynVal <- return $ dynVal
        + Util.dbToDyn (Map.findWithDefault 0 fname dynamicTweaks)
    return $ (Sample.make fname)
        { Sample.envelope = if
            | isMute art -> Signal.constant dynVal
            | otherwise -> Lib.Bali.variableMuteEnv dynVal note
        , Sample.ratios = Signal.constant $ Sample.pitchToRatio sampleNn noteNn
        }
    where
    Instrument { tuning, variations, dynamicTweaks } = inst
    art = Util.articulationDefault Open attrMap $ Note.attributes note

convertFixedDyn :: FixedDyns -> (Dynamic -> (Util.Dyn, (Util.Db, Util.Db)))
    -> Pitch -> Articulation -> Note.Note
    -> Maybe (Sample.SamplePath, Util.Dyn)
convertFixedDyn fixedDyns dynamic pitch art note =
    (, dynVal) <$>
        Util.noteVariation (findFilenames fixedDyns art pitch dyn) note
    where
    (dyn, dynVal) = Util.dynamic dynamic note

convertVarDyn :: VarDyns -> Pitch -> Articulation -> Note.Note
    -> Maybe (Sample.SamplePath, Util.Dyn)
convertVarDyn varDyns pitch art note =
    Drum.pickDynWeighted varRange dynToSamples
        (Note.initial0 Control.dynamic note)
        (Note.initial0 Control.variation note)
    where
    -- It depends on the timbre variation over dynamics.  That actually
    -- changes, so I'd need a variable one of these, but too complicated.
    varRange = 0.1
    dynToSamples = Maps.getM art $ Maps.getM pitch varDyns

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
findFilenames :: FixedDyns -> Articulation -> Pitch -> Util.Dynamic
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
toFilenames :: FixedDyns -> Articulation -> Pitch -> Util.Dynamic
    -> [Sample.SamplePath]
toFilenames fixedDyns art pitch dyn =
    map (unparseFilename pitch art dyn) [1 .. fixedDyns art (pitch, dyn)]

unparseFilename :: Pitch -> Articulation -> Util.Dynamic -> Util.Variation
    -> Sample.SamplePath
unparseFilename pitch art dyn var =
    articulationDir art
        </> Lists.join "-"
            [showPitch pitch, Util.showLower dyn, Util.showVariation var]
        ++ Util.extension sampleFormat

toFilenamesVarDyn :: VarDyns -> Articulation -> Pitch -> [Sample.SamplePath]
toFilenamesVarDyn varDyns art pitch =
    concat $ Map.elems $ Maps.getM art $ Maps.getM pitch varDyns

unparseFilenameVarDyn :: Pitch -> Articulation -> Util.Variation
    -> Sample.SamplePath
unparseFilenameVarDyn pitch art var =
    articulationDir art
        </> Lists.join "-" [showPitch pitch, untxt $ Num.zeroPad 3 var]
        ++ Util.extension sampleFormat

articulationDir :: Articulation -> String
articulationDir = \case
    Mute -> "mute"
    MuteLoose -> "mute-loose"
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

_printIndices :: IO ()
_printIndices = Prepare.printIndices 2 $ allFilenames genderPanerus

-- * pitch

data Pitch = Pitch Pitch.Octave PitchClass
    deriving (Show, Eq, Ord)

instance Enum Pitch where
    toEnum i = Pitch oct (toEnum pc)
        where (oct, pc) = i `divMod` 7
    fromEnum (Pitch oct pc) = oct * 7 + fromEnum pc

data PitchClass = P1 | P2 | P3 | P4 | P5 | P6 | P7
    deriving (Show, Eq, Ord, Enum)

is4 :: Pitch -> Bool
is4 (Pitch _ P4) = True
is4 _ = False

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
