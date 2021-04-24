-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Sampler.Patch.Rambat where
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Sound.File.Sndfile as Sndfile
import qualified System.FilePath as FilePath
import           System.FilePath ((</>))
import qualified Text.Read as Read

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.File as File
import qualified Util.File
import qualified Util.Maps as Maps
import qualified Util.Num as Num
import qualified Util.Seq as Seq

import qualified Cmd.Instrument.Bali as Bali
import qualified Cmd.Instrument.ImInst as ImInst
import qualified Derive.Attrs as Attrs
import qualified Derive.C.Prelude.Note as Prelude.Note
import qualified Derive.Call as Call
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Instrument.DUtil as DUtil
import           Derive.Scale.BaliScales (Tuning(..))
import qualified Derive.Scale.Legong as Legong

import qualified Instrument.Common as Common
import qualified Perform.Im.Patch as Im.Patch
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime

import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Sampler.Calibrate as Calibrate
import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.Patch.Lib.Bali as Lib.Bali
import           Synth.Sampler.Patch.Lib.Bali (Pitch(..), PitchClass(..))
import qualified Synth.Sampler.Patch.Lib.Code as Code
import qualified Synth.Sampler.Patch.Lib.Util as Util
import           Synth.Sampler.Patch.Lib.Util (Dynamic(..))
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import           Global
import           Synth.Types


-- Like Wayang, but support short notes.

patches :: [Patch.DbPatch]
patches = map Patch.DbPatch [make Umbang, make Isep]
    where
    make tuning =
        (Patch.patch $ Text.intercalate "-" ["rambat", Util.showtLower tuning])
        { Patch._dir = dir
        , Patch._convert = convert tuning
        , Patch._karyaPatch = ImInst.code #= code tuning $
            setRange $ setTuning tuning $
            ImInst.make_patch $ Im.Patch.patch
                { Im.Patch.patch_controls = mconcat
                    [ Control.supportPitch
                    , Control.supportDyn
                    , Control.supportVariation
                    , Map.singleton Control.mute
                        "Amount of mute. This becomes a shortened envelope."
                    ]
                , Im.Patch.patch_attribute_map = const () <$> attributeMap
                }
        , Patch._allFilenames = allFilenames tuning
        }
        where
        dir = "rambat" </> Util.showLower tuning
        code tuning = note
            <> Util.thru dir (convert tuning)
            <> ImInst.postproc DUtil.with_symbolic_pitch
    setRange = ImInst.range Legong.rambat_range
    setTuning tuning = -- ImInst.default_scale Legong.scale_id
        ImInst.environ EnvKey.tuning (tuningVal tuning :: Text)

    tuningVal Umbang = "umbang"
    tuningVal Isep = "isep"
    note = Bali.zero_dur_mute_with ""
        (\_args -> transform . Call.multiply_dynamic 0.65)
        (\args -> transform $
            Prelude.Note.default_note Prelude.Note.use_attributes args)
        where transform = Code.withVariation

attributeMap :: Common.AttributeMap Articulation
attributeMap = Common.attribute_map
    [ (calung <> mute, MuteCalung)
    , (calung, Calung)
    , (mute <> gangsa, MuteGangsa)
    , (mute <> Attrs.tight, MuteGenderTight)
    , (mute <> Attrs.loose, MuteGenderLoose)
    , (mute, MuteGenderLoose)
    , (mempty, Open)
    -- OpenShort is intentionally not in here, since it's supposed to be an
    -- implementation detail.
    ]
    where
    mute = Attrs.mute
    calung = Attrs.attr "calung"
    gangsa = Attrs.attr "gangsa"

convert :: Tuning -> Note.Note -> Patch.ConvertM Sample.Sample
convert tuning note = do
    let art = Util.articulationDefault Open attributeMap $ Note.attributes note
    let (dyn, dynVal) = Util.dynamic dynamicRange minDyn note
    symPitch <- Util.symbolicPitch note
    let variableMute = RealTime.seconds $ Note.initial0 Control.mute note
    (pitch, (noteNn, sampleNn)) <- tryRight $ findPitch tuning symPitch
    let filenames = toFilenames tuning art pitch dyn (Note.duration note)
    dynVal <- return $ dynVal * tweakDynamic tuning art pitch dyn
    return $ (Sample.make (Util.chooseVariation filenames note))
        -- TODO duplicate from Wayang
        { Sample.envelope = if
            | isMute art -> Signal.constant dynVal
            | variableMute > 0 -> Signal.from_pairs
                [ (Note.start note, dynVal)
                , (Note.start note
                    + uncurry Num.scale variableMuteRange (1-variableMute), 0)
                ]
            | otherwise -> Signal.from_pairs
                [ (Note.start note, dynVal), (Note.end note, dynVal)
                , (Note.end note + muteTime, 0)
                ]
        , Sample.ratios = Signal.constant $ Sample.pitchToRatio sampleNn noteNn
        }

toFilenames :: Tuning -> Articulation -> Pitch -> Dynamic -> RealTime
    -> [Sample.SamplePath]
toFilenames tuning art pitch dyn dur =
    concatMap (filenamesOf tuning pitch dyn) $
        possibleArticulations tuning pitch dyn dur art

allFilenames :: Tuning -> Set FilePath
allFilenames tuning = Util.assertLength (len tuning) $ Set.fromList
    [ fname
    | art <- Util.enumAll
    , pitch <- Map.keys rambatTuning
    , dyn <- Util.enumAll
    , fname <- toFilenames tuning art pitch dyn 1
        -- dur is irrelevant because I'm already picking OpenShort explicitly
    ]
    where
    len Umbang = 1319
    len Isep = 1351

-- | Reign in some samples that stick out.
tweakDynamic :: Tuning -> Articulation -> Pitch -> Dynamic -> Signal.Y
tweakDynamic Umbang Calung (Pitch 3 A) dyn
    | dyn <= MP = 0.75
    | dyn <= MF = 0.85
    | otherwise = 0.9
tweakDynamic _ _ _ _ = 1

-- If the dur is under the min dur, then I can choose OpenShort in addition to
-- Open.
possibleArticulations :: Tuning -> Pitch -> Dynamic -> RealTime
    -> Articulation -> [Articulation]
possibleArticulations tuning pitch dyn dur art
    | art `elem` [Open, OpenShort] = case minDurationOf (tuning, pitch, dyn) of
        Just sampleDur | AUtil.toFrames dur < sampleDur -> [Open, OpenShort]
        _ -> [Open]
    | otherwise = [art]

filenamesOf :: Tuning -> Pitch -> Dynamic -> Articulation -> [FilePath]
filenamesOf tuning pitch dyn art = map (unparseFilename pitch art dyn) [1..vars]
    where vars = variationsOf tuning pitch dyn art

-- if there's a min dur, then Open -> 1, OpenShort -> 4
-- otherwise all 4
variationsOf :: Tuning -> Pitch -> Dynamic -> Articulation -> Int
variationsOf tuning pitch dyn art = case art of
    Open -> if hasShort then 1 else 4
    OpenShort -> if hasShort then 4 else 0
    Calung -> case tuning of
        -- I started with 4 open vars.
        Isep | pitch `elem` [Pitch 3 E, Pitch 3 U] -> 4
        -- var3 sample has a click and clunk.
        Umbang | pitch == Pitch 3 A, dyn == PP -> 2
        _ -> 3
    MuteCalung -> case tuning of
        -- Forgot this sample.
        Isep | pitch == Pitch 4 I && dyn == FF -> 3
        _ -> 4
    _ -> 4
    where
    hasShort = Maybe.isJust $ minDurationOf (tuning, pitch, dyn)

-- ** envelope, TODO same as wayang

dynamicRange :: Dynamic -> (Int, Int)
dynamicRange = \case
    PP -> (1, 31)
    MP -> (32, 64)
    MF -> (65, 108)
    FF -> (109, 127)

-- | The samples are normalized, so it just scales by Control.dynamic, where
-- 0 gets this value.
minDyn :: Signal.Y
minDyn = 0.5

variableMuteRange :: (RealTime, RealTime)
variableMuteRange = (0.85, 4)

-- | Time to mute at the end of a note.
muteTime :: RealTime
muteTime = 0.35

-- * tuning

-- TODO this is similar to Wayang.findPitch and Reyong.findPitch
findPitch :: Tuning -> Either Pitch.Note Pitch.NoteNumber
    -> Either Text (Pitch, (Pitch.NoteNumber, Pitch.NoteNumber))
findPitch tuning = either (findSymPitch tuning) (findNnPitch tuning)

findSymPitch :: Tuning -> Pitch.Note
    -> Either Text (Pitch, (Pitch.NoteNumber, Pitch.NoteNumber))
findSymPitch tuning (Pitch.Note pitch) = do
    pitch <- tryJust ("can't parse symbolic pitch: " <> pitch) $
        Lib.Bali.parsePitch (untxt pitch)
    (umbang, isep) <- tryJust ("pitch out of range: " <> pretty pitch) $
        Map.lookup pitch rambatTuning
    return $ case tuning of
        Umbang -> (pitch, (umbang, umbang))
        Isep -> (pitch, (isep, isep))

findNnPitch :: Tuning -> Pitch.NoteNumber
    -> Either Text (Pitch, (Pitch.NoteNumber, Pitch.NoteNumber))
findNnPitch tuning nn = do
    (sampleNn, pitch) <- tryJust "no pitches" $ Maps.lookupClosest nn $
        case tuning of
            Umbang -> umbangs
            Isep -> iseps
    return (pitch, (nn, sampleNn))
    where
    umbangs = Map.fromList
        [(umbang, p) | (p, (umbang, _)) <- Map.toList rambatTuning]
    iseps = Map.fromList
        [(isep, p) | (p, (_, isep)) <- Map.toList rambatTuning]

rambatTuning :: Map Pitch (Pitch.NoteNumber, Pitch.NoteNumber)
rambatTuning = Map.fromList $ zip [Pitch 3 E ..]
    [ (51.03,   51.85)  -- 3e
    , (55.05,   55.67)
    , (56.10,   56.50)

    , (59.91,   60.40)  -- 4i
    , (61.80,   62.41)
    , (62.90,   63.27)
    , (67.15,   67.47)
    , (68.06,   68.33)
    , (71.88,   72.15)  -- 5i
    , (73.60,   73.80)
    , (75.13,   75.38)
    , (79.12,   79.28)
    , (80.27,   80.26)
    , (84.09,   84.24)  -- 6i
    ]

-- * implementation

data Articulation =
    Open | OpenShort | MuteGenderLoose | MuteGenderTight | MuteGangsa
    | Calung | MuteCalung
    deriving (Eq, Ord, Show, Enum, Bounded)

isMute :: Articulation -> Bool
isMute = \case
    MuteGenderLoose -> True
    MuteGenderTight -> True
    MuteGangsa -> True
    MuteCalung -> True
    _ -> False

parseFilename :: FilePath
    -> Maybe (Pitch, Articulation, Dynamic, Util.Variation)
parseFilename fname = case Seq.split "-" (FilePath.dropExtension fname) of
    [pitch, art, dyn, 'v':var] ->
        (,,,) <$> Lib.Bali.parsePitch pitch <*> pArt art <*> pDyn dyn
            <*> Read.readMaybe var
    _ -> Nothing
    where
    pArt = (`Map.lookup` filenameToArt)
    filenameToArt = Map.fromList $ Seq.key_on articulationFilename Util.enumAll
    pDyn = Read.readMaybe . map Char.toUpper

unparseFilename :: Pitch -> Articulation -> Dynamic -> Util.Variation
    -> FilePath
unparseFilename pitch art dyn var = Seq.join "-"
    [ prettys pitch, articulationFilename art, Util.showLower dyn
    , 'v' : show var
    ]
    ++ ".flac"

articulationFilename :: Articulation -> String
articulationFilename = \case
    Open -> "open"
    OpenShort -> "open+short"
    MuteGenderLoose -> "mute+gender+loose"
    MuteGenderTight -> "mute+gender+tight"
    MuteGangsa -> "mute+gangsa"
    Calung -> "calung"
    MuteCalung -> "calung+mute"

-- * util

getVariations :: IO [(FilePath, Map Calibrate.Axis Text)]
getVariations = do
    samples <- (++)
        <$> (mapMaybe (get Umbang) <$> Util.File.list (dir </> "umbang"))
        <*> (mapMaybe (get Isep) <$> Util.File.list (dir </> "isep"))
    return
        [ ( dir </> tuningDir tuning </> unparseFilename pitch art dyn var
          , Map.fromList
            [ (Calibrate.tuning, showt tuning), (Calibrate.pitch, pretty pitch)
            , (Calibrate.art, showt art), (Calibrate.dyn, showt dyn)
            , (Calibrate.var, showt var)
            ]
          )
        | (tuning, (pitch, art, dyn, var)) <- List.sort samples
        ]
    where
    tuningDir Umbang = "umbang"
    tuningDir Isep = "isep"
    dir = Config.unsafeSamplerRoot </> "rambat"
    get tuning fname = (tuning,) <$> parseFilename (FilePath.takeFileName fname)

getDurations :: IO [((Tuning, Pitch, Dynamic), Audio.Frames)]
getDurations = fmap group $ (++)
    <$> (mapMaybeM (get Umbang) =<< Util.File.list (dir </> "umbang"))
    <*> (mapMaybeM (get Isep) =<< Util.File.list (dir </> "isep"))
    where
    dir = Config.unsafeSamplerRoot </> "rambat"
    group = map (second minimum) . Seq.group_fst
    get tuning fname = case parseFilename (FilePath.takeFileName fname) of
        Just (pitch, OpenShort, dyn, _var) -> do
            frames <- Audio.Frames . Sndfile.frames <$>
                (File.throwEnoent fname =<< File.getInfo fname)
            return $ Just ((tuning, pitch, dyn), frames)
        _ -> pure Nothing

makeDurationOf :: IO ()
makeDurationOf = do
    durs <- getDurations
    putStrLn "-- Generated by 'makeDurationOf'."
    putStrLn "minDurationOf :: (Tuning, Pitch, Dynamic) -> Maybe Audio.Frames"
    putStrLn "minDurationOf = \\case"
    forM_ durs $ \(pattern, Audio.Frames dur) ->
        putStrLn $ "    " <> show pattern <> " -> Just " <> show dur
    putStrLn "    _ -> Nothing"

-- * generated

-- Generated by 'makeDurationOf'.
minDurationOf :: (Tuning, Pitch, Dynamic) -> Maybe Audio.Frames
minDurationOf = \case
    (Umbang,Pitch 4 U,PP) -> Just 112417
    (Umbang,Pitch 4 U,MP) -> Just 89104
    (Umbang,Pitch 4 U,MF) -> Just 98465
    (Umbang,Pitch 4 U,FF) -> Just 98032
    (Umbang,Pitch 4 A,PP) -> Just 98944
    (Umbang,Pitch 4 A,MP) -> Just 128828
    (Umbang,Pitch 4 A,MF) -> Just 120046
    (Umbang,Pitch 4 A,FF) -> Just 121991
    (Umbang,Pitch 5 I,PP) -> Just 117717
    (Umbang,Pitch 5 I,MP) -> Just 113140
    (Umbang,Pitch 5 I,MF) -> Just 131214
    (Umbang,Pitch 5 I,FF) -> Just 148473
    (Umbang,Pitch 5 O,PP) -> Just 118262
    (Umbang,Pitch 5 O,MP) -> Just 123510
    (Umbang,Pitch 5 O,MF) -> Just 137182
    (Umbang,Pitch 5 O,FF) -> Just 142009
    (Umbang,Pitch 5 E,PP) -> Just 154636
    (Umbang,Pitch 5 E,MP) -> Just 127746
    (Umbang,Pitch 5 E,MF) -> Just 134054
    (Umbang,Pitch 5 E,FF) -> Just 145411
    (Umbang,Pitch 5 U,PP) -> Just 122106
    (Umbang,Pitch 5 U,MP) -> Just 146435
    (Umbang,Pitch 5 U,MF) -> Just 124714
    (Umbang,Pitch 5 U,FF) -> Just 142971
    (Umbang,Pitch 5 A,PP) -> Just 104942
    (Umbang,Pitch 5 A,MP) -> Just 138559
    (Umbang,Pitch 5 A,MF) -> Just 143342
    (Umbang,Pitch 5 A,FF) -> Just 132916
    (Umbang,Pitch 6 I,PP) -> Just 159177
    (Umbang,Pitch 6 I,MP) -> Just 130106
    (Umbang,Pitch 6 I,MF) -> Just 123345
    (Umbang,Pitch 6 I,FF) -> Just 123045
    (Isep,Pitch 3 E,PP) -> Just 162080
    (Isep,Pitch 3 E,MP) -> Just 159529
    (Isep,Pitch 3 E,MF) -> Just 158300
    (Isep,Pitch 3 E,FF) -> Just 146243
    (Isep,Pitch 3 U,PP) -> Just 153505
    (Isep,Pitch 3 U,MP) -> Just 173756
    (Isep,Pitch 3 U,MF) -> Just 142989
    (Isep,Pitch 3 U,FF) -> Just 144636
    (Isep,Pitch 3 A,PP) -> Just 144275
    (Isep,Pitch 3 A,MP) -> Just 162957
    (Isep,Pitch 3 A,MF) -> Just 172313
    (Isep,Pitch 3 A,FF) -> Just 177877
    (Isep,Pitch 4 I,PP) -> Just 133248
    (Isep,Pitch 4 I,MP) -> Just 163591
    (Isep,Pitch 4 I,MF) -> Just 193262
    (Isep,Pitch 4 I,FF) -> Just 171032
    (Isep,Pitch 4 O,PP) -> Just 129181
    (Isep,Pitch 4 O,MP) -> Just 181993
    (Isep,Pitch 4 O,MF) -> Just 169347
    (Isep,Pitch 4 O,FF) -> Just 182898
    (Isep,Pitch 4 E,PP) -> Just 133960
    (Isep,Pitch 4 E,MP) -> Just 173716
    (Isep,Pitch 4 E,MF) -> Just 161932
    (Isep,Pitch 4 E,FF) -> Just 173629
    (Isep,Pitch 4 U,PP) -> Just 138339
    (Isep,Pitch 4 U,MP) -> Just 164364
    (Isep,Pitch 4 U,MF) -> Just 146833
    (Isep,Pitch 4 U,FF) -> Just 141610
    (Isep,Pitch 4 A,PP) -> Just 162173
    (Isep,Pitch 4 A,MP) -> Just 167837
    (Isep,Pitch 4 A,MF) -> Just 161727
    (Isep,Pitch 4 A,FF) -> Just 178778
    (Isep,Pitch 5 I,PP) -> Just 168970
    (Isep,Pitch 5 I,MP) -> Just 164306
    (Isep,Pitch 5 I,MF) -> Just 158971
    (Isep,Pitch 5 I,FF) -> Just 160404
    (Isep,Pitch 5 O,PP) -> Just 142237
    (Isep,Pitch 5 O,MP) -> Just 164023
    (Isep,Pitch 5 O,MF) -> Just 161175
    (Isep,Pitch 5 O,FF) -> Just 159142
    (Isep,Pitch 5 E,PP) -> Just 138153
    (Isep,Pitch 5 E,MP) -> Just 164070
    (Isep,Pitch 5 E,MF) -> Just 166684
    (Isep,Pitch 5 E,FF) -> Just 156899
    (Isep,Pitch 5 U,PP) -> Just 156653
    (Isep,Pitch 5 U,MP) -> Just 124361
    (Isep,Pitch 5 U,MF) -> Just 147572
    (Isep,Pitch 5 U,FF) -> Just 176584
    (Isep,Pitch 5 A,PP) -> Just 143279
    (Isep,Pitch 5 A,MP) -> Just 160463
    (Isep,Pitch 5 A,MF) -> Just 106814
    (Isep,Pitch 5 A,FF) -> Just 142218
    (Isep,Pitch 6 I,PP) -> Just 122746
    (Isep,Pitch 6 I,MP) -> Just 144354
    (Isep,Pitch 6 I,MF) -> Just 185924
    (Isep,Pitch 6 I,FF) -> Just 155587
    _ -> Nothing
