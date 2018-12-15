-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Sampler.Patch.Rambat where
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text

import qualified Sound.File.Sndfile as Sndfile
import qualified System.FilePath as FilePath
import System.FilePath ((</>))
import qualified Text.Read as Read

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.File as File
import qualified Util.File
import qualified Util.Map
import qualified Util.Num as Num
import qualified Util.Seq as Seq

import qualified Cmd.Instrument.Bali as Bali
import qualified Cmd.Instrument.ImInst as ImInst
import qualified Derive.Attrs as Attrs
import qualified Derive.C.Prelude.Note as Prelude.Note
import qualified Derive.Call as Call
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Scale.Legong as Legong

import qualified Instrument.Common as Common
import qualified Perform.Im.Patch as Im.Patch
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime

import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.Patch.Code as Code
import qualified Synth.Sampler.Patch.Util as Util
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import Global
import Synth.Types


-- Like Wayang, but support short notes.

patches :: [Patch.Patch]
patches = [make Umbang, make Isep]
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
        }
    setRange = ImInst.range Legong.rambat_range
    setTuning tuning = -- ImInst.default_scale Legong.scale_id
        ImInst.environ EnvKey.tuning (tuningVal tuning :: Text)

    tuningVal Umbang = "umbang"
    tuningVal Isep = "isep"
    code tuning = note <> Util.thru dir (convert tuning)
    note = Bali.zero_dur_mute_with ""
        (\args -> transform args . Call.multiply_dynamic 0.65)
        (\args -> transform args $
            Prelude.Note.default_note Prelude.Note.use_attributes args)
        where transform args = Code.withSymbolicPitch args . Code.withVariation
    dir = "rambat"

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
    (filenames, noteNn, sampleNn) <- tryRight $
        toFilename tuning art symPitch dyn (Note.duration note)
    return $ Sample.Sample
        { filename = Util.chooseVariation filenames note
        , offset = 0
        -- TODO duplicate from Wayang
        , envelope = if
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
        , ratio = Signal.constant $
            Sample.pitchToRatio (Pitch.nn_to_hz sampleNn) noteNn
        }

toFilename :: Tuning -> Articulation -> Either Pitch.Note Pitch.NoteNumber
    -> Dynamic -> RealTime
    -> Either Text ([Sample.SamplePath], Pitch.NoteNumber, Pitch.NoteNumber)
toFilename tuning art symPitch dyn dur = do
    (pitch, (noteNn, sampleNn)) <- findPitch tuning symPitch
    let arts = possibleArticulations tuning pitch dyn dur art
    return
        ( concatMap (filenamesOf tuning pitch dyn) arts
        , noteNn
        , sampleNn
        )

-- If the dur is under the min dur, then I can choose OpenShort in addition to
-- Open
possibleArticulations :: Tuning -> Pitch -> Dynamic -> RealTime
    -> Articulation -> [Articulation]
possibleArticulations tuning pitch dyn dur art
    | art `elem` [Open, OpenShort] = case minDurationOf (tuning, pitch, dyn) of
        Just sampleDur | AUtil.toFrame dur < sampleDur -> [Open, OpenShort]
        _ -> [Open]
    | otherwise = [art]

filenamesOf :: Tuning -> Pitch -> Dynamic -> Articulation -> [FilePath]
filenamesOf tuning pitch dyn art =
    map ((Util.showLower tuning </>) . unparseFilename pitch art dyn) [1..vars]
    where vars = variationsOf tuning pitch dyn art

-- if there's a min dur, then Open -> 1, OpenShort -> 4
-- otherwise all 4
variationsOf :: Tuning -> Pitch -> Dynamic -> Articulation -> Int
variationsOf tuning pitch dyn
    | hasShort = \case
        Open -> 1
        _ -> 4
    | otherwise = \case
        OpenShort -> 0
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
muteTime = 0.15

-- * tuning

-- TODO this is similar to Wayang.findPitch and Reyong.findPitch
findPitch :: Tuning -> Either Pitch.Note Pitch.NoteNumber
    -> Either Text (Pitch, (Pitch.NoteNumber, Pitch.NoteNumber))
findPitch tuning = either (findSymPitch tuning) (findNnPitch tuning)

findSymPitch :: Tuning -> Pitch.Note
    -> Either Text (Pitch, (Pitch.NoteNumber, Pitch.NoteNumber))
findSymPitch tuning (Pitch.Note pitch) = do
    pitch <- tryJust ("can't parse symbolic pitch: " <> pitch) $
        parsePitch (untxt pitch)
    (umbang, isep) <- tryJust ("pitch out of range: " <> pretty pitch) $
        Map.lookup pitch rambatTuning
    return $ case tuning of
        Umbang -> (pitch, (umbang, umbang))
        Isep -> (pitch, (isep, isep))

findNnPitch :: Tuning -> Pitch.NoteNumber
    -> Either Text (Pitch, (Pitch.NoteNumber, Pitch.NoteNumber))
findNnPitch tuning nn = do
    (sampleNn, pitch) <- tryJust "no pitches" $ Util.Map.lookup_closest nn $
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
    [ (51.82,   51.82)  -- 3e
    , (55.70,   55.70)
    , (56.82,   56.82)

    , (60.73,   60.73)  -- 4i
    , (62.80,   62.80)
    , (63.35,   63.35)
    , (67.70,   67.70)
    , (68.20,   68.20)

    , (72.46,   72.46)  -- 5i
    , (73.90,   73.90)
    , (75.50,   75.50)
    , (79.40,   79.40)
    , (80.50,   80.50)

    , (84.46,   84.46)  -- 6i
    ]

-- * implementation

data Tuning = Umbang | Isep deriving (Eq, Ord, Show, Enum, Bounded)

data Pitch = Pitch !Int !PitchClass
    deriving (Eq, Ord, Show)

instance Pretty Pitch where
    pretty (Pitch oct pc) = showt oct <> Util.showtLower pc

instance Enum Pitch where
    toEnum n = let (oct, pc) = n `divMod` 5 in Pitch oct (toEnum pc)
    fromEnum (Pitch oct pc) = oct * 5 + fromEnum pc

data PitchClass = I | O | E | U | A
    deriving (Eq, Ord, Show, Enum, Read)

data Articulation =
    Open | OpenShort | MuteGenderLoose | MuteGenderTight | MuteGangsa
    | Calung | MuteCalung
    deriving (Eq, Show, Enum, Bounded)

isMute :: Articulation -> Bool
isMute = \case
    MuteGenderLoose -> True
    MuteGenderTight -> True
    MuteGangsa -> True
    MuteCalung -> True
    _ -> False

data Dynamic = PP | MP | MF | FF
    deriving (Eq, Ord, Enum, Bounded, Show, Read)
instance Pretty Dynamic where pretty = showt

parseFilename :: FilePath
    -> Maybe (Pitch, Articulation, Dynamic, Util.Variation)
parseFilename fname = case Seq.split "-" (FilePath.dropExtension fname) of
    [pitch, art, dyn, 'v':var] ->
        (,,,) <$> parsePitch pitch <*> pArt art <*> pDyn dyn
            <*> Read.readMaybe var
    _ -> Nothing
    where
    pArt = (`Map.lookup` filenameToArt)
    filenameToArt = Map.fromList $ Seq.key_on articulationFilename Util.enumAll
    pDyn = Read.readMaybe . map Char.toUpper

unparseFilename :: Pitch -> Articulation -> Dynamic -> Util.Variation
    -> FilePath
unparseFilename pitch art dyn var =
    Seq.join "-"
        [ prettys pitch, articulationFilename art, Util.showLower dyn
        , 'v' : show var
        ]
        ++ ".wav"

articulationFilename :: Articulation -> String
articulationFilename = \case
    Open -> "open"
    OpenShort -> "open+short"
    MuteGenderLoose -> "mute+gender+loose"
    MuteGenderTight -> "mute+gender+tight"
    MuteGangsa -> "mute+gangsa"
    Calung -> "calung"
    MuteCalung -> "calung+mute"

parsePitch :: String -> Maybe Pitch
parsePitch [oct, pc] = Pitch <$> Num.readDigit oct
    <*> Read.readMaybe (Char.toUpper pc : "")
parsePitch _ = Nothing

getDurations :: IO [((Tuning, Pitch, Dynamic), Audio.Frame)]
getDurations = fmap group $ (++)
    <$> (mapMaybeM (get Umbang) =<< Util.File.list (dir </> "umbang"))
    <*> (mapMaybeM (get Isep) =<< Util.File.list (dir </> "isep"))
    where
    dir = Config.unsafeSamplerRoot </> "rambat"
    group = map (second minimum) . Seq.group_fst
    get tuning fname = case parseFilename (FilePath.takeFileName fname) of
        Just (pitch, OpenShort, dyn, _var) -> do
            frames <- Audio.Frame . Sndfile.frames <$> File.getInfo fname
            return $ Just ((tuning, pitch, dyn), frames)
        _ -> pure Nothing

makeDurationOf :: IO ()
makeDurationOf = do
    durs <- getDurations
    putStrLn "-- Generated by 'makeDurationOf'."
    putStrLn "minDurationOf :: (Tuning, Pitch, Dynamic) -> Maybe Audio.Frame"
    putStrLn "minDurationOf = \\case"
    forM_ durs $ \(pattern, Audio.Frame dur) ->
        putStrLn $ "    " <> show pattern <> " -> Just " <> show dur
    putStrLn "    _ -> Nothing"

-- * generated

-- Generated by 'makeDurationOf'.
minDurationOf :: (Tuning, Pitch, Dynamic) -> Maybe Audio.Frame
minDurationOf = \case
    (Umbang,Pitch 4 U,PP) -> Just 122359
    (Umbang,Pitch 4 U,MP) -> Just 96984
    (Umbang,Pitch 4 U,MF) -> Just 107173
    (Umbang,Pitch 4 U,FF) -> Just 106701
    (Umbang,Pitch 4 A,PP) -> Just 107694
    (Umbang,Pitch 4 A,MP) -> Just 140221
    (Umbang,Pitch 4 A,MF) -> Just 130662
    (Umbang,Pitch 4 A,FF) -> Just 132779
    (Umbang,Pitch 5 I,PP) -> Just 128127
    (Umbang,Pitch 5 I,MP) -> Just 123146
    (Umbang,Pitch 5 I,MF) -> Just 142818
    (Umbang,Pitch 5 I,FF) -> Just 161603
    (Umbang,Pitch 5 O,PP) -> Just 128721
    (Umbang,Pitch 5 O,MP) -> Just 134433
    (Umbang,Pitch 5 O,MF) -> Just 149314
    (Umbang,Pitch 5 O,FF) -> Just 154568
    (Umbang,Pitch 5 E,PP) -> Just 168311
    (Umbang,Pitch 5 E,MP) -> Just 139043
    (Umbang,Pitch 5 E,MF) -> Just 145909
    (Umbang,Pitch 5 E,FF) -> Just 158271
    (Umbang,Pitch 5 U,PP) -> Just 132904
    (Umbang,Pitch 5 U,MP) -> Just 159385
    (Umbang,Pitch 5 U,MF) -> Just 135743
    (Umbang,Pitch 5 U,FF) -> Just 155615
    (Umbang,Pitch 5 A,PP) -> Just 114223
    (Umbang,Pitch 5 A,MP) -> Just 150813
    (Umbang,Pitch 5 A,MF) -> Just 156019
    (Umbang,Pitch 5 A,FF) -> Just 153980
    (Umbang,Pitch 6 I,PP) -> Just 173254
    (Umbang,Pitch 6 I,MP) -> Just 141612
    (Umbang,Pitch 6 I,MF) -> Just 134253
    (Umbang,Pitch 6 I,FF) -> Just 133927
    _ -> Nothing
