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

import qualified Cmd.Instrument.Bali as Bali
import qualified Cmd.Instrument.ImInst as ImInst
import qualified Derive.Attrs as Attrs
import qualified Derive.C.Prelude.Note as Prelude.Note
import qualified Derive.Call as Call
import qualified Derive.Instrument.DUtil as DUtil
import qualified Derive.Scale as Scale

import qualified Instrument.Common as Common
import qualified Perform.Im.Patch as Im.Patch
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime

import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.Patch.Lib.Code as Code
import qualified Synth.Sampler.Patch.Lib.Prepare as Prepare
import qualified Synth.Sampler.Patch.Lib.Util as Util
import           Synth.Sampler.Patch.Lib.Util (Dynamic(..))
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import           Global
import           Synth.Types


sampleFormat :: Util.SampleFormat
sampleFormat = Util.Wav

patches :: [Patch.Patch]
patches = map makePatch [slenthem, peking]

data Instrument = Instrument {
    name :: Text
    , variations :: Variations
    , tuning :: Tuning
    , dynamic :: Dynamic -> (Util.DynVal, (Util.DB, Util.DB))
    }

type Tuning = Map Pitch Pitch.NoteNumber
type Variations = Articulation -> (Pitch, Util.Dynamic) -> Util.Variation

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
    , dynamic = dynamicRange
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
    , dynamic = dynamicRange
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

-- TODO also need per-sample tweaks
dynamicRange :: Dynamic -> (Util.DynVal, (Util.DB, Util.DB))
dynamicRange = \case
    PP -> (0.25, (-8, 4))
    MP -> (0.5, (-4, 4))
    MF -> (0.75, (-4, 6))
    FF -> (1, (-5, 4))

makePatch :: Instrument -> Patch.Patch
makePatch inst@(Instrument { name, tuning }) = (Patch.patch name)
    { Patch._dir = dir
    , Patch._convert = convert inst attributeMap
    , Patch._karyaPatch =
        ImInst.code #= code $
        ImInst.range (makeRange tuning) $
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
    , Patch._allFilenames = Set.fromList $ allFilenames inst
    }
    where
    dir = "java" </> untxt name
    -- TODO copy paste with Rambat
    code = note
        <> Util.thru dir (convert inst attributeMap)
        <> ImInst.postproc DUtil.with_symbolic_pitch
    note = Bali.zero_dur_mute_with ""
        (\_args -> transform . Call.multiply_dynamic 0.65)
        (\args -> transform $
            Prelude.Note.default_note Prelude.Note.use_attributes args)
        where transform = Code.withVariation

    -- TODO filter out the ones I don't have
    attributeMap = Common.attribute_map
        [ (Attrs.mute, Mute)
        , (Attrs.attr "character", Character)
        , (mempty, Open)
        ]

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
convert (Instrument { tuning, variations, dynamic }) attrMap note = do
    let art = Util.articulationDefault Open attrMap $
            Note.attributes note
    let (dyn, dynVal) = Util.dynamic dynamic note
    symPitch <- Util.symbolicPitch note
    let variableMute = RealTime.seconds $ Note.initial0 Control.mute note
    (pitch, (noteNn, sampleNn)) <- tryRight $ findPitch tuning symPitch
    let filenames = findFilenames variations art pitch dyn
    return $ (Sample.make (Util.chooseVariation filenames note))
        -- TODO duplicate from Rambat, part of variable mute
        { Sample.envelope = if
            | art == Mute -> Signal.constant dynVal
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

variableMuteRange :: (RealTime, RealTime)
variableMuteRange = (0.85, 4)

-- | Time to mute at the end of a note.
muteTime :: RealTime
muteTime = 0.35

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
    -> FilePath
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
