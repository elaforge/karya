-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE StrictData #-}
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
import qualified Derive.Scale as Scale
import qualified Instrument.Common as Common
import qualified Perform.Im.Patch as Im.Patch
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime

import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.Patch.Lib.Util as Util
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import           Global
import           Synth.Types


sampleFormat :: Util.SampleFormat
sampleFormat = Util.Wav

patches :: [Patch.Patch]
patches =
    [ slenthem
    ]

slenthem :: Patch.Patch
slenthem = (Patch.patch "slenthem")
    { Patch._dir = "java/slenthem"
    , Patch._convert = convert
    , Patch._karyaPatch = ImInst.range slenthemRange $
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
    , Patch._allFilenames = allFilenames
    }

-- TODO parameterize because slenthem is just one instrument

-- TODO get it from Scale.Java because it also informs the relative pitches
slenthemRange :: Scale.Range
slenthemRange = Scale.Range (Pitch.pitch 2 0) (Pitch.pitch 2 6)
-- slenthemRange = Scale.Range (toPitch bottom) (toPitch top)
--     where
--     Just bottom = fst . fst <$> Map.minViewWithKey slenthemTuning
--     Just top = fst . fst <$> Map.maxViewWithKey slenthemTuning

data Articulation = Mute | Open
    deriving (Show, Eq, Ord, Enum, Bounded)

attributeMap :: Common.AttributeMap Articulation
attributeMap = Common.attribute_map
    [ (Attrs.mute, Mute)
    , (mempty, Open)
    ]

convert :: Note.Note -> Patch.ConvertM Sample.Sample
convert note = do
    let art = Util.articulationDefault Open attributeMap $
            Note.attributes note
    let (dyn, dynVal) = Util.dynamic dynamicRange 0 note
    symPitch <- Util.symbolicPitch note
    let variableMute = RealTime.seconds $ Note.initial0 Control.mute note
    (pitch, (noteNn, sampleNn)) <- tryRight $ findPitch symPitch
    let filenames = toFilenames art pitch dyn
    return $ (Sample.make (Util.chooseVariation filenames note))
        -- TODO duplicate from Rambat
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
findPitch :: Either Pitch.Note Pitch.NoteNumber
    -> Either Text (Pitch, (Pitch.NoteNumber, Pitch.NoteNumber))
findPitch = either findSymPitch findNnPitch

findSymPitch :: Pitch.Note
    -> Either Text (Pitch, (Pitch.NoteNumber, Pitch.NoteNumber))
findSymPitch (Pitch.Note pitch) = do
    pitch <- tryJust ("can't parse symbolic pitch: " <> pitch) $
        parsePitch pitch
    -- Symbolic pitch doesn't actually need the nn, but can at least check
    -- range.
    nn <- tryJust ("pitch out of range: " <> txt (showPitch pitch)) $
        Map.lookup pitch slenthemTuning
    return (pitch, (nn, nn))

findNnPitch :: Pitch.NoteNumber
    -> Either Text (Pitch, (Pitch.NoteNumber, Pitch.NoteNumber))
findNnPitch nn = do
    (sampleNn, pitch) <- tryJust "no pitches" $
        Maps.lookupClosest nn nnToPitch
    return (pitch, (nn, sampleNn))
    where
    nnToPitch = Maps.invert slenthemTuning

-- | Find variation samples: {open,mute}/2{1..7}-{pp,mp,mf,ff}-v{1..n}
toFilenames :: Articulation -> Pitch -> Util.Dynamic -> [Sample.SamplePath]
toFilenames art pitch dyn =
    map (unparseFilename pitch art dyn) [1 .. variationsOf art (pitch, dyn)]

allFilenames :: Set FilePath
allFilenames = Util.assertLength 278 $ Set.fromList
    [ fname
    | art <- Util.enumAll
    , pitch <- Map.keys slenthemTuning
    , dyn <- Util.enumAll
    , fname <- toFilenames art pitch dyn
    ]

variationsOf :: Articulation -> (Pitch, Util.Dynamic) -> Util.Variation
variationsOf Open = \case
    (Pitch 2 P6, Util.PP) -> 3
    (Pitch 2 P6, Util.MF) -> 5
    (Pitch 2 P7, Util.FF) -> 3
    _ -> 4
variationsOf Mute = \case
    (Pitch 2 P5, Util.PP) -> 5
    _ -> 6

unparseFilename :: Pitch -> Articulation -> Util.Dynamic -> Util.Variation
    -> FilePath
unparseFilename pitch art dyn var =
    articulationDir art
        </> Lists.join "-" [showPitch pitch, Util.showLower dyn, 'v' : show var]
        ++ Util.extension sampleFormat

slenthemTuning :: Map Pitch Pitch.NoteNumber
slenthemTuning = Map.fromList $ zip (map (Pitch 2) [P1 ..])
    [ 50.18 -- 21
    , 51.65 -- 22
    , 53    -- 23
    , 56    -- 24
    , 57.05 -- 25
    , 58.68 -- 26 6.. (as3 + 0.5)
    , 60.13 -- 27 7.. (c4)
    ]
    -- TODO copy pasted from Scale.Java

dynamicRange :: Util.Dynamic -> (Int, Int)
dynamicRange = \case
    Util.PP -> (1, 31)
    Util.MP -> (32, 64)
    Util.MF -> (65, 108)
    Util.FF -> (109, 127)

articulationDir :: Articulation -> String
articulationDir = \case
    Mute -> "mute"
    Open -> "open"

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
