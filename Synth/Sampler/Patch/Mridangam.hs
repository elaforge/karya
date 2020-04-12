-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Sampler.Patch.Mridangam where
import           System.FilePath ((</>))

import qualified Util.Num as Num
import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.ImInst as ImInst
import qualified Cmd.Instrument.Mridangam as Mridangam

import qualified Derive.Attrs as Attrs
import qualified Instrument.Common as Common
import qualified Perform.Im.Patch as Im.Patch
import qualified Perform.Pitch as Pitch
import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.Patch.Lib.Code as Code
import qualified Synth.Sampler.Patch.Lib.Drum as Drum
import qualified Synth.Sampler.Patch.Lib.Util as Util
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import           Global
import           Synth.Types


patches :: [Patch.DbPatch]
patches = (:[]) $ Patch.DbPatch $ (Patch.patch patchName)
    { Patch._dir = dir
    , Patch._convert = convert
    , Patch._preprocess = Drum.inferDuration strokeMap
    , Patch._karyaPatch = CUtil.im_drum_patch (Drum._strokes strokeMap) $
        ImInst.code #= code $
        ImInst.make_patch $ Im.Patch.patch
            { Im.Patch.patch_controls = mconcat
                [ Control.supportPitch
                , Control.supportDyn
                , Control.supportVariation
                ]
            , Im.Patch.patch_attribute_map = const () <$> attributeMap
            }
    }
    where
    code = Mridangam.code (Util.imThruFunction dir convert) naturalNn
        (Just $ \_ -> Code.withVariationNormal 1)
    dir = untxt patchName

patchName :: Text
patchName = "mridangam-d"

strokeMap :: Drum.StrokeMap Articulation
strokeMap = Drum.strokeMap Mridangam.stops Mridangam.all_notes attributeMap

attributeMap :: Common.AttributeMap Articulation
attributeMap = Common.attribute_map
    [ (Mridangam.tha, Tha)
    , (Mridangam.thom, Thom)
    , (Mridangam.gumki, Gumki)
    , (Mridangam.gumki <> Attrs.up, GumkiUp)
    , (Mridangam.ki, Ki)
    , (Mridangam.ta, Ta)
    , (Mridangam.nam, Nam)
    , (Mridangam.din, Din)
    , (Mridangam.chapu, Chapu)
    , (Mridangam.dheem, Dheem)
    , (Mridangam.kin, Kin)
    , (Mridangam.tan, Tan)
    ]

convert :: Note.Note -> Patch.ConvertM Sample.Sample
convert note = do
    articulation <- Util.articulation attributeMap (Note.attributes note)
    let dynVal = Note.initial0 Control.dynamic note
    let var = maybe 0 (subtract 1 . (*2)) $ Note.initial Control.variation note
    let filename = show articulation
            </> Util.pickDynamicVariation variationRange
                (articulationSamples articulation) dynVal var
    noteNn <- Util.initialPitch note
    let noteDyn = Num.scale minDyn maxDyn dynVal
    return $ (Sample.make filename)
        { Sample.envelope = Signal.from_pairs
            [ (Note.start note, noteDyn), (Note.end note, noteDyn)
            , (Note.end note + muteTime, 0)
            ]
        , Sample.ratios = Signal.constant $ Sample.pitchToRatio naturalNn noteNn
        }

-- | A note may pick a sample of this much dyn difference on either side.
variationRange :: Signal.Y
variationRange = 0.15

minDyn :: Signal.Y
minDyn = 0.4

maxDyn :: Signal.Y
maxDyn = 1.15

naturalNn :: Pitch.NoteNumber
naturalNn = 62.1

-- | Time to mute at the end of a note.
muteTime :: RealTime
muteTime = 0.05

data Articulation = Tha | Thom | Gumki | GumkiUp
    | Ki | Ta | Nam | Din | Chapu | Dheem
    | Kin | Tan
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | Generate 'articulationSamples'.
makeArticulationSamples :: IO ()
makeArticulationSamples = Drum.makeFileList (untxt patchName)
    (map show (Util.enumAll :: [Articulation])) "articulationSamples"

articulationSamples :: Articulation -> [FilePath]
articulationSamples = \case
    Tha ->
        [ "12-18-23-0-3-tha.wav"
        , "12-18-23-4-7-tha.wav"
        , "12-18-23-8-11-tha.wav"
        , "12-18-23-12-15-tha.wav"
        , "12-18-23-16-19-tha.wav"
        , "12-18-23-20-23-tha.wav"
        , "12-18-23-24-27-tha.wav"
        , "12-18-23-28-31-tha.wav"
        , "12-18-23-32-35-tha.wav"
        , "12-18-23-36-39-tha.wav"
        , "12-18-23-40-43-tha.wav"
        , "12-18-23-44-47-tha.wav"
        , "12-18-23-48-51-tha.wav"
        , "12-18-23-52-55-tha.wav"
        , "12-18-23-56-59-tha.wav"
        , "12-18-23-60-63-tha.wav"
        , "12-18-23-64-67-tha.wav"
        , "12-18-23-68-71-tha.wav"
        , "12-18-23-72-75-tha.wav"
        , "12-18-23-76-79-tha.wav"
        , "12-18-23-80-83-tha.wav"
        , "12-18-23-84-87-tha.wav"
        , "12-18-23-88-91-tha.wav"
        , "12-18-23-92-95-tha.wav"
        , "12-18-23-96-99-tha.wav"
        , "12-18-23-100-103-tha.wav"
        , "12-18-23-104-107-tha.wav"
        , "12-18-23-108-111-tha.wav"
        , "12-18-23-112-115-tha.wav"
        , "12-18-23-116-119-tha.wav"
        , "12-18-23-120-123-tha.wav"
        , "12-18-23-124-127-tha.wav"
        ]
    Thom ->
        [ "24-30-35-0-4-thom.wav"
        , "24-30-35-5-9-thom.wav"
        , "24-30-35-10-14-thom.wav"
        , "24-30-35-15-19-thom.wav"
        , "24-30-35-20-24-thom.wav"
        , "24-30-35-25-29-thom.wav"
        , "24-30-35-30-34-thom.wav"
        , "24-30-35-35-39-thom.wav"
        , "24-30-35-40-44-thom.wav"
        , "24-30-35-45-49-thom.wav"
        , "24-30-35-50-54-thom.wav"
        , "24-30-35-55-59-thom.wav"
        , "24-30-35-60-64-thom.wav"
        , "24-30-35-65-69-thom.wav"
        , "24-30-35-70-74-thom.wav"
        , "24-30-35-75-79-thom.wav"
        , "24-30-35-80-84-thom.wav"
        , "24-30-35-85-89-thom.wav"
        , "24-30-35-90-94-thom.wav"
        , "24-30-35-95-99-thom.wav"
        , "24-30-35-100-103-thom.wav"
        , "24-30-35-104-107-thom.wav"
        , "24-30-35-108-111-thom.wav"
        , "24-30-35-112-115-thom.wav"
        , "24-30-35-116-119-thom.wav"
        , "24-30-35-120-123-thom.wav"
        , "24-30-35-124-127-thom.wav"
        ]
    Gumki ->
        [ "24-30-35-0-5-gumki.wav"
        , "24-30-35-6-11-gumki.wav"
        , "24-30-35-12-17-gumki.wav"
        , "24-30-35-18-23-gumki.wav"
        , "24-30-35-24-29-gumki.wav"
        , "24-30-35-30-35-gumki.wav"
        , "24-30-35-36-41-gumki.wav"
        , "24-30-35-42-47-gumki.wav"
        , "24-30-35-48-53-gumki.wav"
        , "24-30-35-54-59-gumki.wav"
        , "24-30-35-60-65-gumki.wav"
        , "24-30-35-66-71-gumki.wav"
        , "24-30-35-72-77-gumki.wav"
        , "24-30-35-78-82-gumki.wav"
        , "24-30-35-83-87-gumki.wav"
        , "24-30-35-88-92-gumki.wav"
        , "24-30-35-93-97-gumki.wav"
        , "24-30-35-98-102-gumki.wav"
        , "24-30-35-103-107-gumki.wav"
        , "24-30-35-108-112-gumki.wav"
        , "24-30-35-113-117-gumki.wav"
        , "24-30-35-118-122-gumki.wav"
        , "24-30-35-123-127-gumki.wav"
        ]
    GumkiUp ->
        [ "24-30-35-0-4-gumki+up.wav"
        , "24-30-35-5-9-gumki+up.wav"
        , "24-30-35-10-14-gumki+up.wav"
        , "24-30-35-15-19-gumki+up.wav"
        , "24-30-35-20-24-gumki+up.wav"
        , "24-30-35-25-29-gumki+up.wav"
        , "24-30-35-30-34-gumki+up.wav"
        , "24-30-35-35-39-gumki+up.wav"
        , "24-30-35-40-44-gumki+up.wav"
        , "24-30-35-45-49-gumki+up.wav"
        , "24-30-35-50-54-gumki+up.wav"
        , "24-30-35-55-59-gumki+up.wav"
        , "24-30-35-60-64-gumki+up.wav"
        , "24-30-35-65-69-gumki+up.wav"
        , "24-30-35-70-74-gumki+up.wav"
        , "24-30-35-75-79-gumki+up.wav"
        , "24-30-35-80-84-gumki+up.wav"
        , "24-30-35-85-89-gumki+up.wav"
        , "24-30-35-90-94-gumki+up.wav"
        , "24-30-35-95-99-gumki+up.wav"
        , "24-30-35-100-103-gumki+up.wav"
        , "24-30-35-104-107-gumki+up.wav"
        , "24-30-35-108-111-gumki+up.wav"
        , "24-30-35-112-115-gumki+up.wav"
        , "24-30-35-116-119-gumki+up.wav"
        , "24-30-35-120-123-gumki+up.wav"
        , "24-30-35-124-127-gumki+up.wav"
        ]
    Ki ->
        [ "36-42-47-0-3-ki.wav"
        , "36-42-47-4-7-ki.wav"
        , "36-42-47-8-10-ki.wav"
        , "36-42-47-11-13-ki.wav"
        , "36-42-47-14-16-ki.wav"
        , "36-42-47-17-19-ki.wav"
        , "36-42-47-20-22-ki.wav"
        , "36-42-47-23-25-ki.wav"
        , "36-42-47-26-28-ki.wav"
        , "36-42-47-29-31-ki.wav"
        , "36-42-47-32-34-ki.wav"
        , "36-42-47-35-37-ki.wav"
        , "36-42-47-38-40-ki.wav"
        , "36-42-47-41-43-ki.wav"
        , "36-42-47-44-46-ki.wav"
        , "36-42-47-47-49-ki.wav"
        , "36-42-47-50-52-ki.wav"
        , "36-42-47-53-55-ki.wav"
        , "36-42-47-56-58-ki.wav"
        , "36-42-47-59-61-ki.wav"
        , "36-42-47-62-64-ki.wav"
        , "36-42-47-65-67-ki.wav"
        , "36-42-47-68-70-ki.wav"
        , "36-42-47-71-73-ki.wav"
        , "36-42-47-74-76-ki.wav"
        , "36-42-47-77-79-ki.wav"
        , "36-42-47-80-82-ki.wav"
        , "36-42-47-83-85-ki.wav"
        , "36-42-47-86-88-ki.wav"
        , "36-42-47-89-91-ki.wav"
        , "36-42-47-92-94-ki.wav"
        , "36-42-47-95-97-ki.wav"
        , "36-42-47-98-100-ki.wav"
        , "36-42-47-101-103-ki.wav"
        , "36-42-47-104-106-ki.wav"
        , "36-42-47-107-109-ki.wav"
        , "36-42-47-110-112-ki.wav"
        , "36-42-47-113-115-ki.wav"
        , "36-42-47-116-118-ki.wav"
        , "36-42-47-119-121-ki.wav"
        , "36-42-47-122-124-ki.wav"
        , "36-42-47-125-127-ki.wav"
        ]
    Ta ->
        [ "48-54-59-0-3-ta.wav"
        , "48-54-59-4-7-ta.wav"
        , "48-54-59-8-11-ta.wav"
        , "48-54-59-12-15-ta.wav"
        , "48-54-59-16-19-ta.wav"
        , "48-54-59-20-23-ta.wav"
        , "48-54-59-24-27-ta.wav"
        , "48-54-59-28-31-ta.wav"
        , "48-54-59-32-35-ta.wav"
        , "48-54-59-36-39-ta.wav"
        , "48-54-59-40-43-ta.wav"
        , "48-54-59-44-47-ta.wav"
        , "48-54-59-48-51-ta.wav"
        , "48-54-59-52-55-ta.wav"
        , "48-54-59-56-59-ta.wav"
        , "48-54-59-60-63-ta.wav"
        , "48-54-59-64-67-ta.wav"
        , "48-54-59-68-71-ta.wav"
        , "48-54-59-72-75-ta.wav"
        , "48-54-59-76-79-ta.wav"
        , "48-54-59-80-83-ta.wav"
        , "48-54-59-84-87-ta.wav"
        , "48-54-59-88-91-ta.wav"
        , "48-54-59-92-95-ta.wav"
        , "48-54-59-96-99-ta.wav"
        , "48-54-59-100-103-ta.wav"
        , "48-54-59-104-106-ta.wav"
        , "48-54-59-107-109-ta.wav"
        , "48-54-59-110-112-ta.wav"
        , "48-54-59-113-115-ta.wav"
        , "48-54-59-116-118-ta.wav"
        , "48-54-59-119-121-ta.wav"
        , "48-54-59-122-124-ta.wav"
        , "48-54-59-125-127-ta.wav"
        ]
    Nam ->
        [ "60-66-71-0-3-nam.wav"
        , "60-66-71-4-7-nam.wav"
        , "60-66-71-8-11-nam.wav"
        , "60-66-71-12-15-nam.wav"
        , "60-66-71-16-19-nam.wav"
        , "60-66-71-20-23-nam.wav"
        , "60-66-71-24-27-nam.wav"
        , "60-66-71-28-31-nam.wav"
        , "60-66-71-32-35-nam.wav"
        , "60-66-71-36-39-nam.wav"
        , "60-66-71-40-43-nam.wav"
        , "60-66-71-44-46-nam.wav"
        , "60-66-71-47-49-nam.wav"
        , "60-66-71-50-52-nam.wav"
        , "60-66-71-53-55-nam.wav"
        , "60-66-71-56-58-nam.wav"
        , "60-66-71-59-61-nam.wav"
        , "60-66-71-62-64-nam.wav"
        , "60-66-71-65-67-nam.wav"
        , "60-66-71-68-70-nam.wav"
        , "60-66-71-71-73-nam.wav"
        , "60-66-71-74-76-nam.wav"
        , "60-66-71-77-79-nam.wav"
        , "60-66-71-80-82-nam.wav"
        , "60-66-71-83-85-nam.wav"
        , "60-66-71-86-88-nam.wav"
        , "60-66-71-89-91-nam.wav"
        , "60-66-71-92-94-nam.wav"
        , "60-66-71-95-97-nam.wav"
        , "60-66-71-98-100-nam.wav"
        , "60-66-71-101-103-nam.wav"
        , "60-66-71-104-106-nam.wav"
        , "60-66-71-107-109-nam.wav"
        , "60-66-71-110-112-nam.wav"
        , "60-66-71-113-115-nam.wav"
        , "60-66-71-116-118-nam.wav"
        , "60-66-71-119-121-nam.wav"
        , "60-66-71-122-124-nam.wav"
        , "60-66-71-125-127-nam.wav"
        ]
    Din ->
        [ "72-78-83-0-3-din.wav"
        , "72-78-83-4-7-din.wav"
        , "72-78-83-8-11-din.wav"
        , "72-78-83-12-15-din.wav"
        , "72-78-83-16-19-din.wav"
        , "72-78-83-20-23-din.wav"
        , "72-78-83-24-27-din.wav"
        , "72-78-83-28-31-din.wav"
        , "72-78-83-32-35-din.wav"
        , "72-78-83-36-39-din.wav"
        , "72-78-83-40-43-din.wav"
        , "72-78-83-44-47-din.wav"
        , "72-78-83-48-51-din.wav"
        , "72-78-83-52-55-din.wav"
        , "72-78-83-56-59-din.wav"
        , "72-78-83-60-63-din.wav"
        , "72-78-83-64-67-din.wav"
        , "72-78-83-68-70-din.wav"
        , "72-78-83-71-73-din.wav"
        , "72-78-83-74-76-din.wav"
        , "72-78-83-77-79-din.wav"
        , "72-78-83-80-82-din.wav"
        , "72-78-83-83-85-din.wav"
        , "72-78-83-86-88-din.wav"
        , "72-78-83-89-91-din.wav"
        , "72-78-83-92-94-din.wav"
        , "72-78-83-95-97-din.wav"
        , "72-78-83-98-100-din.wav"
        , "72-78-83-101-103-din.wav"
        , "72-78-83-104-106-din.wav"
        , "72-78-83-107-109-din.wav"
        , "72-78-83-110-112-din.wav"
        , "72-78-83-113-115-din.wav"
        , "72-78-83-116-118-din.wav"
        , "72-78-83-119-121-din.wav"
        , "72-78-83-122-124-din.wav"
        , "72-78-83-125-127-din.wav"
        ]
    Chapu ->
        [ "84-90-95-0-5-chapu.wav"
        , "84-90-95-6-11-chapu.wav"
        , "84-90-95-12-17-chapu.wav"
        , "84-90-95-18-22-chapu.wav"
        , "84-90-95-23-27-chapu.wav"
        , "84-90-95-28-32-chapu.wav"
        , "84-90-95-33-37-chapu.wav"
        , "84-90-95-38-42-chapu.wav"
        , "84-90-95-43-47-chapu.wav"
        , "84-90-95-48-52-chapu.wav"
        , "84-90-95-53-57-chapu.wav"
        , "84-90-95-58-62-chapu.wav"
        , "84-90-95-63-67-chapu.wav"
        , "84-90-95-68-72-chapu.wav"
        , "84-90-95-73-77-chapu.wav"
        , "84-90-95-78-82-chapu.wav"
        , "84-90-95-83-87-chapu.wav"
        , "84-90-95-88-92-chapu.wav"
        , "84-90-95-93-97-chapu.wav"
        , "84-90-95-98-102-chapu.wav"
        , "84-90-95-103-107-chapu.wav"
        , "84-90-95-108-112-chapu.wav"
        , "84-90-95-113-117-chapu.wav"
        , "84-90-95-118-122-chapu.wav"
        , "84-90-95-123-127-chapu.wav"
        ]
    Dheem ->
        [ "96-102-107-0-3-dheem.wav"
        , "96-102-107-4-7-dheem.wav"
        , "96-102-107-8-11-dheem.wav"
        , "96-102-107-12-15-dheem.wav"
        , "96-102-107-16-19-dheem.wav"
        , "96-102-107-20-23-dheem.wav"
        , "96-102-107-24-27-dheem.wav"
        , "96-102-107-28-31-dheem.wav"
        , "96-102-107-32-35-dheem.wav"
        , "96-102-107-36-39-dheem.wav"
        , "96-102-107-40-43-dheem.wav"
        , "96-102-107-44-47-dheem.wav"
        , "96-102-107-48-51-dheem.wav"
        , "96-102-107-52-55-dheem.wav"
        , "96-102-107-56-59-dheem.wav"
        , "96-102-107-60-63-dheem.wav"
        , "96-102-107-64-67-dheem.wav"
        , "96-102-107-68-71-dheem.wav"
        , "96-102-107-72-75-dheem.wav"
        , "96-102-107-76-79-dheem.wav"
        , "96-102-107-80-83-dheem.wav"
        , "96-102-107-84-87-dheem.wav"
        , "96-102-107-88-91-dheem.wav"
        , "96-102-107-92-95-dheem.wav"
        , "96-102-107-96-99-dheem.wav"
        , "96-102-107-100-103-dheem.wav"
        , "96-102-107-104-107-dheem.wav"
        , "96-102-107-108-111-dheem.wav"
        , "96-102-107-112-115-dheem.wav"
        , "96-102-107-116-119-dheem.wav"
        , "96-102-107-120-123-dheem.wav"
        , "96-102-107-124-127-dheem.wav"
        ]
    Kin ->
        [ "96-102-107-0-5-meetu+ki.wav"
        , "96-102-107-6-11-meetu+ki.wav"
        , "96-102-107-12-17-meetu+ki.wav"
        , "96-102-107-18-22-meetu+ki.wav"
        , "96-102-107-23-27-meetu+ki.wav"
        , "96-102-107-28-32-meetu+ki.wav"
        , "96-102-107-33-37-meetu+ki.wav"
        , "96-102-107-38-42-meetu+ki.wav"
        , "96-102-107-43-47-meetu+ki.wav"
        , "96-102-107-48-52-meetu+ki.wav"
        , "96-102-107-53-57-meetu+ki.wav"
        , "96-102-107-58-62-meetu+ki.wav"
        , "96-102-107-63-67-meetu+ki.wav"
        , "96-102-107-68-72-meetu+ki.wav"
        , "96-102-107-73-77-meetu+ki.wav"
        , "96-102-107-78-82-meetu+ki.wav"
        , "96-102-107-83-87-meetu+ki.wav"
        , "96-102-107-88-92-meetu+ki.wav"
        , "96-102-107-93-97-meetu+ki.wav"
        , "96-102-107-98-102-meetu+ki.wav"
        , "96-102-107-103-107-meetu+ki.wav"
        , "96-102-107-108-112-meetu+ki.wav"
        , "96-102-107-113-117-meetu+ki.wav"
        , "96-102-107-118-122-meetu+ki.wav"
        , "96-102-107-123-127-meetu+ki.wav"
        ]
    Tan ->
        [ "108-114-119-0-9-meetu+ta.wav"
        , "108-114-119-10-19-meetu+ta.wav"
        , "108-114-119-20-29-meetu+ta.wav"
        , "108-114-119-30-39-meetu+ta.wav"
        , "108-114-119-40-49-meetu+ta.wav"
        , "108-114-119-50-59-meetu+ta.wav"
        , "108-114-119-60-69-meetu+ta.wav"
        , "108-114-119-70-79-meetu+ta.wav"
        , "108-114-119-80-89-meetu+ta.wav"
        , "108-114-119-90-99-meetu+ta.wav"
        , "108-114-119-100-109-meetu+ta.wav"
        , "108-114-119-110-118-meetu+ta.wav"
        , "108-114-119-119-127-meetu+ta.wav"
        ]
