-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Sampler.Patch.Mridangam (patches) where
import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.ImInst as ImInst
import qualified Cmd.Instrument.Mridangam as Mridangam

import qualified Derive.Attrs as Attrs
import qualified Instrument.Common as Common
import qualified Perform.Pitch as Pitch
import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.Patch.Lib.Code as Code
import qualified Synth.Sampler.Patch.Lib.Drum as Drum
import qualified Synth.Sampler.Patch.Lib.Util as Util

import           Global


patches :: [Patch.DbPatch]
patches = (:[]) $ Patch.DbPatch $ (Patch.patch patchName)
    { Patch._dir = dir
    , Patch._convert = convert
    , Patch._preprocess = Drum.inferDuration strokeMap
    , Patch._karyaPatch = CUtil.im_drum_patch (Drum._strokes strokeMap) $
        ImInst.code #= code $ Drum.makePatch attributeMap True
    , Patch._allFilenames = Drum._allFilenames convertMap
    }
    where
    convert = Drum.convert attributeMap convertMap
    code = Mridangam.code (Util.imThruFunction dir convert)
        naturalNn (Just $ \_ -> Code.withVariationNormal 1)
    dir = untxt patchName

patchName :: Text
patchName = "mridangam-d"

strokeMap :: Drum.StrokeMap Articulation
strokeMap = Drum.strokeMap Mridangam.stops Mridangam.all_strokes attributeMap

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

convertMap :: Drum.ConvertMap Articulation
convertMap = Drum.ConvertMap
    { _dynRange = (0.4, 1.15)
    , _naturalNn = Just (const naturalNn)
    , _muteTime = Just 0.05
    , _getFilename = Drum.variableDynamic 0.15 articulationSamples
    , _allFilenames = Drum.allFilenames 356 articulationSamples
    }

naturalNn :: Pitch.NoteNumber
naturalNn = 62.65

data Articulation = Tha | Thom | Gumki | GumkiUp
    | Ki | Ta | Nam | Din | Chapu | Dheem
    | Kin | Tan
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | Generate 'articulationSamples'.
_makeArticulationSamples :: IO ()
_makeArticulationSamples = Drum.makeFileList (untxt patchName)
    (map show (Util.enumAll :: [Articulation])) "articulationSamples"

articulationSamples :: Articulation -> [FilePath]
articulationSamples = \case
    Tha ->
        [ "12-18-23-0-3-tha.flac"
        , "12-18-23-4-7-tha.flac"
        , "12-18-23-8-11-tha.flac"
        , "12-18-23-12-15-tha.flac"
        , "12-18-23-16-19-tha.flac"
        , "12-18-23-20-23-tha.flac"
        , "12-18-23-24-27-tha.flac"
        , "12-18-23-28-31-tha.flac"
        , "12-18-23-32-35-tha.flac"
        , "12-18-23-36-39-tha.flac"
        , "12-18-23-40-43-tha.flac"
        , "12-18-23-44-47-tha.flac"
        , "12-18-23-48-51-tha.flac"
        , "12-18-23-52-55-tha.flac"
        , "12-18-23-56-59-tha.flac"
        , "12-18-23-60-63-tha.flac"
        , "12-18-23-64-67-tha.flac"
        , "12-18-23-68-71-tha.flac"
        , "12-18-23-72-75-tha.flac"
        , "12-18-23-76-79-tha.flac"
        , "12-18-23-80-83-tha.flac"
        , "12-18-23-84-87-tha.flac"
        , "12-18-23-88-91-tha.flac"
        , "12-18-23-92-95-tha.flac"
        , "12-18-23-96-99-tha.flac"
        , "12-18-23-100-103-tha.flac"
        , "12-18-23-104-107-tha.flac"
        , "12-18-23-108-111-tha.flac"
        , "12-18-23-112-115-tha.flac"
        , "12-18-23-116-119-tha.flac"
        , "12-18-23-120-123-tha.flac"
        , "12-18-23-124-127-tha.flac"
        ]
    Thom ->
        [ "24-30-35-0-4-thom.flac"
        , "24-30-35-5-9-thom.flac"
        , "24-30-35-10-14-thom.flac"
        , "24-30-35-15-19-thom.flac"
        , "24-30-35-20-24-thom.flac"
        , "24-30-35-25-29-thom.flac"
        , "24-30-35-30-34-thom.flac"
        , "24-30-35-35-39-thom.flac"
        , "24-30-35-40-44-thom.flac"
        , "24-30-35-45-49-thom.flac"
        , "24-30-35-50-54-thom.flac"
        , "24-30-35-55-59-thom.flac"
        , "24-30-35-60-64-thom.flac"
        , "24-30-35-65-69-thom.flac"
        , "24-30-35-70-74-thom.flac"
        , "24-30-35-75-79-thom.flac"
        , "24-30-35-80-84-thom.flac"
        , "24-30-35-85-89-thom.flac"
        , "24-30-35-90-94-thom.flac"
        , "24-30-35-95-99-thom.flac"
        , "24-30-35-100-103-thom.flac"
        , "24-30-35-104-107-thom.flac"
        , "24-30-35-108-111-thom.flac"
        , "24-30-35-112-115-thom.flac"
        , "24-30-35-116-119-thom.flac"
        , "24-30-35-120-123-thom.flac"
        , "24-30-35-124-127-thom.flac"
        ]
    Gumki ->
        [ "24-30-35-0-5-gumki.flac"
        , "24-30-35-6-11-gumki.flac"
        , "24-30-35-12-17-gumki.flac"
        , "24-30-35-18-23-gumki.flac"
        , "24-30-35-24-29-gumki.flac"
        , "24-30-35-30-35-gumki.flac"
        , "24-30-35-36-41-gumki.flac"
        , "24-30-35-42-47-gumki.flac"
        , "24-30-35-48-53-gumki.flac"
        , "24-30-35-54-59-gumki.flac"
        , "24-30-35-60-65-gumki.flac"
        , "24-30-35-66-71-gumki.flac"
        , "24-30-35-72-77-gumki.flac"
        , "24-30-35-78-82-gumki.flac"
        , "24-30-35-83-87-gumki.flac"
        , "24-30-35-88-92-gumki.flac"
        , "24-30-35-93-97-gumki.flac"
        , "24-30-35-98-102-gumki.flac"
        , "24-30-35-103-107-gumki.flac"
        , "24-30-35-108-112-gumki.flac"
        , "24-30-35-113-117-gumki.flac"
        , "24-30-35-118-122-gumki.flac"
        , "24-30-35-123-127-gumki.flac"
        ]
    GumkiUp ->
        [ "24-30-35-0-4-gumki+up.flac"
        , "24-30-35-5-9-gumki+up.flac"
        , "24-30-35-10-14-gumki+up.flac"
        , "24-30-35-15-19-gumki+up.flac"
        , "24-30-35-20-24-gumki+up.flac"
        , "24-30-35-25-29-gumki+up.flac"
        , "24-30-35-30-34-gumki+up.flac"
        , "24-30-35-35-39-gumki+up.flac"
        , "24-30-35-40-44-gumki+up.flac"
        , "24-30-35-45-49-gumki+up.flac"
        , "24-30-35-50-54-gumki+up.flac"
        , "24-30-35-55-59-gumki+up.flac"
        , "24-30-35-60-64-gumki+up.flac"
        , "24-30-35-65-69-gumki+up.flac"
        , "24-30-35-70-74-gumki+up.flac"
        , "24-30-35-75-79-gumki+up.flac"
        , "24-30-35-80-84-gumki+up.flac"
        , "24-30-35-85-89-gumki+up.flac"
        , "24-30-35-90-94-gumki+up.flac"
        , "24-30-35-95-99-gumki+up.flac"
        , "24-30-35-100-103-gumki+up.flac"
        , "24-30-35-104-107-gumki+up.flac"
        , "24-30-35-108-111-gumki+up.flac"
        , "24-30-35-112-115-gumki+up.flac"
        , "24-30-35-116-119-gumki+up.flac"
        , "24-30-35-120-123-gumki+up.flac"
        , "24-30-35-124-127-gumki+up.flac"
        ]
    Ki ->
        [ "36-42-47-0-3-ki.flac"
        , "36-42-47-4-7-ki.flac"
        , "36-42-47-8-10-ki.flac"
        , "36-42-47-11-13-ki.flac"
        , "36-42-47-14-16-ki.flac"
        , "36-42-47-17-19-ki.flac"
        , "36-42-47-20-22-ki.flac"
        , "36-42-47-23-25-ki.flac"
        , "36-42-47-26-28-ki.flac"
        , "36-42-47-29-31-ki.flac"
        , "36-42-47-32-34-ki.flac"
        , "36-42-47-35-37-ki.flac"
        , "36-42-47-38-40-ki.flac"
        , "36-42-47-41-43-ki.flac"
        , "36-42-47-44-46-ki.flac"
        , "36-42-47-47-49-ki.flac"
        , "36-42-47-50-52-ki.flac"
        , "36-42-47-53-55-ki.flac"
        , "36-42-47-56-58-ki.flac"
        , "36-42-47-59-61-ki.flac"
        , "36-42-47-62-64-ki.flac"
        , "36-42-47-65-67-ki.flac"
        , "36-42-47-68-70-ki.flac"
        , "36-42-47-71-73-ki.flac"
        , "36-42-47-74-76-ki.flac"
        , "36-42-47-77-79-ki.flac"
        , "36-42-47-80-82-ki.flac"
        , "36-42-47-83-85-ki.flac"
        , "36-42-47-86-88-ki.flac"
        , "36-42-47-89-91-ki.flac"
        , "36-42-47-92-94-ki.flac"
        , "36-42-47-95-97-ki.flac"
        , "36-42-47-98-100-ki.flac"
        , "36-42-47-101-103-ki.flac"
        , "36-42-47-104-106-ki.flac"
        , "36-42-47-107-109-ki.flac"
        , "36-42-47-110-112-ki.flac"
        , "36-42-47-113-115-ki.flac"
        , "36-42-47-116-118-ki.flac"
        , "36-42-47-119-121-ki.flac"
        , "36-42-47-122-124-ki.flac"
        , "36-42-47-125-127-ki.flac"
        ]
    Ta ->
        [ "48-54-59-0-3-ta.flac"
        , "48-54-59-4-7-ta.flac"
        , "48-54-59-8-11-ta.flac"
        , "48-54-59-12-15-ta.flac"
        , "48-54-59-16-19-ta.flac"
        , "48-54-59-20-23-ta.flac"
        , "48-54-59-24-27-ta.flac"
        , "48-54-59-28-31-ta.flac"
        , "48-54-59-32-35-ta.flac"
        , "48-54-59-36-39-ta.flac"
        , "48-54-59-40-43-ta.flac"
        , "48-54-59-44-47-ta.flac"
        , "48-54-59-48-51-ta.flac"
        , "48-54-59-52-55-ta.flac"
        , "48-54-59-56-59-ta.flac"
        , "48-54-59-60-63-ta.flac"
        , "48-54-59-64-67-ta.flac"
        , "48-54-59-68-71-ta.flac"
        , "48-54-59-72-75-ta.flac"
        , "48-54-59-76-79-ta.flac"
        , "48-54-59-80-83-ta.flac"
        , "48-54-59-84-87-ta.flac"
        , "48-54-59-88-91-ta.flac"
        , "48-54-59-92-95-ta.flac"
        , "48-54-59-96-99-ta.flac"
        , "48-54-59-100-103-ta.flac"
        , "48-54-59-104-106-ta.flac"
        , "48-54-59-107-109-ta.flac"
        , "48-54-59-110-112-ta.flac"
        , "48-54-59-113-115-ta.flac"
        , "48-54-59-116-118-ta.flac"
        , "48-54-59-119-121-ta.flac"
        , "48-54-59-122-124-ta.flac"
        , "48-54-59-125-127-ta.flac"
        ]
    Nam ->
        [ "60-66-71-0-3-nam.flac"
        , "60-66-71-4-7-nam.flac"
        , "60-66-71-8-11-nam.flac"
        , "60-66-71-12-15-nam.flac"
        , "60-66-71-16-19-nam.flac"
        , "60-66-71-20-23-nam.flac"
        , "60-66-71-24-27-nam.flac"
        , "60-66-71-28-31-nam.flac"
        , "60-66-71-32-35-nam.flac"
        , "60-66-71-36-39-nam.flac"
        , "60-66-71-40-43-nam.flac"
        , "60-66-71-44-46-nam.flac"
        , "60-66-71-47-49-nam.flac"
        , "60-66-71-50-52-nam.flac"
        , "60-66-71-53-55-nam.flac"
        , "60-66-71-56-58-nam.flac"
        , "60-66-71-59-61-nam.flac"
        , "60-66-71-62-64-nam.flac"
        , "60-66-71-65-67-nam.flac"
        , "60-66-71-68-70-nam.flac"
        , "60-66-71-71-73-nam.flac"
        , "60-66-71-74-76-nam.flac"
        , "60-66-71-77-79-nam.flac"
        , "60-66-71-80-82-nam.flac"
        , "60-66-71-83-85-nam.flac"
        , "60-66-71-86-88-nam.flac"
        , "60-66-71-89-91-nam.flac"
        , "60-66-71-92-94-nam.flac"
        , "60-66-71-95-97-nam.flac"
        , "60-66-71-98-100-nam.flac"
        , "60-66-71-101-103-nam.flac"
        , "60-66-71-104-106-nam.flac"
        , "60-66-71-107-109-nam.flac"
        , "60-66-71-110-112-nam.flac"
        , "60-66-71-113-115-nam.flac"
        , "60-66-71-116-118-nam.flac"
        , "60-66-71-119-121-nam.flac"
        , "60-66-71-122-124-nam.flac"
        , "60-66-71-125-127-nam.flac"
        ]
    Din ->
        [ "72-78-83-0-3-din.flac"
        , "72-78-83-4-7-din.flac"
        , "72-78-83-8-11-din.flac"
        , "72-78-83-12-15-din.flac"
        , "72-78-83-16-19-din.flac"
        , "72-78-83-20-23-din.flac"
        , "72-78-83-24-27-din.flac"
        , "72-78-83-28-31-din.flac"
        , "72-78-83-32-35-din.flac"
        , "72-78-83-36-39-din.flac"
        , "72-78-83-40-43-din.flac"
        , "72-78-83-44-47-din.flac"
        , "72-78-83-48-51-din.flac"
        , "72-78-83-52-55-din.flac"
        , "72-78-83-56-59-din.flac"
        , "72-78-83-60-63-din.flac"
        , "72-78-83-64-67-din.flac"
        , "72-78-83-68-70-din.flac"
        , "72-78-83-71-73-din.flac"
        , "72-78-83-74-76-din.flac"
        , "72-78-83-77-79-din.flac"
        , "72-78-83-80-82-din.flac"
        , "72-78-83-83-85-din.flac"
        , "72-78-83-86-88-din.flac"
        , "72-78-83-89-91-din.flac"
        , "72-78-83-92-94-din.flac"
        , "72-78-83-95-97-din.flac"
        , "72-78-83-98-100-din.flac"
        , "72-78-83-101-103-din.flac"
        , "72-78-83-104-106-din.flac"
        , "72-78-83-107-109-din.flac"
        , "72-78-83-110-112-din.flac"
        , "72-78-83-113-115-din.flac"
        , "72-78-83-116-118-din.flac"
        , "72-78-83-119-121-din.flac"
        , "72-78-83-122-124-din.flac"
        , "72-78-83-125-127-din.flac"
        ]
    Chapu ->
        [ "84-90-95-0-5-chapu.flac"
        , "84-90-95-6-11-chapu.flac"
        , "84-90-95-12-17-chapu.flac"
        , "84-90-95-18-22-chapu.flac"
        , "84-90-95-23-27-chapu.flac"
        , "84-90-95-28-32-chapu.flac"
        , "84-90-95-33-37-chapu.flac"
        , "84-90-95-38-42-chapu.flac"
        , "84-90-95-43-47-chapu.flac"
        , "84-90-95-48-52-chapu.flac"
        , "84-90-95-53-57-chapu.flac"
        , "84-90-95-58-62-chapu.flac"
        , "84-90-95-63-67-chapu.flac"
        , "84-90-95-68-72-chapu.flac"
        , "84-90-95-73-77-chapu.flac"
        , "84-90-95-78-82-chapu.flac"
        , "84-90-95-83-87-chapu.flac"
        , "84-90-95-88-92-chapu.flac"
        , "84-90-95-93-97-chapu.flac"
        , "84-90-95-98-102-chapu.flac"
        , "84-90-95-103-107-chapu.flac"
        , "84-90-95-108-112-chapu.flac"
        , "84-90-95-113-117-chapu.flac"
        , "84-90-95-118-122-chapu.flac"
        , "84-90-95-123-127-chapu.flac"
        ]
    Dheem ->
        [ "96-102-107-0-3-dheem.flac"
        , "96-102-107-4-7-dheem.flac"
        , "96-102-107-8-11-dheem.flac"
        , "96-102-107-12-15-dheem.flac"
        , "96-102-107-16-19-dheem.flac"
        , "96-102-107-20-23-dheem.flac"
        , "96-102-107-24-27-dheem.flac"
        , "96-102-107-28-31-dheem.flac"
        , "96-102-107-32-35-dheem.flac"
        , "96-102-107-36-39-dheem.flac"
        , "96-102-107-40-43-dheem.flac"
        , "96-102-107-44-47-dheem.flac"
        , "96-102-107-48-51-dheem.flac"
        , "96-102-107-52-55-dheem.flac"
        , "96-102-107-56-59-dheem.flac"
        , "96-102-107-60-63-dheem.flac"
        , "96-102-107-64-67-dheem.flac"
        , "96-102-107-68-71-dheem.flac"
        , "96-102-107-72-75-dheem.flac"
        , "96-102-107-76-79-dheem.flac"
        , "96-102-107-80-83-dheem.flac"
        , "96-102-107-84-87-dheem.flac"
        , "96-102-107-88-91-dheem.flac"
        , "96-102-107-92-95-dheem.flac"
        , "96-102-107-96-99-dheem.flac"
        , "96-102-107-100-103-dheem.flac"
        , "96-102-107-104-107-dheem.flac"
        , "96-102-107-108-111-dheem.flac"
        , "96-102-107-112-115-dheem.flac"
        , "96-102-107-116-119-dheem.flac"
        , "96-102-107-120-123-dheem.flac"
        , "96-102-107-124-127-dheem.flac"
        ]
    Kin ->
        [ "96-102-107-0-5-meetu+ki.flac"
        , "96-102-107-6-11-meetu+ki.flac"
        , "96-102-107-12-17-meetu+ki.flac"
        , "96-102-107-18-22-meetu+ki.flac"
        , "96-102-107-23-27-meetu+ki.flac"
        , "96-102-107-28-32-meetu+ki.flac"
        , "96-102-107-33-37-meetu+ki.flac"
        , "96-102-107-38-42-meetu+ki.flac"
        , "96-102-107-43-47-meetu+ki.flac"
        , "96-102-107-48-52-meetu+ki.flac"
        , "96-102-107-53-57-meetu+ki.flac"
        , "96-102-107-58-62-meetu+ki.flac"
        , "96-102-107-63-67-meetu+ki.flac"
        , "96-102-107-68-72-meetu+ki.flac"
        , "96-102-107-73-77-meetu+ki.flac"
        , "96-102-107-78-82-meetu+ki.flac"
        , "96-102-107-83-87-meetu+ki.flac"
        , "96-102-107-88-92-meetu+ki.flac"
        , "96-102-107-93-97-meetu+ki.flac"
        , "96-102-107-98-102-meetu+ki.flac"
        , "96-102-107-103-107-meetu+ki.flac"
        , "96-102-107-108-112-meetu+ki.flac"
        , "96-102-107-113-117-meetu+ki.flac"
        , "96-102-107-118-122-meetu+ki.flac"
        , "96-102-107-123-127-meetu+ki.flac"
        ]
    Tan ->
        [ "108-114-119-0-9-meetu+ta.flac"
        , "108-114-119-10-19-meetu+ta.flac"
        , "108-114-119-20-29-meetu+ta.flac"
        , "108-114-119-30-39-meetu+ta.flac"
        , "108-114-119-40-49-meetu+ta.flac"
        , "108-114-119-50-59-meetu+ta.flac"
        , "108-114-119-60-69-meetu+ta.flac"
        , "108-114-119-70-79-meetu+ta.flac"
        , "108-114-119-80-89-meetu+ta.flac"
        , "108-114-119-90-99-meetu+ta.flac"
        , "108-114-119-100-109-meetu+ta.flac"
        , "108-114-119-110-118-meetu+ta.flac"
        , "108-114-119-119-127-meetu+ta.flac"
        ]
