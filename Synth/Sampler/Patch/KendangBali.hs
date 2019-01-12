-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Sampler.Patch.KendangBali where
import qualified Data.List as List
import qualified Data.Map as Map
import           System.FilePath ((</>))

import qualified Util.Map
import qualified Util.Num as Num
import qualified Util.Seq as Seq

import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Cmd.Instrument.ImInst as ImInst
import qualified Cmd.Instrument.KendangBali as K

import qualified Derive.Attrs as Attrs
import qualified Instrument.Common as Common
import qualified Perform.Im.Patch as Im.Patch
import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.Patch.Util as Util
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import           Global
import           Synth.Types


patches :: [Patch.Patch]
patches = map make [Wadon, Lanang]
    where
    make tuning = (Patch.patch name)
        { Patch._dir = dir
        , Patch._convert = convert tuning
        , Patch._preprocess = inferDuration
        , Patch._karyaPatch = CUtil.im_drum_patch K.tunggal_notes $
            ImInst.code #= code $
            ImInst.make_patch $ Im.Patch.patch
                { Im.Patch.patch_controls = mconcat
                    [ Control.supportDyn
                    , Control.supportVariation
                    ]
                , Im.Patch.patch_attribute_map = const () <$> attributeMap
                }
        }
        where
        name = patchName <> "-" <> txt (Util.showLower tuning)
        code = CUtil.drum_code thru (Just "kendang-tune")
            K.tunggal_notes
        thru = Util.imThruFunction dir (convert tuning)
    dir = untxt patchName

patchName :: Text
patchName = "kendang-bali"

data Tuning = Wadon | Lanang
    deriving (Eq, Show)

data Articulation =
    Plak -- both
    | De | DeStaccato | DeThumb | DeClosed -- right
    | Dag | DagStaccato | Tek
    | Tut | Ka
    | Pak | Pang | TutL | DeL -- left
    deriving (Eq, Show, Enum, Bounded)

articulationToAttrs :: Articulation -> Attrs.Attributes
articulationToAttrs = \case
    Plak -> K.plak
    De -> K.de
    DeStaccato -> K.de <> Attrs.staccato
    DeThumb -> K.de <> Attrs.thumb
    DeClosed -> K.de <> Attrs.closed
    Dag -> K.dag
    DagStaccato -> K.dag <> Attrs.staccato
    Tek -> K.tek
    Tut -> K.tut
    Ka -> K.ka
    Pak -> K.pak
    Pang -> K.pang
    TutL -> K.tut <> Attrs.left
    DeL -> K.de <> Attrs.left

attributeMap :: Common.AttributeMap Articulation
attributeMap =
    Common.attribute_map (Seq.key_on articulationToAttrs Util.enumAll)

-- Structure:
-- legong/{lanang,wadon}/$attr/variable-samples.wav
convert :: Tuning -> Note.Note -> Patch.ConvertM Sample.Sample
convert tuning note = do
    articulation <- Util.articulation attributeMap (Note.attributes note)
    let dynVal = Note.initial0 Control.dynamic note
    -- TODO also shared with other drums like this
    let var = maybe 0 (subtract 1 . (*2)) $ Note.initial Control.variation note
    let dir = "legong" </> Util.showLower tuning </> Util.showLower articulation
    let getSamples = case tuning of
            Wadon -> legongWadonSamples
            Lanang -> legongLanangSamples
    let filename = dir
            </> Util.pickDynamicVariation variationRange
                (getSamples articulation) dynVal var
    let noteDyn = Num.scale minDyn maxDyn dynVal
    return $ Sample.Sample
        { filename = filename
        , offset = 0
        , envelope = Signal.from_pairs
            [ (Note.start note, noteDyn), (Note.end note, noteDyn)
            , (Note.end note + muteTime, 0)
            ]
        , ratio = Signal.constant 1
        }

-- | A note may pick a sample of this much dyn difference on either side.
variationRange :: Signal.Y
variationRange = 0.15

minDyn :: Signal.Y
minDyn = 0.4

maxDyn :: Signal.Y
maxDyn = 1.15

-- | Time to mute at the end of a note.
muteTime :: RealTime
muteTime = 0.05

-- TODO common drum code, except it uses K.stops, K.tunggal_notes etc.

-- | Notes ring until stopped by their stop note.
inferDuration :: [Note.Note] -> [Note.Note]
inferDuration = map infer . Util.nexts
    where
    infer (note, nexts) =
        note { Note.duration = inferEnd note nexts - Note.start note }

inferEnd :: Note.Note -> [Note.Note] -> RealTime
inferEnd note nexts = case List.find ((`elem` stops) . noteGroup) nexts of
    Nothing -> Sample.forever
    Just stop -> Note.start stop
    where
    stops = fromMaybe [] $ Map.lookup (noteGroup note) stoppedBy
    noteGroup = groupOf . Note.attributes

stoppedBy :: Map Drums.Group [Drums.Group]
stoppedBy = Util.Map.multimap $
    concatMap (\(group, stops) -> map (, group) stops) K.stops

groupOf :: Attrs.Attributes -> Drums.Group
groupOf attrs = maybe "" Drums._group $
    List.find ((`Attrs.contain` attrs) . Drums._attributes) K.tunggal_notes


makeSampleLists :: IO ()
makeSampleLists = do
    Util.makeFileList (untxt patchName </> "legong/wadon")
        (map show (Util.enumAll :: [Articulation])) "legongWadonSamples"
    Util.makeFileList (untxt patchName </> "legong/lanang")
        (map show (Util.enumAll :: [Articulation])) "legongLanangSamples"


-- generated
legongWadonSamples :: Articulation -> [FilePath]
legongWadonSamples = \case
    Plak ->
        [ "12-18-23-0-3-plak.flac"
        , "12-18-23-4-7-plak.flac"
        , "12-18-23-8-11-plak.flac"
        , "12-18-23-12-15-plak.flac"
        , "12-18-23-16-19-plak.flac"
        , "12-18-23-20-23-plak.flac"
        , "12-18-23-24-27-plak.flac"
        , "12-18-23-28-31-plak.flac"
        , "12-18-23-32-35-plak.flac"
        , "12-18-23-36-39-plak.flac"
        , "12-18-23-40-43-plak.flac"
        , "12-18-23-44-47-plak.flac"
        , "12-18-23-48-51-plak.flac"
        , "12-18-23-52-55-plak.flac"
        , "12-18-23-56-59-plak.flac"
        , "12-18-23-60-63-plak.flac"
        , "12-18-23-64-67-plak.flac"
        , "12-18-23-68-71-plak.flac"
        , "12-18-23-72-75-plak.flac"
        , "12-18-23-76-79-plak.flac"
        , "12-18-23-80-82-plak.flac"
        , "12-18-23-83-85-plak.flac"
        , "12-18-23-86-88-plak.flac"
        , "12-18-23-89-91-plak.flac"
        , "12-18-23-92-94-plak.flac"
        , "12-18-23-95-97-plak.flac"
        , "12-18-23-98-100-plak.flac"
        , "12-18-23-101-103-plak.flac"
        , "12-18-23-104-106-plak.flac"
        , "12-18-23-107-109-plak.flac"
        , "12-18-23-110-112-plak.flac"
        , "12-18-23-113-115-plak.flac"
        , "12-18-23-116-118-plak.flac"
        , "12-18-23-119-121-plak.flac"
        , "12-18-23-122-124-plak.flac"
        , "12-18-23-125-127-plak.flac"
        ]
    De ->
        [ "36-42-47-0-3-de.flac"
        , "36-42-47-4-7-de.flac"
        , "36-42-47-8-11-de.flac"
        , "36-42-47-12-15-de.flac"
        , "36-42-47-16-19-de.flac"
        , "36-42-47-20-23-de.flac"
        , "36-42-47-24-27-de.flac"
        , "36-42-47-28-31-de.flac"
        , "36-42-47-32-35-de.flac"
        , "36-42-47-36-39-de.flac"
        , "36-42-47-40-43-de.flac"
        , "36-42-47-44-47-de.flac"
        , "36-42-47-48-51-de.flac"
        , "36-42-47-52-55-de.flac"
        , "36-42-47-56-59-de.flac"
        , "36-42-47-60-63-de.flac"
        , "36-42-47-64-67-de.flac"
        , "36-42-47-68-71-de.flac"
        , "36-42-47-72-75-de.flac"
        , "36-42-47-76-79-de.flac"
        , "36-42-47-80-83-de.flac"
        , "36-42-47-84-87-de.flac"
        , "36-42-47-88-91-de.flac"
        , "36-42-47-92-95-de.flac"
        , "36-42-47-96-99-de.flac"
        , "36-42-47-100-103-de.flac"
        , "36-42-47-104-106-de.flac"
        , "36-42-47-107-109-de.flac"
        , "36-42-47-110-112-de.flac"
        , "36-42-47-113-115-de.flac"
        , "36-42-47-116-118-de.flac"
        , "36-42-47-119-121-de.flac"
        , "36-42-47-122-124-de.flac"
        , "36-42-47-125-127-de.flac"
        ]
    DeStaccato ->
        [ "12-18-23-0-3-de+staccato.flac"
        , "12-18-23-4-7-de+staccato.flac"
        , "12-18-23-8-11-de+staccato.flac"
        , "12-18-23-12-15-de+staccato.flac"
        , "12-18-23-16-19-de+staccato.flac"
        , "12-18-23-20-23-de+staccato.flac"
        , "12-18-23-24-27-de+staccato.flac"
        , "12-18-23-28-31-de+staccato.flac"
        , "12-18-23-32-35-de+staccato.flac"
        , "12-18-23-36-39-de+staccato.flac"
        , "12-18-23-40-43-de+staccato.flac"
        , "12-18-23-44-47-de+staccato.flac"
        , "12-18-23-48-51-de+staccato.flac"
        , "12-18-23-52-55-de+staccato.flac"
        , "12-18-23-56-59-de+staccato.flac"
        , "12-18-23-60-63-de+staccato.flac"
        , "12-18-23-64-67-de+staccato.flac"
        , "12-18-23-68-71-de+staccato.flac"
        , "12-18-23-72-75-de+staccato.flac"
        , "12-18-23-76-79-de+staccato.flac"
        , "12-18-23-80-82-de+staccato.flac"
        , "12-18-23-83-85-de+staccato.flac"
        , "12-18-23-86-88-de+staccato.flac"
        , "12-18-23-89-91-de+staccato.flac"
        , "12-18-23-92-94-de+staccato.flac"
        , "12-18-23-95-97-de+staccato.flac"
        , "12-18-23-98-100-de+staccato.flac"
        , "12-18-23-101-103-de+staccato.flac"
        , "12-18-23-104-106-de+staccato.flac"
        , "12-18-23-107-109-de+staccato.flac"
        , "12-18-23-110-112-de+staccato.flac"
        , "12-18-23-113-115-de+staccato.flac"
        , "12-18-23-116-118-de+staccato.flac"
        , "12-18-23-119-121-de+staccato.flac"
        , "12-18-23-122-124-de+staccato.flac"
        , "12-18-23-125-127-de+staccato.flac"
        ]
    DeThumb ->
        [ "24-30-35-0-3-de+thumb.flac"
        , "24-30-35-4-7-de+thumb.flac"
        , "24-30-35-8-11-de+thumb.flac"
        , "24-30-35-12-15-de+thumb.flac"
        , "24-30-35-16-19-de+thumb.flac"
        , "24-30-35-20-23-de+thumb.flac"
        , "24-30-35-24-27-de+thumb.flac"
        , "24-30-35-28-31-de+thumb.flac"
        , "24-30-35-32-35-de+thumb.flac"
        , "24-30-35-36-39-de+thumb.flac"
        , "24-30-35-40-43-de+thumb.flac"
        , "24-30-35-44-47-de+thumb.flac"
        , "24-30-35-48-51-de+thumb.flac"
        , "24-30-35-52-55-de+thumb.flac"
        , "24-30-35-56-59-de+thumb.flac"
        , "24-30-35-60-63-de+thumb.flac"
        , "24-30-35-64-67-de+thumb.flac"
        , "24-30-35-68-71-de+thumb.flac"
        , "24-30-35-72-75-de+thumb.flac"
        , "24-30-35-76-79-de+thumb.flac"
        , "24-30-35-80-83-de+thumb.flac"
        , "24-30-35-84-87-de+thumb.flac"
        , "24-30-35-88-91-de+thumb.flac"
        , "24-30-35-92-95-de+thumb.flac"
        , "24-30-35-96-99-de+thumb.flac"
        , "24-30-35-100-103-de+thumb.flac"
        , "24-30-35-104-107-de+thumb.flac"
        , "24-30-35-108-111-de+thumb.flac"
        , "24-30-35-112-115-de+thumb.flac"
        , "24-30-35-116-119-de+thumb.flac"
        , "24-30-35-120-123-de+thumb.flac"
        , "24-30-35-124-127-de+thumb.flac"
        ]
    DeClosed ->
        [ "48-54-59-0-3-de+closed.flac"
        , "48-54-59-4-7-de+closed.flac"
        , "48-54-59-8-11-de+closed.flac"
        , "48-54-59-12-15-de+closed.flac"
        , "48-54-59-16-19-de+closed.flac"
        , "48-54-59-20-23-de+closed.flac"
        , "48-54-59-24-27-de+closed.flac"
        , "48-54-59-28-31-de+closed.flac"
        , "48-54-59-32-35-de+closed.flac"
        , "48-54-59-36-39-de+closed.flac"
        , "48-54-59-40-43-de+closed.flac"
        , "48-54-59-44-47-de+closed.flac"
        , "48-54-59-48-51-de+closed.flac"
        , "48-54-59-52-55-de+closed.flac"
        , "48-54-59-56-59-de+closed.flac"
        , "48-54-59-60-63-de+closed.flac"
        , "48-54-59-64-67-de+closed.flac"
        , "48-54-59-68-71-de+closed.flac"
        , "48-54-59-72-75-de+closed.flac"
        , "48-54-59-76-79-de+closed.flac"
        , "48-54-59-80-82-de+closed.flac"
        , "48-54-59-83-85-de+closed.flac"
        , "48-54-59-86-88-de+closed.flac"
        , "48-54-59-89-91-de+closed.flac"
        , "48-54-59-92-94-de+closed.flac"
        , "48-54-59-95-97-de+closed.flac"
        , "48-54-59-98-100-de+closed.flac"
        , "48-54-59-101-103-de+closed.flac"
        , "48-54-59-104-106-de+closed.flac"
        , "48-54-59-107-109-de+closed.flac"
        , "48-54-59-110-112-de+closed.flac"
        , "48-54-59-113-115-de+closed.flac"
        , "48-54-59-116-118-de+closed.flac"
        , "48-54-59-119-121-de+closed.flac"
        , "48-54-59-122-124-de+closed.flac"
        , "48-54-59-125-127-de+closed.flac"
        ]
    Dag ->
        [ "36-42-47-0-3-dag.flac"
        , "36-42-47-4-7-dag.flac"
        , "36-42-47-8-11-dag.flac"
        , "36-42-47-12-15-dag.flac"
        , "36-42-47-16-19-dag.flac"
        , "36-42-47-20-23-dag.flac"
        , "36-42-47-24-27-dag.flac"
        , "36-42-47-28-31-dag.flac"
        , "36-42-47-32-35-dag.flac"
        , "36-42-47-36-39-dag.flac"
        , "36-42-47-40-43-dag.flac"
        , "36-42-47-44-47-dag.flac"
        , "36-42-47-48-51-dag.flac"
        , "36-42-47-52-55-dag.flac"
        , "36-42-47-56-59-dag.flac"
        , "36-42-47-60-63-dag.flac"
        , "36-42-47-64-67-dag.flac"
        , "36-42-47-68-71-dag.flac"
        , "36-42-47-72-75-dag.flac"
        , "36-42-47-76-79-dag.flac"
        , "36-42-47-80-83-dag.flac"
        , "36-42-47-84-87-dag.flac"
        , "36-42-47-88-91-dag.flac"
        , "36-42-47-92-94-dag.flac"
        , "36-42-47-95-97-dag.flac"
        , "36-42-47-98-100-dag.flac"
        , "36-42-47-101-103-dag.flac"
        , "36-42-47-104-106-dag.flac"
        , "36-42-47-107-109-dag.flac"
        , "36-42-47-110-112-dag.flac"
        , "36-42-47-113-115-dag.flac"
        , "36-42-47-116-118-dag.flac"
        , "36-42-47-119-121-dag.flac"
        , "36-42-47-122-124-dag.flac"
        , "36-42-47-125-127-dag.flac"
        ]
    DagStaccato ->
        [ "24-30-35-0-4-dag+staccato.flac"
        , "24-30-35-5-9-dag+staccato.flac"
        , "24-30-35-10-14-dag+staccato.flac"
        , "24-30-35-15-19-dag+staccato.flac"
        , "24-30-35-20-24-dag+staccato.flac"
        , "24-30-35-25-29-dag+staccato.flac"
        , "24-30-35-30-34-dag+staccato.flac"
        , "24-30-35-35-39-dag+staccato.flac"
        , "24-30-35-40-44-dag+staccato.flac"
        , "24-30-35-45-49-dag+staccato.flac"
        , "24-30-35-50-54-dag+staccato.flac"
        , "24-30-35-55-59-dag+staccato.flac"
        , "24-30-35-60-64-dag+staccato.flac"
        , "24-30-35-65-69-dag+staccato.flac"
        , "24-30-35-70-74-dag+staccato.flac"
        , "24-30-35-75-79-dag+staccato.flac"
        , "24-30-35-80-84-dag+staccato.flac"
        , "24-30-35-85-89-dag+staccato.flac"
        , "24-30-35-90-94-dag+staccato.flac"
        , "24-30-35-95-99-dag+staccato.flac"
        , "24-30-35-100-103-dag+staccato.flac"
        , "24-30-35-104-107-dag+staccato.flac"
        , "24-30-35-108-111-dag+staccato.flac"
        , "24-30-35-112-115-dag+staccato.flac"
        , "24-30-35-116-119-dag+staccato.flac"
        , "24-30-35-120-123-dag+staccato.flac"
        , "24-30-35-124-127-dag+staccato.flac"
        ]
    Tek ->
        [ "48-54-59-0-3-tek.flac"
        , "48-54-59-4-7-tek.flac"
        , "48-54-59-8-11-tek.flac"
        , "48-54-59-12-15-tek.flac"
        , "48-54-59-16-19-tek.flac"
        , "48-54-59-20-23-tek.flac"
        , "48-54-59-24-27-tek.flac"
        , "48-54-59-28-31-tek.flac"
        , "48-54-59-32-35-tek.flac"
        , "48-54-59-36-39-tek.flac"
        , "48-54-59-40-43-tek.flac"
        , "48-54-59-44-47-tek.flac"
        , "48-54-59-48-51-tek.flac"
        , "48-54-59-52-55-tek.flac"
        , "48-54-59-56-59-tek.flac"
        , "48-54-59-60-63-tek.flac"
        , "48-54-59-64-67-tek.flac"
        , "48-54-59-68-70-tek.flac"
        , "48-54-59-71-73-tek.flac"
        , "48-54-59-74-76-tek.flac"
        , "48-54-59-77-79-tek.flac"
        , "48-54-59-80-82-tek.flac"
        , "48-54-59-83-85-tek.flac"
        , "48-54-59-86-88-tek.flac"
        , "48-54-59-89-91-tek.flac"
        , "48-54-59-92-94-tek.flac"
        , "48-54-59-95-97-tek.flac"
        , "48-54-59-98-100-tek.flac"
        , "48-54-59-101-103-tek.flac"
        , "48-54-59-104-106-tek.flac"
        , "48-54-59-107-109-tek.flac"
        , "48-54-59-110-112-tek.flac"
        , "48-54-59-113-115-tek.flac"
        , "48-54-59-116-118-tek.flac"
        , "48-54-59-119-121-tek.flac"
        , "48-54-59-122-124-tek.flac"
        , "48-54-59-125-127-tek.flac"
        ]
    Tut ->
        [ "60-66-71-0-3-tut.flac"
        , "60-66-71-4-7-tut.flac"
        , "60-66-71-8-11-tut.flac"
        , "60-66-71-12-15-tut.flac"
        , "60-66-71-16-19-tut.flac"
        , "60-66-71-20-23-tut.flac"
        , "60-66-71-24-27-tut.flac"
        , "60-66-71-28-31-tut.flac"
        , "60-66-71-32-35-tut.flac"
        , "60-66-71-36-39-tut.flac"
        , "60-66-71-40-43-tut.flac"
        , "60-66-71-44-47-tut.flac"
        , "60-66-71-48-51-tut.flac"
        , "60-66-71-52-55-tut.flac"
        , "60-66-71-56-58-tut.flac"
        , "60-66-71-59-61-tut.flac"
        , "60-66-71-62-64-tut.flac"
        , "60-66-71-65-67-tut.flac"
        , "60-66-71-68-70-tut.flac"
        , "60-66-71-71-73-tut.flac"
        , "60-66-71-74-76-tut.flac"
        , "60-66-71-77-79-tut.flac"
        , "60-66-71-80-82-tut.flac"
        , "60-66-71-83-85-tut.flac"
        , "60-66-71-86-88-tut.flac"
        , "60-66-71-89-91-tut.flac"
        , "60-66-71-92-94-tut.flac"
        , "60-66-71-95-97-tut.flac"
        , "60-66-71-98-100-tut.flac"
        , "60-66-71-101-103-tut.flac"
        , "60-66-71-104-106-tut.flac"
        , "60-66-71-107-109-tut.flac"
        , "60-66-71-110-112-tut.flac"
        , "60-66-71-113-115-tut.flac"
        , "60-66-71-116-118-tut.flac"
        , "60-66-71-119-121-tut.flac"
        , "60-66-71-122-124-tut.flac"
        , "60-66-71-125-127-tut.flac"
        ]
    Ka ->
        [ "72-78-83-0-3-ka.flac"
        , "72-78-83-4-7-ka.flac"
        , "72-78-83-8-11-ka.flac"
        , "72-78-83-12-15-ka.flac"
        , "72-78-83-16-19-ka.flac"
        , "72-78-83-20-23-ka.flac"
        , "72-78-83-24-27-ka.flac"
        , "72-78-83-28-31-ka.flac"
        , "72-78-83-32-35-ka.flac"
        , "72-78-83-36-39-ka.flac"
        , "72-78-83-40-43-ka.flac"
        , "72-78-83-44-47-ka.flac"
        , "72-78-83-48-51-ka.flac"
        , "72-78-83-52-55-ka.flac"
        , "72-78-83-56-59-ka.flac"
        , "72-78-83-60-63-ka.flac"
        , "72-78-83-64-67-ka.flac"
        , "72-78-83-68-71-ka.flac"
        , "72-78-83-72-75-ka.flac"
        , "72-78-83-76-79-ka.flac"
        , "72-78-83-80-82-ka.flac"
        , "72-78-83-83-85-ka.flac"
        , "72-78-83-86-88-ka.flac"
        , "72-78-83-89-91-ka.flac"
        , "72-78-83-92-94-ka.flac"
        , "72-78-83-95-97-ka.flac"
        , "72-78-83-98-100-ka.flac"
        , "72-78-83-101-103-ka.flac"
        , "72-78-83-104-106-ka.flac"
        , "72-78-83-107-109-ka.flac"
        , "72-78-83-110-112-ka.flac"
        , "72-78-83-113-115-ka.flac"
        , "72-78-83-116-118-ka.flac"
        , "72-78-83-119-121-ka.flac"
        , "72-78-83-122-124-ka.flac"
        , "72-78-83-125-127-ka.flac"
        ]
    Pak ->
        [ "96-102-107-0-3-pak.flac"
        , "96-102-107-4-7-pak.flac"
        , "96-102-107-8-11-pak.flac"
        , "96-102-107-12-15-pak.flac"
        , "96-102-107-16-19-pak.flac"
        , "96-102-107-20-23-pak.flac"
        , "96-102-107-24-27-pak.flac"
        , "96-102-107-28-31-pak.flac"
        , "96-102-107-32-35-pak.flac"
        , "96-102-107-36-39-pak.flac"
        , "96-102-107-40-43-pak.flac"
        , "96-102-107-44-46-pak.flac"
        , "96-102-107-47-49-pak.flac"
        , "96-102-107-50-52-pak.flac"
        , "96-102-107-53-55-pak.flac"
        , "96-102-107-56-58-pak.flac"
        , "96-102-107-59-61-pak.flac"
        , "96-102-107-62-64-pak.flac"
        , "96-102-107-65-67-pak.flac"
        , "96-102-107-68-70-pak.flac"
        , "96-102-107-71-73-pak.flac"
        , "96-102-107-74-76-pak.flac"
        , "96-102-107-77-79-pak.flac"
        , "96-102-107-80-82-pak.flac"
        , "96-102-107-83-85-pak.flac"
        , "96-102-107-86-88-pak.flac"
        , "96-102-107-89-91-pak.flac"
        , "96-102-107-92-94-pak.flac"
        , "96-102-107-95-97-pak.flac"
        , "96-102-107-98-100-pak.flac"
        , "96-102-107-101-103-pak.flac"
        , "96-102-107-104-106-pak.flac"
        , "96-102-107-107-109-pak.flac"
        , "96-102-107-110-112-pak.flac"
        , "96-102-107-113-115-pak.flac"
        , "96-102-107-116-118-pak.flac"
        , "96-102-107-119-121-pak.flac"
        , "96-102-107-122-124-pak.flac"
        , "96-102-107-125-127-pak.flac"
        ]
    Pang ->
        [ "84-90-95-0-3-pang.flac"
        , "84-90-95-4-7-pang.flac"
        , "84-90-95-8-11-pang.flac"
        , "84-90-95-12-15-pang.flac"
        , "84-90-95-16-19-pang.flac"
        , "84-90-95-20-23-pang.flac"
        , "84-90-95-24-27-pang.flac"
        , "84-90-95-28-31-pang.flac"
        , "84-90-95-32-35-pang.flac"
        , "84-90-95-36-39-pang.flac"
        , "84-90-95-40-43-pang.flac"
        , "84-90-95-44-47-pang.flac"
        , "84-90-95-48-51-pang.flac"
        , "84-90-95-52-55-pang.flac"
        , "84-90-95-56-59-pang.flac"
        , "84-90-95-60-63-pang.flac"
        , "84-90-95-64-67-pang.flac"
        , "84-90-95-68-71-pang.flac"
        , "84-90-95-72-75-pang.flac"
        , "84-90-95-76-79-pang.flac"
        , "84-90-95-80-83-pang.flac"
        , "84-90-95-84-87-pang.flac"
        , "84-90-95-88-91-pang.flac"
        , "84-90-95-92-95-pang.flac"
        , "84-90-95-96-99-pang.flac"
        , "84-90-95-100-103-pang.flac"
        , "84-90-95-104-106-pang.flac"
        , "84-90-95-107-109-pang.flac"
        , "84-90-95-110-112-pang.flac"
        , "84-90-95-113-115-pang.flac"
        , "84-90-95-116-118-pang.flac"
        , "84-90-95-119-121-pang.flac"
        , "84-90-95-122-124-pang.flac"
        , "84-90-95-125-127-pang.flac"
        ]
    TutL ->
        [ "108-114-119-0-4-tut+left.flac"
        , "108-114-119-5-9-tut+left.flac"
        , "108-114-119-10-14-tut+left.flac"
        , "108-114-119-15-19-tut+left.flac"
        , "108-114-119-20-24-tut+left.flac"
        , "108-114-119-25-29-tut+left.flac"
        , "108-114-119-30-34-tut+left.flac"
        , "108-114-119-35-39-tut+left.flac"
        , "108-114-119-40-43-tut+left.flac"
        , "108-114-119-44-47-tut+left.flac"
        , "108-114-119-48-51-tut+left.flac"
        , "108-114-119-52-55-tut+left.flac"
        , "108-114-119-56-59-tut+left.flac"
        , "108-114-119-60-63-tut+left.flac"
        , "108-114-119-64-67-tut+left.flac"
        , "108-114-119-68-71-tut+left.flac"
        , "108-114-119-72-75-tut+left.flac"
        , "108-114-119-76-79-tut+left.flac"
        , "108-114-119-80-83-tut+left.flac"
        , "108-114-119-84-87-tut+left.flac"
        , "108-114-119-88-91-tut+left.flac"
        , "108-114-119-92-95-tut+left.flac"
        , "108-114-119-96-99-tut+left.flac"
        , "108-114-119-100-103-tut+left.flac"
        , "108-114-119-104-107-tut+left.flac"
        , "108-114-119-108-111-tut+left.flac"
        , "108-114-119-112-115-tut+left.flac"
        , "108-114-119-116-119-tut+left.flac"
        , "108-114-119-120-123-tut+left.flac"
        , "108-114-119-124-127-tut+left.flac"
        ]
    DeL ->
        [ "108-114-119-0-4-de+left.flac"
        , "108-114-119-5-9-de+left.flac"
        , "108-114-119-10-14-de+left.flac"
        , "108-114-119-15-19-de+left.flac"
        , "108-114-119-20-24-de+left.flac"
        , "108-114-119-25-29-de+left.flac"
        , "108-114-119-30-34-de+left.flac"
        , "108-114-119-35-39-de+left.flac"
        , "108-114-119-40-44-de+left.flac"
        , "108-114-119-45-49-de+left.flac"
        , "108-114-119-50-54-de+left.flac"
        , "108-114-119-55-59-de+left.flac"
        , "108-114-119-60-64-de+left.flac"
        , "108-114-119-65-69-de+left.flac"
        , "108-114-119-70-74-de+left.flac"
        , "108-114-119-75-79-de+left.flac"
        , "108-114-119-80-84-de+left.flac"
        , "108-114-119-85-89-de+left.flac"
        , "108-114-119-90-94-de+left.flac"
        , "108-114-119-95-99-de+left.flac"
        , "108-114-119-100-104-de+left.flac"
        , "108-114-119-105-109-de+left.flac"
        , "108-114-119-110-114-de+left.flac"
        , "108-114-119-115-119-de+left.flac"
        , "108-114-119-120-123-de+left.flac"
        , "108-114-119-124-127-de+left.flac"
        ]
legongLanangSamples :: Articulation -> [FilePath]
legongLanangSamples = \case
    Plak ->
        [ "12-18-23-0-4-plak.flac"
        , "12-18-23-5-9-plak.flac"
        , "12-18-23-10-14-plak.flac"
        , "12-18-23-15-19-plak.flac"
        , "12-18-23-20-24-plak.flac"
        , "12-18-23-25-29-plak.flac"
        , "12-18-23-30-34-plak.flac"
        , "12-18-23-35-39-plak.flac"
        , "12-18-23-40-44-plak.flac"
        , "12-18-23-45-49-plak.flac"
        , "12-18-23-50-54-plak.flac"
        , "12-18-23-55-59-plak.flac"
        , "12-18-23-60-64-plak.flac"
        , "12-18-23-65-69-plak.flac"
        , "12-18-23-70-74-plak.flac"
        , "12-18-23-75-79-plak.flac"
        , "12-18-23-80-84-plak.flac"
        , "12-18-23-85-89-plak.flac"
        , "12-18-23-90-94-plak.flac"
        , "12-18-23-95-99-plak.flac"
        , "12-18-23-100-103-plak.flac"
        , "12-18-23-104-107-plak.flac"
        , "12-18-23-108-111-plak.flac"
        , "12-18-23-112-115-plak.flac"
        , "12-18-23-116-119-plak.flac"
        , "12-18-23-120-123-plak.flac"
        , "12-18-23-124-127-plak.flac"
        ]
    De ->
        [ "36-42-47-0-3-de.flac"
        , "36-42-47-4-7-de.flac"
        , "36-42-47-8-11-de.flac"
        , "36-42-47-12-15-de.flac"
        , "36-42-47-16-19-de.flac"
        , "36-42-47-20-23-de.flac"
        , "36-42-47-24-27-de.flac"
        , "36-42-47-28-31-de.flac"
        , "36-42-47-32-35-de.flac"
        , "36-42-47-36-39-de.flac"
        , "36-42-47-40-43-de.flac"
        , "36-42-47-44-47-de.flac"
        , "36-42-47-48-51-de.flac"
        , "36-42-47-52-55-de.flac"
        , "36-42-47-56-59-de.flac"
        , "36-42-47-60-63-de.flac"
        , "36-42-47-64-67-de.flac"
        , "36-42-47-68-71-de.flac"
        , "36-42-47-72-75-de.flac"
        , "36-42-47-76-79-de.flac"
        , "36-42-47-80-83-de.flac"
        , "36-42-47-84-87-de.flac"
        , "36-42-47-88-91-de.flac"
        , "36-42-47-92-95-de.flac"
        , "36-42-47-96-99-de.flac"
        , "36-42-47-100-103-de.flac"
        , "36-42-47-104-106-de.flac"
        , "36-42-47-107-109-de.flac"
        , "36-42-47-110-112-de.flac"
        , "36-42-47-113-115-de.flac"
        , "36-42-47-116-118-de.flac"
        , "36-42-47-119-121-de.flac"
        , "36-42-47-122-124-de.flac"
        , "36-42-47-125-127-de.flac"
        ]
    DeStaccato ->
        [ "12-18-23-0-4-de+staccato.flac"
        , "12-18-23-5-9-de+staccato.flac"
        , "12-18-23-10-14-de+staccato.flac"
        , "12-18-23-15-19-de+staccato.flac"
        , "12-18-23-20-24-de+staccato.flac"
        , "12-18-23-25-29-de+staccato.flac"
        , "12-18-23-30-34-de+staccato.flac"
        , "12-18-23-35-39-de+staccato.flac"
        , "12-18-23-40-44-de+staccato.flac"
        , "12-18-23-45-49-de+staccato.flac"
        , "12-18-23-50-54-de+staccato.flac"
        , "12-18-23-55-59-de+staccato.flac"
        , "12-18-23-60-64-de+staccato.flac"
        , "12-18-23-65-69-de+staccato.flac"
        , "12-18-23-70-74-de+staccato.flac"
        , "12-18-23-75-79-de+staccato.flac"
        , "12-18-23-80-84-de+staccato.flac"
        , "12-18-23-85-89-de+staccato.flac"
        , "12-18-23-90-94-de+staccato.flac"
        , "12-18-23-95-99-de+staccato.flac"
        , "12-18-23-100-104-de+staccato.flac"
        , "12-18-23-105-109-de+staccato.flac"
        , "12-18-23-110-114-de+staccato.flac"
        , "12-18-23-115-119-de+staccato.flac"
        , "12-18-23-120-123-de+staccato.flac"
        , "12-18-23-124-127-de+staccato.flac"
        ]
    DeThumb ->
        [ "24-30-35-0-4-de+thumb.flac"
        , "24-30-35-5-9-de+thumb.flac"
        , "24-30-35-10-14-de+thumb.flac"
        , "24-30-35-15-19-de+thumb.flac"
        , "24-30-35-20-24-de+thumb.flac"
        , "24-30-35-25-29-de+thumb.flac"
        , "24-30-35-30-34-de+thumb.flac"
        , "24-30-35-35-39-de+thumb.flac"
        , "24-30-35-40-44-de+thumb.flac"
        , "24-30-35-45-49-de+thumb.flac"
        , "24-30-35-50-54-de+thumb.flac"
        , "24-30-35-55-59-de+thumb.flac"
        , "24-30-35-60-64-de+thumb.flac"
        , "24-30-35-65-69-de+thumb.flac"
        , "24-30-35-70-74-de+thumb.flac"
        , "24-30-35-75-79-de+thumb.flac"
        , "24-30-35-80-83-de+thumb.flac"
        , "24-30-35-84-87-de+thumb.flac"
        , "24-30-35-88-91-de+thumb.flac"
        , "24-30-35-92-95-de+thumb.flac"
        , "24-30-35-96-99-de+thumb.flac"
        , "24-30-35-100-103-de+thumb.flac"
        , "24-30-35-104-107-de+thumb.flac"
        , "24-30-35-108-111-de+thumb.flac"
        , "24-30-35-112-115-de+thumb.flac"
        , "24-30-35-116-119-de+thumb.flac"
        , "24-30-35-120-123-de+thumb.flac"
        , "24-30-35-124-127-de+thumb.flac"
        ]
    DeClosed ->
        [ "48-54-59-0-5-de+closed.flac"
        , "48-54-59-6-11-de+closed.flac"
        , "48-54-59-12-17-de+closed.flac"
        , "48-54-59-18-22-de+closed.flac"
        , "48-54-59-23-27-de+closed.flac"
        , "48-54-59-28-32-de+closed.flac"
        , "48-54-59-33-37-de+closed.flac"
        , "48-54-59-38-42-de+closed.flac"
        , "48-54-59-43-47-de+closed.flac"
        , "48-54-59-48-52-de+closed.flac"
        , "48-54-59-53-57-de+closed.flac"
        , "48-54-59-58-62-de+closed.flac"
        , "48-54-59-63-67-de+closed.flac"
        , "48-54-59-68-72-de+closed.flac"
        , "48-54-59-73-77-de+closed.flac"
        , "48-54-59-78-82-de+closed.flac"
        , "48-54-59-83-87-de+closed.flac"
        , "48-54-59-88-92-de+closed.flac"
        , "48-54-59-93-97-de+closed.flac"
        , "48-54-59-98-102-de+closed.flac"
        , "48-54-59-103-107-de+closed.flac"
        , "48-54-59-108-112-de+closed.flac"
        , "48-54-59-113-117-de+closed.flac"
        , "48-54-59-118-122-de+closed.flac"
        , "48-54-59-123-127-de+closed.flac"
        ]
    Dag ->
        [ "36-42-47-0-4-dag.flac"
        , "36-42-47-5-9-dag.flac"
        , "36-42-47-10-14-dag.flac"
        , "36-42-47-15-19-dag.flac"
        , "36-42-47-20-24-dag.flac"
        , "36-42-47-25-29-dag.flac"
        , "36-42-47-30-34-dag.flac"
        , "36-42-47-35-39-dag.flac"
        , "36-42-47-40-44-dag.flac"
        , "36-42-47-45-49-dag.flac"
        , "36-42-47-50-54-dag.flac"
        , "36-42-47-55-59-dag.flac"
        , "36-42-47-60-64-dag.flac"
        , "36-42-47-65-69-dag.flac"
        , "36-42-47-70-74-dag.flac"
        , "36-42-47-75-79-dag.flac"
        , "36-42-47-80-83-dag.flac"
        , "36-42-47-84-87-dag.flac"
        , "36-42-47-88-91-dag.flac"
        , "36-42-47-92-95-dag.flac"
        , "36-42-47-96-99-dag.flac"
        , "36-42-47-100-103-dag.flac"
        , "36-42-47-104-107-dag.flac"
        , "36-42-47-108-111-dag.flac"
        , "36-42-47-112-115-dag.flac"
        , "36-42-47-116-119-dag.flac"
        , "36-42-47-120-123-dag.flac"
        , "36-42-47-124-127-dag.flac"
        ]
    DagStaccato ->
        [ "24-30-35-0-5-dag+staccato.flac"
        , "24-30-35-6-11-dag+staccato.flac"
        , "24-30-35-12-17-dag+staccato.flac"
        , "24-30-35-18-23-dag+staccato.flac"
        , "24-30-35-24-29-dag+staccato.flac"
        , "24-30-35-30-35-dag+staccato.flac"
        , "24-30-35-36-41-dag+staccato.flac"
        , "24-30-35-42-47-dag+staccato.flac"
        , "24-30-35-48-52-dag+staccato.flac"
        , "24-30-35-53-57-dag+staccato.flac"
        , "24-30-35-58-62-dag+staccato.flac"
        , "24-30-35-63-67-dag+staccato.flac"
        , "24-30-35-68-72-dag+staccato.flac"
        , "24-30-35-73-77-dag+staccato.flac"
        , "24-30-35-78-82-dag+staccato.flac"
        , "24-30-35-83-87-dag+staccato.flac"
        , "24-30-35-88-92-dag+staccato.flac"
        , "24-30-35-93-97-dag+staccato.flac"
        , "24-30-35-98-102-dag+staccato.flac"
        , "24-30-35-103-107-dag+staccato.flac"
        , "24-30-35-108-112-dag+staccato.flac"
        , "24-30-35-113-117-dag+staccato.flac"
        , "24-30-35-118-122-dag+staccato.flac"
        , "24-30-35-123-127-dag+staccato.flac"
        ]
    Tek ->
        [ "48-54-59-0-4-tek.flac"
        , "48-54-59-5-9-tek.flac"
        , "48-54-59-10-14-tek.flac"
        , "48-54-59-15-19-tek.flac"
        , "48-54-59-20-23-tek.flac"
        , "48-54-59-24-27-tek.flac"
        , "48-54-59-28-31-tek.flac"
        , "48-54-59-32-35-tek.flac"
        , "48-54-59-36-39-tek.flac"
        , "48-54-59-40-43-tek.flac"
        , "48-54-59-44-47-tek.flac"
        , "48-54-59-48-51-tek.flac"
        , "48-54-59-52-55-tek.flac"
        , "48-54-59-56-59-tek.flac"
        , "48-54-59-60-63-tek.flac"
        , "48-54-59-64-67-tek.flac"
        , "48-54-59-68-71-tek.flac"
        , "48-54-59-72-75-tek.flac"
        , "48-54-59-76-79-tek.flac"
        , "48-54-59-80-83-tek.flac"
        , "48-54-59-84-87-tek.flac"
        , "48-54-59-88-91-tek.flac"
        , "48-54-59-92-95-tek.flac"
        , "48-54-59-96-99-tek.flac"
        , "48-54-59-100-103-tek.flac"
        , "48-54-59-104-107-tek.flac"
        , "48-54-59-108-111-tek.flac"
        , "48-54-59-112-115-tek.flac"
        , "48-54-59-116-119-tek.flac"
        , "48-54-59-120-123-tek.flac"
        , "48-54-59-124-127-tek.flac"
        ]
    Tut ->
        [ "60-66-71-0-5-tut.flac"
        , "60-66-71-6-11-tut.flac"
        , "60-66-71-12-17-tut.flac"
        , "60-66-71-18-22-tut.flac"
        , "60-66-71-23-27-tut.flac"
        , "60-66-71-28-32-tut.flac"
        , "60-66-71-33-37-tut.flac"
        , "60-66-71-38-42-tut.flac"
        , "60-66-71-43-47-tut.flac"
        , "60-66-71-48-52-tut.flac"
        , "60-66-71-53-57-tut.flac"
        , "60-66-71-58-62-tut.flac"
        , "60-66-71-63-67-tut.flac"
        , "60-66-71-68-72-tut.flac"
        , "60-66-71-73-77-tut.flac"
        , "60-66-71-78-82-tut.flac"
        , "60-66-71-83-87-tut.flac"
        , "60-66-71-88-92-tut.flac"
        , "60-66-71-93-97-tut.flac"
        , "60-66-71-98-102-tut.flac"
        , "60-66-71-103-107-tut.flac"
        , "60-66-71-108-112-tut.flac"
        , "60-66-71-113-117-tut.flac"
        , "60-66-71-118-122-tut.flac"
        , "60-66-71-123-127-tut.flac"
        ]
    Ka ->
        [ "72-78-83-0-4-ka.flac"
        , "72-78-83-5-9-ka.flac"
        , "72-78-83-10-14-ka.flac"
        , "72-78-83-15-19-ka.flac"
        , "72-78-83-20-24-ka.flac"
        , "72-78-83-25-29-ka.flac"
        , "72-78-83-30-34-ka.flac"
        , "72-78-83-35-39-ka.flac"
        , "72-78-83-40-44-ka.flac"
        , "72-78-83-45-49-ka.flac"
        , "72-78-83-50-54-ka.flac"
        , "72-78-83-55-59-ka.flac"
        , "72-78-83-60-64-ka.flac"
        , "72-78-83-65-69-ka.flac"
        , "72-78-83-70-74-ka.flac"
        , "72-78-83-75-79-ka.flac"
        , "72-78-83-80-83-ka.flac"
        , "72-78-83-84-87-ka.flac"
        , "72-78-83-88-91-ka.flac"
        , "72-78-83-92-95-ka.flac"
        , "72-78-83-96-99-ka.flac"
        , "72-78-83-100-103-ka.flac"
        , "72-78-83-104-107-ka.flac"
        , "72-78-83-108-111-ka.flac"
        , "72-78-83-112-115-ka.flac"
        , "72-78-83-116-119-ka.flac"
        , "72-78-83-120-123-ka.flac"
        , "72-78-83-124-127-ka.flac"
        ]
    Pak ->
        [ "96-102-107-0-4-pak.flac"
        , "96-102-107-5-9-pak.flac"
        , "96-102-107-10-14-pak.flac"
        , "96-102-107-15-19-pak.flac"
        , "96-102-107-20-24-pak.flac"
        , "96-102-107-25-29-pak.flac"
        , "96-102-107-30-34-pak.flac"
        , "96-102-107-35-39-pak.flac"
        , "96-102-107-40-43-pak.flac"
        , "96-102-107-44-47-pak.flac"
        , "96-102-107-48-51-pak.flac"
        , "96-102-107-52-55-pak.flac"
        , "96-102-107-56-59-pak.flac"
        , "96-102-107-60-63-pak.flac"
        , "96-102-107-64-67-pak.flac"
        , "96-102-107-68-71-pak.flac"
        , "96-102-107-72-75-pak.flac"
        , "96-102-107-76-79-pak.flac"
        , "96-102-107-80-83-pak.flac"
        , "96-102-107-84-87-pak.flac"
        , "96-102-107-88-91-pak.flac"
        , "96-102-107-92-95-pak.flac"
        , "96-102-107-96-99-pak.flac"
        , "96-102-107-100-103-pak.flac"
        , "96-102-107-104-107-pak.flac"
        , "96-102-107-108-111-pak.flac"
        , "96-102-107-112-115-pak.flac"
        , "96-102-107-116-119-pak.flac"
        , "96-102-107-120-123-pak.flac"
        , "96-102-107-124-127-pak.flac"
        ]
    Pang ->
        [ "84-90-95-0-5-pang.flac"
        , "84-90-95-6-11-pang.flac"
        , "84-90-95-12-17-pang.flac"
        , "84-90-95-18-23-pang.flac"
        , "84-90-95-24-29-pang.flac"
        , "84-90-95-30-35-pang.flac"
        , "84-90-95-36-41-pang.flac"
        , "84-90-95-42-47-pang.flac"
        , "84-90-95-48-52-pang.flac"
        , "84-90-95-53-57-pang.flac"
        , "84-90-95-58-62-pang.flac"
        , "84-90-95-63-67-pang.flac"
        , "84-90-95-68-72-pang.flac"
        , "84-90-95-73-77-pang.flac"
        , "84-90-95-78-82-pang.flac"
        , "84-90-95-83-87-pang.flac"
        , "84-90-95-88-92-pang.flac"
        , "84-90-95-93-97-pang.flac"
        , "84-90-95-98-102-pang.flac"
        , "84-90-95-103-107-pang.flac"
        , "84-90-95-108-112-pang.flac"
        , "84-90-95-113-117-pang.flac"
        , "84-90-95-118-122-pang.flac"
        , "84-90-95-123-127-pang.flac"
        ]
    TutL ->
        [ "108-114-119-0-4-tut+left.flac"
        , "108-114-119-5-9-tut+left.flac"
        , "108-114-119-10-14-tut+left.flac"
        , "108-114-119-15-19-tut+left.flac"
        , "108-114-119-20-24-tut+left.flac"
        , "108-114-119-25-29-tut+left.flac"
        , "108-114-119-30-34-tut+left.flac"
        , "108-114-119-35-39-tut+left.flac"
        , "108-114-119-40-44-tut+left.flac"
        , "108-114-119-45-49-tut+left.flac"
        , "108-114-119-50-54-tut+left.flac"
        , "108-114-119-55-59-tut+left.flac"
        , "108-114-119-60-64-tut+left.flac"
        , "108-114-119-65-69-tut+left.flac"
        , "108-114-119-70-74-tut+left.flac"
        , "108-114-119-75-79-tut+left.flac"
        , "108-114-119-80-84-tut+left.flac"
        , "108-114-119-85-89-tut+left.flac"
        , "108-114-119-90-94-tut+left.flac"
        , "108-114-119-95-99-tut+left.flac"
        , "108-114-119-100-104-tut+left.flac"
        , "108-114-119-105-109-tut+left.flac"
        , "108-114-119-110-114-tut+left.flac"
        , "108-114-119-115-119-tut+left.flac"
        , "108-114-119-120-123-tut+left.flac"
        , "108-114-119-124-127-tut+left.flac"
        ]
    DeL ->
        [ "108-114-119-0-4-de+left.flac"
        , "108-114-119-5-9-de+left.flac"
        , "108-114-119-10-14-de+left.flac"
        , "108-114-119-15-19-de+left.flac"
        , "108-114-119-20-24-de+left.flac"
        , "108-114-119-25-29-de+left.flac"
        , "108-114-119-30-34-de+left.flac"
        , "108-114-119-35-39-de+left.flac"
        , "108-114-119-40-44-de+left.flac"
        , "108-114-119-45-49-de+left.flac"
        , "108-114-119-50-54-de+left.flac"
        , "108-114-119-55-59-de+left.flac"
        , "108-114-119-60-63-de+left.flac"
        , "108-114-119-64-67-de+left.flac"
        , "108-114-119-68-71-de+left.flac"
        , "108-114-119-72-75-de+left.flac"
        , "108-114-119-76-79-de+left.flac"
        , "108-114-119-80-83-de+left.flac"
        , "108-114-119-84-87-de+left.flac"
        , "108-114-119-88-91-de+left.flac"
        , "108-114-119-92-95-de+left.flac"
        , "108-114-119-96-99-de+left.flac"
        , "108-114-119-100-103-de+left.flac"
        , "108-114-119-104-107-de+left.flac"
        , "108-114-119-108-111-de+left.flac"
        , "108-114-119-112-115-de+left.flac"
        , "108-114-119-116-119-de+left.flac"
        , "108-114-119-120-123-de+left.flac"
        , "108-114-119-124-127-de+left.flac"
        ]
