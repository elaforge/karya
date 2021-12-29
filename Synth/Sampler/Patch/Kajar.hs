-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Sampler.Patch.Kajar (patches) where
import qualified Cmd.Instrument.CUtil as CUtil
import qualified Derive.Attrs as Attrs
import qualified Derive.C.Bali.Gong as Gong
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Derive as Derive
import qualified Derive.Eval as Eval
import qualified Derive.Instrument.DUtil as DUtil

import qualified Perform.RealTime as RealTime
import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.Patch.Lib.Drum as Drum
import           Synth.Sampler.Patch.Lib.Drum (Call(..))
import qualified Synth.Sampler.Patch.Lib.Util as Util

import qualified Ui.Meter.Meter as Meter

import           Global


patches :: [Patch.DbPatch]
patches = (:[]) $ Patch.DbPatch $
    Drum.patch dir patchName strokeMap convertMap (const config)
    where
    config = CUtil.call_config { CUtil._tuning_control = Just "kajar-tune" }
    dir = untxt patchName

patchName :: Text
patchName = "kajar"

data Articulation =
    CenterClosed | CenterOpen | Damp | RimClosed | RimOpen | RimStaccato
    deriving (Eq, Ord, Show, Enum, Bounded)

-- copied from User.Elaforge.Instrument.Kontakt.Gong

strokeMap :: Drum.StrokeMap Articulation
strokeMap = Drum.replaceSoft 0.75 $ Drum.strokeMapTable stops
    [ ('q', "P", Stroke (Attrs.rim <> Attrs.closed)     RimClosed closed)

    , ('a', "+/", Stroke (Attrs.rim <> Attrs.staccato)  RimStaccato open)
    , ('z', "+", Stroke (Attrs.rim <> Attrs.open)       RimOpen open)
    , ('s', ".", Stroke (Attrs.center <> Attrs.closed <> Attrs.soft)
                        CenterClosed closed)
    , ('x', "o", Stroke (Attrs.center <> Attrs.closed)  CenterClosed closed)
    , ('c', "oo", Call $
        DUtil.doubled_call "o" "oo" DUtil.After (RealTime.seconds 0.09) 0.75)
    , ('f', "o..", Call c_nruk)

    -- This is not commonly used.
    , ('v', "c", Stroke (Attrs.center <> Attrs.open)    CenterOpen open)
    -- I think the idea was to use this to damp open strokes.
    , ('m', "m", Stroke Attrs.damp                      Damp closed)

    , (' ', "k", Call $
        Gong.make_cycle "kajar" (Just (Left "o")) (Just (Left Meter.Q)))
    ]
    where
    stops = [(closed, [open])]
    open = "open"
    closed = "closed"

convertMap :: Drum.ConvertMap Articulation
convertMap = Drum.ConvertMap
    { _dynRange = (0.5, 1)
    , _naturalNn = Nothing
    , _muteTime = Just 0.05
    , _getFilename = Drum.variableDynamic 0.15 articulationSamples
    , _allFilenames = Drum.allFilenames 171 articulationSamples
    }

-- * calls

-- copied from User.Elaforge.Instrument.Kontakt.Gong

c_nruk :: Derive.Generator Derive.Note
c_nruk = Gong.nruk_generator Module.instrument "nruk" "Nruktuk on `o`." $
    Sub.inverting $ \args -> do
        gen <- Eval.get_generator "o"
        Eval.apply_generator (Derive.passed_ctx args) gen []

-- * articulationSamples

_makeArticulationSamples :: IO ()
_makeArticulationSamples = Drum.makeFileList (untxt patchName)
    (map show (Util.enumAll :: [Articulation])) "articulationSamples"

-- generated
articulationSamples :: Articulation -> [FilePath]
articulationSamples = \case
    CenterClosed ->
        [ "24-30-35-0-4-center+closed.flac"
        , "24-30-35-5-9-center+closed.flac"
        , "24-30-35-10-14-center+closed.flac"
        , "24-30-35-15-19-center+closed.flac"
        , "24-30-35-20-24-center+closed.flac"
        , "24-30-35-25-29-center+closed.flac"
        , "24-30-35-30-34-center+closed.flac"
        , "24-30-35-35-39-center+closed.flac"
        , "24-30-35-40-43-center+closed.flac"
        , "24-30-35-44-47-center+closed.flac"
        , "24-30-35-48-51-center+closed.flac"
        , "24-30-35-52-55-center+closed.flac"
        , "24-30-35-56-59-center+closed.flac"
        , "24-30-35-60-63-center+closed.flac"
        , "24-30-35-64-67-center+closed.flac"
        , "24-30-35-68-71-center+closed.flac"
        , "24-30-35-72-75-center+closed.flac"
        , "24-30-35-76-79-center+closed.flac"
        , "24-30-35-80-83-center+closed.flac"
        , "24-30-35-84-87-center+closed.flac"
        , "24-30-35-88-91-center+closed.flac"
        , "24-30-35-92-95-center+closed.flac"
        , "24-30-35-96-99-center+closed.flac"
        , "24-30-35-100-103-center+closed.flac"
        , "24-30-35-104-107-center+closed.flac"
        , "24-30-35-108-111-center+closed.flac"
        , "24-30-35-112-115-center+closed.flac"
        , "24-30-35-116-119-center+closed.flac"
        , "24-30-35-120-123-center+closed.flac"
        , "24-30-35-124-127-center+closed.flac"
        ]
    CenterOpen ->
        [ "36-42-47-0-4-center+open.flac"
        , "36-42-47-5-9-center+open.flac"
        , "36-42-47-10-14-center+open.flac"
        , "36-42-47-15-19-center+open.flac"
        , "36-42-47-20-24-center+open.flac"
        , "36-42-47-25-29-center+open.flac"
        , "36-42-47-30-34-center+open.flac"
        , "36-42-47-35-39-center+open.flac"
        , "36-42-47-40-44-center+open.flac"
        , "36-42-47-45-49-center+open.flac"
        , "36-42-47-50-54-center+open.flac"
        , "36-42-47-55-59-center+open.flac"
        , "36-42-47-60-64-center+open.flac"
        , "36-42-47-65-69-center+open.flac"
        , "36-42-47-70-74-center+open.flac"
        , "36-42-47-75-79-center+open.flac"
        , "36-42-47-80-83-center+open.flac"
        , "36-42-47-84-87-center+open.flac"
        , "36-42-47-88-91-center+open.flac"
        , "36-42-47-92-95-center+open.flac"
        , "36-42-47-96-99-center+open.flac"
        , "36-42-47-100-103-center+open.flac"
        , "36-42-47-104-107-center+open.flac"
        , "36-42-47-108-111-center+open.flac"
        , "36-42-47-112-115-center+open.flac"
        , "36-42-47-116-119-center+open.flac"
        , "36-42-47-120-123-center+open.flac"
        , "36-42-47-124-127-center+open.flac"
        ]
    Damp ->
        [ "84-90-95-0-14-damp.flac"
        , "84-90-95-15-29-damp.flac"
        , "84-90-95-30-43-damp.flac"
        , "84-90-95-44-57-damp.flac"
        , "84-90-95-58-71-damp.flac"
        , "84-90-95-72-85-damp.flac"
        , "84-90-95-86-99-damp.flac"
        , "84-90-95-100-113-damp.flac"
        , "84-90-95-114-127-damp.flac"
        ]
    RimClosed ->
        [ "48-54-59-0-3-rim+closed.flac"
        , "48-54-59-4-7-rim+closed.flac"
        , "48-54-59-8-10-rim+closed.flac"
        , "48-54-59-11-13-rim+closed.flac"
        , "48-54-59-14-16-rim+closed.flac"
        , "48-54-59-17-19-rim+closed.flac"
        , "48-54-59-20-22-rim+closed.flac"
        , "48-54-59-23-25-rim+closed.flac"
        , "48-54-59-26-28-rim+closed.flac"
        , "48-54-59-29-31-rim+closed.flac"
        , "48-54-59-32-34-rim+closed.flac"
        , "48-54-59-35-37-rim+closed.flac"
        , "48-54-59-38-40-rim+closed.flac"
        , "48-54-59-41-43-rim+closed.flac"
        , "48-54-59-44-46-rim+closed.flac"
        , "48-54-59-47-49-rim+closed.flac"
        , "48-54-59-50-52-rim+closed.flac"
        , "48-54-59-53-55-rim+closed.flac"
        , "48-54-59-56-58-rim+closed.flac"
        , "48-54-59-59-61-rim+closed.flac"
        , "48-54-59-62-64-rim+closed.flac"
        , "48-54-59-65-67-rim+closed.flac"
        , "48-54-59-68-70-rim+closed.flac"
        , "48-54-59-71-73-rim+closed.flac"
        , "48-54-59-74-76-rim+closed.flac"
        , "48-54-59-77-79-rim+closed.flac"
        , "48-54-59-80-82-rim+closed.flac"
        , "48-54-59-83-85-rim+closed.flac"
        , "48-54-59-86-88-rim+closed.flac"
        , "48-54-59-89-91-rim+closed.flac"
        , "48-54-59-92-94-rim+closed.flac"
        , "48-54-59-95-97-rim+closed.flac"
        , "48-54-59-98-100-rim+closed.flac"
        , "48-54-59-101-103-rim+closed.flac"
        , "48-54-59-104-106-rim+closed.flac"
        , "48-54-59-107-109-rim+closed.flac"
        , "48-54-59-110-112-rim+closed.flac"
        , "48-54-59-113-115-rim+closed.flac"
        , "48-54-59-116-118-rim+closed.flac"
        , "48-54-59-119-121-rim+closed.flac"
        , "48-54-59-122-124-rim+closed.flac"
        , "48-54-59-125-127-rim+closed.flac"
        ]
    RimOpen ->
        [ "60-66-71-0-5-rim+open.flac"
        , "60-66-71-6-11-rim+open.flac"
        , "60-66-71-12-17-rim+open.flac"
        , "60-66-71-18-23-rim+open.flac"
        , "60-66-71-24-29-rim+open.flac"
        , "60-66-71-30-35-rim+open.flac"
        , "60-66-71-36-41-rim+open.flac"
        , "60-66-71-42-47-rim+open.flac"
        , "60-66-71-48-53-rim+open.flac"
        , "60-66-71-54-59-rim+open.flac"
        , "60-66-71-60-65-rim+open.flac"
        , "60-66-71-66-71-rim+open.flac"
        , "60-66-71-72-77-rim+open.flac"
        , "60-66-71-78-83-rim+open.flac"
        , "60-66-71-84-89-rim+open.flac"
        , "60-66-71-90-95-rim+open.flac"
        , "60-66-71-96-101-rim+open.flac"
        , "60-66-71-102-107-rim+open.flac"
        , "60-66-71-108-112-rim+open.flac"
        , "60-66-71-113-117-rim+open.flac"
        , "60-66-71-118-122-rim+open.flac"
        , "60-66-71-123-127-rim+open.flac"
        ]
    RimStaccato ->
        [ "72-78-83-0-3-rim+staccato.flac"
        , "72-78-83-4-7-rim+staccato.flac"
        , "72-78-83-8-11-rim+staccato.flac"
        , "72-78-83-12-15-rim+staccato.flac"
        , "72-78-83-16-19-rim+staccato.flac"
        , "72-78-83-20-23-rim+staccato.flac"
        , "72-78-83-24-27-rim+staccato.flac"
        , "72-78-83-28-31-rim+staccato.flac"
        , "72-78-83-32-34-rim+staccato.flac"
        , "72-78-83-35-37-rim+staccato.flac"
        , "72-78-83-38-40-rim+staccato.flac"
        , "72-78-83-41-43-rim+staccato.flac"
        , "72-78-83-44-46-rim+staccato.flac"
        , "72-78-83-47-49-rim+staccato.flac"
        , "72-78-83-50-52-rim+staccato.flac"
        , "72-78-83-53-55-rim+staccato.flac"
        , "72-78-83-56-58-rim+staccato.flac"
        , "72-78-83-59-61-rim+staccato.flac"
        , "72-78-83-62-64-rim+staccato.flac"
        , "72-78-83-65-67-rim+staccato.flac"
        , "72-78-83-68-70-rim+staccato.flac"
        , "72-78-83-71-73-rim+staccato.flac"
        , "72-78-83-74-76-rim+staccato.flac"
        , "72-78-83-77-79-rim+staccato.flac"
        , "72-78-83-80-82-rim+staccato.flac"
        , "72-78-83-83-85-rim+staccato.flac"
        , "72-78-83-86-88-rim+staccato.flac"
        , "72-78-83-89-91-rim+staccato.flac"
        , "72-78-83-92-94-rim+staccato.flac"
        , "72-78-83-95-97-rim+staccato.flac"
        , "72-78-83-98-100-rim+staccato.flac"
        , "72-78-83-101-103-rim+staccato.flac"
        , "72-78-83-104-106-rim+staccato.flac"
        , "72-78-83-107-109-rim+staccato.flac"
        , "72-78-83-110-112-rim+staccato.flac"
        , "72-78-83-113-115-rim+staccato.flac"
        , "72-78-83-116-118-rim+staccato.flac"
        , "72-78-83-119-121-rim+staccato.flac"
        , "72-78-83-122-124-rim+staccato.flac"
        , "72-78-83-125-127-rim+staccato.flac"
        ]
