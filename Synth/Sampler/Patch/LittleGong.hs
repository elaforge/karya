-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Synth.Sampler.Patch.LittleGong where
import           System.FilePath ((</>))

import qualified Util.Num as Num
import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.ImInst as ImInst
import qualified Derive.Attrs as Attrs
import qualified Instrument.Common as Common
import qualified Perform.Im.Patch as Im.Patch
import qualified Perform.Pitch as Pitch
import qualified Synth.Sampler.Patch as Patch
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
    , Patch._preprocess = Drum.inferDuration smap
    , Patch._karyaPatch = CUtil.im_drum_patch (Drum._strokes smap) $
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
    code = CUtil.drum_code thru Nothing (Drum._strokes smap)
    thru = Util.imThruFunction dir convert
    dir = untxt patchName

patchName :: Text
patchName = "little-gong"

data Articulation = OpenCenter | MuteCenter | OpenEdge | MuteEdge
    deriving (Eq, Ord, Show, Enum, Bounded)

attributeMap :: Common.AttributeMap Articulation
attributeMap = Drum._attributeMap smap

smap :: Drum.StrokeMap Articulation
smap = Drum.strokeMapTable stops
    [ ('1', "+", Attrs.mute <> Attrs.center, MuteCenter, closed)
    , ('q', "o", mempty, OpenCenter,  open)
    , ('2', "/", Attrs.mute <> Attrs.edge, MuteEdge, closed)
    , ('w', "T", Attrs.edge, OpenEdge, open)
    ]
    where
    stops = [(closed, [open])]
    open = "open"
    closed = "closed"

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
        { Sample.envelope = Util.asr noteDyn muteTime note
        , Sample.ratios = Signal.constant $ Sample.pitchToRatio naturalNn noteNn
        }

-- | A note may pick a sample of this much dyn difference on either side.
variationRange :: Signal.Y
variationRange = 0.15

minDyn :: Signal.Y
minDyn = 0.8

maxDyn :: Signal.Y
maxDyn = 1.15

naturalNn :: Pitch.NoteNumber
naturalNn = 79.92 -- 5g#

-- | Time to mute at the end of a note.
muteTime :: RealTime
muteTime = 0.15

-- | Generate 'articulationSamples'.
makeArticulationSamples :: IO ()
makeArticulationSamples = Drum.makeFileList (untxt patchName)
    (map show (Util.enumAll :: [Articulation])) "articulationSamples"

articulationSamples :: Articulation -> [FilePath]
articulationSamples = \case
    OpenCenter ->
        [ "open+center-001.flac"
        , "open+center-002.flac"
        , "open+center-003.flac"
        , "open+center-004.flac"
        , "open+center-005.flac"
        , "open+center-006.flac"
        , "open+center-007.flac"
        , "open+center-008.flac"
        , "open+center-009.flac"
        , "open+center-010.flac"
        , "open+center-011.flac"
        , "open+center-012.flac"
        , "open+center-013.flac"
        , "open+center-014.flac"
        , "open+center-015.flac"
        , "open+center-016.flac"
        , "open+center-017.flac"
        , "open+center-018.flac"
        ]
    MuteCenter ->
        [ "mute+center-001.flac"
        , "mute+center-002.flac"
        , "mute+center-003.flac"
        , "mute+center-004.flac"
        , "mute+center-005.flac"
        , "mute+center-006.flac"
        , "mute+center-007.flac"
        , "mute+center-008.flac"
        , "mute+center-009.flac"
        , "mute+center-010.flac"
        , "mute+center-011.flac"
        , "mute+center-012.flac"
        , "mute+center-013.flac"
        , "mute+center-014.flac"
        , "mute+center-015.flac"
        , "mute+center-016.flac"
        , "mute+center-017.flac"
        , "mute+center-018.flac"
        ]
    OpenEdge ->
        [ "open+edge-001.flac"
        , "open+edge-002.flac"
        , "open+edge-003.flac"
        , "open+edge-004.flac"
        , "open+edge-005.flac"
        , "open+edge-006.flac"
        , "open+edge-007.flac"
        , "open+edge-008.flac"
        , "open+edge-009.flac"
        , "open+edge-010.flac"
        , "open+edge-011.flac"
        , "open+edge-012.flac"
        , "open+edge-013.flac"
        , "open+edge-014.flac"
        ]
    MuteEdge ->
        [ "mute+edge-001.flac"
        , "mute+edge-002.flac"
        , "mute+edge-003.flac"
        , "mute+edge-004.flac"
        , "mute+edge-005.flac"
        , "mute+edge-006.flac"
        , "mute+edge-007.flac"
        , "mute+edge-008.flac"
        , "mute+edge-009.flac"
        , "mute+edge-010.flac"
        , "mute+edge-011.flac"
        , "mute+edge-012.flac"
        ]
