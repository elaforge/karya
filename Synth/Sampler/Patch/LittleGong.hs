-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Sampler.Patch.LittleGong (patches) where
import qualified Cmd.Instrument.CUtil as CUtil
import qualified Derive.Attrs as Attrs
import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.Patch.Lib.Drum as Drum
import qualified Synth.Sampler.Patch.Lib.Util as Util

import           Global


patches :: [Patch.DbPatch]
patches = (:[]) $ Patch.DbPatch $
    Drum.patch dir patchName strokeMap convertMap (const CUtil.call_config)
    where dir = untxt patchName

patchName :: Text
patchName = "little-gong"

data Articulation = OpenCenter | MuteCenter | OpenEdge | MuteEdge
    deriving (Eq, Ord, Show, Enum, Bounded)

convertMap :: Drum.ConvertMap Articulation
convertMap = Drum.ConvertMap
    { _dynRange = (0.8, 1.15)
    , _naturalNn = Just (const 79.92) -- 5g#
    , _muteTime = Just 0.15
    , _convertAttributeMap = Drum._attributeMap strokeMap
    , _getFilename = Drum.variableDynamic 0.15 articulationSamples
    }

strokeMap :: Drum.StrokeMap Articulation
strokeMap = Drum.strokeMapTable stops
    [ ('1', "+", Attrs.mute <> Attrs.center, MuteCenter, closed)
    , ('q', "o", mempty, OpenCenter,  open)
    , ('2', "/", Attrs.mute <> Attrs.edge, MuteEdge, closed)
    , ('w', "T", Attrs.edge, OpenEdge, open)
    ]
    where
    stops = [(closed, [open])]
    open = "open"
    closed = "closed"

-- | Generate 'articulationSamples'.
_makeArticulationSamples :: IO ()
_makeArticulationSamples = Drum.makeFileList (untxt patchName)
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
