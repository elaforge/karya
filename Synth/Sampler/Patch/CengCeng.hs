-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Sampler.Patch.CengCeng (patches) where
import qualified Util.Seq as Seq
import qualified Cmd.Instrument.CUtil as CUtil
import qualified Derive.Attrs as Attrs
import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.Patch.Lib.Drum as Drum
import qualified Synth.Sampler.Patch.Lib.Util as Util
import qualified Synth.Shared.Signal as Signal


patches :: [Patch.DbPatch]
patches = map Patch.DbPatch
    [ Drum.patch "cengceng/kopyak" "cengceng-kopyak"
        strokeMap convertMap (const config)
    ]
    where
    config = CUtil.call_config
        { CUtil._tuning_control = Just "cengceng-kopyak-tune" }

data Articulation = Open | Closed
    deriving (Eq, Ord, Show)

strokeMap :: Drum.StrokeMap Articulation
strokeMap = Drum.replaceSoft 0.75 $ Drum.strokeMapTable stops
    [ ('a', ".", Attrs.open <> Attrs.soft, Open, open)
    , ('z', "o", Attrs.open, Open, open)
    , ('s', "/", Attrs.closed <> Attrs.soft, Closed, closed)
    , ('x', "x", Attrs.closed, Closed, closed)
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
    , _convertAttributeMap = Drum._attributeMap strokeMap
    , _getFilename = getFilename
    }

data Dynamic = PP | MP | MF | FF
    deriving (Eq, Show, Bounded, Enum)

dynamicThreshold :: Dynamic -> Signal.Y
dynamicThreshold = \case
    PP -> 0.25
    MP -> 0.5
    MF -> 0.75
    FF -> 1

-- | {open,closed}-{pp,mp,mf,ff}-v{1,2,3,4}.flac
getFilename :: Articulation -> Signal.Y -> Signal.Y
    -> (FilePath, Maybe (Signal.Y, Signal.Y))
getFilename art dyn var = (fname, Just dynRange)
    where
    dynRange =
        ( if dynSym == PP then 0 else dynamicThreshold (pred dynSym)
        , dynamicThreshold dynSym
        )
    (dynSym, _) = Util.findDynamic dynamicThreshold dyn
    fname = Seq.join "-"
        [ case art of
            Open -> "open"
            Closed -> "closed"
        , Util.showLower dynSym
        , Util.pickVariation ['v' : show n | n <- [1..4]] var
        ] <> ".flac"
