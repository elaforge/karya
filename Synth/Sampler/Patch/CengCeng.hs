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

import           Global


patches :: [Patch.DbPatch]
patches = map Patch.DbPatch kopyaks

kopyaks :: [Patch.Patch]
kopyaks = map make [K1, K2, K3]
    where
    make inst = Drum.patch
        (kopyakDir inst) ("cengceng-kopyak" <> txt (kopyakNum inst))
        strokeMap (convertMap inst) (const config)
    config = CUtil.call_config
        { CUtil._tuning_control = Just "cengceng-kopyak-tune" }

data Inst = K1 | K2 | K3 deriving (Show)

kopyakDir :: Inst -> FilePath
kopyakDir inst = "cengceng/kopyak/" <> kopyakNum inst

kopyakNum :: Inst -> String
kopyakNum = drop 1 . show

{-
rincik:

    dyns are [soft, med, loud], variations are not constant
    open, [right, left]
    closed, [right, left]
    muted, [right, left], 6 var
    both open
    both closed
    muted, [right, left], 8 var
        again!
    kopyak style, [closed open], [soft med loud], 8 var
-}
-- data Rincik =
--     ClosedLeft | ClosedRight | OpenLeft | OpenRight | BothOpen | BothClosed
--     | MutedLeft | MutedRight
--     deriving (Eq, Ord, Show)

data Rincik = ROpen Hand | RClosed Hand
    | RMutedLeft | RMutedRight | RKopyakOpen | RKopyakClosed
    deriving (Eq, Ord, Show)

data Hand = HLeft | HRight | HBoth
    deriving (Eq, Ord, Show)

-- rincik :: Patch.Patch
-- rincik = Drum.patch

data Articulation = Open | Closed | Rim
    deriving (Eq, Ord, Show, Enum)

strokeMap :: Drum.StrokeMap Articulation
strokeMap = Drum.replaceSoft 0.75 $ Drum.strokeMapTable stops
    [ ('a', ".", Attrs.open <> soft,    Open, open)
    , ('z', "o", Attrs.open,            Open, open)
    , ('s', "/", Attrs.closed <> soft,  Closed, closed)
    , ('x', "x", Attrs.closed,          Closed, closed)
    , ('d', "t", Attrs.rim <> soft,     Rim, open)
    , ('c', "T", Attrs.rim,             Rim, open)
    ]
    where
    stops = [(closed, [open])]
    soft = Attrs.soft
    open = "open"
    closed = "closed"

convertMap :: Inst -> Drum.ConvertMap Articulation
convertMap inst = Drum.ConvertMap
    { _dynRange = (0.5, 1)
    , _naturalNn = Nothing
    , _muteTime = Just 0.05
    , _convertAttributeMap = Drum._attributeMap strokeMap
    , _getFilename = getFilename inst
    }

data Dynamic = PP | MP | MF | FF
    deriving (Eq, Ord, Show, Bounded, Enum)

dynamicThreshold :: Dynamic -> Signal.Y
dynamicThreshold = \case
    PP -> 0.25
    MP -> 0.5
    MF -> 0.75
    FF -> 1

-- | {open,closed}-{pp,mp,mf,ff}-v{1..n}.flac
getFilename :: Inst -> Articulation -> Signal.Y -> Signal.Y
    -> (FilePath, Maybe (Signal.Y, Signal.Y))
getFilename inst art dyn var = (fname, Just dynRange)
    where
    dynRange =
        ( if dynSym == PP then 0 else dynamicThreshold (pred dynSym)
        , dynamicThreshold dynSym
        )
    (dynSym, _) = Util.findDynamic dynamicThreshold dyn
    fname = Seq.join "-"
        [ Util.showLower art
        , Util.showLower dynSym
        , 'v' : show (Util.pickVariation [1 .. variations] var)
        ] <> ".flac"
    variations = case (inst, art) of
        (_, Closed) -> 8
        (K1, Open) -> 6
        (K1, Rim) -> ix [7, 6, 7, 7]
        (K2, Open) -> ix [8, 7, 8, 7]
        (K2, Rim) -> ix [8, 8, 6, 8]
        (K3, Open) -> ix [8, 7, 8, 8]
        (K3, Rim) -> ix [8, 7, 7, 7]
        where ix xs = xs !! fromEnum dynSym
