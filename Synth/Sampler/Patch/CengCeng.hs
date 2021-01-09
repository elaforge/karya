-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Sampler.Patch.CengCeng (patches) where
import qualified Data.Set as Set

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
        kopyakStrokeMap (kopyakConvertMap inst) (const config)
    config = CUtil.call_config
        { CUtil._tuning_control = Just "cengceng-kopyak-tune" }

data Inst = K1 | K2 | K3 deriving (Show)

kopyakDir :: Inst -> FilePath
kopyakDir inst = "cengceng/kopyak/" <> kopyakNum inst

kopyakNum :: Inst -> String
kopyakNum = drop 1 . show

data Kopyak = Open | Closed | Rim
    deriving (Eq, Ord, Show, Bounded, Enum)

kopyakStrokeMap :: Drum.StrokeMap Kopyak
kopyakStrokeMap = Drum.replaceSoft 0.75 $ Drum.strokeMapTable stops
    [ ('a', "o", Attrs.open <> soft,    Open, open)
    , ('z', "O", Attrs.open,            Open, open)
    , ('s', "x", Attrs.closed <> soft,  Closed, closed)
    , ('x', "X", Attrs.closed,          Closed, closed)
    , ('d', "t", Attrs.rim <> soft,     Rim, open)
    , ('c', "T", Attrs.rim,             Rim, open)
    ]
    where
    stops = [(closed, [open])]
    soft = Attrs.soft
    open = "open"
    closed = "closed"

kopyakConvertMap :: Inst -> Drum.ConvertMap Kopyak
kopyakConvertMap inst = Drum.ConvertMap
    { _dynRange = (0.5, 1)
    , _naturalNn = Nothing
    , _muteTime = Just 0.05
    , _convertAttributeMap = Drum._attributeMap kopyakStrokeMap
    , _getFilename = kopyakGetFilename inst
    , _allFilenames = kopyakAllFilenames inst
    }

kopyakAllFilenames :: Inst -> Set FilePath
kopyakAllFilenames inst = Util.assertLength len $ Set.fromList
    [ fst $ kopyakGetFilename inst art dyn var
    | art <- Util.enumAll
    , dyn <- [0, 0.25, 0.5, 0.75]
    , var <- Seq.range 0 1 (1/8)
    ]
    where
    len = case inst of
        K1 -> 83
        K2 -> 92
        K3 -> 92

-- | {open,closed}-{pp,mp,mf,ff}-v{1..n}.flac
kopyakGetFilename :: Inst -> Kopyak -> Signal.Y -> Signal.Y
    -> (FilePath, Maybe (Signal.Y, Signal.Y))
kopyakGetFilename inst art dyn var = (fname, Just dynRange)
    where
    dynRange =
        ( if dynSym == minBound then 0 else dynamicThreshold (pred dynSym)
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
        (K1, Rim) -> d (7, 6, 7, 7)
        (K2, Open) -> d (8, 7, 8, 7)
        (K2, Rim) -> d (8, 8, 6, 8)
        (K3, Open) -> d (8, 7, 8, 8)
        (K3, Rim) -> d (8, 7, 7, 7)
    d (pp, mp, mf, ff) = case dynSym of
        Util.PP -> pp
        Util.MP -> mp
        Util.MF -> mf
        Util.FF -> ff
    dynamicThreshold = \case
        Util.PP -> 0.25
        Util.MP -> 0.5
        Util.MF -> 0.75
        Util.FF -> 1
