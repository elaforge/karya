-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Sampler.Patch.CengCeng (patches) where
import qualified Data.Set as Set

import qualified Util.Seq as Seq
import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.ImInst as ImInst
import qualified Derive.Attrs as Attrs
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Derive as Derive
import qualified Derive.Expr as Expr
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig

import qualified Instrument.Common as Common
import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.Patch.Lib.Code as Code
import qualified Synth.Sampler.Patch.Lib.Drum as Drum
import qualified Synth.Sampler.Patch.Lib.Util as Util
import qualified Synth.Shared.Signal as Signal

import           Global


patches :: [Patch.DbPatch]
patches = map Patch.DbPatch (rincik : kopyaks)

-- * cengceng rincik

rincik :: Patch.Patch
rincik = setPatch $
    Drum.patch dir "cengceng-rincik" rincikStrokeMap rincikConvertMap
        (const rincikCallConfig)
    where
    setPatch patch = Patch.addCode code $ patch
        { Patch._karyaPatch =
            Drum.karyaPatch dir rincikStrokeMap rincikConvertMap
                (const rincikCallConfig)
                [(char, sym) | (char, sym, _) <- rincikCalls]
        }
    code = ImInst.note_generators [(sym, call) | (_, sym, call) <- rincikCalls]
        <> ImInst.note_transformers [("infer-hands", c_inferHands)]
    dir = "cengceng/rincik"

rincikCallConfig :: CUtil.CallConfig
rincikCallConfig = CUtil.call_config
    { CUtil._tuning_control = Just "cengceng-rincik-tune"
    , CUtil._transform = Code.withVariation
    }

data Dynamic3 = P | M | F
    deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Rincik = ROpen Hand | RClosed Hand | RMute Hand
    | ROpenBoth | RClosedBoth
    | RKopyakOpen | RKopyakClosed
    deriving (Eq, Ord, Show)

data Hand = HLeft | HRight
    deriving (Eq, Ord, Show)

rincikAll :: [Rincik]
rincikAll =
    [ ROpenBoth, RClosedBoth
    , RKopyakOpen, RKopyakClosed
    ] ++ [r h | r <- [ROpen, RClosed, RMute], h <- [HLeft, HRight]]

rincikStrokeMap :: Drum.StrokeMap Rincik
rincikStrokeMap =
    Drum.addAttributeMap rincikExtraAttributes $ Drum.replaceSoft 0.75 $
    Drum.strokeMapTable stops
    [ ('1', "ko", Attrs.open <> kopyak <> soft, RKopyakOpen, open)
    , ('q', "kO", Attrs.open <> kopyak, RKopyakOpen, open)
    , ('2', "kx", Attrs.closed <> kopyak <> soft, RKopyakClosed, open)
    , ('w', "kX", Attrs.closed <> kopyak, RKopyakClosed, open)
    , ('v', "OO", Attrs.open <> both,   ROpenBoth, open)
    , ('b', "++", Attrs.closed <> both, RClosedBoth, closed)
    ]
    where
    kopyak = Attrs.attr "kopyak"
    both = Attrs.attr "both"
    stops = [(closed, [open])]
    open = "open"
    closed = "closed"
    soft = Attrs.soft

rincikExtraAttributes :: Common.AttributeMap Rincik
rincikExtraAttributes =
    Common.attribute_map $ concatMap make
        [(ROpen, Attrs.open), (RClosed, Attrs.closed), (RMute, Attrs.mute)]
    where
    make (stroke, attr) =
        [ (attr, stroke HLeft)
        , (attr <> Attrs.left, stroke HLeft)
        , (attr <> Attrs.right, stroke HRight)
        ]

rincikCalls :: [(Char, Expr.Symbol, Derive.Generator Derive.Note)]
rincikCalls =
    [ make 'a' "o" True Attrs.open
    , make 'z' "O" False Attrs.open
    , make 's' "-" True Attrs.closed
    , make 'x' "+" False Attrs.closed
    , make 'd' "x" True Attrs.mute
    , make 'c' "X" False Attrs.mute
    ]
    where
    make char name soft attrs =
        (char, name, CUtil.drum_call config name attrs)
        where
        config = rincikCallConfig
            { CUtil._stroke_dyn = if soft then 0.75 else 1 }

c_inferHands :: Derive.Transformer Derive.Note
c_inferHands = Derive.transformer Module.instrument "infer-hands" mempty
    "Infer alternating +right and +left for notes without it.  Only applies\
    \ to +open, +closed, +mute."
    $ Sig.call0t $ \_ deriver -> snd . Post.emap1 assign HRight <$> deriver
    where
    assign hand event
        | has Attrs.left = (HRight, event)
        | has Attrs.right = (HLeft, event)
        | any has [Attrs.open, Attrs.closed, Attrs.mute] =
            (other hand, Score.add_attributes (handAttr hand) event)
        | otherwise = (hand, event)
        where
        has a = Score.has_attribute a event
    other HRight = HLeft
    other HLeft = HRight
    handAttr HRight = Attrs.right
    handAttr HLeft = Attrs.left

rincikConvertMap :: Drum.ConvertMap Rincik
rincikConvertMap = Drum.ConvertMap
    { _dynRange = (0.5, 1)
    , _naturalNn = Nothing
    , _muteTime = Just 0.05
    , _getFilename = rincikGetFilename
    , _allFilenames = rincikAllFilenames
    }

rincikAllFilenames :: Set FilePath
rincikAllFilenames = Util.assertLength 186 $ Set.fromList
    [ fst $ rincikGetFilename art dyn var
    | art <- rincikAll
    , dyn <- [0, 0.35, 0.7, 1]
    , var <- Seq.range 0 1 (1/8)
    ]

rincikGetFilename :: Rincik -> Signal.Y -> Signal.Y
    -> (FilePath, Maybe (Signal.Y, Signal.Y))
rincikGetFilename art dyn var = (fname, Just dynRange)
    where
    dynRange =
        ( if dynSym == minBound then 0 else dynamicThreshold (pred dynSym)
        , dynamicThreshold dynSym
        )
    (dynSym, _) = Util.findDynamic dynamicThreshold dyn
    fname = Seq.join "-"
        [ showArt art
        , Util.showLower dynSym
        , 'v' : show (Util.pickVariation [1 .. variations] var)
        ] <> ".flac"
    showArt = \case
        ROpen h -> "open+" <> showHand h
        RClosed h -> "closed+" <> showHand h
        RMute h -> "mute+" <> showHand h
        ROpenBoth -> "open+both"
        RClosedBoth -> "closed+both"
        RKopyakOpen -> "kopyak+open"
        RKopyakClosed -> "kopyak+closed"
    variations = case art of
        ROpen _ -> 8 `div` 2
        RClosed _ -> d (14, 8, 8) `div` 2
        RMute _ -> 16 `div` 2
        ROpenBoth -> 6
        RClosedBoth -> 6
        RKopyakOpen -> 8
        RKopyakClosed -> 8
    d (p, m, f) = case dynSym of
        P -> p
        M -> m
        F -> f
    showHand = \case
        HLeft -> "left"
        HRight -> "right"
    dynamicThreshold = \case
        P -> 0.35
        M -> 0.7
        F -> 1


-- * cengceng kopyak

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
