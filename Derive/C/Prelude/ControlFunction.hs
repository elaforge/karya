-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls and functions for 'DeriveT.CFunction's.
module Derive.C.Prelude.ControlFunction (
    library
    , cCfRndAround
) where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified System.Random.Mersenne.Pure64 as Pure64

import qualified Util.Doc as Doc
import qualified Util.Lists as Lists
import qualified Util.Num as Num

import qualified Derive.Call as Call
import qualified Derive.Call.ControlUtil as ControlUtil
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Expr as Expr
import qualified Derive.Library as Library
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Typecheck as Typecheck
import qualified Derive.ValType as ValType
import qualified Derive.Warp as Warp

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import qualified Ui.Meter.Mark as Mark
import qualified Ui.Meter.Meter as Meter
import qualified Ui.Ruler as Ruler
import qualified Ui.ScoreTime as ScoreTime

import           Global
import           Types


library :: Library.Library
library = Library.vals $
    [ ("cf-rnd", cCfRnd const)
    , ("cf-rnd+", cCfRnd (+))
    , ("cf-rnd*", cCfRnd (*))
    , ("cf-rnd-a", cCfRndAround const)
    , ("cf-rnd-a+", cCfRndAround (+))
    , ("cf-rnd-a*", cCfRndAround (*))
    , ("cf-rnd01", cCfRnd01)
    , ("cf-swing", cCfSwing)
    , ("cf-clamp", cCfClamp)
    ] ++ map (makeCurveCall Nothing . snd) ControlUtil.standardCurves
    ++ map (uncurry makeCurveCall . first Just) curves

makeCurveCall :: Maybe Doc.Doc -> ControlUtil.CurveD
    -> (Expr.Symbol, Derive.ValCall)
makeCurveCall doc curve =
    ( "curve-" <> Expr.Symbol (ControlUtil.curveName curve)
    , ControlUtil.makeCurveCall doc curve
    )

data Distribution
    = Uniform
    -- | Approximate a bounded normal distribution.
    | Normal
    -- | This is like Normal, but rotated, so the peaks are at the extremities.
    | Bimodal
    deriving (Bounded, Eq, Enum, Show)

instance ShowVal.ShowVal Distribution
instance Typecheck.Typecheck Distribution
instance Typecheck.ToVal Distribution

cCfRnd :: (Signal.Y -> Signal.Y -> Signal.Y) -> Derive.ValCall
cCfRnd combine = valCall "cf-rnd" (Tags.controlFunction <> Tags.random)
    "Randomize a control. Normally it replaces the control of the same name,\
    \ while the `+` and `*` variants add to and multiply with it."
    $ Sig.call ((,,)
    <$> Sig.required "low" "Low end of the range."
    <*> Sig.required "high" "High end of the range."
    <*> Sig.environ "distribution" Sig.Prefixed Normal "Random distribution."
    ) $ \(low, high, distribution) _args -> pure $!
        makeCf "cf-rnd" $ \cfDyn control pos -> combine
            (cfRnd distribution low high (randomStream pos (dynSeed cfDyn)))
            (Signal.at control pos)

makeCf :: Text -> (DeriveT.Dynamic -> Signal.Control -> RealTime -> Signal.Y)
    -> DeriveT.CFunction
makeCf name f = DeriveT.CFunction
    { cfName = name
    , cfSignal = ScoreT.untyped (Signal.constant 0)
    , cfFunction = f
    }

cCfRndAround :: (Signal.Y -> Signal.Y -> Signal.Y) -> Derive.ValCall
cCfRndAround combine = valCall "cf-rnd-a"
    (Tags.controlFunction <> Tags.random)
    "Randomize a control around a center point.\
    \ Normally it replaces the control of the same name,\
    \ while the `+` and `*` variants add to and multiply with it."
    $ Sig.call ((,,)
    <$> Sig.required "range" "Range this far from the center."
    <*> Sig.defaulted "center" (0 :: Double) "Center of the range."
    <*> Sig.environ "distribution" Sig.Prefixed Normal "Random distribution."
    ) $ \(range, center, distribution) _args -> pure $!
        makeCf "cf-rnd-a" $ \cfDyn control pos -> combine
            (cfRnd distribution (center-range) (center+range)
                (randomStream pos (dynSeed cfDyn)))
            (Signal.at control pos)

cCfRnd01 :: Derive.ValCall
cCfRnd01 = Make.modifyVcall (cCfRnd (+)) Module.prelude "cf-rnd01"
    "This is an abbreviation for `(cf-clamp (cf-rnd+ ..) 0 1)`." $
    \val -> case Typecheck.fromValSimple val of
        Just cf -> Typecheck.toVal $ cfCompose "cf-rnd01" (Num.clamp 0 1) cf
        Nothing -> val

cfRnd :: Distribution -> Double -> Double -> [Double] -> Double
cfRnd dist low high rnds = Num.scale low high $ case dist of
    Uniform -> head rnds
    Normal -> Call.makeNormal 1 rnds
    Bimodal
        | v >= 0.5 -> v - 0.5
        | otherwise -> v + 0.5
        where v = Call.makeNormal 1 rnds

randomStream :: RealTime -> Double -> [Double]
randomStream pos =
    List.unfoldr (Just . Pure64.randomDouble) . Pure64.pureMT . floor
    . (+ RealTime.toSeconds pos)


-- * cf-swing

cCfSwing :: Derive.ValCall
cCfSwing = valCall "cf-swing" Tags.controlFunction
    ("Add a curved  offset to the control, suitable for swing tempo when added\
    \ to " <> ShowVal.doc Controls.startS <> ". The curve is a sine wave,\
    \ from trough to trough.")
    $ Sig.call ((,)
    <$> Sig.defaulted "rank" Meter.Q
        "The time steps are on the beat, and midway between offset by the\
        \ given amount."
    <*> Sig.defaulted "amount" (makeRef "swing" (1/3))
        "Swing amount, multiplied by the rank duration / 2."
    ) $ \(rank, amount) _args -> do
        amount <- fromControlRef amount
        pure $! makeCf "cf-swing" (swing rank amount)
    where
    swing rank amount cfDyn control pos
        | Just marks <- maybeMarks =
            Signal.at control pos + RealTime.toSeconds (swing marks)
        | otherwise = 0
        where
        swing marks = cfSwing (real cfDyn) rank
            (toFunction cfDyn 0 amount) marks (score cfDyn pos)
        maybeMarks = snd <$>
            Map.lookup Ruler.meterName (DeriveT.dynRuler cfDyn)

-- | TODO Hacky ref, should be temporary until I clean up cfs.
type Ref = Either DeriveT.ControlRef (ScoreT.Typed Signal.Y)

-- | I intentionally don't have Typecheck ControlRef, because in almost
-- all cases it should just be a scalar or function.  But ControlFunctions
-- are like little calls, so they need to delay control resolution just
-- like calls do, so they wind up duplicating all that.  TODO if I'm able to
-- unify calls and ControlFunctions then this goes away.
fromControlRef :: DeriveT.Val -> Derive.Deriver Ref
fromControlRef = \case
    DeriveT.VControlRef ref -> pure $ Left ref
    val | Just num <- DeriveT.constantVal val -> pure $ Right num
    val -> Derive.throw $ "expected ControlRef or Num, but got "
        <> pretty (ValType.specificTypeOf val)

-- | Defaulted control from a RealTime.
makeRef :: ScoreT.Control -> RealTime -> DeriveT.ControlRef
makeRef c deflt = DeriveT.Ref c $
    Just $ ScoreT.untyped $ Signal.constant (RealTime.toSeconds deflt)

cfSwing :: (ScoreTime -> RealTime) -> Meter.Rank -> ScoreT.Function
    -> Mark.Marklist -> ScoreTime -> RealTime
cfSwing toReal rank amount marks pos = case marksAround rank marks pos of
    Nothing -> 0
    Just (pre, post) -> (toReal post - toReal pre) / 2
        * RealTime.seconds (amount (toReal pos))
        * swing (Num.normalize pre post pos)

marksAround :: Meter.Rank -> Mark.Marklist -> ScoreTime
    -> Maybe (ScoreTime, ScoreTime)
marksAround rank marks pos =
    (,) <$> get (Mark.descending pos marks) <*> get (Mark.ascending pos marks)
    where get = fmap fst . Lists.head . filter ((<=rank) . Mark.markRank . snd)

swing :: ScoreTime -- ^ time from this beat to the next, normalized 0 to 1
    -> RealTime -- ^ amount of swing offset, also normalized 0 to 1
swing = RealTime.seconds . Num.normalize (-1) 1 . sin . (*pi)
    . Num.scale (-0.5) 1.5 . ScoreTime.toDouble

-- * cf-clamp

cCfClamp :: Derive.ValCall
cCfClamp = valCall "cf-clamp" Tags.controlFunction
    "Clamp the output of a control function to the given range."
    $ Sig.call ((,,)
    <$> Sig.required "cf" "Control function."
    <*> Sig.defaulted "low" (0 :: Double) "Low value."
    <*> Sig.defaulted "high" (1 :: Double) "High value."
    ) $ \(cf, low, high) _args ->
        pure $ cfCompose ("cf-clamp(" <> DeriveT.cfName cf <> ")")
            (Num.clamp low high) cf

cfCompose :: Text -> (Signal.Y -> Signal.Y) -> DeriveT.CFunction
    -> DeriveT.CFunction
cfCompose name f cf = DeriveT.CFunction
    { cfName = name
    , cfSignal = DeriveT.cfSignal cf
    , cfFunction = \cfDyn c -> f . DeriveT.cfFunction cf cfDyn c
    }

-- * curve interpolators

curves :: [(Doc.Doc, ControlUtil.CurveD)]
curves =
    [ ( "Jump to the destination at 0.5."
      , ControlUtil.CurveD "jump" (pure ()) $
        \() -> ControlUtil.Function $ \n -> if n < 0.5 then 0 else 1
      )
    , ("No interpolation.", ControlUtil.CurveD "const" (pure ()) $
        \() -> ControlUtil.Function (const 0))
    ]

-- * DeriveT.Dynamic

dynSeed :: DeriveT.Dynamic -> Double
dynSeed cfDyn = fromIntegral (DeriveT.dynEventSerial cfDyn) + seed cfDyn
    where seed = fromMaybe 0 . Env.maybeVal EnvKey.seed . DeriveT.dynEnviron

real :: DeriveT.Dynamic -> ScoreTime -> RealTime
real = Warp.warp . DeriveT.dynWarp

score :: DeriveT.Dynamic -> RealTime -> ScoreTime
score = Warp.unwarp . DeriveT.dynWarp

-- ** ControlRef

toFunction :: DeriveT.Dynamic -> Signal.Y -> Ref -> ScoreT.Function
toFunction cfDyn deflt =
    maybe (const deflt) ScoreT.valOf . lookupFunction cfDyn

-- | TODO duplicated with Typecheck.lookup_function except it
-- can't be in Deriver.
lookupFunction :: DeriveT.Dynamic -> Ref -> Maybe ScoreT.TypedFunction
lookupFunction _ (Right ty) = Just $ const <$> ty
lookupFunction cfDyn (Left (DeriveT.Ref control deflt)) =
    maybe defltF get $ DeriveT.lookup (ScoreT.controlName control) $
        DeriveT.dynEnviron cfDyn
    where
    defltF = fmap Signal.at <$> deflt
    get = valToFunction cfDyn

valToFunction :: DeriveT.Dynamic -> DeriveT.Val -> Maybe ScoreT.TypedFunction
valToFunction cfDyn = \case
    DeriveT.VSignal sig -> Just $ Signal.at <$> sig
    DeriveT.VControlRef ref -> lookupFunction cfDyn (Left ref)
    DeriveT.VCFunction cf -> Just $ DeriveT.callCfunction cfDyn cf
    _ -> Nothing


-- * misc

valCall :: Typecheck.ToVal a => Derive.CallName -> Tags.Tags -> Doc.Doc
    -> Derive.WithArgDoc (Derive.PassedArgs Derive.Tagged -> Derive.Deriver a)
    -> Derive.ValCall
valCall = Derive.valCall Module.prelude
