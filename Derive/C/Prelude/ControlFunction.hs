-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls and functions for 'DeriveT.ControlFunction's.
module Derive.C.Prelude.ControlFunction where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified System.Random.Mersenne.Pure64 as Pure64

import qualified Util.Doc as Doc
import qualified Util.Num as Num
import qualified Util.Seq as Seq

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
import qualified Derive.ValType as ValType
import qualified Derive.Sig as Sig
import qualified Derive.Typecheck as Typecheck
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
    [ ("cf-rnd", c_cf_rnd const)
    , ("cf-rnd+", c_cf_rnd (+))
    , ("cf-rnd*", c_cf_rnd (*))
    , ("cf-rnd-a", c_cf_rnd_around const)
    , ("cf-rnd-a+", c_cf_rnd_around (+))
    , ("cf-rnd-a*", c_cf_rnd_around (*))
    , ("cf-rnd01", c_cf_rnd01)
    , ("cf-swing", c_cf_swing)
    , ("cf-clamp", c_cf_clamp)
    ] ++ map (make_call Nothing . snd) ControlUtil.standard_curves
    ++ map (uncurry make_call . first Just) curves

make_call :: Maybe Doc.Doc -> ControlUtil.CurveD
    -> (Expr.Symbol, Derive.ValCall)
make_call doc curve =
    ( "cf-" <> Expr.Symbol (ControlUtil.curve_name curve)
    , ControlUtil.make_curve_call doc curve
    )

data Distribution =
    Uniform
    -- | Approximate a bounded normal distribution.
    | Normal
    -- | This is like Normal, but rotated, so the peaks are at the extremities.
    | Bimodal
    deriving (Bounded, Eq, Enum, Show)

instance ShowVal.ShowVal Distribution
instance Typecheck.Typecheck Distribution
instance Typecheck.ToVal Distribution

c_cf_rnd :: (Signal.Y -> Signal.Y -> Signal.Y) -> Derive.ValCall
c_cf_rnd combine = val_call "cf-rnd"
    (Tags.control_function <> Tags.random)
    "Randomize a control. Normally it replaces the control of the same name,\
    \ while the `+` and `*` variants add to and multiply with it."
    $ Sig.call ((,,)
    <$> Sig.required "low" "Low end of the range."
    <*> Sig.required "high" "High end of the range."
    <*> Sig.environ "distribution" Sig.Prefixed Normal "Random distribution."
    ) $ \(low, high, distribution) _args -> return $!
        DeriveT.ControlFunction "cf-rnd" $ \control dyn pos ->
            ScoreT.untyped $ combine
                (cf_rnd distribution low high
                    (random_stream pos (dyn_seed dyn)))
                (dyn_control dyn control pos)

c_cf_rnd_around :: (Signal.Y -> Signal.Y -> Signal.Y) -> Derive.ValCall
c_cf_rnd_around combine = val_call "cf-rnd-a"
    (Tags.control_function <> Tags.random)
    "Randomize a control around a center point.\
    \ Normally it replaces the control of the same name,\
    \ while the `+` and `*` variants add to and multiply with it."
    $ Sig.call ((,,)
    <$> Sig.required "range" "Range this far from the center."
    <*> Sig.defaulted "center" (0 :: Double) "Center of the range."
    <*> Sig.environ "distribution" Sig.Prefixed Normal "Random distribution."
    ) $ \(range, center, distribution) _args -> return $!
        DeriveT.ControlFunction "cf-rnd-a" $ \control dyn pos ->
            ScoreT.untyped $ combine
                (cf_rnd distribution (center-range) (center+range)
                    (random_stream pos (dyn_seed dyn)))
                (dyn_control dyn control pos)

c_cf_rnd01 :: Derive.ValCall
c_cf_rnd01 = Make.modify_vcall (c_cf_rnd (+)) Module.prelude "cf-rnd01"
    "This is an abbreviation for `(cf-clamp (cf-rnd+ ..) 0 1)`." $
    \val -> case Typecheck.from_val_simple val of
        Just cf -> Typecheck.to_val $ cf_compose "cf-clamp" (Num.clamp 0 1) cf
        Nothing -> val

cf_rnd :: Distribution -> Double -> Double -> [Double] -> Double
cf_rnd dist low high rnds = Num.scale low high $ case dist of
    Uniform -> head rnds
    Normal -> Call.make_normal 1 rnds
    Bimodal
        | v >= 0.5 -> v - 0.5
        | otherwise -> v + 0.5
        where v = Call.make_normal 1 rnds

random_stream :: RealTime -> Double -> [Double]
random_stream pos =
    List.unfoldr (Just . Pure64.randomDouble) . Pure64.pureMT . floor
    . (+ RealTime.to_seconds pos)


-- * cf-swing

c_cf_swing :: Derive.ValCall
c_cf_swing = val_call "cf-swing" Tags.control_function
    ("Add a curved  offset to the control, suitable for swing tempo when added\
    \ to " <> ShowVal.doc Controls.start_s <> ". The curve is a sine wave,\
    \ from trough to trough.")
    $ Sig.call ((,)
    <$> Sig.defaulted "rank" Meter.Q
        "The time steps are on the beat, and midway between offset by the\
        \ given amount."
    <*> Sig.defaulted "amount" (make_ref "swing" (1/3))
        "Swing amount, multiplied by the rank duration / 2."
    ) $ \(rank, amount) _args -> do
        amount <- from_control_ref amount
        return $! DeriveT.ControlFunction "cf-swing" (cf_swing_ rank amount)
    where
    cf_swing_ rank amount control dyn pos
        | Just marks <- maybe_marks = ScoreT.untyped $
            dyn_control dyn control pos + RealTime.to_seconds
                (cf_swing (real dyn) rank
                    (to_function dyn 0 amount) marks (score dyn pos))
        | otherwise = ScoreT.untyped 0
        where
        maybe_marks = snd <$>
            Map.lookup Ruler.meter_name (DeriveT.dyn_ruler dyn)

-- | TODO Hacky ref, should be temporary until I clean up cfs.
type Ref = Either DeriveT.ControlRef (ScoreT.Typed Signal.Y)

-- | I intentionally don't have Typecheck ControlRef, because in almost
-- all cases it should just be a scalar or function.  But ControlFunctions
-- are like little calls, so they need to delay control resolution just
-- like calls do, so they wind up duplicating all that.  TODO if I'm able to
-- unify calls and ControlFunctions then this goes away.
from_control_ref :: DeriveT.Val -> Derive.Deriver Ref
from_control_ref = \case
    DeriveT.VControlRef ref -> pure $ Left ref
    DeriveT.VNum num -> pure $ Right num
    val -> Derive.throw $ "expected ControlRef or Num, but got "
        <> pretty (ValType.type_of val)

-- | Defaulted control from a RealTime.
make_ref :: ScoreT.Control -> RealTime -> DeriveT.ControlRef
make_ref c deflt = DeriveT.Ref c $
    Just $ ScoreT.untyped $ Signal.constant (RealTime.to_seconds deflt)

cf_swing :: (ScoreTime -> RealTime) -> Meter.Rank -> DeriveT.Function
    -> Mark.Marklist -> ScoreTime -> RealTime
cf_swing to_real rank amount marks pos = case marks_around rank marks pos of
    Nothing -> 0
    Just (pre, post) -> (to_real post - to_real pre) / 2
        * RealTime.seconds (amount (to_real pos))
        * swing (Num.normalize pre post pos)

marks_around :: Meter.Rank -> Mark.Marklist -> ScoreTime
    -> Maybe (ScoreTime, ScoreTime)
marks_around rank marks pos =
    (,) <$> get (Mark.descending pos marks) <*> get (Mark.ascending pos marks)
    where get = fmap fst . Seq.head . filter ((<=rank) . Mark.mark_rank . snd)

swing :: ScoreTime -- ^ time from this beat to the next, normalized 0 to 1
    -> RealTime -- ^ amount of swing offset, also normalized 0 to 1
swing = RealTime.seconds . Num.normalize (-1) 1 . sin . (*pi)
    . Num.scale (-0.5) 1.5 . ScoreTime.to_double

-- * cf-clamp

c_cf_clamp :: Derive.ValCall
c_cf_clamp = val_call "cf-clamp" Tags.control_function
    "Clamp the output of a control function to the given range."
    $ Sig.call ((,,)
    <$> Sig.required "cf" "Control function."
    <*> Sig.defaulted "low" (0 :: Double) "Low value."
    <*> Sig.defaulted "high" (1 :: Double) "High value."
    ) $ \(cf, low, high) _args ->
        return $ cf_compose "cf-clamp" (Num.clamp low high) cf

cf_compose :: Text -> (Signal.Y -> Signal.Y) -> DeriveT.ControlFunction
    -> DeriveT.ControlFunction
cf_compose name f (DeriveT.ControlFunction cf_name cf) =
    DeriveT.ControlFunction (name <> " . " <> cf_name)
        (\c dyn x -> f <$> cf c dyn x)

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

dyn_seed :: DeriveT.Dynamic -> Double
dyn_seed dyn = fromIntegral (DeriveT.dyn_event_serial dyn) + seed dyn
    where seed = fromMaybe 0 . Env.maybe_val EnvKey.seed . DeriveT.dyn_environ

dyn_control :: DeriveT.Dynamic -> ScoreT.Control -> RealTime -> Double
dyn_control dyn control pos =
    maybe 0 (Signal.at pos . ScoreT.typed_val) $ dyn_signal dyn control

dyn_signal :: DeriveT.Dynamic -> ScoreT.Control
    -> Maybe (ScoreT.Typed Signal.Control)
dyn_signal dyn control =
    either (const Nothing) Just =<< Typecheck.val_to_signal
        =<< Env.lookup (ScoreT.control_name control) (DeriveT.dyn_environ dyn)

real :: DeriveT.Dynamic -> ScoreTime -> RealTime
real dyn = Warp.warp (DeriveT.dyn_warp dyn)

score :: DeriveT.Dynamic -> RealTime -> ScoreTime
score dyn = Warp.unwarp (DeriveT.dyn_warp dyn)

-- ** ControlRef

to_function :: DeriveT.Dynamic -> Signal.Y -> Ref -> DeriveT.Function
to_function dyn deflt =
    ScoreT.typed_val . to_typed_function dyn (ScoreT.untyped deflt)

-- | TODO duplicated with Typecheck.val_to_function_dyn except it
-- can't be in Deriver.
to_typed_function :: DeriveT.Dynamic -> ScoreT.Typed Signal.Y -> Ref
    -> DeriveT.TypedFunction
to_typed_function dyn deflt ref =
    case to_signal_or_function dyn ref of
        Nothing -> const <$> deflt
        Just (Left typed_sig) -> (\sig t -> Signal.at t sig) <$> typed_sig
        -- TODO discard its type and put on Untyped, it's ok because only
        -- to_function uses this.  Later I want to put the Typed outside the
        -- function.
        Just (Right cf) -> ScoreT.Typed ScoreT.Untyped $
            ScoreT.typed_val . DeriveT.cf_function cf score_control dyn
    where
    score_control = case ref of
        Left (DeriveT.Ref c _) -> c
        Right _ -> Controls.null

-- | TODO this is a copy of Typecheck.to_signal_or_function, except it
-- uses DeriveT.Dynamic instead of Derive.Dynamic, which illustrates the
-- duplication that happens due to ControlFunction not being Deriver
to_signal_or_function :: DeriveT.Dynamic -> Ref
    -> Maybe (Either (ScoreT.Typed Signal.Control) DeriveT.ControlFunction)
to_signal_or_function dyn = \case
    Right sig -> return $ Left $ Signal.constant <$> sig
    Left (DeriveT.Ref control deflt) ->
        get_control control $ case deflt of
            Nothing -> (ScoreT.Untyped, Nothing)
            Just sig -> (ScoreT.type_of sig, return $ Left sig)
    where
    get_control control (default_type, deflt) = case lookup_function control of
        Just f -> return $ Right $
            DeriveT.modify_control_function (inherit_type default_type .) f
        Nothing -> maybe deflt (return . Left) $ lookup_signal control
    lookup_function control =
        Map.lookup control $ DeriveT.dyn_control_functions dyn
    lookup_signal = dyn_signal dyn
    -- If the signal was untyped, it gets the type of the default, since
    -- presumably the caller expects that type.
    inherit_type default_type val =
        val { ScoreT.type_of = ScoreT.type_of val <> default_type }

-- * misc

val_call :: Typecheck.ToVal a => Derive.CallName -> Tags.Tags -> Doc.Doc
    -> Derive.WithArgDoc (Derive.PassedArgs Derive.Tagged -> Derive.Deriver a)
    -> Derive.ValCall
val_call = Derive.val_call Module.prelude
