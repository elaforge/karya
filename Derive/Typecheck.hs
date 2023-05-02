-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
module Derive.Typecheck (
    -- * signal functions
    RealTimeFunction(..), ScoreTimeFunction(..)
    , RealTimeFunctionT(..), ScoreTimeFunctionT(..)
    , DiatonicTransposeFunctionT(..)
    , ChromaticTransposeFunctionT(..)
    , NnTransposeFunctionT(..)

    -- * type wrappers
    , DefaultReal(..), DefaultScore(..)
    , real, score
    , Positive(..), NonNegative(..), Normalized(..), NormalizedBipolar(..)
    , DefaultDiatonic(..), diatonic

    -- * typecheck
    , typecheck, typecheck_simple
    , Checked(..)
    , Result(..)
    , success, failure
    , Typecheck(..)
    , from_val_eval
    , from_val_simple

    , TEnum
    , ToVal(..)
    , to_type_symbol
    , from_val_symbol
    , num_to_type
    , TypecheckNum(..)

    -- * util
    , to_transpose, transpose_control

    -- * controls
    , coerce_to_scalar
    , lookup_function
    , lookup_signal
    , val_to_signal
    , val_to_function, val_to_function_dyn

    -- * pitch signals
    , val_to_pitch_signal
    , lookup_pitch_signal
    , resolve_pitch_ref

    -- * compatibility
    , resolve_function
#ifdef TESTING
    , resolve_signal
#endif
) where
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Ratio as Ratio
import qualified Data.Set as Set

import qualified Util.Lists as Lists
import qualified Util.Texts as Texts
import qualified Derive.Attrs as Attrs
import qualified Derive.Call.SubT as SubT
import qualified Derive.Controls as Controls
import qualified Derive.DeriveT as DeriveT
import           Derive.DeriveT (Val(..))
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Deriver.Monad as Derive
import qualified Derive.Expr as Expr
import qualified Derive.PSignal as PSignal
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.ValType as ValType

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import qualified Ui.Meter.Meter as Meter
import qualified Ui.ScoreTime as ScoreTime

import           Global
import           Types


-- * type wrappers

-- | Either RealTime or ScoreTime, but untyped defaults to RealTime.
-- This has a short accessor to make unwrapping more concise.
newtype DefaultReal = DefaultReal { _real :: DeriveT.Duration }
    deriving (Eq, Show, ShowVal.ShowVal)
instance Internal.Time DefaultReal where
    real = Internal.real . _real
    score = Internal.score . _real
    to_duration = _real

-- | Same as 'DefaultReal' but untyped defaults to ScoreTime.
-- This has a short accessor to make unwrapping more concise.
newtype DefaultScore = DefaultScore { _score :: DeriveT.Duration }
    deriving (Eq, Show, ShowVal.ShowVal)

instance Internal.Time DefaultScore where
    real = Internal.real . _score
    score = Internal.score . _score
    to_duration = _score

-- | Create DefaultReal and DefaultScores for use in "Derive.Sig" signatures
-- for default values.  It would be nice to use literals and let type
-- inference do its thing, but there's no good definition for the rest of
-- the methods in Integral and Fractional.
real :: RealTime -> DefaultReal
real = DefaultReal . DeriveT.RealDuration

score :: ScoreTime -> DefaultScore
score = DefaultScore . DeriveT.ScoreDuration

-- | An annotation that says this value must be >0.  Instances only exist
-- for numeric types.
--
-- This is an instance of Num just so numeric literals work.  Of course that
-- means you also have (-) which can make it not positive, but this is only
-- intended to be a type tag for signatures, unwrapped as soon as it gets
-- passed to the call.
newtype Positive a = Positive { positive :: a }
    deriving (Show, Eq, ShowVal.ShowVal, Num, Fractional)

-- | Like Positive, but also includes 0.
newtype NonNegative a = NonNegative { non_negative :: a }
    deriving (Show, Eq, ShowVal.ShowVal, Num, Fractional)

-- | 0 <= x <= 1
newtype Normalized = Normalized { normalized :: Double }
    deriving (Show, Eq, ShowVal.ShowVal, Pretty)

-- | -1 <= x <= 1
newtype NormalizedBipolar = NormalizedBipolar { normalized_bipolar :: Double }
    deriving (Show, Eq, ShowVal.ShowVal, Pretty)

-- | Normally Transpose will default to Chromatic if the val is untyped,
-- but some calls would prefer to default to Diatonic.
newtype DefaultDiatonic =
    DefaultDiatonic { default_diatonic :: Pitch.Transpose }
    deriving (Show, Eq, ShowVal.ShowVal)

diatonic :: Double -> DefaultDiatonic
diatonic = DefaultDiatonic . Pitch.Diatonic

-- * typecheck utils

-- | Typecheck a single Val, and throw if it's the wrong type.
typecheck :: forall a. Typecheck a => Text -> ScoreTime -> Val
    -> Derive.Deriver a
typecheck msg pos val = from_val_eval pos val >>= \case
    Just a -> return a
    Nothing -> Derive.throw $
        Texts.join2 ": " msg $ type_error_msg (Proxy @a) val
    -- TODO throw a TypeError directly?

-- | Typecheck a simple value, with no evaluation.  This means you can't
-- get a deriver or coerce signal to a number.
typecheck_simple :: forall a. Typecheck a => Val -> Either Text a
typecheck_simple val =
    justErr (type_error_msg (Proxy @a) val) (from_val_simple val)

type_error_msg :: Typecheck a => Proxy a -> Val -> Text
type_error_msg expected val = "expected " <> pretty (to_type expected)
    <> " but got " <> pretty (ValType.specific_type_of val)

-- * Typecheck class

data Checked a =
    Val (Result a)
    -- | This val needs to be evaluated to know if it will typecheck.  The
    -- argument is the call start time.  This is needed when coercing a
    -- function to a scalar, because I only know the value to check after
    -- calling the function.
    | Eval (RealTime -> Derive.Deriver (Maybe a))
    deriving (Functor)

data Result a = Failure | Success !a
    -- | This is a {Note,Control,Pitch}Deriver, which needs a Context to
    -- evaluate.  'DeriveT.Quoted' can be coerced to this, at which point
    -- 'Derive.Eval.eval_quoted' needs a Context.  As with Quoted evaluation in
    -- general, this is only supported by Derive.Sig, not by the general
    -- 'typecehck' mechanism.
    | Derive !(Derive.Context Derive.Tagged -> a)
    deriving (Functor)

success :: a -> Checked a
success = Val . Success

failure :: Checked a
failure = Val Failure

-- | Further check a Checked.  I feel like this should correspond to some kind
-- of monad transformer lift.
check :: (a -> Maybe b) -> Checked a -> Checked b
check f (Val a) = Val $ case a of
    Success a -> maybe Failure Success $ f a
    Failure -> Failure
    Derive {} -> Failure -- can't really check one of these
check f (Eval fa) = Eval (\t -> (f =<<) <$> fa t)

-- | I can automatically derive a Typecheck for enum types.  So a simple enum
-- can get a Typecheck by "just" deriving these plus Typecheck.
type TEnum a = (ShowVal.ShowVal a, Bounded a, Enum a)

-- | This is the class of values which can be converted to a 'Val'.  'ToVal' is
-- the inverse transformation.
class Typecheck a where
    from_val :: Val -> Checked a
    default from_val :: TEnum a => Val -> Checked a
    from_val = from_val_symbol enum_map

    to_type :: Proxy a -> ValType.Type
    default to_type :: TEnum a => Proxy a -> ValType.Type
    to_type Proxy = to_type_symbol [minBound :: a ..]

    from_subtrack :: SubT.Track -> Maybe a
    from_subtrack = const Nothing

-- | 'from_val', but evaluate if it's an Eval.
from_val_eval :: Typecheck a => ScoreTime -> Val -> Derive.Deriver (Maybe a)
from_val_eval pos val = case from_val val of
    Val (Success a) -> return $ Just a
    Val Failure -> return Nothing
    -- This is a deriver, which needs a Derive.Context.
    Val (Derive _) -> return Nothing
    Eval deriver -> deriver =<< Derive.score_to_real pos

from_val_simple :: Typecheck a => Val -> Maybe a
from_val_simple val = case from_val val of
    Val (Success a) -> Just a
    _ -> Nothing

-- | Return a simple Eval check which doesn't depend on RealTime.
eval :: Typecheck a => (a -> Derive.Deriver b) -> Val -> Checked b
eval parse val = Eval $ \_ -> case from_val_simple val of
    Nothing -> return Nothing
    Just x -> Just <$> parse x

-- | This is the inverse of Typecheck's 'from_val'.
class ToVal a where
    to_val :: a -> Val
    -- This has overly constrictive constraints because generally only TEnum
    -- types correspond to a Str via show_val.
    default to_val :: TEnum a => a -> Val
    to_val = VStr . Expr.Str . ShowVal.show_val

to_type_symbol :: ShowVal.ShowVal a => [a] -> ValType.Type
to_type_symbol = ValType.TStr . Just . map ShowVal.show_val

from_val_symbol :: Map Text a -> Val -> Checked a
from_val_symbol syms = \case
    VStr (Expr.Str str) -> Val $ maybe Failure Success $ Map.lookup str syms
    _ -> failure
    -- Debug claims that syms is captured and evaluated only once even without
    -- the explicit lambda, but let's do it anyway, I think it has implications
    -- for inlining.

enum_map :: forall a. TEnum a => Map Text a
enum_map = Map.fromList $ Lists.keyOn ShowVal.show_val [minBound ..]

num_to_type :: TypecheckNum a => Proxy a -> ValType.Type
num_to_type proxy = ValType.TSignal (num_type proxy) ValType.TAny

class Typecheck a => TypecheckNum a where
    num_type :: Proxy a -> ValType.NumType

instance Typecheck Bool
instance ToVal Bool

instance ShowVal.ShowVal Meter.Rank
instance Typecheck Meter.Rank
instance ToVal Meter.Rank

instance Typecheck ScoreT.Type
instance ToVal ScoreT.Type

-- * Typecheck instances

instance ToVal Val where to_val = id
instance Typecheck Val where
    from_val = success
    to_type _ = ValType.TVal

-- | Putting Maybe in Typecheck means I can have optional arguments with no
-- defaults.  Further docs in 'Derive.Sig.defaulted'.
instance Typecheck a => Typecheck (Maybe a) where
    from_val VNotGiven = success Nothing
    from_val a = Just <$> from_val a
    to_type _ = ValType.TMaybe $ to_type (Proxy :: Proxy a)
    -- Propagate from_subtrack through a Maybe, so they can be optional.
    from_subtrack = fmap Just . from_subtrack
instance ToVal a => ToVal (Maybe a) where
    to_val = maybe VNotGiven to_val

-- | A non-list is coerced into a singleton list.
instance Typecheck a => Typecheck [a] where
    from_val (VList xs) = check xs
        where
        check [] = success []
        check (x:xs) = case from_val x of
            Val Failure -> Val Failure
            Val (Success a) -> (a:) <$> check xs
            Val (Derive deriver) -> case check xs of
                Val (Derive rest) -> Val $ Derive $ \ctx ->
                    deriver ctx : rest ctx
                _ -> Val Failure
            Eval a -> case check xs of
                Val Failure -> Val Failure
                Val (Derive {}) -> Val Failure
                Val (Success as) -> (:as) <$> Eval a
                Eval as -> Eval $ \p -> cons <$> a p <*> as p
        cons a as = (:) <$> a <*> as
    from_val v = (:[]) <$> from_val v
    to_type _ = ValType.TList $ to_type (Proxy :: Proxy a)
instance ToVal a => ToVal [a] where to_val = VList . map to_val

instance (Typecheck a, Ord a) => Typecheck (Set a) where
    from_val = fmap Set.fromList . from_val
    to_type _ = ValType.TList $ to_type (Proxy :: Proxy a)
instance ToVal a => ToVal (Set a) where
    to_val = VList . map to_val . Set.toList

instance Typecheck a => Typecheck (NonEmpty a) where
    from_val val = check NonEmpty.nonEmpty (from_val val)
    to_type _ = ValType.TList $ to_type (Proxy :: Proxy a)

instance (Typecheck a, Typecheck b) => Typecheck (Either a b) where
    from_val a = case from_val a of
        Val Failure -> Right <$> from_val a
        a -> Left <$> a
    to_type _ = ValType.TEither (to_type (Proxy :: Proxy a))
        (to_type (Proxy :: Proxy b))
instance (ToVal a, ToVal b) => ToVal (Either a b) where
    to_val = either to_val to_val


-- ** numeric types


-- | Signal.Control has ToVal but not Typecheck, because calls should be
-- using Function.
instance ToVal ScoreT.TypedSignal where to_val = VSignal
instance ToVal PSignal.PSignal where to_val = VPSignal

instance Typecheck ScoreT.TypedSignal where
    from_val = coerce_to_signal
    to_type _ = ValType.TSignal ValType.TUntyped ValType.TAny

instance Typecheck Signal.Control where
    from_val = fmap ScoreT.val_of . from_val
    to_type _ = ValType.TSignal ValType.TUntyped ValType.TAny

-- *** eval only

-- These don't have ToVal instances.  This means they can be used in a call,
-- but not turned back into a Val to put in the environ, or printed in log
-- msgs.

instance Typecheck ScoreT.TypedFunction where
    from_val = coerce_to_function Just
    to_type _ = ValType.TOther "typed signal"

instance Typecheck ScoreT.Function where
    from_val = fmap ScoreT.val_of . from_val
    to_type _ = ValType.TOther "untyped signal"

instance Typecheck (RealTime -> RealTime) where
    from_val = coerce_to_function $ \(ScoreT.Typed typ f) -> case typ of
        ScoreT.Real -> Just (RealTime.seconds . f)
        ScoreT.Untyped -> Just (RealTime.seconds . f)
        _ -> Nothing
    to_type _ = ValType.TOther "realtime signal"

instance Typecheck PSignal.PSignal where
    from_val = coerce_to_pitch_signal
    to_type _ = ValType.TPSignal

instance Typecheck DeriveT.PitchFunction where
    from_val = fmap (flip PSignal.at) . coerce_to_pitch_signal
    to_type _ = ValType.TPSignal

-- Returning Duration is convenient for Derive.real or Derive.score, e.g.
-- Gender.ngoret.
data RealTimeFunction = RealTimeFunction !(RealTime -> DeriveT.Duration)
data ScoreTimeFunction = ScoreTimeFunction !(RealTime -> DeriveT.Duration)

-- | Returning them separately is used in (at least) Speed.starts
data RealTimeFunctionT = RealTimeFunctionT !ScoreT.TimeT !ScoreT.Function
data ScoreTimeFunctionT = ScoreTimeFunctionT !ScoreT.TimeT !ScoreT.Function

instance Typecheck RealTimeFunctionT where
    from_val = coerce_to_typed_function RealTimeFunctionT
        (ScoreT.time_t ScoreT.TReal)
    to_type _ = ValType.TOther "time signal (default real)"
instance Typecheck ScoreTimeFunctionT where
    from_val = coerce_to_typed_function ScoreTimeFunctionT
        (ScoreT.time_t ScoreT.TScore)
    to_type _ = ValType.TOther "time signal (default score)"

instance Typecheck RealTimeFunction where
    from_val = coerce_to_typed_function (\dur f -> RealTimeFunction $ dur . f)
        (time_constructor ScoreT.TReal)
    to_type _ = ValType.TOther "time signal (default real)"

instance Typecheck ScoreTimeFunction where
    from_val = coerce_to_typed_function (\dur f -> ScoreTimeFunction $ dur . f)
        (time_constructor ScoreT.TScore)
    to_type _ = ValType.TOther "time signal (default score)"

time_constructor :: ScoreT.TimeT -> ScoreT.Type
    -> Maybe (Signal.Y -> DeriveT.Duration)
time_constructor deflt typ = case ScoreT.time_t deflt typ of
    Just ScoreT.TReal -> Just $ DeriveT.RealDuration . RealTime.seconds
    Just ScoreT.TScore -> Just $ DeriveT.ScoreDuration . ScoreTime.from_double
    Nothing -> Nothing

-- Originally I used DataKinds e.g.
-- TransposeFunction (deflt :: TransposeType), but it seemed less
-- convenient than separate data types.

data DiatonicTransposeFunctionT =
    DiatonicTransposeFunctionT !ScoreT.TransposeT !ScoreT.Function
instance Typecheck DiatonicTransposeFunctionT where
    from_val = coerce_to_typed_function DiatonicTransposeFunctionT
        (ScoreT.transpose_t ScoreT.TDiatonic)
    to_type _ = ValType.TOther "transpose signal (default diatonic)"

data ChromaticTransposeFunctionT =
    ChromaticTransposeFunctionT !ScoreT.TransposeT !ScoreT.Function
instance Typecheck ChromaticTransposeFunctionT where
    from_val = coerce_to_typed_function ChromaticTransposeFunctionT
        (ScoreT.transpose_t ScoreT.TChromatic)
    to_type _ = ValType.TOther "transpose signal (default chromatic)"

data NnTransposeFunctionT =
    NnTransposeFunctionT !ScoreT.TransposeT !ScoreT.Function
instance Typecheck NnTransposeFunctionT where
    from_val = coerce_to_typed_function NnTransposeFunctionT
        (ScoreT.transpose_t ScoreT.TNn)
    to_type _ = ValType.TOther "transpose signal (default nn)"


-- *** scalar

instance Typecheck (ScoreT.Typed Signal.Y) where
    from_val = coerce_to_scalar Just
    to_type = num_to_type
instance ToVal (ScoreT.Typed Signal.Y) where
    to_val = VSignal . fmap Signal.constant
instance TypecheckNum (ScoreT.Typed Signal.Y) where
    num_type _ = ValType.TUntyped

instance Typecheck Double where
    from_val = coerce_to_scalar (Just . ScoreT.val_of)
    to_type = num_to_type
instance ToVal Double where to_val = DeriveT.num
instance TypecheckNum Double where num_type _ = ValType.TUntyped

instance Typecheck (Ratio.Ratio Int) where
    from_val = coerce_to_scalar $
        Just . realToFrac . flip Ratio.approxRational 0.001 . ScoreT.val_of
    to_type = num_to_type
instance ToVal (Ratio.Ratio Int) where
    to_val = DeriveT.num . realToFrac
instance TypecheckNum (Ratio.Ratio Int) where num_type _ = ValType.TUntyped

instance Typecheck Int where
    from_val = from_integral_val
    to_type = num_to_type

instance Typecheck Integer where
    from_val = from_integral_val
    to_type = num_to_type

from_integral_val :: Integral a => Val -> Checked a
from_integral_val = coerce_to_scalar (check . ScoreT.val_of)
    where
    check a = if frac == 0 then Just int else Nothing
        where (int, frac) = properFraction a

instance ToVal Int where to_val = DeriveT.num . fromIntegral
instance ToVal Integer where to_val = DeriveT.num . fromIntegral
instance TypecheckNum Int where num_type _ = ValType.TInt
instance TypecheckNum Integer where num_type _ = ValType.TInt

-- | VSignals can also be coerced into chromatic transposition, so you can
-- write a plain number if you don't care about diatonic.
--
-- This is different from 'DeriveT.Duration', which does not default an
-- untyped literal, so you have to supply the type explicitly.  The rationale
-- is that many scales don't have diatonic or chromatic, and it would be
-- annoying to have to specify one or the other when it was definitely
-- irrelevant.  But the RealTime ScoreTime distinction is universal, there is
-- no single default that is appropriate for all calls.  So they have to
-- specify a default by taking a 'DefaultScore' or 'DefaultReal', or require
-- the caller to distinguish with 'DeriveT.Duration'.
instance Typecheck Pitch.Transpose where
    from_val = coerce_to_scalar $ \(ScoreT.Typed typ val) -> case typ of
        ScoreT.Untyped -> Just (Pitch.Chromatic val)
        ScoreT.Chromatic -> Just (Pitch.Chromatic val)
        ScoreT.Diatonic -> Just (Pitch.Diatonic val)
        ScoreT.Nn -> Just (Pitch.Nn val)
        _ -> Nothing
    to_type = num_to_type
instance TypecheckNum Pitch.Transpose where num_type _ = ValType.TTranspose

instance ToVal Pitch.Transpose where
    to_val = \case
        Pitch.Chromatic a -> DeriveT.constant ScoreT.Chromatic a
        Pitch.Diatonic a -> DeriveT.constant ScoreT.Diatonic a
        Pitch.Nn a -> DeriveT.constant ScoreT.Nn a

-- | But some calls want to default to diatonic, not chromatic.
instance Typecheck DefaultDiatonic where
    from_val = coerce_to_scalar $ \(ScoreT.Typed typ val) ->
        DefaultDiatonic <$> case typ of
            ScoreT.Untyped -> Just (Pitch.Diatonic val)
            ScoreT.Chromatic -> Just (Pitch.Chromatic val)
            ScoreT.Diatonic -> Just (Pitch.Diatonic val)
            ScoreT.Nn -> Just (Pitch.Nn val)
            _ -> Nothing
    to_type = num_to_type
instance ToVal DefaultDiatonic where to_val (DefaultDiatonic a) = to_val a
instance TypecheckNum DefaultDiatonic where
    num_type _ = ValType.TDefaultDiatonic

instance Typecheck Pitch.NoteNumber where
    from_val = coerce_to_scalar $ \(ScoreT.Typed typ val) ->
        Pitch.nn <$> case typ of
            ScoreT.Untyped -> Just val
            ScoreT.Nn -> Just val
            _ -> Nothing
    to_type = num_to_type
instance ToVal Pitch.NoteNumber where
    to_val = DeriveT.constant ScoreT.Nn . Pitch.nn_to_double
instance TypecheckNum Pitch.NoteNumber where num_type _ = ValType.TNoteNumber

instance Typecheck ScoreTime where
    from_val = coerce_to_scalar $ \(ScoreT.Typed typ val) ->
        ScoreTime.from_double <$> case typ of
            ScoreT.Untyped -> Just val
            ScoreT.Score -> Just val
            _ -> Nothing
    to_type = num_to_type
instance ToVal ScoreTime where
    to_val = DeriveT.constant ScoreT.Score . ScoreTime.to_double
instance TypecheckNum ScoreTime where num_type _ = ValType.TScoreTime

instance Typecheck RealTime where
    from_val = coerce_to_scalar $ \(ScoreT.Typed typ val) ->
        RealTime.seconds <$> case typ of
            ScoreT.Untyped -> Just val
            ScoreT.Real -> Just val
            _ -> Nothing
    to_type = num_to_type
instance ToVal RealTime where
    to_val = DeriveT.constant ScoreT.Real . RealTime.to_seconds
instance TypecheckNum RealTime where num_type _ = ValType.TRealTime

instance Typecheck DeriveT.Duration where
    from_val = coerce_to_scalar $ \(ScoreT.Typed typ val) -> case typ of
        -- Untyped is abiguous, and there doesn't seem to be a natural
        -- default.
        ScoreT.Score -> Just $ DeriveT.ScoreDuration (ScoreTime.from_double val)
        ScoreT.Real -> Just $ DeriveT.RealDuration (RealTime.seconds val)
        _ -> Nothing
    to_type = num_to_type

instance ToVal DeriveT.Duration where
    to_val (DeriveT.ScoreDuration a) = to_val a
    to_val (DeriveT.RealDuration a) = to_val a
instance TypecheckNum DeriveT.Duration where num_type _ = ValType.TTime

instance Typecheck DefaultReal where
    from_val = coerce_to_scalar $ \(ScoreT.Typed typ val) ->
        DefaultReal <$> case typ of
            ScoreT.Untyped ->
                Just $ DeriveT.RealDuration (RealTime.seconds val)
            ScoreT.Score ->
                Just $ DeriveT.ScoreDuration (ScoreTime.from_double val)
            ScoreT.Real -> Just $ DeriveT.RealDuration (RealTime.seconds val)
            _ -> Nothing
    to_type = num_to_type
instance ToVal DefaultReal where to_val (DefaultReal a) = to_val a
instance TypecheckNum DefaultReal where num_type _ = ValType.TDefaultReal

instance Typecheck DefaultScore where
    from_val = coerce_to_scalar $ \(ScoreT.Typed typ val) ->
        DefaultScore <$> case typ of
            ScoreT.Untyped ->
                Just $ DeriveT.ScoreDuration (ScoreTime.from_double val)
            ScoreT.Score ->
                Just $ DeriveT.ScoreDuration (ScoreTime.from_double val)
            ScoreT.Real -> Just $ DeriveT.RealDuration (RealTime.seconds val)
            _ -> Nothing
    to_type = num_to_type

instance ToVal DefaultScore where to_val (DefaultScore a) = to_val a
instance TypecheckNum DefaultScore where num_type _ = ValType.TDefaultScore

instance TypecheckNum a => Typecheck (Positive a) where
    from_val v@(VSignal (ScoreT.Typed _ sig))
        | Just n <- Signal.constant_val sig, n > 0 = Positive <$> from_val v
        | otherwise = failure
    from_val _ = failure
    to_type _ = ValType.TSignal (num_type (Proxy @a)) ValType.TPositive
instance ToVal a => ToVal (Positive a) where
    to_val (Positive val) = to_val val

instance TypecheckNum a => Typecheck (NonNegative a) where
    from_val v@(VSignal (ScoreT.Typed _ sig))
        | Just n <- Signal.constant_val sig, n >= 0 = NonNegative <$> from_val v
        | otherwise = failure
    from_val _ = failure
    to_type _ = ValType.TSignal (num_type (Proxy @a)) ValType.TNonNegative
instance ToVal a => ToVal (NonNegative a) where
    to_val (NonNegative val) = to_val val

instance Typecheck Normalized where
    from_val = coerce_to_scalar (check . ScoreT.val_of)
        where
        check a
            | 0 <= a && a <= 1 = Just (Normalized a)
            | otherwise = Nothing
    to_type _ = ValType.TSignal ValType.TUntyped ValType.TNormalized
instance ToVal Normalized where to_val = DeriveT.num . normalized

instance Typecheck NormalizedBipolar where
    from_val = coerce_to_scalar (check . ScoreT.val_of)
        where
        check a
            | -1 <= a && a <= 1 = Just (NormalizedBipolar a)
            | otherwise = Nothing
    to_type _ = ValType.TSignal ValType.TUntyped ValType.TNormalizedBipolar
instance ToVal NormalizedBipolar where
    to_val = DeriveT.num . normalized_bipolar

-- ** text\/symbol

instance Typecheck Expr.Symbol where
    from_val (VStr (Expr.Str sym)) = success $ Expr.Symbol sym
    from_val _ = failure
    to_type _ = ValType.TStr Nothing
instance ToVal Expr.Symbol where to_val = VStr . Expr.Str . Expr.unsym

instance Typecheck Expr.Str where
    from_val (VStr a) = success a
    from_val _ = failure
    to_type _ = ValType.TStr Nothing
instance ToVal Expr.Str where to_val = VStr

instance Typecheck Text where
    from_val (VStr (Expr.Str s)) = success s
    from_val _ = failure
    to_type _ = ValType.TStr Nothing
instance ToVal Text where to_val = VStr . Expr.Str

instance Typecheck ScoreT.Control where
    from_val (VStr (Expr.Str s)) =
        Val $ either (const Failure) Success (ScoreT.checked_control s)
    from_val _ = failure
    to_type _ = ValType.TControl
instance ToVal ScoreT.Control where
    to_val c = VStr (Expr.Str (ScoreT.control_name c))

instance Typecheck ScoreT.PControl where
    from_val (VStr (Expr.Str s)) =
        Val $ either (const Failure) Success (ScoreT.checked_pcontrol s)
    from_val _ = failure
    to_type _ = ValType.TPControl
instance ToVal ScoreT.PControl where
    to_val c = VStr (Expr.Str (ScoreT.pcontrol_name c))

-- ** other types

instance Typecheck Attrs.Attributes where
    from_val (VAttributes a) = success a
    from_val _ = failure
    to_type _ = ValType.TAttributes
instance ToVal Attrs.Attributes where to_val = VAttributes

-- Intentionally no Typecheck ControlRef, use Function instances.
instance ToVal DeriveT.ControlRef where to_val = VControlRef

-- Intentionally no Typecheck PControlRef, use PSignal instances.
instance ToVal DeriveT.PControlRef where to_val = VPControlRef

instance Typecheck PSignal.Pitch where
    from_val = coerce_to_pitch
    to_type _ = ValType.TPitch
instance ToVal PSignal.Pitch where to_val = VPitch

instance Typecheck Pitch.Pitch where
    from_val (VNotePitch a) = success a
    from_val _ = failure
    to_type _ = ValType.TNotePitch
instance ToVal Pitch.Pitch where to_val = VNotePitch

instance Typecheck ScoreT.Instrument where
    from_val (VStr (Expr.Str a)) = success (ScoreT.Instrument a)
    from_val _ = failure
    to_type _ = ValType.TStr Nothing
instance ToVal ScoreT.Instrument where
    to_val (ScoreT.Instrument a) = VStr (Expr.Str a)

instance Typecheck DeriveT.CFunction where
    from_val (VCFunction a) = success a
    from_val _ = failure
    to_type _ = ValType.TCFunction
instance ToVal DeriveT.CFunction where to_val = VCFunction

instance Typecheck DeriveT.PFunction where
    from_val (VPFunction a) = success a
    from_val _ = failure
    to_type _ = ValType.TPFunction
instance ToVal DeriveT.PFunction where to_val = VPFunction

-- | Anything except a pitch can be coerced to a quoted, using ShowVal.  This
-- means you can write a lot of things without quotes.
--
-- Pitches have to be quoted because they explicitly have an invalid ShowVal.
instance Typecheck DeriveT.Quoted where
    from_val val = case val of
        VQuoted a -> success a
        VPitch {} -> failure
        VStr (Expr.Str sym) -> to_quoted sym
        _ -> to_quoted $ ShowVal.show_val val
        where
        to_quoted sym = success $
            DeriveT.Quoted $ Expr.Call (Expr.Symbol sym) [] :| []
    to_type _ = ValType.TQuoted
instance ToVal DeriveT.Quoted where to_val = VQuoted

data NotGiven = NotGiven deriving (Show, Eq)

instance ShowVal.ShowVal NotGiven where
    show_val NotGiven = "_"

instance Typecheck NotGiven where
    from_val VNotGiven = success NotGiven
    from_val _ = failure
    to_type _ = ValType.TNotGiven

instance ToVal NotGiven where
    to_val NotGiven = VNotGiven

-- * util

-- TODO There are four types that divide into two kinds.  Then I have
-- every possible combination:
-- any type: ScoreT.Real
-- time type without value: Real
-- time type with value: DeriveT.RealDuration
--
-- This means I wind up with a lot of duplication here to handle time types and
-- transpose types.  Surely there's a better way?  Maybe put the two kinds into
-- a typeclass?

to_transpose :: ScoreT.TransposeT -> Double -> Pitch.Transpose
to_transpose typ val = case typ of
    ScoreT.TDiatonic -> Pitch.Diatonic val
    ScoreT.TChromatic -> Pitch.Chromatic val
    ScoreT.TNn -> Pitch.Nn val

transpose_control :: ScoreT.TransposeT -> ScoreT.Control
transpose_control = \case
    ScoreT.TDiatonic -> Controls.diatonic
    ScoreT.TChromatic -> Controls.chromatic
    ScoreT.TNn -> Controls.nn

-- ** controls

coerce_to_typed_function :: (typ -> ScoreT.Function -> b)
    -> (ScoreT.Type -> Maybe typ) -> Val -> Checked b
coerce_to_typed_function make check = coerce_to_function $
    \(ScoreT.Typed typ f) -> flip make f <$> check typ

-- | Coerce any numeric value to a ScoreT.Typed Signal.Y, and check it against
-- the given function.
coerce_to_scalar :: (ScoreT.Typed Signal.Y -> Maybe a) -> Val -> Checked a
coerce_to_scalar check val
    -- It's important that constant VSignals remain Val, which means they don't
    -- need Eval, which means they don't need a time.  This is because
    -- non-signal constants in the environ like srate use from_val_simple,
    -- which ignores Eval.  TODO if I want all numeric values to be variable,
    -- then I should merge control_at with Derive.get_val, so it takes a time.
    | Just num <- DeriveT.constant_val val =
        Val $ maybe Failure Success $ check num
    | otherwise = case val_to_function val of
        Just (Right tf) -> Eval $ \t -> return $ check (($ t) <$> tf)
        Just (Left df) -> Eval $ \t -> check . (($ t) <$>) <$> df
        Nothing -> failure

-- | Coerce any numeric value to a function, and check it against the given
-- function.
coerce_to_function :: (ScoreT.TypedFunction -> Maybe a) -> Val -> Checked a
coerce_to_function check val = case val_to_function val of
    Just (Right f) -> Val $ maybe Failure Success $ check f
    -- Eval's t time is thrown away, because I'm creating a function and thus
    -- don't need to know at which time to evaluate it.
    Just (Left df) -> Eval $ \_t -> check <$> df
    Nothing -> failure

val_to_function :: Val
    -> Maybe (Either (Derive.Deriver ScoreT.TypedFunction) ScoreT.TypedFunction)
val_to_function = \case
    VSignal sig -> Just $ Right $ flip Signal.at <$> sig
    VControlRef ref -> Just $ Left $ resolve_function ref
    VCFunction cf -> Just $ Left $ do
        cf_dyn <- Internal.get_control_function_dynamic
        return $ DeriveT.call_cfunction cf_dyn cf
    VPFunction f -> Just $ Right $ DeriveT.pf_function f
    _ -> Nothing

-- | Unfortunately Internal.get_control_function_dynamic is non-trivial,
-- so it makes sense to do it only once when converting many.
--
-- TODO does it really?  It seems like a bogus tradeoff to have to make.
-- I could cache it, but is that not what this is?
-- If it's cheap to call Internal.get_control_function_dynamic and only
-- expensive to force it, then I should always pass it, and rely on laziness.
val_to_function_dyn :: DeriveT.Dynamic -> Val
    -> Maybe (Either (Derive.Deriver ScoreT.TypedFunction) ScoreT.TypedFunction)
val_to_function_dyn cf_dyn = \case
    VSignal sig -> Just $ Right $ flip Signal.at <$> sig
    -- TODO propagate cf_dyn through
    VControlRef ref -> Just $ Left $ resolve_function ref
    VCFunction cf -> Just $ Right $ DeriveT.call_cfunction cf_dyn cf
    _ -> Nothing

resolve_function :: DeriveT.ControlRef -> Derive.Deriver ScoreT.TypedFunction
resolve_function ref =
    Derive.require ("control not found: " <> ShowVal.show_val ref)
        =<< lookup_function ref

-- | Resolve a ref to a function, applying a CFunction if there is one.
lookup_function :: DeriveT.ControlRef
    -> Derive.Deriver (Maybe ScoreT.TypedFunction)
lookup_function (DeriveT.Ref control deflt) = do
    maybe (return deflt_f) get . DeriveT.lookup (ScoreT.control_name control)
        =<< Internal.get_environ
    where
    deflt_f = fmap (flip Signal.at) <$> deflt
    get :: Val -> Derive.Deriver (Maybe ScoreT.TypedFunction)
    get val = case val_to_function val of
        Nothing -> return Nothing
        Just (Right tf) -> return $ Just tf
        Just (Left dtf) -> Just <$> dtf

-- *** signal

-- | Resolve ref to a ScoreT.TypedSignal.  This does not take ControlFunctions
-- into account, but is necessary when you need the actual signal.
resolve_signal :: DeriveT.ControlRef
    -> Derive.Deriver (Maybe ScoreT.TypedSignal)
resolve_signal (DeriveT.Ref control deflt) =
    (<|> deflt) <$> lookup_signal control

coerce_to_signal :: Val -> Checked ScoreT.TypedSignal
coerce_to_signal val = case val_to_signal val of
    Nothing -> failure
    Just (Left dsig) -> Eval $ \_ -> Just <$> dsig
    Just (Right sig) -> success sig

-- | As with 'lookup_pitch_function', this should be in Deriver.Lib.
lookup_signal :: ScoreT.Control -> Derive.Deriver (Maybe ScoreT.TypedSignal)
lookup_signal control =
    traverse get . DeriveT.lookup (ScoreT.control_name control)
        =<< Internal.get_environ
    where
    get val = case val_to_signal val of
        Nothing -> Derive.throw $ "can't be coerced to signal: "
            <> ShowVal.show_val val
        Just (Left df) -> df
        Just (Right f) -> return f

val_to_signal :: Val
    -> Maybe (Either (Derive.Deriver ScoreT.TypedSignal) ScoreT.TypedSignal)
val_to_signal = \case
    VSignal a -> Just $ Right a
    VControlRef ref -> Just $ Left $
        Derive.require ("control not found: " <> ShowVal.show_val ref)
            =<< resolve_signal ref
    VCFunction cf -> Just $ Right $ DeriveT.cf_signal cf
    _ -> Nothing


-- ** pitch signals

-- Unlike controls, this works with signals instead of functions.  Ultimately
-- this is because there is no equaivalent of CFunction.  Signals are
-- less flexible than functions, but more useful since I can splice a signal
-- back into the environment, while functions are opaque.

-- | This is the pitch version of 'coerce_to_scalar'.
coerce_to_pitch :: Val -> Checked PSignal.Pitch
coerce_to_pitch = \case
    VPitch a -> success a
    val | Just (ScoreT.Typed ScoreT.Nn nn) <- DeriveT.constant_val val ->
        success $ PSignal.nn_pitch (Pitch.nn nn)
    val -> case val_to_pitch_signal val of
        Nothing -> failure
        Just (Left dsig) -> Eval $ \pos ->
            fmap Just . require pos . (PSignal.at pos) =<< dsig
        Just (Right sig) -> Eval $ \pos ->
            Just <$> require pos (PSignal.at pos sig)
        where
        require pos = Derive.require $
            "no pitch at " <> pretty pos <> " for " <> ShowVal.show_val val

coerce_to_pitch_signal :: Val -> Checked PSignal.PSignal
coerce_to_pitch_signal val = case val_to_pitch_signal val of
    Nothing -> failure
    Just (Left dsig) -> Eval $ \_ -> Just <$> dsig
    Just (Right sig) -> success sig

-- | This is the pitch version of 'resolve_control_ref', except simpler
-- because there's no pitch equivalent of CFunction.
resolve_pitch_ref :: DeriveT.PControlRef -> Derive.Deriver PSignal.PSignal
resolve_pitch_ref (DeriveT.Ref control deflt) =
    Derive.require
        ("named pitch not found and no default: " <> ShowVal.show_val control)
            =<< (<|> deflt) <$> lookup_pitch_signal control

-- | This should be in Deriver.Lib, but has to be here so the instance
-- Typecheck PitchFunction can be declared here and avoid circular import.
lookup_pitch_signal :: ScoreT.PControl -> Derive.Deriver (Maybe PSignal.PSignal)
lookup_pitch_signal pcontrol
    | pcontrol == ScoreT.default_pitch =
        Just <$> Internal.get_dynamic Derive.state_pitch
    | otherwise =
        traverse get . DeriveT.lookup (ScoreT.pcontrol_name pcontrol)
            =<< Internal.get_environ
    where
    -- This is recursive, because if a PControl resolves to a VPControlRef
    -- it will trigger another lookup.  If the refs point to each other then
    -- we will loop, so don't do that!  This also re-implements Env.checked_val
    -- to avoid circular import.
    get val = case val_to_pitch_signal val of
        Nothing -> Derive.throw $
            pretty pcontrol <> ": " <> pretty (ValType.specific_type_of val)
                <> " can't be coerced to pitch signal"
        Just (Left df) -> df
        Just (Right f) -> return f

val_to_pitch_signal :: Val
    -> Maybe (Either (Derive.Deriver PSignal.PSignal) PSignal.PSignal)
val_to_pitch_signal = \case
    VPControlRef ref -> Just $ Left $ resolve_pitch_ref ref
    VPitch pitch -> Just $ Right $ PSignal.constant pitch
    VPSignal sig -> Just $ Right sig
    val -> case DeriveT.constant_val val of
        Just (ScoreT.Typed ScoreT.Nn nn) ->
            Just $ Right $ PSignal.constant $ PSignal.nn_pitch (Pitch.nn nn)
        _ -> Nothing

-- * sub tracks

instance Typecheck SubT.Track where
    from_val _ = failure
    to_type _ = ValType.TDeriver "note"
    from_subtrack = Just
