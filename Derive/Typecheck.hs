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
    , typecheck, typecheckSimple
    , Checked(..)
    , Result(..)
    , success, failure
    , Typecheck(..)
    , fromValEval
    , fromValSimple

    , TEnum
    , ToVal(..)
    , toTypeSymbol
    , fromValSymbol
    , numToType
    , TypecheckNum(..)

    -- * util
    , toTranspose, transposeControl

    -- * controls
    , coerceToScalar
    , lookupFunction
    , lookupSignal
    , valToSignal
    , valToFunction, valToFunctionDyn

    -- * pitch signals
    , valToPitchSignal
    , lookupPitchSignal
    , resolvePitchRef

    -- * compatibility
    , resolveFunction
#ifdef TESTING
    , resolveSignal
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
    toDuration = _real

-- | Same as 'DefaultReal' but untyped defaults to ScoreTime.
-- This has a short accessor to make unwrapping more concise.
newtype DefaultScore = DefaultScore { _score :: DeriveT.Duration }
    deriving (Eq, Show, ShowVal.ShowVal)

instance Internal.Time DefaultScore where
    real = Internal.real . _score
    score = Internal.score . _score
    toDuration = _score

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
newtype NonNegative a = NonNegative { nonNegative :: a }
    deriving (Show, Eq, ShowVal.ShowVal, Num, Fractional)

-- | 0 <= x <= 1
newtype Normalized = Normalized { normalized :: Double }
    deriving (Show, Eq, ShowVal.ShowVal, Pretty)

-- | -1 <= x <= 1
newtype NormalizedBipolar = NormalizedBipolar { normalizedBipolar :: Double }
    deriving (Show, Eq, ShowVal.ShowVal, Pretty)

-- | Normally Transpose will default to Chromatic if the val is untyped,
-- but some calls would prefer to default to Diatonic.
newtype DefaultDiatonic =
    DefaultDiatonic { defaultDiatonic :: Pitch.Transpose }
    deriving (Show, Eq, ShowVal.ShowVal)

diatonic :: Double -> DefaultDiatonic
diatonic = DefaultDiatonic . Pitch.Diatonic

-- * typecheck utils

-- | Typecheck a single Val, and throw if it's the wrong type.
typecheck :: forall a. Typecheck a => Text -> ScoreTime -> Val
    -> Derive.Deriver a
typecheck msg pos val = fromValEval pos val >>= \case
    Just a -> pure a
    Nothing -> Derive.throw $
        Texts.join2 ": " msg $ typeErrorMsg (Proxy @a) val
    -- TODO throw a TypeError directly?

-- | Typecheck a simple value, with no evaluation.  This means you can't
-- get a deriver or coerce signal to a number.
typecheckSimple :: forall a. Typecheck a => Val -> Either Text a
typecheckSimple val =
    justErr (typeErrorMsg (Proxy @a) val) (fromValSimple val)

typeErrorMsg :: Typecheck a => Proxy a -> Val -> Text
typeErrorMsg expected val = "expected " <> pretty (toType expected)
    <> " but got " <> pretty (ValType.specificTypeOf val)

-- * Typecheck class

data Checked a
    = Val (Result a)
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
    fromVal :: Val -> Checked a
    default fromVal :: TEnum a => Val -> Checked a
    fromVal = fromValSymbol enumMap

    toType :: Proxy a -> ValType.Type
    default toType :: TEnum a => Proxy a -> ValType.Type
    toType Proxy = toTypeSymbol [minBound :: a ..]

    fromSubtrack :: SubT.Track -> Maybe a
    fromSubtrack = const Nothing

-- | 'from_val', but evaluate if it's an Eval.
fromValEval :: Typecheck a => ScoreTime -> Val -> Derive.Deriver (Maybe a)
fromValEval pos val = case fromVal val of
    Val (Success a) -> pure $ Just a
    Val Failure -> pure Nothing
    -- This is a deriver, which needs a Derive.Context.
    Val (Derive _) -> pure Nothing
    Eval deriver -> deriver =<< Derive.scoreToReal pos

fromValSimple :: Typecheck a => Val -> Maybe a
fromValSimple val = case fromVal val of
    Val (Success a) -> Just a
    _ -> Nothing

-- | Return a simple Eval check which doesn't depend on RealTime.
eval :: Typecheck a => (a -> Derive.Deriver b) -> Val -> Checked b
eval parse val = Eval $ \_ -> case fromValSimple val of
    Nothing -> pure Nothing
    Just x -> Just <$> parse x

-- | This is the inverse of Typecheck's 'from_val'.
class ToVal a where
    toVal :: a -> Val
    -- This has overly constrictive constraints because generally only TEnum
    -- types correspond to a Str via show_val.
    default toVal :: TEnum a => a -> Val
    toVal = VStr . Expr.Str . ShowVal.showVal

toTypeSymbol :: ShowVal.ShowVal a => [a] -> ValType.Type
toTypeSymbol = ValType.TStr . Just . map ShowVal.showVal

fromValSymbol :: Map Text a -> Val -> Checked a
fromValSymbol syms = \case
    VStr (Expr.Str str) -> Val $ maybe Failure Success $ Map.lookup str syms
    _ -> failure
    -- Debug claims that syms is captured and evaluated only once even without
    -- the explicit lambda, but let's do it anyway, I think it has implications
    -- for inlining.

enumMap :: forall a. TEnum a => Map Text a
enumMap = Map.fromList $ Lists.keyOn ShowVal.showVal [minBound ..]

numToType :: TypecheckNum a => Proxy a -> ValType.Type
numToType proxy = ValType.TSignal (numType proxy) ValType.TAny

class Typecheck a => TypecheckNum a where
    numType :: Proxy a -> ValType.NumType

instance Typecheck Bool
instance ToVal Bool

instance ShowVal.ShowVal Meter.Rank
instance Typecheck Meter.Rank
instance ToVal Meter.Rank

instance Typecheck ScoreT.Type
instance ToVal ScoreT.Type

-- * Typecheck instances

instance ToVal Val where toVal = id
instance Typecheck Val where
    fromVal = success
    toType _ = ValType.TVal

-- | Putting Maybe in Typecheck means I can have optional arguments with no
-- defaults.  Further docs in 'Derive.Sig.defaulted'.
instance Typecheck a => Typecheck (Maybe a) where
    fromVal VNotGiven = success Nothing
    fromVal a = Just <$> fromVal a
    toType _ = ValType.TMaybe $ toType (Proxy :: Proxy a)
    -- Propagate from_subtrack through a Maybe, so they can be optional.
    fromSubtrack = fmap Just . fromSubtrack
instance ToVal a => ToVal (Maybe a) where
    toVal = maybe VNotGiven toVal

-- | A non-list is coerced into a singleton list.
instance Typecheck a => Typecheck [a] where
    fromVal (VList xs) = check xs
        where
        check [] = success []
        check (x:xs) = case fromVal x of
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
    fromVal v = (:[]) <$> fromVal v
    toType _ = ValType.TList $ toType (Proxy :: Proxy a)
instance ToVal a => ToVal [a] where toVal = VList . map toVal

instance (Typecheck a, Ord a) => Typecheck (Set a) where
    fromVal = fmap Set.fromList . fromVal
    toType _ = ValType.TList $ toType (Proxy :: Proxy a)
instance ToVal a => ToVal (Set a) where
    toVal = VList . map toVal . Set.toList

instance Typecheck a => Typecheck (NonEmpty a) where
    fromVal val = check NonEmpty.nonEmpty (fromVal val)
    toType _ = ValType.TList $ toType (Proxy :: Proxy a)

instance (Typecheck a, Typecheck b) => Typecheck (Either a b) where
    fromVal a = case fromVal a of
        Val Failure -> Right <$> fromVal a
        a -> Left <$> a
    toType _ = ValType.TEither (toType (Proxy :: Proxy a))
        (toType (Proxy :: Proxy b))
instance (ToVal a, ToVal b) => ToVal (Either a b) where
    toVal = either toVal toVal


-- ** numeric types


-- | Signal.Control has ToVal but not Typecheck, because calls should be
-- using Function.
instance ToVal ScoreT.TypedSignal where toVal = VSignal
instance ToVal PSignal.PSignal where toVal = VPSignal

instance Typecheck ScoreT.TypedSignal where
    fromVal = coerceToSignal
    toType _ = ValType.TSignal ValType.TUntyped ValType.TAny

instance Typecheck Signal.Control where
    fromVal = fmap ScoreT.valOf . fromVal
    toType _ = ValType.TSignal ValType.TUntyped ValType.TAny

-- *** eval only

-- These don't have ToVal instances.  This means they can be used in a call,
-- but not turned back into a Val to put in the environ, or printed in log
-- msgs.

instance Typecheck ScoreT.TypedFunction where
    fromVal = coerceToFunction Just
    toType _ = ValType.TOther "typed signal"

instance Typecheck ScoreT.Function where
    fromVal = fmap ScoreT.valOf . fromVal
    toType _ = ValType.TOther "untyped signal"

instance Typecheck (RealTime -> RealTime) where
    fromVal = coerceToFunction $ \(ScoreT.Typed typ f) -> case typ of
        ScoreT.Real -> Just (RealTime.seconds . f)
        ScoreT.Untyped -> Just (RealTime.seconds . f)
        _ -> Nothing
    toType _ = ValType.TOther "realtime signal"

instance Typecheck PSignal.PSignal where
    fromVal = coerceToPitchSignal
    toType _ = ValType.TPSignal

instance Typecheck DeriveT.PitchFunction where
    fromVal = fmap PSignal.at . coerceToPitchSignal
    toType _ = ValType.TPSignal

-- Returning Duration is convenient for Derive.real or Derive.score, e.g.
-- Gender.ngoret.
data RealTimeFunction = RealTimeFunction !(RealTime -> DeriveT.Duration)
data ScoreTimeFunction = ScoreTimeFunction !(RealTime -> DeriveT.Duration)

-- | Returning them separately is used in (at least) Speed.starts
data RealTimeFunctionT = RealTimeFunctionT !ScoreT.TimeT !ScoreT.Function
data ScoreTimeFunctionT = ScoreTimeFunctionT !ScoreT.TimeT !ScoreT.Function

instance Typecheck RealTimeFunctionT where
    fromVal = coerceToTypedFunction RealTimeFunctionT
        (ScoreT.timeT ScoreT.TReal)
    toType _ = ValType.TOther "time signal (default real)"
instance Typecheck ScoreTimeFunctionT where
    fromVal = coerceToTypedFunction ScoreTimeFunctionT
        (ScoreT.timeT ScoreT.TScore)
    toType _ = ValType.TOther "time signal (default score)"

instance Typecheck RealTimeFunction where
    fromVal = coerceToTypedFunction (\dur f -> RealTimeFunction $ dur . f)
        (timeConstructor ScoreT.TReal)
    toType _ = ValType.TOther "time signal (default real)"

instance Typecheck ScoreTimeFunction where
    fromVal = coerceToTypedFunction (\dur f -> ScoreTimeFunction $ dur . f)
        (timeConstructor ScoreT.TScore)
    toType _ = ValType.TOther "time signal (default score)"

timeConstructor :: ScoreT.TimeT -> ScoreT.Type
    -> Maybe (Signal.Y -> DeriveT.Duration)
timeConstructor deflt typ = case ScoreT.timeT deflt typ of
    Just ScoreT.TReal -> Just $ DeriveT.RealDuration . RealTime.seconds
    Just ScoreT.TScore -> Just $ DeriveT.ScoreDuration . ScoreTime.fromDouble
    Nothing -> Nothing

-- Originally I used DataKinds e.g.
-- TransposeFunction (deflt :: TransposeType), but it seemed less
-- convenient than separate data types.

data DiatonicTransposeFunctionT =
    DiatonicTransposeFunctionT !ScoreT.TransposeT !ScoreT.Function
instance Typecheck DiatonicTransposeFunctionT where
    fromVal = coerceToTypedFunction DiatonicTransposeFunctionT
        (ScoreT.transposeT ScoreT.TDiatonic)
    toType _ = ValType.TOther "transpose signal (default diatonic)"

data ChromaticTransposeFunctionT =
    ChromaticTransposeFunctionT !ScoreT.TransposeT !ScoreT.Function
instance Typecheck ChromaticTransposeFunctionT where
    fromVal = coerceToTypedFunction ChromaticTransposeFunctionT
        (ScoreT.transposeT ScoreT.TChromatic)
    toType _ = ValType.TOther "transpose signal (default chromatic)"

data NnTransposeFunctionT =
    NnTransposeFunctionT !ScoreT.TransposeT !ScoreT.Function
instance Typecheck NnTransposeFunctionT where
    fromVal = coerceToTypedFunction NnTransposeFunctionT
        (ScoreT.transposeT ScoreT.TNn)
    toType _ = ValType.TOther "transpose signal (default nn)"


-- *** scalar

instance Typecheck (ScoreT.Typed Signal.Y) where
    fromVal = coerceToScalar Just
    toType = numToType
instance ToVal (ScoreT.Typed Signal.Y) where
    toVal = VSignal . fmap Signal.constant
instance TypecheckNum (ScoreT.Typed Signal.Y) where
    numType _ = ValType.TUntyped

instance Typecheck Double where
    fromVal = coerceToScalar (Just . ScoreT.valOf)
    toType = numToType
instance ToVal Double where toVal = DeriveT.num
instance TypecheckNum Double where numType _ = ValType.TUntyped

instance Typecheck (Ratio.Ratio Int) where
    fromVal = coerceToScalar $
        Just . realToFrac . flip Ratio.approxRational 0.001 . ScoreT.valOf
    toType = numToType
instance ToVal (Ratio.Ratio Int) where
    toVal = DeriveT.num . realToFrac
instance TypecheckNum (Ratio.Ratio Int) where numType _ = ValType.TUntyped

instance Typecheck Int where
    fromVal = fromIntegralVal
    toType = numToType

instance Typecheck Integer where
    fromVal = fromIntegralVal
    toType = numToType

fromIntegralVal :: Integral a => Val -> Checked a
fromIntegralVal = coerceToScalar (check . ScoreT.valOf)
    where
    check a = if frac == 0 then Just int else Nothing
        where (int, frac) = properFraction a

instance ToVal Int where toVal = DeriveT.num . fromIntegral
instance ToVal Integer where toVal = DeriveT.num . fromIntegral
instance TypecheckNum Int where numType _ = ValType.TInt
instance TypecheckNum Integer where numType _ = ValType.TInt

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
    fromVal = coerceToScalar $ \(ScoreT.Typed typ val) -> case typ of
        ScoreT.Untyped -> Just (Pitch.Chromatic val)
        ScoreT.Chromatic -> Just (Pitch.Chromatic val)
        ScoreT.Diatonic -> Just (Pitch.Diatonic val)
        ScoreT.Nn -> Just (Pitch.Nn val)
        _ -> Nothing
    toType = numToType
instance TypecheckNum Pitch.Transpose where numType _ = ValType.TTranspose

instance ToVal Pitch.Transpose where
    toVal = \case
        Pitch.Chromatic a -> DeriveT.constant ScoreT.Chromatic a
        Pitch.Diatonic a -> DeriveT.constant ScoreT.Diatonic a
        Pitch.Nn a -> DeriveT.constant ScoreT.Nn a

-- | But some calls want to default to diatonic, not chromatic.
instance Typecheck DefaultDiatonic where
    fromVal = coerceToScalar $ \(ScoreT.Typed typ val) ->
        DefaultDiatonic <$> case typ of
            ScoreT.Untyped -> Just (Pitch.Diatonic val)
            ScoreT.Chromatic -> Just (Pitch.Chromatic val)
            ScoreT.Diatonic -> Just (Pitch.Diatonic val)
            ScoreT.Nn -> Just (Pitch.Nn val)
            _ -> Nothing
    toType = numToType
instance ToVal DefaultDiatonic where toVal (DefaultDiatonic a) = toVal a
instance TypecheckNum DefaultDiatonic where
    numType _ = ValType.TDefaultDiatonic

instance Typecheck Pitch.NoteNumber where
    fromVal = coerceToScalar $ \(ScoreT.Typed typ val) ->
        Pitch.nn <$> case typ of
            ScoreT.Untyped -> Just val
            ScoreT.Nn -> Just val
            _ -> Nothing
    toType = numToType
instance ToVal Pitch.NoteNumber where
    toVal = DeriveT.constant ScoreT.Nn . Pitch.nnToDouble
instance TypecheckNum Pitch.NoteNumber where numType _ = ValType.TNoteNumber

instance Typecheck ScoreTime where
    fromVal = coerceToScalar $ \(ScoreT.Typed typ val) ->
        ScoreTime.fromDouble <$> case typ of
            ScoreT.Untyped -> Just val
            ScoreT.Score -> Just val
            _ -> Nothing
    toType = numToType
instance ToVal ScoreTime where
    toVal = DeriveT.constant ScoreT.Score . ScoreTime.toDouble
instance TypecheckNum ScoreTime where numType _ = ValType.TScoreTime

instance Typecheck RealTime where
    fromVal = coerceToScalar $ \(ScoreT.Typed typ val) ->
        RealTime.seconds <$> case typ of
            ScoreT.Untyped -> Just val
            ScoreT.Real -> Just val
            _ -> Nothing
    toType = numToType
instance ToVal RealTime where
    toVal = DeriveT.constant ScoreT.Real . RealTime.toSeconds
instance TypecheckNum RealTime where numType _ = ValType.TRealTime

instance Typecheck DeriveT.Duration where
    fromVal = coerceToScalar $ \(ScoreT.Typed typ val) -> case typ of
        -- Untyped is abiguous, and there doesn't seem to be a natural
        -- default.
        ScoreT.Score -> Just $ DeriveT.ScoreDuration (ScoreTime.fromDouble val)
        ScoreT.Real -> Just $ DeriveT.RealDuration (RealTime.seconds val)
        _ -> Nothing
    toType = numToType

instance ToVal DeriveT.Duration where
    toVal (DeriveT.ScoreDuration a) = toVal a
    toVal (DeriveT.RealDuration a) = toVal a
instance TypecheckNum DeriveT.Duration where numType _ = ValType.TTime

instance Typecheck DefaultReal where
    fromVal = coerceToScalar $ \(ScoreT.Typed typ val) ->
        DefaultReal <$> case typ of
            ScoreT.Untyped ->
                Just $ DeriveT.RealDuration (RealTime.seconds val)
            ScoreT.Score ->
                Just $ DeriveT.ScoreDuration (ScoreTime.fromDouble val)
            ScoreT.Real -> Just $ DeriveT.RealDuration (RealTime.seconds val)
            _ -> Nothing
    toType = numToType
instance ToVal DefaultReal where toVal (DefaultReal a) = toVal a
instance TypecheckNum DefaultReal where numType _ = ValType.TDefaultReal

instance Typecheck DefaultScore where
    fromVal = coerceToScalar $ \(ScoreT.Typed typ val) ->
        DefaultScore <$> case typ of
            ScoreT.Untyped ->
                Just $ DeriveT.ScoreDuration (ScoreTime.fromDouble val)
            ScoreT.Score ->
                Just $ DeriveT.ScoreDuration (ScoreTime.fromDouble val)
            ScoreT.Real -> Just $ DeriveT.RealDuration (RealTime.seconds val)
            _ -> Nothing
    toType = numToType

instance ToVal DefaultScore where toVal (DefaultScore a) = toVal a
instance TypecheckNum DefaultScore where numType _ = ValType.TDefaultScore

instance TypecheckNum a => Typecheck (Positive a) where
    fromVal v@(VSignal (ScoreT.Typed _ sig))
        | Just n <- Signal.constantVal sig, n > 0 = Positive <$> fromVal v
        | otherwise = failure
    fromVal _ = failure
    toType _ = ValType.TSignal (numType (Proxy @a)) ValType.TPositive
instance ToVal a => ToVal (Positive a) where
    toVal (Positive val) = toVal val

instance TypecheckNum a => Typecheck (NonNegative a) where
    fromVal v@(VSignal (ScoreT.Typed _ sig))
        | Just n <- Signal.constantVal sig, n >= 0 = NonNegative <$> fromVal v
        | otherwise = failure
    fromVal _ = failure
    toType _ = ValType.TSignal (numType (Proxy @a)) ValType.TNonNegative
instance ToVal a => ToVal (NonNegative a) where
    toVal (NonNegative val) = toVal val

instance Typecheck Normalized where
    fromVal = coerceToScalar (check . ScoreT.valOf)
        where
        check a
            | 0 <= a && a <= 1 = Just (Normalized a)
            | otherwise = Nothing
    toType _ = ValType.TSignal ValType.TUntyped ValType.TNormalized
instance ToVal Normalized where toVal = DeriveT.num . normalized

instance Typecheck NormalizedBipolar where
    fromVal = coerceToScalar (check . ScoreT.valOf)
        where
        check a
            | -1 <= a && a <= 1 = Just (NormalizedBipolar a)
            | otherwise = Nothing
    toType _ = ValType.TSignal ValType.TUntyped ValType.TNormalizedBipolar
instance ToVal NormalizedBipolar where
    toVal = DeriveT.num . normalizedBipolar

-- ** text\/symbol

instance Typecheck Expr.Symbol where
    fromVal (VStr (Expr.Str sym)) = success $ Expr.Symbol sym
    fromVal _ = failure
    toType _ = ValType.TStr Nothing
instance ToVal Expr.Symbol where toVal = VStr . Expr.Str . Expr.unsym

instance Typecheck Expr.Str where
    fromVal (VStr a) = success a
    fromVal _ = failure
    toType _ = ValType.TStr Nothing
instance ToVal Expr.Str where toVal = VStr

instance Typecheck Text where
    fromVal (VStr (Expr.Str s)) = success s
    fromVal _ = failure
    toType _ = ValType.TStr Nothing
instance ToVal Text where toVal = VStr . Expr.Str

instance Typecheck ScoreT.Control where
    fromVal (VStr (Expr.Str s)) =
        Val $ either (const Failure) Success (ScoreT.checkedControl s)
    fromVal _ = failure
    toType _ = ValType.TControl
instance ToVal ScoreT.Control where
    toVal c = VStr (Expr.Str (ScoreT.controlName c))

instance Typecheck ScoreT.PControl where
    fromVal (VStr (Expr.Str s)) =
        Val $ either (const Failure) Success (ScoreT.checkedPcontrol s)
    fromVal _ = failure
    toType _ = ValType.TPControl
instance ToVal ScoreT.PControl where
    toVal c = VStr (Expr.Str (ScoreT.pcontrolName c))

-- ** other types

instance Typecheck Attrs.Attributes where
    fromVal (VAttributes a) = success a
    fromVal _ = failure
    toType _ = ValType.TAttributes
instance ToVal Attrs.Attributes where toVal = VAttributes

-- Intentionally no Typecheck ControlRef, use Function instances.
instance ToVal DeriveT.ControlRef where toVal = VControlRef

-- Intentionally no Typecheck PControlRef, use PSignal instances.
instance ToVal DeriveT.PControlRef where toVal = VPControlRef

instance Typecheck PSignal.Pitch where
    fromVal = coerceToPitch
    toType _ = ValType.TPitch
instance ToVal PSignal.Pitch where toVal = VPSignal . PSignal.constant

instance Typecheck Pitch.Pitch where
    fromVal (VNotePitch a) = success a
    fromVal _ = failure
    toType _ = ValType.TNotePitch
instance ToVal Pitch.Pitch where toVal = VNotePitch

instance Typecheck ScoreT.Instrument where
    fromVal (VStr (Expr.Str a)) = success (ScoreT.Instrument a)
    fromVal _ = failure
    toType _ = ValType.TStr Nothing
instance ToVal ScoreT.Instrument where
    toVal (ScoreT.Instrument a) = VStr (Expr.Str a)

instance Typecheck DeriveT.CFunction where
    fromVal (VCFunction a) = success a
    fromVal _ = failure
    toType _ = ValType.TCFunction
instance ToVal DeriveT.CFunction where toVal = VCFunction

instance Typecheck DeriveT.PFunction where
    fromVal (VPFunction a) = success a
    fromVal _ = failure
    toType _ = ValType.TPFunction
instance ToVal DeriveT.PFunction where toVal = VPFunction

-- | Anything except a pitch can be coerced to a quoted, using ShowVal.  This
-- means you can write a lot of things without quotes.
--
-- Pitches have to be quoted because they explicitly have an invalid ShowVal.
instance Typecheck DeriveT.Quoted where
    fromVal val = case val of
        VQuoted a -> success a
        VPSignal {} -> failure
        VStr (Expr.Str sym) -> toQuoted sym
        _ -> toQuoted $ ShowVal.showVal val
        where
        toQuoted sym = success $
            DeriveT.Quoted $ Expr.Call (Expr.Symbol sym) [] :| []
    toType _ = ValType.TQuoted
instance ToVal DeriveT.Quoted where toVal = VQuoted

data NotGiven = NotGiven deriving (Show, Eq)

instance ShowVal.ShowVal NotGiven where
    showVal NotGiven = "_"

instance Typecheck NotGiven where
    fromVal VNotGiven = success NotGiven
    fromVal _ = failure
    toType _ = ValType.TNotGiven

instance ToVal NotGiven where
    toVal NotGiven = VNotGiven

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

toTranspose :: ScoreT.TransposeT -> Double -> Pitch.Transpose
toTranspose typ val = case typ of
    ScoreT.TDiatonic -> Pitch.Diatonic val
    ScoreT.TChromatic -> Pitch.Chromatic val
    ScoreT.TNn -> Pitch.Nn val

transposeControl :: ScoreT.TransposeT -> ScoreT.Control
transposeControl = \case
    ScoreT.TDiatonic -> Controls.diatonic
    ScoreT.TChromatic -> Controls.chromatic
    ScoreT.TNn -> Controls.nn

-- ** controls

coerceToTypedFunction :: (typ -> ScoreT.Function -> b)
    -> (ScoreT.Type -> Maybe typ) -> Val -> Checked b
coerceToTypedFunction make check = coerceToFunction $
    \(ScoreT.Typed typ f) -> flip make f <$> check typ

-- | Coerce any numeric value to a ScoreT.Typed Signal.Y, and check it against
-- the given function.
coerceToScalar :: (ScoreT.Typed Signal.Y -> Maybe a) -> Val -> Checked a
coerceToScalar check val
    -- It's important that constant VSignals remain Val, which means they don't
    -- need Eval, which means they don't need a time.  This is because
    -- non-signal constants in the environ like srate use from_val_simple,
    -- which ignores Eval.  TODO if I want all numeric values to be variable,
    -- then I should merge control_at with Derive.get_val, so it takes a time.
    | Just num <- DeriveT.constantVal val =
        Val $ maybe Failure Success $ check num
    | otherwise = case valToFunction val of
        Just (Right tf) -> Eval $ \t -> pure $ check (($ t) <$> tf)
        Just (Left df) -> Eval $ \t -> check . (($ t) <$>) <$> df
        Nothing -> failure

-- | Coerce any numeric value to a function, and check it against the given
-- function.
coerceToFunction :: (ScoreT.TypedFunction -> Maybe a) -> Val -> Checked a
coerceToFunction check val = case valToFunction val of
    Just (Right f) -> Val $ maybe Failure Success $ check f
    -- Eval's t time is thrown away, because I'm creating a function and thus
    -- don't need to know at which time to evaluate it.
    Just (Left df) -> Eval $ \_t -> check <$> df
    Nothing -> failure

valToFunction :: Val
    -> Maybe (Either (Derive.Deriver ScoreT.TypedFunction) ScoreT.TypedFunction)
valToFunction = \case
    VSignal sig -> Just $ Right $ Signal.at <$> sig
    VControlRef ref -> Just $ Left $ resolveFunction ref
    VCFunction cf -> Just $ Left $ do
        cfDyn <- Internal.getControlFunctionDynamic
        pure $ DeriveT.callCfunction cfDyn cf
    VPFunction f -> Just $ Right $ DeriveT.pfFunction f
    _ -> Nothing

-- | Unfortunately Internal.get_control_function_dynamic is non-trivial,
-- so it makes sense to do it only once when converting many.
--
-- TODO does it really?  It seems like a bogus tradeoff to have to make.
-- I could cache it, but is that not what this is?
-- If it's cheap to call Internal.get_control_function_dynamic and only
-- expensive to force it, then I should always pass it, and rely on laziness.
valToFunctionDyn :: DeriveT.Dynamic -> Val
    -> Maybe (Either (Derive.Deriver ScoreT.TypedFunction) ScoreT.TypedFunction)
valToFunctionDyn cfDyn = \case
    VSignal sig -> Just $ Right $ Signal.at <$> sig
    -- TODO propagate cf_dyn through
    VControlRef ref -> Just $ Left $ resolveFunction ref
    VCFunction cf -> Just $ Right $ DeriveT.callCfunction cfDyn cf
    _ -> Nothing

resolveFunction :: DeriveT.ControlRef -> Derive.Deriver ScoreT.TypedFunction
resolveFunction ref =
    Derive.require ("control not found: " <> ShowVal.showVal ref)
        =<< lookupFunction ref

-- | Resolve a ref to a function, applying a CFunction if there is one.
lookupFunction :: DeriveT.ControlRef
    -> Derive.Deriver (Maybe ScoreT.TypedFunction)
lookupFunction (DeriveT.Ref control deflt) = do
    maybe (pure defltF) get . DeriveT.lookup (ScoreT.controlName control)
        =<< Internal.getEnviron
    where
    defltF = fmap Signal.at <$> deflt
    get :: Val -> Derive.Deriver (Maybe ScoreT.TypedFunction)
    get val = case valToFunction val of
        Nothing -> pure Nothing
        Just (Right tf) -> pure $ Just tf
        Just (Left dtf) -> Just <$> dtf

-- *** signal

-- | Resolve ref to a ScoreT.TypedSignal.  This does not take ControlFunctions
-- into account, but is necessary when you need the actual signal.
resolveSignal :: DeriveT.ControlRef
    -> Derive.Deriver (Maybe ScoreT.TypedSignal)
resolveSignal (DeriveT.Ref control deflt) =
    (<|> deflt) <$> lookupSignal control

coerceToSignal :: Val -> Checked ScoreT.TypedSignal
coerceToSignal val = case valToSignal val of
    Nothing -> failure
    Just (Left dsig) -> Eval $ \_ -> Just <$> dsig
    Just (Right sig) -> success sig

-- | As with 'lookup_pitch_function', this should be in Deriver.Lib.
lookupSignal :: ScoreT.Control -> Derive.Deriver (Maybe ScoreT.TypedSignal)
lookupSignal control =
    traverse get . DeriveT.lookup (ScoreT.controlName control)
        =<< Internal.getEnviron
    where
    get val = case valToSignal val of
        Nothing -> Derive.throw $ "can't be coerced to signal: "
            <> ShowVal.showVal val
        Just (Left df) -> df
        Just (Right f) -> pure f

valToSignal :: Val
    -> Maybe (Either (Derive.Deriver ScoreT.TypedSignal) ScoreT.TypedSignal)
valToSignal = \case
    VSignal a -> Just $ Right a
    VControlRef ref -> Just $ Left $
        Derive.require ("control not found: " <> ShowVal.showVal ref)
            =<< resolveSignal ref
    VCFunction cf -> Just $ Right $ DeriveT.cfSignal cf
    _ -> Nothing


-- ** pitch signals

-- Unlike controls, this works with signals instead of functions.  Ultimately
-- this is because there is no equaivalent of CFunction.  Signals are
-- less flexible than functions, but more useful since I can splice a signal
-- back into the environment, while functions are opaque.

-- | This is the pitch version of 'coerce_to_scalar'.
coerceToPitch :: Val -> Checked PSignal.Pitch
coerceToPitch = \case
    VPSignal sig | Just p <- PSignal.constantVal sig -> success p
    val | Just (ScoreT.Typed ScoreT.Nn nn) <- DeriveT.constantVal val ->
        success $ PSignal.nnPitch (Pitch.nn nn)
    val -> case valToPitchSignal val of
        Nothing -> failure
        Just (Left dsig) -> Eval $ \pos ->
            fmap Just . require pos . (`PSignal.at` pos) =<< dsig
        Just (Right sig) -> Eval $ \pos ->
            Just <$> require pos (PSignal.at sig pos)
        where
        require pos = Derive.require $
            "no pitch at " <> pretty pos <> " for " <> ShowVal.showVal val

coerceToPitchSignal :: Val -> Checked PSignal.PSignal
coerceToPitchSignal val = case valToPitchSignal val of
    Nothing -> failure
    Just (Left dsig) -> Eval $ \_ -> Just <$> dsig
    Just (Right sig) -> success sig

-- | This is the pitch version of 'resolve_control_ref', except simpler
-- because there's no pitch equivalent of CFunction.
resolvePitchRef :: DeriveT.PControlRef -> Derive.Deriver PSignal.PSignal
resolvePitchRef (DeriveT.Ref control deflt) =
    Derive.require
        ("named pitch not found and no default: " <> ShowVal.showVal control)
            =<< (<|> deflt) <$> lookupPitchSignal control

-- | This should be in Deriver.Lib, but has to be here so the instance
-- Typecheck PitchFunction can be declared here and avoid circular import.
lookupPitchSignal :: ScoreT.PControl -> Derive.Deriver (Maybe PSignal.PSignal)
lookupPitchSignal pcontrol
    | pcontrol == ScoreT.defaultPitch =
        Just <$> Internal.getDynamic Derive.statePitch
    | otherwise =
        traverse get . DeriveT.lookup (ScoreT.pcontrolName pcontrol)
            =<< Internal.getEnviron
    where
    -- This is recursive, because if a PControl resolves to a VPControlRef
    -- it will trigger another lookup.  If the refs point to each other then
    -- we will loop, so don't do that!  This also re-implements Env.checked_val
    -- to avoid circular import.
    get val = case valToPitchSignal val of
        Nothing -> Derive.throw $
            pretty pcontrol <> ": " <> pretty (ValType.specificTypeOf val)
                <> " can't be coerced to pitch signal"
        Just (Left df) -> df
        Just (Right f) -> pure f

valToPitchSignal :: Val
    -> Maybe (Either (Derive.Deriver PSignal.PSignal) PSignal.PSignal)
valToPitchSignal = \case
    VPControlRef ref -> Just $ Left $ resolvePitchRef ref
    VPSignal sig -> Just $ Right sig
    val -> case DeriveT.constantVal val of
        Just (ScoreT.Typed ScoreT.Nn nn) ->
            Just $ Right $ PSignal.constant $ PSignal.nnPitch (Pitch.nn nn)
        _ -> Nothing

-- * sub tracks

instance Typecheck SubT.Track where
    fromVal _ = failure
    toType _ = ValType.TDeriver "note"
    fromSubtrack = Just
