-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DefaultSignatures, DeriveFunctor #-}
{-# LANGUAGE ConstraintKinds #-}
module Derive.Typecheck where
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Ratio as Ratio
import qualified Data.Set as Set

import qualified Util.Seq as Seq
import qualified Util.Texts as Texts
import qualified Cmd.Ruler.Meter as Meter
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

import qualified Ui.ScoreTime as ScoreTime

import           Global
import           Types


-- * signal functions

type TypedFunction = RealTime -> ScoreT.Typed Signal.Y
type Function = RealTime -> Signal.Y

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
typecheck :: forall a. Typecheck a => Text -> ScoreTime -> DeriveT.Val
    -> Derive.Deriver a
typecheck msg pos val = from_val_eval pos val >>= \case
    Just a -> return a
    Nothing -> Derive.throw $
        Texts.join2 ": " msg $ type_error_msg (Proxy :: Proxy a) val
    -- TODO throw a TypeError directly?

-- | Typecheck a simple value, with no evaluation.  This means you can't
-- get a deriver or coerce signal to a number.
typecheck_simple :: forall a. Typecheck a => DeriveT.Val -> Either Text a
typecheck_simple val =
    justErr (type_error_msg (Proxy :: Proxy a) val) (from_val_simple val)

type_error_msg :: Typecheck a => Proxy a -> Val -> Text
type_error_msg expected val = "expected " <> pretty (to_type expected)
    <> " but got " <> pretty (ValType.type_of val)

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

-- | This is the class of values which can be converted to from
-- a 'Val'.
class Typecheck a where
    from_val :: Val -> Checked a
    default from_val :: TEnum a => Val -> Checked a
    from_val = from_val_symbol enum_map

    to_type :: Proxy a -> ValType.Type
    default to_type :: TEnum a => Proxy a -> ValType.Type
    to_type Proxy = to_type_symbol [minBound :: a .. maxBound]

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
enum_map = Map.fromList $ Seq.key_on ShowVal.show_val vals
    where vals = [minBound :: a .. maxBound]

num_to_type :: TypecheckNum a => Proxy a -> ValType.Type
num_to_type proxy = ValType.TNum (num_type proxy) ValType.TAny

class Typecheck a => TypecheckNum a where
    num_type :: Proxy a -> ValType.NumType

instance Typecheck Bool
instance ToVal Bool

instance ShowVal.ShowVal Meter.RankName
instance Typecheck Meter.RankName

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

-- | Coerce any numeric value to a ScoreT.Typed Signal.Y, and check it against
-- the given function.
num_to_scalar :: (ScoreT.Typed Signal.Y -> Maybe a) -> Val -> Checked a
num_to_scalar check val = case val of
    VNum a -> Val $ maybe Failure Success $ check a
    VControlRef cref -> Eval $ \p -> check . ($p) <$> to_typed_function cref
    VControlFunction cf -> Eval $ \p -> check . ($p) <$> control_function cf
    _ -> failure

-- | Coerce any numeric value to a function, and check it against the given
-- function.
num_to_function :: (TypedFunction -> Maybe a) -> Val -> Checked a
num_to_function check val = case val of
    VNum a -> Val $ maybe Failure Success $ check $ const a
    VControlRef cref -> Eval $ const $ check <$> to_typed_function cref
    VControlFunction cf -> Eval $ const $ check <$> control_function cf
    _ -> failure

-- | Like 'num_to_function', but take a constructor with a type argument,
-- and a separate function to verify the type.
num_to_checked_function :: (Function -> typ -> b)
    -> (ScoreT.Type -> Maybe typ) -> Val -> Checked b
num_to_checked_function make check_type = num_to_function $ \f ->
    make (ScoreT.typed_val . f) <$> check_type (ScoreT.type_of (f 0))

-- | Evaluate a control function with no backing control.
control_function :: DeriveT.ControlFunction -> Derive.Deriver TypedFunction
control_function cf = DeriveT.call_control_function cf Controls.null <$>
    Internal.get_control_function_dynamic

-- *** eval only

-- These don't have ToVal instances.

instance Typecheck TypedFunction where
    from_val = num_to_function Just
    to_type _ = ValType.TOther "typed number or signal"

-- TODO use a special character to indicate that these are not parseable, and
-- thus invalid ShowVal instances.
instance ShowVal.ShowVal TypedFunction where
    show_val _ = "((TypedFunction))"

instance Typecheck Function where
    from_val = num_to_function (Just . fmap ScoreT.typed_val)
    to_type _ = ValType.TOther "untyped number or signal"

instance ShowVal.ShowVal Function where
    show_val _ = "((Function))"

data DefaultRealTimeFunction = DefaultRealTimeFunction !Function !TimeType

instance ShowVal.ShowVal DefaultRealTimeFunction where
    show_val _ = "((DefaultRealTimeFunction))"

instance Typecheck DefaultRealTimeFunction where
    from_val = num_to_checked_function DefaultRealTimeFunction (time_type Real)
    to_type _ = ValType.TOther "time number or signal"

-- Originally I used DataKinds e.g.
-- TransposeFunction (deflt :: TransposeType), but it seemed less
-- convenient than separate data types.

data TransposeFunctionDiatonic =
    TransposeFunctionDiatonic !Function !ScoreT.Control
instance ShowVal.ShowVal TransposeFunctionDiatonic where
    show_val _ = "((TransposeFunctionDiatonic))"
instance Typecheck TransposeFunctionDiatonic where
    from_val = num_to_checked_function TransposeFunctionDiatonic
        (type_to_control Diatonic)
    to_type _ = ValType.TOther "transpose number or signal, default diatonic"

data TransposeFunctionChromatic =
    TransposeFunctionChromatic !Function !ScoreT.Control
instance ShowVal.ShowVal TransposeFunctionChromatic where
    show_val _ = "((TransposeFunctionChromatic))"
instance Typecheck TransposeFunctionChromatic where
    from_val = num_to_checked_function TransposeFunctionChromatic
        (type_to_control Chromatic)
    to_type _ = ValType.TOther "transpose number or signal, default chromatic"

data TransposeFunctionNn = TransposeFunctionNn !Function !ScoreT.Control
instance ShowVal.ShowVal TransposeFunctionNn where
    show_val _ = "((TransposeFunctionNn))"
instance Typecheck TransposeFunctionNn where
    from_val = num_to_checked_function TransposeFunctionNn
        (type_to_control Nn)
    to_type _ = ValType.TOther "transpose number or signal, default nn"

type_to_control :: TransposeType -> ScoreT.Type -> Maybe ScoreT.Control
type_to_control deflt = fmap transpose_control . transpose_type deflt

-- *** scalar

instance Typecheck (ScoreT.Typed Signal.Y) where
    from_val = num_to_scalar Just
    to_type = num_to_type
instance ToVal (ScoreT.Typed Signal.Y) where to_val = VNum
instance TypecheckNum (ScoreT.Typed Signal.Y) where
    num_type _ = ValType.TUntyped

instance Typecheck Double where
    from_val = num_to_scalar (Just . ScoreT.typed_val)
    to_type = num_to_type
instance ToVal Double where to_val = VNum . ScoreT.untyped
instance TypecheckNum Double where num_type _ = ValType.TUntyped

instance Typecheck (Ratio.Ratio Int) where
    from_val = num_to_scalar $
        Just . realToFrac . flip Ratio.approxRational 0.001 . ScoreT.typed_val
    to_type = num_to_type
instance ToVal (Ratio.Ratio Int) where
    to_val = VNum . ScoreT.untyped . realToFrac
instance TypecheckNum (Ratio.Ratio Int) where num_type _ = ValType.TUntyped

instance Typecheck Int where
    from_val = from_integral_val
    to_type = num_to_type

instance Typecheck Integer where
    from_val = from_integral_val
    to_type = num_to_type

from_integral_val :: Integral a => Val -> Checked a
from_integral_val = num_to_scalar (check . ScoreT.typed_val)
    where
    check a = if frac == 0 then Just int else Nothing
        where (int, frac) = properFraction a

instance ToVal Int where to_val = VNum . ScoreT.untyped . fromIntegral
instance ToVal Integer where to_val = VNum . ScoreT.untyped . fromIntegral
instance TypecheckNum Int where num_type _ = ValType.TInt
instance TypecheckNum Integer where num_type _ = ValType.TInt

-- | VNums can also be coerced into chromatic transposition, so you can write
-- a plain number if you don't care about diatonic.
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
    from_val = num_to_scalar $ \(ScoreT.Typed typ val) -> case typ of
        ScoreT.Untyped -> Just (Pitch.Chromatic val)
        ScoreT.Chromatic -> Just (Pitch.Chromatic val)
        ScoreT.Diatonic -> Just (Pitch.Diatonic val)
        ScoreT.Nn -> Just (Pitch.Nn val)
        _ -> Nothing
    to_type = num_to_type
instance TypecheckNum Pitch.Transpose where num_type _ = ValType.TTranspose

instance ToVal Pitch.Transpose where
    to_val (Pitch.Chromatic a) = VNum $ ScoreT.Typed ScoreT.Chromatic a
    to_val (Pitch.Diatonic a) = VNum $ ScoreT.Typed ScoreT.Diatonic a
    to_val (Pitch.Nn a) = VNum $ ScoreT.Typed ScoreT.Nn a

-- | But some calls want to default to diatonic, not chromatic.
instance Typecheck DefaultDiatonic where
    from_val = num_to_scalar $ \(ScoreT.Typed typ val) ->
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
    from_val = num_to_scalar $ \(ScoreT.Typed typ val) ->
        Pitch.nn <$> case typ of
            ScoreT.Untyped -> Just val
            ScoreT.Nn -> Just val
            _ -> Nothing
    to_type = num_to_type
instance ToVal Pitch.NoteNumber where
    to_val = VNum . ScoreT.Typed ScoreT.Nn . Pitch.nn_to_double
instance TypecheckNum Pitch.NoteNumber where num_type _ = ValType.TNoteNumber

instance Typecheck ScoreTime where
    from_val = num_to_scalar $ \(ScoreT.Typed typ val) ->
        ScoreTime.from_double <$> case typ of
            ScoreT.Untyped -> Just val
            ScoreT.Score -> Just val
            _ -> Nothing
    to_type = num_to_type
instance ToVal ScoreTime where
    to_val a = VNum $ ScoreT.Typed ScoreT.Score (ScoreTime.to_double a)
instance TypecheckNum ScoreTime where num_type _ = ValType.TScoreTime

instance Typecheck RealTime where
    from_val = num_to_scalar $ \(ScoreT.Typed typ val) ->
        RealTime.seconds <$> case typ of
            ScoreT.Untyped -> Just val
            ScoreT.Real -> Just val
            _ -> Nothing
    to_type = num_to_type
instance ToVal RealTime where
    to_val a = VNum $ ScoreT.Typed ScoreT.Real (RealTime.to_seconds a)
instance TypecheckNum RealTime where num_type _ = ValType.TRealTime

instance Typecheck DeriveT.Duration where
    from_val = num_to_scalar $ \(ScoreT.Typed typ val) -> case typ of
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
    from_val = num_to_scalar $ \(ScoreT.Typed typ val) ->
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
    from_val = num_to_scalar $ \(ScoreT.Typed typ val) ->
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
    from_val v@(VNum val)
        | ScoreT.typed_val val > 0 = Positive <$> from_val v
        | otherwise = failure
    from_val _ = failure
    to_type _ = ValType.TNum (num_type (Proxy :: Proxy a)) ValType.TPositive
instance ToVal a => ToVal (Positive a) where
    to_val (Positive val) = to_val val

instance TypecheckNum a => Typecheck (NonNegative a) where
    from_val v@(VNum val)
        | ScoreT.typed_val val >= 0 = NonNegative <$> from_val v
        | otherwise = failure
    from_val _ = failure
    to_type _ = ValType.TNum (num_type (Proxy :: Proxy a)) ValType.TNonNegative
instance ToVal a => ToVal (NonNegative a) where
    to_val (NonNegative val) = to_val val

instance Typecheck Normalized where
    from_val = num_to_scalar (check . ScoreT.typed_val)
        where
        check a
            | 0 <= a && a <= 1 = Just (Normalized a)
            | otherwise = Nothing
    to_type _ = ValType.TNum ValType.TUntyped ValType.TNormalized
instance ToVal Normalized where to_val = VNum . ScoreT.untyped . normalized

instance Typecheck NormalizedBipolar where
    from_val = num_to_scalar (check . ScoreT.typed_val)
        where
        check a
            | -1 <= a && a <= 1 = Just (NormalizedBipolar a)
            | otherwise = Nothing
    to_type _ = ValType.TNum ValType.TUntyped ValType.TNormalizedBipolar
instance ToVal NormalizedBipolar where
    to_val = VNum . ScoreT.untyped . normalized_bipolar

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
        Val $ either (const Failure) Success (ScoreT.control s)
    from_val _ = failure
    to_type _ = ValType.TControl
instance ToVal ScoreT.Control where
    to_val c = VStr (Expr.Str (ScoreT.control_name c))

instance Typecheck ScoreT.PControl where
    from_val (VStr (Expr.Str s)) =
        Val $ either (const Failure) Success (ScoreT.pcontrol s)
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

-- | Use a 'TypedFunction' or 'Function' instead of this.
instance Typecheck DeriveT.ControlRef where
    from_val (VControlRef a) = success a
    from_val (VNum a) = success $ DeriveT.ControlSignal $
        Signal.constant <$> a
    from_val _ = failure
    to_type _ = ValType.TControlRef
instance ToVal DeriveT.ControlRef where to_val = VControlRef

instance Typecheck DeriveT.PControlRef where
    from_val (VPControlRef a) = success a
    from_val (VPitch a) = success $ DeriveT.ControlSignal $
        PSignal.constant a
    from_val _ = failure
    to_type _ = ValType.TPControlRef
instance ToVal DeriveT.PControlRef where to_val = VPControlRef

instance Typecheck PSignal.Pitch where
    from_val (VPitch a) = success a
    from_val (VPControlRef pref) = Eval $ \pos -> Just <$> pitch_at pos pref
    from_val (VNum (ScoreT.Typed ScoreT.Nn nn)) =
        success $ PSignal.nn_pitch (Pitch.nn nn)
    from_val _ = failure
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

instance Typecheck DeriveT.ControlFunction where
    from_val (VControlFunction a) = success a
    from_val _ = failure
    to_type _ = ValType.TControlFunction
instance ToVal DeriveT.ControlFunction where to_val = VControlFunction

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

data TimeType = Real | Score deriving (Eq, Show)

instance Pretty TimeType where pretty = showt

time_type :: TimeType -> ScoreT.Type -> Maybe TimeType
time_type deflt typ = case typ of
    ScoreT.Untyped -> Just deflt
    ScoreT.Real -> Just Real
    ScoreT.Score -> Just Score
    _ -> Nothing

data TransposeType = Diatonic | Chromatic | Nn deriving (Eq, Show)

instance Pretty TransposeType where pretty = showt

transpose_type :: TransposeType -> ScoreT.Type -> Maybe TransposeType
transpose_type deflt typ = case typ of
    ScoreT.Untyped -> Just deflt
    ScoreT.Diatonic -> Just Diatonic
    ScoreT.Chromatic -> Just Chromatic
    ScoreT.Nn -> Just Nn
    _ -> Nothing

to_transpose :: TransposeType -> Double -> Pitch.Transpose
to_transpose typ val = case typ of
    Diatonic -> Pitch.Diatonic val
    Chromatic -> Pitch.Chromatic val
    Nn -> Pitch.Nn val

transpose_control :: TransposeType -> ScoreT.Control
transpose_control Diatonic = Controls.diatonic
transpose_control Chromatic = Controls.chromatic
transpose_control Nn = Controls.nn

-- ** to_typed_function

-- | Convert a 'DeriveT.ControlRef' to a function.
--
-- If a signal exists but doesn't have a type, the type will be inherited from
-- the default.  This way a call can cause a signal parameter to default to
-- a certain type.
to_typed_function :: DeriveT.ControlRef -> Derive.Deriver TypedFunction
to_typed_function control =
    convert_to_function control =<< to_signal_or_function control

to_function :: DeriveT.ControlRef -> Derive.Deriver Function
to_function = fmap (ScoreT.typed_val .) . to_typed_function

convert_to_function :: DeriveT.ControlRef
    -> Either (ScoreT.Typed Signal.Control) DeriveT.ControlFunction
    -> Derive.Deriver TypedFunction
convert_to_function control = either (return . signal_function) from_function
    where
    signal_function sig t = Signal.at t <$> sig
    from_function f = DeriveT.call_control_function f score_control <$>
        Internal.get_control_function_dynamic
    score_control = case control of
        DeriveT.ControlSignal {} -> Controls.null
        DeriveT.DefaultedControl cont _ -> cont
        DeriveT.LiteralControl cont -> cont

to_signal_or_function :: DeriveT.ControlRef
    -> Derive.Deriver (Either (ScoreT.Typed Signal.Control)
        DeriveT.ControlFunction)
to_signal_or_function control = case control of
    DeriveT.ControlSignal sig -> return $ Left sig
    DeriveT.DefaultedControl cont deflt ->
        get_control (ScoreT.type_of deflt) (return (Left deflt)) cont
    DeriveT.LiteralControl cont ->
        get_control ScoreT.Untyped (Derive.throw $ "not found: " <> showt cont)
            cont
    where
    get_control default_type deflt cont = get_function cont >>= \case
        Just f -> return $ Right $
            DeriveT.modify_control_function (inherit_type default_type .) f
        Nothing -> get_control_signal cont >>= \case
            Just sig -> return $ Left sig
            Nothing -> deflt
    get_function cont = Internal.get_dynamic $
        Map.lookup cont . Derive.state_control_functions
    -- If the signal was untyped, it gets the type of the default, since
    -- presumably the caller expects that type.
    inherit_type default_type val =
        val { ScoreT.type_of = ScoreT.type_of val <> default_type }
    get_control_signal control = Map.lookup control <$>
        Internal.get_dynamic Derive.state_controls

-- | This is the pitch signal version of 'to_signal_or_function', except
-- simpler because there's no pitch equivalent of ControlFunction.
--
-- I could actually have a pitch version of 'Function', which I guess would be
-- called PitchFunction, except be unlike ControlFunction, in that it actually
-- is a function, where ControlFunction isn't.   What a mess, I wish I could
-- get rid of ControlFunction...
pitch_at :: RealTime -> DeriveT.PControlRef -> Derive.Deriver PSignal.Pitch
pitch_at pos control = case control of
    DeriveT.ControlSignal sig -> require sig
    DeriveT.DefaultedControl control deflt ->
        maybe (require deflt) return =<< named_pitch_at control
    DeriveT.LiteralControl control -> do
        Derive.require
            ("pitch not found and no default given: " <> showt control)
            =<< named_pitch_at control
    where
    -- There is a Derive.named_pitch_at, but it's in Derive.Deriver.Lib, which
    -- imports this.
    named_pitch_at control = do
        maybe_sig <- Internal.get_named_pitch control
        return $ PSignal.at pos =<< maybe_sig
    require = Derive.require ("ControlSignal pitch at " <> pretty pos)
        . PSignal.at pos

-- * sub tracks

instance Typecheck SubT.Track where
    from_val _ = failure
    to_type _ = ValType.TDeriver "note"
    from_subtrack = Just
