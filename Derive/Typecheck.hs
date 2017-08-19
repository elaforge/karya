-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures, DeriveFunctor #-}
module Derive.Typecheck where
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.TextUtil as TextUtil
import qualified Ui.ScoreTime as ScoreTime
import qualified Cmd.Ruler.Meter as Meter
import qualified Derive.Attrs as Attrs
import qualified Derive.BaseTypes as BaseTypes
import Derive.BaseTypes (Val(..))
import qualified Derive.Controls as Controls
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Deriver.Monad as Derive
import qualified Derive.Expr as Expr
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.ValType as ValType

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import Global
import Types


-- * signal functions

type TypedFunction = RealTime -> Score.TypedVal
type Function = RealTime -> Signal.Y

-- * type wrappers

-- | Either RealTime or ScoreTime, but untyped defaults to RealTime.
-- This has a short accessor to make unwrapping more concise.
newtype DefaultReal = DefaultReal { _real :: BaseTypes.Duration }
    deriving (Eq, Show, ShowVal.ShowVal)
instance Internal.Time DefaultReal where
    real = Internal.real . _real
    score = Internal.score . _real
    to_duration = _real

-- | Same as 'DefaultReal' but untyped defaults to ScoreTime.
-- This has a short accessor to make unwrapping more concise.
newtype DefaultScore = DefaultScore { _score :: BaseTypes.Duration }
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
real = DefaultReal . BaseTypes.RealDuration

score :: ScoreTime -> DefaultScore
score = DefaultScore . BaseTypes.ScoreDuration

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

-- | Normally Transpose will default to Chromatic if the val is untyped,
-- but some calls would prefer to default to Diatonic.
newtype DefaultDiatonic =
    DefaultDiatonic { default_diatonic :: Pitch.Transpose }
    deriving (Show, Eq, ShowVal.ShowVal)

diatonic :: Double -> DefaultDiatonic
diatonic = DefaultDiatonic . Pitch.Diatonic

-- * typecheck utils

-- | Typecheck a single Val, and throw if it's the wrong type.
typecheck :: forall a. Typecheck a => Text -> ScoreTime -> BaseTypes.Val
    -> Derive.Deriver a
typecheck msg pos val = from_val_eval pos val >>= \x -> case x of
    Just a -> return a
    Nothing -> Derive.throw $
        TextUtil.joinWith ": " msg $ type_error_msg (Proxy :: Proxy a) val
    -- TODO throw a TypeError directly?

typecheck_simple :: forall a. Typecheck a => BaseTypes.Val -> Either Text a
typecheck_simple val =
    justErr (type_error_msg (Proxy :: Proxy a) val) (from_val_simple val)

type_error_msg :: Typecheck a => Proxy a -> Val -> Text
type_error_msg expected val = "expected " <> pretty (to_type expected)
    <> " but got " <> pretty (ValType.type_of val)

-- * Typecheck class

data Checked a = Val (Maybe a)
    -- | This val needs to be evaluated to know if it will typecheck.  The
    -- argument is the call start time, used when coercing a function to
    -- a scalar.
    | Eval (RealTime -> Derive.Deriver (Maybe a))
    deriving (Functor)

-- | This is the class of values which can be converted to from
-- a 'Val'.
class Typecheck a where
    from_val :: Val -> Checked a
    default from_val :: TypecheckSymbol a => Val -> Checked a
    from_val (VStr str) = Val $ parse_symbol str
    from_val _ = Val Nothing

    to_type :: Proxy a -> ValType.Type
    default to_type :: TypecheckSymbol a => Proxy a -> ValType.Type
    to_type proxy = ValType.TStr (symbol_values proxy)

-- | 'from_val', but evaluate if it's an Eval.
from_val_eval :: Typecheck a => ScoreTime -> Val -> Derive.Deriver (Maybe a)
from_val_eval pos val = case from_val val of
    Val a -> return a
    Eval deriver -> deriver =<< Derive.score_to_real pos

from_val_simple :: Typecheck a => Val -> Maybe a
from_val_simple val = case from_val val of
    Val (Just a) -> Just a
    _ -> Nothing

-- | Return a simple Eval check which doesn't depend on RealTime.
eval :: Typecheck a => (a -> Derive.Deriver b) -> Val -> Checked b
eval parse val = Eval $ \_ -> case from_val_simple val of
    Nothing -> return Nothing
    Just x -> Just <$> parse x

class ToVal a where
    to_val :: a -> Val
    -- This could be just ShowVal and thus a plain default method, but I want
    -- it to only apply when the type is explicitly in TypecheckSymbol, since
    -- it's not valid in general.
    default to_val :: TypecheckSymbol a => a -> Val
    to_val = VStr . Expr.Str . ShowVal.show_val

-- | This is for text strings which are parsed to call-specific types.  You
-- can declare an instance and the default Typecheck instance will allow you
-- to incorporate the type directly into the signature of the call.
--
-- If your type is a Bounded Enum, you get a default parser, and the enum
-- values go in the 'ValType.TStr' so the docs can mention them.
--
-- So the type needs to be in (Bounded, Enum, ShowVal, TypecheckSymbol,
-- Typecheck), though all of these can use default implementations.
class ShowVal.ShowVal a => TypecheckSymbol a where
    parse_symbol :: Expr.Str -> Maybe a
    default parse_symbol :: (Bounded a, Enum a) => Expr.Str -> Maybe a
    parse_symbol = make_parse_enum [minBound :: a .. maxBound]

    symbol_values :: Proxy a -> Maybe [Text]
    default symbol_values :: (Bounded a, Enum a) => Proxy a -> Maybe [Text]
    symbol_values _ = Just $ map ShowVal.show_val [minBound :: a .. maxBound]

make_parse_enum :: ShowVal.ShowVal a => [a] -> (Expr.Str -> Maybe a)
make_parse_enum vals = flip Map.lookup m
    where m = Map.fromList (zip (map (Expr.Str . ShowVal.show_val) vals) vals)

-- | Make a ShowVal from a Show instance.
enum_show_val :: Show a => a -> Text
enum_show_val = Text.toLower . showt

num_to_type :: TypecheckNum a => Proxy a -> ValType.Type
num_to_type proxy = ValType.TNum (num_type proxy) ValType.TAny

class Typecheck a => TypecheckNum a where
    num_type :: Proxy a -> ValType.NumType

instance Typecheck Bool
instance TypecheckSymbol Bool
instance ToVal Bool

instance ShowVal.ShowVal Meter.RankName where
    show_val = enum_show_val
instance Typecheck Meter.RankName
instance TypecheckSymbol Meter.RankName

-- * Typecheck instances

instance ToVal Val where to_val = id
instance Typecheck Val where
    from_val = Val . Just
    to_type _ = ValType.TVal

-- | Putting Maybe in Typecheck means I can have optional arguments with no
-- defaults.  Further docs in 'Derive.Sig.defaulted'.
instance Typecheck a => Typecheck (Maybe a) where
    from_val VNotGiven = Val (Just Nothing)
    from_val a = Just <$> from_val a
    to_type _ = ValType.TMaybe $ to_type (Proxy :: Proxy a)
instance ToVal a => ToVal (Maybe a) where
    to_val = maybe VNotGiven to_val

-- | Non-lists are coerced into singleton lists.
instance Typecheck a => Typecheck [a] where
    from_val (VList xs) = check xs
        where
        check [] = Val (Just [])
        -- TODO surely I can further reduce this with fmap
        check (x:xs) = case from_val x of
            Val Nothing -> Val Nothing
            Val (Just a) -> (a:) <$> check xs
            Eval a -> case check xs of
                Val Nothing -> Val Nothing
                Val (Just as) -> (:as) <$> Eval a
                Eval as -> Eval $ \p -> cons <$> a p <*> as p
        cons a as = (:) <$> a <*> as
    from_val v = (:[]) <$> from_val v
    to_type _ = ValType.TList $ to_type (Proxy :: Proxy a)
instance ToVal a => ToVal [a] where to_val = VList . map to_val

instance (Typecheck a, Typecheck b) => Typecheck (Either a b) where
    from_val a = case from_val a of
        Val Nothing -> Right <$> from_val a
        a -> Left <$> a
    to_type _ = ValType.TEither (to_type (Proxy :: Proxy a))
        (to_type (Proxy :: Proxy b))
instance (ToVal a, ToVal b) => ToVal (Either a b) where
    to_val = either to_val to_val


-- ** numeric types

-- | Coerce any numeric value to a TypedVal, and check it against the given
-- function.
num_to_scalar :: (Score.TypedVal -> Maybe a) -> Val -> Checked a
num_to_scalar check val = case val of
    VNum a -> Val $ check a
    VControlRef cref -> Eval $ \p ->
        check . ($p) <$> to_typed_function cref
    VControlFunction cf -> Eval $ \p -> check . ($p) <$> control_function cf
    _ -> Val Nothing

-- | Coerce any numeric value to a function, and check it against the given
-- function.
num_to_function :: (TypedFunction -> Maybe a) -> Val -> Checked a
num_to_function check val = case val of
    VNum a -> Val $ check $ const a
    VControlRef cref -> Eval $ const $ check <$> to_typed_function cref
    VControlFunction cf -> Eval $ const $ check <$> control_function cf
    _ -> Val Nothing

-- | Like 'num_to_function', but take a constructor with a type argument,
-- and a separate function to verify the type.
num_to_checked_function :: (Function -> typ -> b)
    -> (Score.Type -> Maybe typ) -> Val -> Checked b
num_to_checked_function make check_type = num_to_function $ \f ->
    make (Score.typed_val . f) <$> check_type (Score.type_of (f 0))

-- | Evaluate a control function with no backing control.
control_function :: BaseTypes.ControlFunction -> Derive.Deriver TypedFunction
control_function cf = BaseTypes.call_control_function cf Controls.null <$>
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
    from_val = num_to_function (Just . fmap Score.typed_val)
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
    TransposeFunctionDiatonic !Function !Score.Control
instance ShowVal.ShowVal TransposeFunctionDiatonic where
    show_val _ = "((TransposeFunctionDiatonic))"
instance Typecheck TransposeFunctionDiatonic where
    from_val = num_to_checked_function TransposeFunctionDiatonic
        (type_to_control Diatonic)
    to_type _ = ValType.TOther "transpose number or signal, default diatonic"

data TransposeFunctionChromatic =
    TransposeFunctionChromatic !Function !Score.Control
instance ShowVal.ShowVal TransposeFunctionChromatic where
    show_val _ = "((TransposeFunctionChromatic))"
instance Typecheck TransposeFunctionChromatic where
    from_val = num_to_checked_function TransposeFunctionChromatic
        (type_to_control Chromatic)
    to_type _ = ValType.TOther "transpose number or signal, default chromatic"

data TransposeFunctionNn = TransposeFunctionNn !Function !Score.Control
instance ShowVal.ShowVal TransposeFunctionNn where
    show_val _ = "((TransposeFunctionNn))"
instance Typecheck TransposeFunctionNn where
    from_val = num_to_checked_function TransposeFunctionNn
        (type_to_control Nn)
    to_type _ = ValType.TOther "transpose number or signal, default nn"

type_to_control :: TransposeType -> Score.Type -> Maybe Score.Control
type_to_control deflt = fmap transpose_control . transpose_type deflt

-- *** scalar

instance Typecheck Score.TypedVal where
    from_val = num_to_scalar Just
    to_type = num_to_type
instance ToVal Score.TypedVal where to_val = VNum
instance TypecheckNum Score.TypedVal where num_type _ = ValType.TUntyped

instance Typecheck Double where
    from_val = num_to_scalar (Just . Score.typed_val)
    to_type = num_to_type
instance ToVal Double where to_val = VNum . Score.untyped
instance TypecheckNum Double where num_type _ = ValType.TUntyped

instance Typecheck Int where
    from_val = from_integral_val
    to_type = num_to_type

instance Typecheck Integer where
    from_val = from_integral_val
    to_type = num_to_type

from_integral_val :: Integral a => Val -> Checked a
from_integral_val = num_to_scalar (check . Score.typed_val)
    where
    check a = if frac == 0 then Just int else Nothing
        where (int, frac) = properFraction a

instance ToVal Int where to_val = VNum . Score.untyped . fromIntegral
instance ToVal Integer where to_val = VNum . Score.untyped . fromIntegral
instance TypecheckNum Int where num_type _ = ValType.TInt
instance TypecheckNum Integer where num_type _ = ValType.TInt

-- | VNums can also be coerced into chromatic transposition, so you can write
-- a plain number if you don't care about diatonic.
--
-- This is different from 'BaseTypes.Duration', which does not default an
-- untyped literal, so you have to supply the type explicitly.  The rationale
-- is that many scales don't have diatonic or chromatic, and it would be
-- annoying to have to specify one or the other when it was definitely
-- irrelevant.  But the RealTime ScoreTime distinction is universal, there is
-- no single default that is appropriate for all calls.  So they have to
-- specify a default by taking a 'DefaultScore' or 'DefaultReal', or require
-- the caller to distinguish with 'BaseTypes.Duration'.
instance Typecheck Pitch.Transpose where
    from_val = num_to_scalar $ \(Score.Typed typ val) -> case typ of
        Score.Untyped -> Just (Pitch.Chromatic val)
        Score.Chromatic -> Just (Pitch.Chromatic val)
        Score.Diatonic -> Just (Pitch.Diatonic val)
        Score.Nn -> Just (Pitch.Nn val)
        _ -> Nothing
    to_type = num_to_type
instance TypecheckNum Pitch.Transpose where num_type _ = ValType.TTranspose

instance ToVal Pitch.Transpose where
    to_val (Pitch.Chromatic a) = VNum $ Score.Typed Score.Chromatic a
    to_val (Pitch.Diatonic a) = VNum $ Score.Typed Score.Diatonic a
    to_val (Pitch.Nn a) = VNum $ Score.Typed Score.Nn a

-- | But some calls want to default to diatonic, not chromatic.
instance Typecheck DefaultDiatonic where
    from_val = num_to_scalar $ \(Score.Typed typ val) ->
        DefaultDiatonic <$> case typ of
            Score.Untyped -> Just (Pitch.Diatonic val)
            Score.Chromatic -> Just (Pitch.Chromatic val)
            Score.Diatonic -> Just (Pitch.Diatonic val)
            Score.Nn -> Just (Pitch.Nn val)
            _ -> Nothing
    to_type = num_to_type
instance ToVal DefaultDiatonic where to_val (DefaultDiatonic a) = to_val a
instance TypecheckNum DefaultDiatonic where
    num_type _ = ValType.TDefaultDiatonic

instance Typecheck Pitch.NoteNumber where
    from_val = num_to_scalar $ \(Score.Typed typ val) ->
        Pitch.nn <$> case typ of
            Score.Untyped -> Just val
            Score.Nn -> Just val
            _ -> Nothing
    to_type = num_to_type
instance ToVal Pitch.NoteNumber where
    to_val = VNum . Score.Typed Score.Nn . Pitch.nn_to_double
instance TypecheckNum Pitch.NoteNumber where num_type _ = ValType.TNoteNumber

instance Typecheck ScoreTime where
    from_val = num_to_scalar $ \(Score.Typed typ val) ->
        ScoreTime.double <$> case typ of
            Score.Untyped -> Just val
            Score.Score -> Just val
            _ -> Nothing
    to_type = num_to_type
instance ToVal ScoreTime where
    to_val a = VNum $ Score.Typed Score.Score (ScoreTime.to_double a)
instance TypecheckNum ScoreTime where num_type _ = ValType.TScoreTime

instance Typecheck RealTime where
    from_val = num_to_scalar $ \(Score.Typed typ val) ->
        RealTime.seconds <$> case typ of
            Score.Untyped -> Just val
            Score.Real -> Just val
            _ -> Nothing
    to_type = num_to_type
instance ToVal RealTime where
    to_val a = VNum $ Score.Typed Score.Real (RealTime.to_seconds a)
instance TypecheckNum RealTime where num_type _ = ValType.TRealTime

instance Typecheck BaseTypes.Duration where
    from_val = num_to_scalar $ \(Score.Typed typ val) -> case typ of
        -- Untyped is abiguous, and there doesn't seem to be a natural
        -- default.
        Score.Score -> Just $ BaseTypes.ScoreDuration (ScoreTime.double val)
        Score.Real -> Just $ BaseTypes.RealDuration (RealTime.seconds val)
        _ -> Nothing
    to_type = num_to_type

instance ToVal BaseTypes.Duration where
    to_val (BaseTypes.ScoreDuration a) = to_val a
    to_val (BaseTypes.RealDuration a) = to_val a
instance TypecheckNum BaseTypes.Duration where num_type _ = ValType.TTime

instance Typecheck DefaultReal where
    from_val = num_to_scalar $ \(Score.Typed typ val) ->
        DefaultReal <$> case typ of
            Score.Untyped ->
                Just $ BaseTypes.RealDuration (RealTime.seconds val)
            Score.Score -> Just $ BaseTypes.ScoreDuration (ScoreTime.double val)
            Score.Real -> Just $ BaseTypes.RealDuration (RealTime.seconds val)
            _ -> Nothing
    to_type = num_to_type
instance ToVal DefaultReal where to_val (DefaultReal a) = to_val a
instance TypecheckNum DefaultReal where num_type _ = ValType.TDefaultReal

instance Typecheck DefaultScore where
    from_val = num_to_scalar $ \(Score.Typed typ val) ->
        DefaultScore <$> case typ of
            Score.Untyped ->
                Just $ BaseTypes.ScoreDuration (ScoreTime.double val)
            Score.Score -> Just $ BaseTypes.ScoreDuration (ScoreTime.double val)
            Score.Real -> Just $ BaseTypes.RealDuration (RealTime.seconds val)
            _ -> Nothing
    to_type = num_to_type

instance ToVal DefaultScore where to_val (DefaultScore a) = to_val a
instance TypecheckNum DefaultScore where num_type _ = ValType.TDefaultScore

instance TypecheckNum a => Typecheck (Positive a) where
    from_val v@(VNum val)
        | Score.typed_val val > 0 = Positive <$> from_val v
        | otherwise = Val Nothing
    from_val _ = Val Nothing
    to_type _ = ValType.TNum (num_type (Proxy :: Proxy a)) ValType.TPositive
instance ToVal a => ToVal (Positive a) where
    to_val (Positive val) = to_val val

instance TypecheckNum a => Typecheck (NonNegative a) where
    from_val v@(VNum val)
        | Score.typed_val val >= 0 = NonNegative <$> from_val v
        | otherwise = Val Nothing
    from_val _ = Val Nothing
    to_type _ = ValType.TNum (num_type (Proxy :: Proxy a)) ValType.TNonNegative
instance ToVal a => ToVal (NonNegative a) where
    to_val (NonNegative val) = to_val val

instance Typecheck Normalized where
    from_val = num_to_scalar (check . Score.typed_val)
        where
        check a
            | a <= a && a <= 1 = Just (Normalized a)
            | otherwise = Nothing
    to_type _ = ValType.TNum ValType.TUntyped ValType.TNormalized
instance ToVal Normalized where to_val = VNum . Score.untyped . normalized

-- ** text\/symbol

instance Typecheck Expr.Symbol where
    from_val (VStr (Expr.Str sym)) = Val $ Just $ Expr.Symbol sym
    from_val _ = Val Nothing
    to_type _ = ValType.TStr Nothing
instance ToVal Expr.Symbol where to_val = VStr . Expr.Str . Expr.unsym

instance Typecheck Expr.Str where
    from_val (VStr a) = Val $ Just a
    from_val _ = Val Nothing
    to_type _ = ValType.TStr Nothing
instance ToVal Expr.Str where to_val = VStr

instance Typecheck Text where
    from_val (VStr (Expr.Str s)) = Val $ Just s
    from_val _ = Val Nothing
    to_type _ = ValType.TStr Nothing
instance ToVal Text where to_val = VStr . Expr.Str

instance Typecheck Score.Control where
    from_val (VStr (Expr.Str s)) =
        Val $ either (const Nothing) Just (Score.control s)
    from_val _ = Val Nothing
    to_type _ = ValType.TControl
instance ToVal Score.Control where
    to_val c = VStr (Expr.Str (Score.control_name c))

instance Typecheck Score.PControl where
    from_val (VStr (Expr.Str s))
        | Just name <- Text.stripPrefix "#" s =
            Val $ either (const Nothing) Just (Score.pcontrol name)
    from_val _ = Val Nothing
    to_type _ = ValType.TPControl
instance ToVal Score.PControl where
    to_val c = VStr (Expr.Str (Score.pcontrol_name c))

-- ** other types

instance Typecheck Attrs.Attributes where
    from_val (VAttributes a) = Val $ Just a
    from_val _ = Val Nothing
    to_type _ = ValType.TAttributes
instance ToVal Attrs.Attributes where to_val = VAttributes

-- | Use a 'TypedFunction' or 'Function' instead of this.
instance Typecheck BaseTypes.ControlRef where
    from_val (VControlRef a) = Val $ Just a
    from_val (VNum a) = Val $ Just $ BaseTypes.ControlSignal $
        Signal.constant <$> a
    from_val _ = Val Nothing
    to_type _ = ValType.TControlRef
instance ToVal BaseTypes.ControlRef where to_val = VControlRef

instance Typecheck BaseTypes.PControlRef where
    from_val (VPControlRef a) = Val $ Just a
    from_val (VPitch a) = Val $ Just $ BaseTypes.ControlSignal $
        PSignal.constant a
    from_val _ = Val Nothing
    to_type _ = ValType.TPControlRef
instance ToVal BaseTypes.PControlRef where to_val = VPControlRef

instance Typecheck PSignal.Pitch where
    from_val (VPitch a) = Val $ Just a
    from_val (VNum (Score.Typed Score.Nn nn)) =
        Val $ Just $ PSignal.nn_pitch (Pitch.nn nn)
    from_val _ = Val Nothing
    to_type _ = ValType.TPitch
instance ToVal PSignal.Pitch where to_val = VPitch

instance Typecheck Pitch.Pitch where
    from_val (VNotePitch a) = Val $ Just a
    from_val _ = Val Nothing
    to_type _ = ValType.TNotePitch
instance ToVal Pitch.Pitch where to_val = VNotePitch

instance Typecheck Score.Instrument where
    from_val (VStr (Expr.Str a)) = Val $ Just (Score.Instrument a)
    from_val _ = Val Nothing
    to_type _ = ValType.TStr Nothing
instance ToVal Score.Instrument where
    to_val (Score.Instrument a) = VStr (Expr.Str a)

instance Typecheck BaseTypes.ControlFunction where
    from_val (VControlFunction a) = Val $ Just a
    from_val _ = Val Nothing
    to_type _ = ValType.TControlFunction
instance ToVal BaseTypes.ControlFunction where to_val = VControlFunction

-- | Anything except a pitch can be coerced to a quoted, using ShowVal.  This
-- means you can write a lot of things without quotes.
--
-- Pitches have to be quoted because they explicitly have an invalid ShowVal.
instance Typecheck BaseTypes.Quoted where
    from_val val = case val of
        VQuoted a -> Val $ Just a
        VPitch {} -> Val Nothing
        VStr (Expr.Str sym) -> to_quoted sym
        _ -> to_quoted $ ShowVal.show_val val
        where
        to_quoted sym = Val $ Just $
            BaseTypes.Quoted (Expr.Call (Expr.Symbol sym) [] :| [])
    to_type _ = ValType.TQuoted
instance ToVal BaseTypes.Quoted where to_val = VQuoted

data NotGiven = NotGiven deriving (Show, Eq)

instance ShowVal.ShowVal NotGiven where
    show_val NotGiven = "_"

instance Typecheck NotGiven where
    from_val VNotGiven = Val $ Just NotGiven
    from_val _ = Val Nothing
    to_type _ = ValType.TNotGiven

instance ToVal NotGiven where
    to_val NotGiven = VNotGiven

-- * util

-- TODO There are four types that divide into two kinds.  Then I have
-- every possible combination:
-- any type: Score.Real
-- time type without value: Real
-- time type with value: BaseTypes.RealDuration
--
-- This means I wind up with a lot of duplication here to handle time types and
-- transpose types.  Surely there's a better way?  Maybe put the two kinds into
-- a typeclass?

data TimeType = Real | Score deriving (Eq, Show)

instance Pretty TimeType where pretty = showt

time_type :: TimeType -> Score.Type -> Maybe TimeType
time_type deflt typ = case typ of
    Score.Untyped -> Just deflt
    Score.Real -> Just Real
    Score.Score -> Just Score
    _ -> Nothing

data TransposeType = Diatonic | Chromatic | Nn deriving (Eq, Show)

instance Pretty TransposeType where pretty = showt

transpose_type :: TransposeType -> Score.Type -> Maybe TransposeType
transpose_type deflt typ = case typ of
    Score.Untyped -> Just deflt
    Score.Diatonic -> Just Diatonic
    Score.Chromatic -> Just Chromatic
    Score.Nn -> Just Nn
    _ -> Nothing

to_transpose :: TransposeType -> Double -> Pitch.Transpose
to_transpose typ val = case typ of
    Diatonic -> Pitch.Diatonic val
    Chromatic -> Pitch.Chromatic val
    Nn -> Pitch.Nn val

transpose_control :: TransposeType -> Score.Control
transpose_control Diatonic = Controls.diatonic
transpose_control Chromatic = Controls.chromatic
transpose_control Nn = Controls.nn

-- ** to_typed_function

-- | Convert a 'BaseTypes.ControlRef' to a function.
--
-- If a signal exists but doesn't have a type, the type will be inherited from
-- the default.  This way a call can cause a signal parameter to default to
-- a certain type.
to_typed_function :: BaseTypes.ControlRef -> Derive.Deriver TypedFunction
to_typed_function control =
    convert_to_function control =<< to_signal_or_function control

convert_to_function :: BaseTypes.ControlRef
    -> Either Score.TypedControl BaseTypes.ControlFunction
    -> Derive.Deriver TypedFunction
convert_to_function control =
    either (return . signal_function) from_function
    where
    signal_function sig t = Signal.at t <$> sig
    from_function f = BaseTypes.call_control_function f score_control <$>
        Internal.get_control_function_dynamic
    score_control = case control of
        BaseTypes.ControlSignal {} -> Controls.null
        BaseTypes.DefaultedControl cont _ -> cont
        BaseTypes.LiteralControl cont -> cont

to_signal_or_function :: BaseTypes.ControlRef
    -> Derive.Deriver (Either Score.TypedControl BaseTypes.ControlFunction)
to_signal_or_function control = case control of
    BaseTypes.ControlSignal sig -> return $ Left sig
    BaseTypes.DefaultedControl cont deflt ->
        get_control (Score.type_of deflt) (return (Left deflt)) cont
    BaseTypes.LiteralControl cont ->
        get_control Score.Untyped (Derive.throw $ "not found: " <> showt cont)
            cont
    where
    get_control default_type deflt cont = get_function cont >>= \x -> case x of
        Just f -> return $ Right $
            BaseTypes.modify_control_function (inherit_type default_type .) f
        Nothing -> get_control_signal cont >>= \x -> case x of
            Just sig -> return $ Left sig
            Nothing -> deflt
    get_function cont = Internal.get_dynamic $
        Map.lookup cont . Derive.state_control_functions
    -- If the signal was untyped, it gets the type of the default, since
    -- presumably the caller expects that type.
    inherit_type default_type val =
        val { Score.type_of = Score.type_of val <> default_type }
    get_control_signal control = Map.lookup control <$>
        Internal.get_dynamic Derive.state_controls
