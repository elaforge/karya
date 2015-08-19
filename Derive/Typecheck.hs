-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DefaultSignatures, DeriveFunctor #-}
module Derive.Typecheck where
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Pretty as Pretty
import qualified Ui.ScoreTime as ScoreTime
import qualified Derive.BaseTypes as BaseTypes
import Derive.BaseTypes (Val(..))
import qualified Derive.Call as Call
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.TrackLang as TrackLang
import qualified Derive.ValType as ValType

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import Global
import Types


-- * type wrappers

-- | Some calls can operate in either RealTime or ScoreTime.
data Duration = Real RealTime | Score ScoreTime deriving (Eq, Show)

instance ShowVal.ShowVal Duration where
    show_val (Real x) = ShowVal.show_val x
    show_val (Score x) = ShowVal.show_val x

instance Pretty.Pretty Duration where
    pretty (Real t) = pretty t
    pretty (Score t) = pretty t

multiply_duration :: Duration -> Int -> Duration
multiply_duration (Real t) n = Real (t * fromIntegral n)
multiply_duration (Score t) n = Score (t * fromIntegral n)

-- | Either RealTime or ScoreTime, but untyped defaults to RealTime.
newtype DefaultReal = DefaultReal { default_real :: Duration }
    deriving (Eq, Show, ShowVal.ShowVal)
-- | Same as 'DefaultReal' but untyped defaults to ScoreTime.
newtype DefaultScore = DefaultScore { default_score :: Duration }
    deriving (Eq, Show, ShowVal.ShowVal)

-- | Create DefaultReal and DefaultScores for use in "Derive.Sig" signatures
-- for default values.  It would be nice to use literals and let type
-- inference do its thing, but there's no good definition for the rest of
-- the methods in Integral and Fractional.
real :: RealTime -> DefaultReal
real = DefaultReal . Real

score :: ScoreTime -> DefaultScore
score = DefaultScore . Score

-- | An annotation that says this value must be >0.  Instances only exist
-- for numeric types.
--
-- This is an instance of Num just so numeric literals work.  Of course that
-- means you also have (-) which can make it not positive, but this is only
-- intended to be a type tag for signatures, unwrapped as soon as it gets
-- passed to the call.
newtype Positive a = Positive { positive :: a }
    deriving (Show, Eq, ShowVal.ShowVal, Num, Fractional)

-- | Like 'Positive', but >=0.
newtype Natural a = Natural { natural :: a }
    deriving (Show, Eq, ShowVal.ShowVal, Num, Fractional)

-- | 0 <= x <= 1
newtype Normalized = Normalized { normalized :: Double }
    deriving (Show, Eq, ShowVal.ShowVal, Pretty.Pretty)

-- | Normally Transpose will default to Chromatic if the val is untyped,
-- but some calls would prefer to default to Diatonic.
newtype DefaultDiatonic =
    DefaultDiatonic { default_diatonic :: Pitch.Transpose }
    deriving (Show, Eq, ShowVal.ShowVal)

diatonic :: Double -> DefaultDiatonic
diatonic = DefaultDiatonic . Pitch.Diatonic

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
    from_val (VSymbol (BaseTypes.Symbol a)) = Val $ parse_symbol a
    from_val _ = Val Nothing

    to_type :: Proxy a -> ValType.Type
    default to_type :: TypecheckSymbol a => Proxy a -> ValType.Type
    to_type proxy = ValType.TSymbol (symbol_values proxy)

eval_checked :: Typecheck a => ScoreTime -> Val -> Derive.Deriver (Maybe a)
eval_checked pos val = case from_val val of
    Val v -> return v
    Eval deriver -> deriver =<< Derive.score_to_real pos

class ToVal a where
    to_val :: a -> Val
    -- This could be just ShowVal and thus a plain default method, but I want
    -- it to only apply when the type is explicitly in TypecheckSymbol, since
    -- it's not valid in general.
    default to_val :: TypecheckSymbol a => a -> Val
    to_val = VSymbol . BaseTypes.Symbol . ShowVal.show_val

-- | This is for text strings which are parsed to call-specific types.  You
-- can declare an instance and the default Typecheck instance will allow you
-- to incorporate the type directly into the signature of the call.
--
-- If your type is a Bounded Enum, you get a default parser, and the enum
-- values go in the 'TSymbol' so the docs can mention them.
--
-- So the type needs to be in (Bounded, Enum, ShowVal, TypecheckSymbol,
-- Typecheck), though all of these can use default implementations.
class ShowVal.ShowVal a => TypecheckSymbol a where
    parse_symbol :: Text -> Maybe a
    default parse_symbol :: (Bounded a, Enum a) => Text -> Maybe a
    parse_symbol = make_parse_enum [minBound :: a .. maxBound]

    symbol_values :: Proxy a -> Maybe [Text]
    default symbol_values :: (Bounded a, Enum a) => Proxy a -> Maybe [Text]
    symbol_values _ = Just $ map ShowVal.show_val [minBound :: a .. maxBound]

make_parse_enum :: ShowVal.ShowVal a => [a] -> (Text -> Maybe a)
make_parse_enum vals = flip Map.lookup m
    where m = Map.fromList (zip (map ShowVal.show_val vals) vals)

-- | Make a ShowVal from a Show instance.
default_show_val :: Show a => a -> Text
default_show_val = Text.toLower . showt

num_to_type :: TypecheckNum a => Proxy a -> ValType.Type
num_to_type proxy = ValType.TNum (num_type proxy) ValType.TAny

class Typecheck a => TypecheckNum a where num_type :: Proxy a -> ValType.NumType

instance Typecheck Bool
instance TypecheckSymbol Bool

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
instance (Typecheck a, ToVal a) => Typecheck [a] where
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
    from_val a = either Left Right <$> from_val a
    to_type _ = ValType.TEither (to_type (Proxy :: Proxy a))
        (to_type (Proxy :: Proxy b))


-- ** numeric types

-- | Coerce any numeric value to a TypedVal, and check it against the given
-- function.
num_to_scalar :: (Score.TypedVal -> Maybe a) -> Val -> Checked a
num_to_scalar check val = case val of
    VNum a -> Val $ check a
    VControlRef cref -> Eval $ \p ->
        check . ($p) <$> Call.to_typed_function cref
    VControlFunction cf -> Eval $ \p -> check . ($p) <$> control_function cf
    _ -> Val Nothing

-- | Coerce any numeric value to a function, and check it against the given
-- function.
num_to_function :: (Call.TypedFunction -> Maybe a) -> Val -> Checked a
num_to_function check val = case val of
    VNum a -> Val $ check $ const a
    VControlRef cref -> Eval $ const $ check <$> Call.to_typed_function cref
    VControlFunction cf -> Eval $ const $ check <$> control_function cf
    _ -> Val Nothing

-- | Like 'num_to_function', but take a constructor with a type argument,
-- and a separate function to verify the type.
num_to_checked_function :: (Call.Function -> typ -> b)
    -> (Score.Type -> Maybe typ) -> Val -> Checked b
num_to_checked_function make check_type = num_to_function $ \f ->
    make (Score.typed_val . f) <$> check_type (Score.type_of (f 0))

-- | Evaluate a control function with no backing control.
control_function :: TrackLang.ControlFunction
    -> Derive.Deriver Call.TypedFunction
control_function cf = TrackLang.call_control_function cf Controls.null <$>
    Derive.get_control_function_dynamic

-- *** eval only

-- These don't have ToVal instances.

instance Typecheck Call.TypedFunction where
    from_val = num_to_function Just
    -- TODO rather than have to add a ValType for every single instance,
    -- I should have a Text description
    to_type _ = ValType.TControlFunction

instance Typecheck Call.Function where
    from_val = num_to_function (Just . fmap Score.typed_val)
    to_type _ = ValType.TControlFunction -- TODO

data DefaultRealTimeFunction =
    DefaultRealTimeFunction !Call.Function !Call.TimeType

instance Typecheck DefaultRealTimeFunction where
    from_val = num_to_checked_function DefaultRealTimeFunction
        (Call.time_type Call.Real)
    to_type _ = ValType.TControlFunction -- TODO

-- Originally I used DataKinds e.g.
-- TransposeFunction (deflt :: Call.TransposeType), but it seemed less
-- convenient than separate data types.

data TransposeFunctionDiatonic =
    TransposeFunctionDiatonic !Call.Function !Score.Control
instance Typecheck TransposeFunctionDiatonic where
    from_val = num_to_checked_function TransposeFunctionDiatonic
        (transpose_control Call.Diatonic)
    to_type _ = ValType.TControlFunction -- TODO

data TransposeFunctionChromatic =
    TransposeFunctionChromatic !Call.Function !Score.Control
instance Typecheck TransposeFunctionChromatic where
    from_val = num_to_checked_function TransposeFunctionChromatic
        (transpose_control Call.Chromatic)
    to_type _ = ValType.TControlFunction -- TODO

data TransposeFunctionNn = TransposeFunctionNn !Call.Function !Score.Control
instance Typecheck TransposeFunctionNn where
    from_val = num_to_checked_function TransposeFunctionNn
        (transpose_control Call.Nn)
    to_type _ = ValType.TControlFunction -- TODO

transpose_control :: Call.TransposeType -> Score.Type -> Maybe Score.Control
transpose_control deflt =
    fmap Call.transpose_control . Call.transpose_type deflt

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
    from_val = num_to_scalar (check . Score.typed_val)
        where
        check a = if frac == 0 then Just int else Nothing
            where (int, frac) = properFraction a
    to_type = num_to_type
instance ToVal Int where to_val = VNum . Score.untyped . fromIntegral
instance TypecheckNum Int where num_type _ = ValType.TInt

instance Typecheck Normalized where
    from_val = num_to_scalar (check . Score.typed_val)
        where
        check a
            | a <= a && a <= 1 = Just (Normalized a)
            | otherwise = Nothing
    to_type _ = ValType.TNum ValType.TUntyped ValType.TNormalized
instance ToVal Normalized where to_val = VNum . Score.untyped . normalized

-- | VNums can also be coerced into chromatic transposition, so you can write
-- a plain number if you don't care about diatonic.
--
-- This is different from Duration, which does not default an untyped
-- literal, so you have to supply the type explicitly.  The rationale is that
-- many scales don't have diatonic or chromatic, and it would be annoying to
-- have to specify one or the other when it was definitely irrelevant.  But
-- the RealTime ScoreTime distinction is universal, there is no single default
-- that is appropriate for all calls.  So they have to specify a default by
-- taking a 'DefaultScore' or 'DefaultReal', or require the caller to
-- distinguish with 'Duration'.
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

instance Typecheck Duration where
    from_val = num_to_scalar $ \(Score.Typed typ val) -> case typ of
        -- Untyped is abiguous, and there doesn't seem to be a natural
        -- default.
        Score.Score -> Just $ Score (ScoreTime.double val)
        Score.Real -> Just $ Real (RealTime.seconds val)
        _ -> Nothing
    to_type = num_to_type

instance ToVal Duration where
    to_val (Score a) = to_val a
    to_val (Real a) = to_val a
instance TypecheckNum Duration where num_type _ = ValType.TTime

instance Typecheck DefaultReal where
    from_val = num_to_scalar $ \(Score.Typed typ val) ->
        DefaultReal <$> case typ of
            Score.Untyped -> Just $ Real (RealTime.seconds val)
            Score.Score -> Just $ Score (ScoreTime.double val)
            Score.Real -> Just $ Real (RealTime.seconds val)
            _ -> Nothing
    to_type = num_to_type
instance ToVal DefaultReal where to_val (DefaultReal a) = to_val a
instance TypecheckNum DefaultReal where num_type _ = ValType.TDefaultReal

instance Typecheck DefaultScore where
    from_val = num_to_scalar $ \(Score.Typed typ val) ->
        DefaultScore <$> case typ of
            Score.Untyped -> Just $ Score (ScoreTime.double val)
            Score.Score -> Just $ Score (ScoreTime.double val)
            Score.Real -> Just $ Real (RealTime.seconds val)
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

instance TypecheckNum a => Typecheck (Natural a) where
    from_val v@(VNum val)
        | Score.typed_val val >= 0 = Natural <$> from_val v
        | otherwise = Val Nothing
    from_val _ = Val Nothing
    to_type _ = ValType.TNum (num_type (Proxy :: Proxy a)) ValType.TNatural
instance ToVal a => ToVal (Natural a) where
    to_val (Natural val) = to_val val

-- ** text\/symbol

instance Typecheck BaseTypes.Symbol where
    from_val (VSymbol a) = Val $ Just a
    from_val _ = Val Nothing
    to_type _ = ValType.TSymbol Nothing
instance ToVal BaseTypes.Symbol where to_val = VSymbol

instance Typecheck Text where
    from_val (VSymbol (BaseTypes.Symbol s)) = Val $ Just s
    from_val _ = Val Nothing
    to_type _ = ValType.TSymbol Nothing
instance ToVal Text where to_val = VSymbol . BaseTypes.Symbol

instance Typecheck Score.Control where
    from_val (VSymbol (BaseTypes.Symbol s)) =
        Val $ either (const Nothing) Just (Score.control s)
    from_val _ = Val Nothing
    to_type _ = ValType.TControl
instance ToVal Score.Control where
    to_val c = VSymbol (BaseTypes.Symbol (Score.control_name c))

instance Typecheck Score.PControl where
    from_val (VSymbol (BaseTypes.Symbol s))
        | Just name <- Text.stripPrefix "#" s =
            Val $ either (const Nothing) Just (Score.pcontrol name)
    from_val _ = Val Nothing
    to_type _ = ValType.TPControl
instance ToVal Score.PControl where
    to_val c = VSymbol (BaseTypes.Symbol (Score.pcontrol_name c))

-- ** other types

instance Typecheck Score.Attributes where
    from_val (VAttributes a) = Val $ Just a
    from_val _ = Val Nothing
    to_type _ = ValType.TAttributes
instance ToVal Score.Attributes where to_val = VAttributes

-- | Use a 'Call.TypedFunction' or 'Call.Function' instead of this.
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
    from_val (VInstrument a) = Val $ Just a
    from_val _ = Val Nothing
    to_type _ = ValType.TInstrument
instance ToVal Score.Instrument where to_val = VInstrument

instance Typecheck BaseTypes.ControlFunction where
    from_val (VControlFunction a) = Val $ Just a
    from_val _ = Val Nothing
    to_type _ = ValType.TControlFunction
instance ToVal BaseTypes.ControlFunction where to_val = VControlFunction

instance Typecheck BaseTypes.Quoted where
    from_val (VQuoted a) = Val $ Just a
    from_val (VSymbol sym) =
        Val $ Just (BaseTypes.Quoted (BaseTypes.Call sym [] :| []))
    from_val _ = Val Nothing
    to_type _ = ValType.TQuoted
instance ToVal BaseTypes.Quoted where to_val = VQuoted
