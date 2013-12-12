-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- | This module exports the basic types for \"tracklang\", which is the
    language parsed by "Derive.ParseBs" and interpreted by "Derive.Call".

    It also defines 'Typecheck', which is used along with "Derive.Sig" to
    infer type signatures for calls.

    To avoid circular imports, many of the types are actually defined in
    "Derive.BasicTypes", but they should be imported as if they were defined
    here.
-}
module Derive.TrackLang (
    module Derive.TrackLang, module Derive.BaseTypes, show_val
) where
import qualified Data.Map as Map

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Ui.ScoreTime as ScoreTime
import qualified Derive.BaseTypes as Score
import Derive.BaseTypes
       (Environ, make_environ, environ_to_list, insert_val, delete_val,
        lookup_val, null_environ, ValName, RawVal, Val, ValType(..), Symbol(..),
        ControlRef(..), PitchControl, RawPitchControl, ValControl,
        show_call_val, CallId, Expr, Call(..), PitchCall, Term(..))
import qualified Derive.Environ as Environ
import qualified Derive.PitchSignal as PitchSignal
import Derive.ShowVal (ShowVal(..))

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import Types


-- | An empty instrument literal is a no-op, see 'VInstrument'.
is_null_instrument :: Score.Instrument -> Bool
is_null_instrument (Score.Instrument "") = True
is_null_instrument _ = False

-- | Call used by the infix @=@ syntax.
c_equal :: CallId
c_equal = "="

-- * make literals

-- | Make an untyped VNum.
num :: Double -> Val
num = to_val

str :: Text -> Val
str = to_val

score_time :: ScoreTime -> Val
score_time = VNum . Score.Typed Score.Score . ScoreTime.to_double

real_time :: RealTime -> Val
real_time = VNum . Score.Typed Score.Real . RealTime.to_seconds

transposition :: Pitch.Transpose -> Val
transposition t = VNum $ case t of
    Pitch.Diatonic d -> Score.Typed Score.Diatonic d
    Pitch.Chromatic d -> Score.Typed Score.Chromatic d
    Pitch.Nn d -> Score.Typed Score.Nn d

type_to_transpose :: Score.TypedVal -> Maybe Pitch.Transpose
type_to_transpose (Score.Typed typ val) = case typ of
    Score.Diatonic -> Just $ Pitch.Diatonic val
    Score.Chromatic -> Just $ Pitch.Chromatic val
    Score.Nn -> Just $ Pitch.Nn val
    _ -> Nothing

to_scale_id :: (Typecheck a) => a -> Maybe Pitch.ScaleId
to_scale_id val
    | VSymbol (Symbol s) <- to_val val = Just (Pitch.ScaleId s)
    | otherwise = Nothing

sym_to_scale_id :: Symbol -> Pitch.ScaleId
sym_to_scale_id (Symbol s) = Pitch.ScaleId s

scale_id_to_sym :: Pitch.ScaleId -> Symbol
scale_id_to_sym (Pitch.ScaleId s) = Symbol s

-- | Constant control from a RealTime.
real_control :: Score.Control -> RealTime -> ValControl
real_control c deflt =
    DefaultedControl c $
        Score.untyped (Signal.constant (RealTime.to_seconds deflt))

constant_control :: Signal.Y -> ValControl
constant_control = ControlSignal . Score.untyped . Signal.constant

unsym :: Symbol -> Text
unsym (Symbol sym) = sym

-- * time

-- | Some calls can operate in either RealTime or ScoreTime.
data RealOrScore = Real RealTime | Score ScoreTime deriving (Eq, Show)

instance ShowVal RealOrScore where
    show_val (Real x) = show_val x
    show_val (Score x) = show_val x

-- | Normally Transpose will default to Chromatic if the val is untyped,
-- but some calls would prefer to default to Diatonic.
newtype DefaultDiatonic = DefaultDiatonic Pitch.Transpose
    deriving (Show, ShowVal)

defaulted_diatonic :: DefaultDiatonic -> Pitch.Transpose
defaulted_diatonic (DefaultDiatonic x) = x

default_diatonic :: Double -> DefaultDiatonic
default_diatonic = DefaultDiatonic . Pitch.Diatonic

-- | Either RealTime or ScoreTime, but untyped defaults to RealTime.
newtype DefaultReal = DefaultReal RealOrScore deriving (Eq, Show, ShowVal)
-- | Same as 'DefaultReal' but untyped defaults to ScoreTime.
newtype DefaultScore = DefaultScore RealOrScore deriving (Eq, Show, ShowVal)

-- | Create DefaultReal and DefaultScores for use in "Derive.Sig" signatures
-- for default values.  It would be nice to use literals and let type
-- inference do its thing, but there's no good definition for the rest of
-- the methods in Integral and Fractional.
real :: RealTime -> DefaultReal
real = DefaultReal . Real

score :: ScoreTime -> DefaultScore
score = DefaultScore . Score

defaulted_real :: DefaultReal -> RealOrScore
defaulted_real (DefaultReal t) = t

defaulted_score :: DefaultScore -> RealOrScore
defaulted_score (DefaultScore t) = t

-- | An annotation that says this value must be >0.  Instances only exist
-- for numeric types.
newtype Positive a = Positive a deriving (Show, Eq, ShowVal)
-- | Like 'Positive', but >=0.
newtype Natural a = Natural a deriving (Show, Eq, ShowVal)

-- * show val

instance (ShowVal a) => ShowVal (Maybe a) where
    show_val Nothing = "Nothing"
    show_val (Just a) = show_val a

instance (ShowVal a, ShowVal b) => ShowVal (Either a b) where
    show_val = either show_val show_val

-- * types

data Type = TNum NumType NumValue
    | TAttributes | TControl | TPitchControl | TPitch | TInstrument | TSymbol
    | TNotGiven | TMaybe Type | TEither Type Type | TVal
    deriving (Eq, Ord, Show)

data NumType = TUntyped | TTranspose | TDefaultDiatonic | TDefaultChromatic
    | TTime | TDefaultReal | TDefaultScore | TRealTime | TScoreTime
    | TInt
    deriving (Eq, Ord, Show)

data NumValue = TAny | TNatural | TPositive deriving (Eq, Ord, Show)

to_num_type :: Score.Type -> NumType
to_num_type typ = case typ of
    Score.Untyped -> TUntyped
    Score.Real -> TTime
    Score.Score -> TTime
    Score.Diatonic -> TTranspose
    Score.Chromatic -> TTranspose
    Score.Nn -> TTranspose

instance Pretty.Pretty Type where
    pretty (TMaybe typ) = "Maybe " ++ Pretty.pretty typ
    pretty (TEither a b) = Pretty.pretty a ++ " or " ++ Pretty.pretty b
    pretty (TNum typ val) =
        "Num" <> if null desc then "" else " (" <> desc <> ")"
        where desc = Seq.join2 ", " (Pretty.pretty typ) (Pretty.pretty val)
    pretty typ = drop 1 (show typ)

instance Pretty.Pretty NumType where
    pretty t = case t of
        TUntyped -> ""
        TInt -> "integral"
        TTranspose -> "transposition"
        TDefaultDiatonic -> "transposition, default diatonic"
        TDefaultChromatic -> "transposition, default chromatic"
        TTime -> "time"
        TDefaultReal -> "time, default real"
        TDefaultScore -> "time, default score"
        TRealTime -> "realtime"
        TScoreTime -> "scoretime"

instance Pretty.Pretty NumValue where
    pretty t = case t of
        TAny -> ""
        TNatural -> ">=0"
        TPositive -> ">0"

type_of :: Val -> Type
type_of val = case val of
    -- Yes, it's duplicated with 'to_type', and I can merge them by adding
    -- a VMaybe and doing @type_of . to_val@, but 'to_val' and 'type_of' both
    -- promising to not evaluate the value seems even more hacky than just
    -- 'to_type' making that promise.
    VNum num -> TNum (to_num_type (Score.type_of num)) TAny
    VAttributes {} -> TAttributes
    VControl {} -> TControl
    VPitchControl {} -> TPitchControl
    VPitch {} -> TPitch
    VInstrument {} -> TInstrument
    VSymbol {} -> TSymbol
    VNotGiven -> TNotGiven

-- ** special types

class (Show a, ShowVal a) => Typecheck a where
    from_val :: Val -> Maybe a
    to_val :: a -> Val
    -- | This shouldn't evaluate its argument, so you can use
    -- @maybe undefined id@ to get the type of a @Maybe a@.
    -- This is an unsatisfying dangerous hack.
    to_type :: a -> Type

instance Typecheck Val where
    from_val = Just
    to_val = id
    to_type _ = TVal

-- | Putting Maybe in Typecheck means I can have optional arguments with no
-- defaults.  Further docs in 'Derive.Sig.defaulted'.
instance (Typecheck a) => Typecheck (Maybe a) where
    from_val VNotGiven = Just Nothing
    from_val a = case from_val a of
        Nothing -> Nothing
        Just v -> Just (Just v)
    to_val Nothing = VNotGiven
    to_val (Just a) = to_val a
    to_type val = TMaybe $ to_type $
        fromMaybe (error "to_type shouldn't evaluate its argument") val

instance (Typecheck a, Typecheck b) => Typecheck (Either a b) where
    from_val a = case from_val a of
        Just left -> Just $ Left left
        Nothing -> case from_val a of
            Just right -> Just $ Right right
            Nothing -> Nothing
    to_val = either to_val to_val
    to_type _ = TEither (to_type (error "Either to_type" :: a))
        (to_type (error "Either to_type" :: b))

-- ** numeric types

instance Typecheck Double where
    from_val (VNum (Score.Typed _ a)) = Just a
    from_val _ = Nothing
    to_val = VNum . Score.untyped
    to_type = num_to_type

instance Typecheck Int where
    from_val (VNum (Score.Typed _ a))
        | frac == 0 = Just int
        | otherwise = Nothing
        where (int, frac) = properFraction a
    from_val _ = Nothing
    to_val = VNum . Score.untyped . fromIntegral
    to_type = num_to_type

-- | VNums can also be coerced into chromatic transposition, so you can write
-- a plain number if you don't care about diatonic.
instance Typecheck Pitch.Transpose where
    from_val (VNum (Score.Typed typ val)) = case typ of
        Score.Untyped -> Just (Pitch.Chromatic val)
        Score.Chromatic -> Just (Pitch.Chromatic val)
        Score.Diatonic -> Just (Pitch.Diatonic val)
        Score.Nn -> Just (Pitch.Nn val)
        _ -> Nothing
    from_val _ = Nothing
    to_val (Pitch.Chromatic a) = to_val a
    to_val (Pitch.Diatonic a) = to_val a
    to_val (Pitch.Nn a) = to_val a
    to_type = num_to_type

-- | But some calls want to default to diatonic, not chromatic.
instance Typecheck DefaultDiatonic where
    from_val (VNum (Score.Typed typ val)) = case typ of
        Score.Untyped -> Just (DefaultDiatonic (Pitch.Diatonic val))
        Score.Chromatic -> Just (DefaultDiatonic (Pitch.Chromatic val))
        Score.Diatonic -> Just (DefaultDiatonic (Pitch.Diatonic val))
        _ -> Nothing
    from_val _ = Nothing
    to_val (DefaultDiatonic a) = to_val a
    to_type = num_to_type

instance Typecheck ScoreTime where
    from_val (VNum (Score.Typed typ val)) = case typ of
        Score.Untyped -> Just (ScoreTime.double val)
        Score.Score -> Just (ScoreTime.double val)
        _ -> Nothing
    from_val _ = Nothing
    to_val a = VNum $ Score.Typed Score.Score (ScoreTime.to_double a)
    to_type = num_to_type

instance Typecheck RealTime where
    from_val (VNum (Score.Typed typ val)) = case typ of
        Score.Untyped -> Just (RealTime.seconds val)
        Score.Real -> Just (RealTime.seconds val)
        _ -> Nothing
    from_val _ = Nothing
    to_val a = VNum $ Score.Typed Score.Real (RealTime.to_seconds a)
    to_type = num_to_type

instance Typecheck RealOrScore where
    from_val (VNum (Score.Typed typ val)) = case typ of
        -- Untyped is abiguous, and there doesn't seem to be a natural
        -- default.
        Score.Score -> Just $ Score (ScoreTime.double val)
        Score.Real -> Just $ Real (RealTime.seconds val)
        _ -> Nothing
    from_val _ = Nothing
    to_val (Score a) = to_val a
    to_val (Real a) = to_val a
    to_type = num_to_type

instance Typecheck DefaultReal where
    from_val (VNum (Score.Typed typ val)) = case typ of
        Score.Untyped -> Just $ DefaultReal $ Real (RealTime.seconds val)
        Score.Score -> Just $ DefaultReal $ Score (ScoreTime.double val)
        Score.Real -> Just $ DefaultReal $ Real (RealTime.seconds val)
        _ -> Nothing
    from_val _ = Nothing
    to_val (DefaultReal a) = to_val a
    to_type = num_to_type

instance Typecheck DefaultScore where
    from_val (VNum (Score.Typed typ val)) = case typ of
        Score.Untyped -> Just $ DefaultScore $ Score (ScoreTime.double val)
        Score.Score -> Just $ DefaultScore $ Score (ScoreTime.double val)
        Score.Real -> Just $ DefaultScore $ Real (RealTime.seconds val)
        _ -> Nothing
    from_val _ = Nothing
    to_val (DefaultScore a) = to_val a
    to_type = num_to_type

-- *** refined numeric types

num_to_type :: (TypecheckNum a) => a -> Type
num_to_type undef = TNum (num_type undef) TAny

class (Typecheck a) => TypecheckNum a where num_type :: a -> NumType
instance TypecheckNum Double where num_type _ = TUntyped
instance TypecheckNum Int where num_type _ = TInt
instance TypecheckNum Pitch.Transpose where num_type _ = TTranspose
instance TypecheckNum DefaultDiatonic where num_type _ = TDefaultDiatonic
instance TypecheckNum ScoreTime where num_type _ = TScoreTime
instance TypecheckNum RealTime where num_type _ = TRealTime
instance TypecheckNum RealOrScore where num_type _ = TTime
instance TypecheckNum DefaultReal where num_type _ = TDefaultReal
instance TypecheckNum DefaultScore where num_type _ = TDefaultScore

instance (TypecheckNum a) => Typecheck (Positive a) where
    from_val v@(VNum val)
        | Score.typed_val val > 0 = Positive <$> from_val v
        | otherwise = Nothing
    from_val _ = Nothing
    to_val (Positive val) = to_val val
    to_type _ = TNum (num_type result) TPositive
        where
        result :: a
        result = error "TrackLang.Positive: unevaluated"

instance (TypecheckNum a) => Typecheck (Natural a) where
    from_val v@(VNum val)
        | Score.typed_val val >= 0 = Natural <$> from_val v
        | otherwise = Nothing
    from_val _ = Nothing
    to_val (Natural val) = to_val val
    to_type _ = TNum (num_type result) TNatural
        where
        result :: a
        result = error "TrackLang.Natural: unevaluated"

-- ** other types

instance Typecheck Score.Attributes where
    from_val (VAttributes a) = Just a
    from_val _ = Nothing
    to_val = VAttributes
    to_type _ = TAttributes

instance Typecheck ValControl where
    from_val (VControl a) = Just a
    from_val (VNum a) = Just $ ControlSignal $ Signal.constant <$> a
    from_val _ = Nothing
    to_val = VControl
    to_type _ = TControl

instance Typecheck PitchControl where
    from_val (VPitchControl a) = Just a
    from_val (VPitch a) = Just $ ControlSignal $ PitchSignal.constant a
    from_val _ = Nothing
    to_val = VPitchControl
    to_type _ = TPitchControl

instance Typecheck PitchSignal.Pitch where
    from_val (VPitch a) = Just a
    from_val _ = Nothing
    to_val = VPitch
    to_type _ = TPitch

instance Typecheck Score.Instrument where
    from_val (VInstrument a) = Just a
    from_val _ = Nothing
    to_val = VInstrument
    to_type _ = TInstrument

instance Typecheck Symbol where
    from_val (VSymbol a) = Just a
    from_val _ = Nothing
    to_val = VSymbol
    to_type _ = TSymbol

instance Typecheck Text where
    from_val (VSymbol (Symbol s)) = Just s
    from_val _ = Nothing
    to_val = VSymbol . Symbol
    to_type _ = TSymbol

-- * environ

-- | Insert a new val, but return Left if it changes the type of an existing
-- one, so Once you put a key of a given type into the environ, it can only
-- ever be overwritten by a Val of the same type.  The idea is that being
-- inconsistent with types will just lead to confusion.
--
-- 'VNotGiven' is another special case, it deletes the given key.
put_val :: (Typecheck val) => ValName -> val -> Environ -> Either Type Environ
put_val name val environ
    | VNotGiven <- to_val val = Right $ delete_val name environ
    | otherwise = case lookup_val name environ of
        Nothing -> case Map.lookup name hardcoded_types of
            Just expected | type_of new_val /= expected -> Left expected
            _ -> Right $ insert_val name new_val environ
        Just old_val
            | type_of old_val == type_of new_val ->
                Right $ insert_val name new_val environ
            | otherwise -> Left (type_of old_val)
    where new_val = to_val val

-- | If a standard val gets set to the wrong type, it will cause confusing
-- errors later on.
hardcoded_types :: Map.Map ValName Type
hardcoded_types = Map.fromList
    [ (Environ.attributes, TAttributes)
    , (Environ.instrument, TInstrument)
    , (Environ.control, TSymbol)
    , (Environ.key, TSymbol)
    , (Environ.scale, TSymbol)
    , (Environ.seed, TNum TUntyped TAny)
    , (Environ.srate, TNum TUntyped TAny)
    , (Environ.tuning, TSymbol)
    , (Environ.voice, TNum TUntyped TAny)
    ]

data LookupError = NotFound | WrongType Type deriving (Show)

get_val :: (Typecheck a) => ValName -> Environ -> Either LookupError a
get_val name environ = case lookup_val name environ of
    Nothing -> Left NotFound
    Just val -> case from_val val of
        Nothing -> Left (WrongType (type_of val))
        Just v -> Right v

-- | Like 'get_val', except that type errors and not found both turn into
-- Nothing.
maybe_val :: (Typecheck a) => ValName -> Environ -> Maybe a
maybe_val name = either (const Nothing) Just . get_val name

-- | Like 'get_val' but format a WrongType nicely.
checked_val :: forall a. (Typecheck a) => ValName -> Environ
    -> Either String (Maybe a)
checked_val name environ = case get_val name environ of
        Left NotFound -> return Nothing
        Left (WrongType typ) ->
            Left $ show name ++ ": expected " ++ Pretty.pretty return_type
                ++ " but val type is " ++ Pretty.pretty typ
        Right v -> return (Just v)
    where return_type = to_type (error "checked_val" :: a)

-- | Like 'checked_val', but juggle the return type around so NotFound is just
-- Nothing, which is more convenient in some cases.
checked_val2 :: (Typecheck a) => ValName -> Environ -> Maybe (Either String a)
checked_val2 name environ = case checked_val name environ of
    Right Nothing -> Nothing
    Right (Just val) -> Just (Right val)
    Left err -> Just (Left err)


-- * expressions

-- | Transform the Symbols in an expression.  This affects both symbols in call
-- position, and argument symbols.
map_symbol :: (Symbol -> Symbol) -> Expr -> Expr
map_symbol f = fmap call
    where
    call (Call sym terms) = Call (f sym) (map term terms)
    term (ValCall c) = ValCall (call c)
    term (Literal (VSymbol sym)) = Literal (VSymbol (f sym))
    term (Literal lit) = Literal lit

-- | Transform the arguments in an expression.  This affects only vals in
-- argument position.
map_args :: (RawVal -> RawVal) -> Expr -> Expr
map_args f = fmap call
    where
    call (Call sym terms) = Call sym (map term terms)
    term (ValCall c) = ValCall (call c)
    term (Literal lit) = Literal (f lit)

-- | Convenient constructor for Call.  Not to be confused with 'call0'--calln.
call :: Text -> [Term] -> Call
call sym = Call (Symbol sym)

inst :: Text -> Term
inst = Literal . VInstrument . Score.Instrument

val_call :: Text -> [RawVal] -> Term
val_call sym args = ValCall (Call (Symbol sym) (map Literal args))
