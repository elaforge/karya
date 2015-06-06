-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DefaultSignatures #-}
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
import qualified Data.Text as Text

import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil

import qualified Ui.Id as Id
import qualified Ui.ScoreTime as ScoreTime
import qualified Derive.BaseTypes as Score
import qualified Derive.BaseTypes as BaseTypes
import Derive.BaseTypes
       (Environ, make_environ, environ_to_list, delete_val, lookup_val, val_set,
        null_environ, ValName, Val(..), vals_equal, Quoted(..),
        ControlFunction(..), Dynamic(..), empty_dynamic, Symbol(..),
        ControlRef(..), PitchControl, ValControl, show_call_val, CallId, Expr,
        Call(..), PitchCall, Term(..))
import qualified Derive.Environ as Environ
import qualified Derive.PitchSignal as PitchSignal
import Derive.ShowVal (ShowVal(..))

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import Global
import Types


-- | Call used by the infix @=@ syntax.
c_equal :: CallId
c_equal = "="

-- * make Val literals

-- | Make an untyped VNum.
num :: Double -> Val
num = VNum . Score.untyped

str :: Text -> Val
str = VSymbol . Symbol

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

to_scale_id :: Typecheck a => a -> Maybe Pitch.ScaleId
to_scale_id val
    | VSymbol (Symbol s) <- to_val val = Just (Pitch.ScaleId s)
    | otherwise = Nothing

sym_to_scale_id :: Symbol -> Pitch.ScaleId
sym_to_scale_id (Symbol s) = Pitch.ScaleId s

scale_id_to_sym :: Pitch.ScaleId -> Symbol
scale_id_to_sym (Pitch.ScaleId s) = Symbol s

-- | Defaulted control from a RealTime.
real_control :: Score.Control -> RealTime -> ValControl
real_control c deflt = DefaultedControl c $
    Score.untyped $ Signal.constant (RealTime.to_seconds deflt)

constant_control :: Signal.Y -> ValControl
constant_control = ControlSignal . Score.untyped . Signal.constant

quoted :: Symbol -> [Val] -> Quoted
quoted name args = Quoted $ literal_call name args :| []

unsym :: Symbol -> Text
unsym (Symbol sym) = sym

-- * general purpose types

-- | This is for arguments which can be high or low.
data UpDown = Up | Down deriving (Show, Enum, Bounded, Eq, Ord)

instance Pretty.Pretty UpDown where pretty = showt
instance Typecheck UpDown
instance TypecheckSymbol UpDown
instance ShowVal UpDown where
    show_val Up = "u"
    show_val Down = "d"

-- * type wrappers

-- | Some calls can operate in either RealTime or ScoreTime.
data Duration = Real RealTime | Score ScoreTime deriving (Eq, Show)

instance ShowVal Duration where
    show_val (Real x) = show_val x
    show_val (Score x) = show_val x

instance Pretty.Pretty Duration where
    pretty (Real t) = pretty t
    pretty (Score t) = pretty t

multiply_duration :: Duration -> Int -> Duration
multiply_duration (Real t) n = Real (t * fromIntegral n)
multiply_duration (Score t) n = Score (t * fromIntegral n)

-- | Either RealTime or ScoreTime, but untyped defaults to RealTime.
newtype DefaultReal = DefaultReal { default_real :: Duration }
    deriving (Eq, Show, ShowVal)
-- | Same as 'DefaultReal' but untyped defaults to ScoreTime.
newtype DefaultScore = DefaultScore { default_score :: Duration }
    deriving (Eq, Show, ShowVal)

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
    deriving (Show, Eq, ShowVal, Num, Fractional)

-- | Like 'Positive', but >=0.
newtype Natural a = Natural { natural :: a }
    deriving (Show, Eq, ShowVal, Num, Fractional)

-- | Normally Transpose will default to Chromatic if the val is untyped,
-- but some calls would prefer to default to Diatonic.
newtype DefaultDiatonic =
    DefaultDiatonic { default_diatonic :: Pitch.Transpose }
    deriving (Show, ShowVal)

diatonic :: Double -> DefaultDiatonic
diatonic = DefaultDiatonic . Pitch.Diatonic


-- * show val

instance ShowVal a => ShowVal (Maybe a) where
    show_val Nothing = "Nothing"
    show_val (Just a) = show_val a

instance (ShowVal a, ShowVal b) => ShowVal (Either a b) where
    show_val = either show_val show_val

-- * types

data Type =
    TNum NumType NumValue
    | TAttributes | TControl | TPitchControl | TPitch | TNotePitch | TInstrument
    -- | Text string, with enum values if it's an enum.
    | TSymbol (Maybe [Text]) | TControlName | TPitchControlName
    | TNotGiven | TSeparator | TMaybe Type | TEither Type Type | TVal
    -- | A 'VQuoted'.  This has no Typecheck instance so it should never show
    -- up as a call argument.
    | TQuoted
    | TControlFunction
    | TList !Type
    deriving (Eq, Ord, Show)

-- | These are kind of muddled.  This is because they were originally just
-- documentation, so the more specific the better, but are also used for
-- typechecking in 'put_val', so the subtype relations need to be respected.
-- But since some are just documentation (e.g. TDefaultReal), they should never
-- show up on the LHS of a put_val typecheck.
data NumType = TUntyped | TInt
    | TTranspose | TDefaultDiatonic | TDefaultChromatic | TNoteNumber
    | TTime | TDefaultReal | TDefaultScore | TRealTime | TScoreTime
    deriving (Eq, Ord, Show)

-- | Numeric subtypes, from most general to most specific.
data NumValue = TAny
    -- | >=0
    | TNatural
    -- | >0
    | TPositive
    deriving (Eq, Ord, Show)

-- | This typechecking already exists in the Typecheck instances, but all it
-- can do is go from a Val to a @Typecheck a => Maybe a@.  So I can't reuse it
-- to check a type against a type, so it has to be duplicated, similar to how
-- 'type_of' can't reuse 'to_type'.
--
-- The result is I have redundant functions like 'subtypes_of' and 'type_of'
-- and 'to_num_type', and a mistake or inconsistency with 'to_type' or 'to_val'
-- will cause typechecking to fail in some subtle case.  Fortunately there are
-- relatively few types and hopefully won't be many more, and it only affects
-- 'put_val'.  It could all do with a cleanup, but obviously I don't know
-- anything aobut how this sort of thing is supposed to be done.
types_match :: Type -> Type -> Bool
types_match t1 t2 = case (t1, t2) of
    (TNum n1 v1, TNum n2 v2) -> num_types_match n1 n2 && num_vals_match v1 v2
    (TMaybe t1, TMaybe t2) -> types_match t1 t2
    (TEither t1 u1, TEither t2 u2) -> types_match t1 t2 && types_match u1 u2
    (TList t1, TList t2) -> types_match t1 t2
    (t1, t2) -> t1 == t2
    where
    num_types_match n1 n2 = n2 `elem` subtypes_of n1
    num_vals_match v1 v2 = v1 <= v2

-- | Nothing if the type of the rhs matches the lhs, otherwise the expected
-- type.
val_types_match :: Val -> Val -> Maybe Type
val_types_match lhs rhs
    | types_match expected (type_of rhs) = Nothing
    | otherwise = Just expected
    where expected = infer_type_of False lhs

subtypes_of :: NumType -> [NumType]
subtypes_of n
    | n `elem` [TTime, TDefaultReal, TDefaultScore] =
        [TTime, TDefaultReal, TDefaultScore, TRealTime, TScoreTime]
    | n `elem` transpose = transpose
    | otherwise = [n]
    where
    transpose = [TTranspose, TDefaultDiatonic, TDefaultChromatic, TNoteNumber]

instance Pretty.Pretty Type where
    pretty (TMaybe typ) = "Maybe " <> pretty typ
    pretty (TEither a b) = pretty a <> " or " <> pretty b
    pretty (TNum typ val) = append_parens "Num" $
        TextUtil.joinWith ", " (pretty typ) (pretty val)
    pretty (TSymbol enums) =
        append_parens "Symbol" $ maybe "" Text.unwords enums
    -- There is no corresponding Val type for these, so I might as well be
    -- clear about what they mean.
    pretty TControlName = append_parens "ControlName" Id.valid_description
    pretty TPitchControlName =
        append_parens "PitchControlName" ("#" <> Id.valid_description)
    pretty (TList typ) = "list of " <> pretty typ
    pretty typ = Text.drop 1 (showt typ)

append_parens :: Text -> Text -> Text
append_parens name desc
    | Text.null desc = name
    | otherwise = name <> " (" <> desc <> ")"

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
        TNoteNumber -> "nn"

instance Pretty.Pretty NumValue where
    pretty t = case t of
        TAny -> ""
        TNatural -> ">=0"
        TPositive -> ">0"

type_of :: Val -> Type
type_of = infer_type_of True

infer_type_of :: Bool -- ^ If True, infer the most specific type possible.
    -- Otherwise, infer a general type.  This is because if 'put_val' gets a
    -- 1 it doesn't mean it's intended to be a TPositive.
    -> Val -> Type
infer_type_of specific val = case val of
    VNum (Score.Typed typ val) -> TNum (to_num_type typ) $ if specific
        then (if val > 0 then TPositive
            else if val >= 0 then TNatural else TAny)
        else TAny
    VAttributes {} -> TAttributes
    VControl {} -> TControl
    VPitchControl {} -> TPitchControl
    VPitch {} -> TPitch
    VNotePitch {} -> TNotePitch
    VInstrument {} -> TInstrument
    VSymbol {} -> TSymbol Nothing
    VQuoted {} -> TQuoted
    VControlFunction {} -> TControlFunction
    VNotGiven -> TNotGiven
    VSeparator -> TSeparator
    VList {} -> TList TVal

to_num_type :: Score.Type -> NumType
to_num_type typ = case typ of
    Score.Untyped -> TUntyped
    Score.Real -> TRealTime
    Score.Score -> TScoreTime
    Score.Diatonic -> TTranspose
    Score.Chromatic -> TTranspose
    Score.Nn -> TNoteNumber

-- ** special types

-- | This can automatically derive a Typecheck instance for 'VSymbol' types
-- if they are in 'TypecheckSymbol'.
class (Show a, ShowVal a) => Typecheck a where
    from_val :: Val -> Maybe a
    default from_val :: TypecheckSymbol a => Val -> Maybe a
    from_val (VSymbol (Symbol a)) = parse_symbol a
    from_val _ = Nothing

    to_val :: a -> Val
    -- This could be just ShowVal and thus a plain default method, but I want
    -- it to only apply when the type is explicitly in TypecheckSymbol, since
    -- it's not valid in general.
    default to_val :: TypecheckSymbol a => a -> Val
    to_val = VSymbol . Symbol . show_val

    to_type :: Proxy a -> Type
    default to_type :: TypecheckSymbol a => Proxy a -> Type
    to_type proxy = TSymbol (symbol_values proxy)

instance Typecheck Val where
    from_val = Just
    to_val = id
    to_type _ = TVal

-- | Putting Maybe in Typecheck means I can have optional arguments with no
-- defaults.  Further docs in 'Derive.Sig.defaulted'.
instance Typecheck a => Typecheck (Maybe a) where
    from_val VNotGiven = Just Nothing
    from_val a = case from_val a of
        Nothing -> Nothing
        Just v -> Just (Just v)
    to_val Nothing = VNotGiven
    to_val (Just a) = to_val a
    to_type _ = TMaybe $ to_type (Proxy :: Proxy a)

instance (Typecheck a, Typecheck b) => Typecheck (Either a b) where
    from_val a = case from_val a of
        Just left -> Just $ Left left
        Nothing -> case from_val a of
            Just right -> Just $ Right right
            Nothing -> Nothing
    to_val = either to_val to_val
    to_type _ = TEither (to_type (Proxy :: Proxy a))
        (to_type (Proxy :: Proxy b))

-- | Non-lists are coerced into singleton lists.
instance Typecheck a => Typecheck [a] where
    from_val (VList xs) = mapM from_val xs
    from_val v = (:[]) <$> from_val v
    to_val = VList . map to_val
    to_type _ = TList $ to_type (Proxy :: Proxy a)

-- ** numeric types

instance Typecheck Score.TypedVal where
    from_val (VNum a) = Just a
    from_val _ = Nothing
    to_val = VNum
    to_type = num_to_type

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
    from_val (VNum (Score.Typed typ val)) = case typ of
        Score.Untyped -> Just (Pitch.Chromatic val)
        Score.Chromatic -> Just (Pitch.Chromatic val)
        Score.Diatonic -> Just (Pitch.Diatonic val)
        Score.Nn -> Just (Pitch.Nn val)
        _ -> Nothing
    from_val _ = Nothing
    to_val (Pitch.Chromatic a) = VNum $ Score.Typed Score.Chromatic a
    to_val (Pitch.Diatonic a) = VNum $ Score.Typed Score.Diatonic a
    to_val (Pitch.Nn a) = VNum $ Score.Typed Score.Nn a
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

instance Typecheck Pitch.NoteNumber where
    from_val (VNum (Score.Typed typ val))
        | typ == Score.Untyped || typ == Score.Nn = Just $ Pitch.nn val
        | otherwise = Nothing
    from_val _ = Nothing
    to_val = VNum . Score.Typed Score.Nn . Pitch.nn_to_double
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

instance Typecheck Duration where
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

num_to_type :: TypecheckNum a => Proxy a -> Type
num_to_type proxy = TNum (num_type proxy) TAny

class Typecheck a => TypecheckNum a where num_type :: Proxy a -> NumType
instance TypecheckNum Score.TypedVal where num_type _ = TUntyped
instance TypecheckNum Double where num_type _ = TUntyped
instance TypecheckNum Int where num_type _ = TInt
instance TypecheckNum Pitch.Transpose where num_type _ = TTranspose
instance TypecheckNum DefaultDiatonic where num_type _ = TDefaultDiatonic
instance TypecheckNum Pitch.NoteNumber where num_type _ = TNoteNumber
instance TypecheckNum ScoreTime where num_type _ = TScoreTime
instance TypecheckNum RealTime where num_type _ = TRealTime
instance TypecheckNum Duration where num_type _ = TTime
instance TypecheckNum DefaultReal where num_type _ = TDefaultReal
instance TypecheckNum DefaultScore where num_type _ = TDefaultScore

instance TypecheckNum a => Typecheck (Positive a) where
    from_val v@(VNum val)
        | Score.typed_val val > 0 = Positive <$> from_val v
        | otherwise = Nothing
    from_val _ = Nothing
    to_val (Positive val) = to_val val
    to_type _ = TNum (num_type (Proxy :: Proxy a)) TPositive

instance TypecheckNum a => Typecheck (Natural a) where
    from_val v@(VNum val)
        | Score.typed_val val >= 0 = Natural <$> from_val v
        | otherwise = Nothing
    from_val _ = Nothing
    to_val (Natural val) = to_val val
    to_type _ = TNum (num_type (Proxy :: Proxy a)) TNatural

-- *** text\/symbol types

instance Typecheck Symbol where
    from_val (VSymbol a) = Just a
    from_val _ = Nothing
    to_val = VSymbol
    to_type _ = TSymbol Nothing

instance Typecheck Text where
    from_val (VSymbol (Symbol s)) = Just s
    from_val _ = Nothing
    to_val = VSymbol . Symbol
    to_type _ = TSymbol Nothing

-- These should use Score.control and Score.pcontrol, but that would be a
-- circular import.
instance Typecheck Score.Control where
    from_val (VSymbol (Symbol s)) | Id.valid s = Just $ Score.Control s
    from_val _ = Nothing
    to_val c = VSymbol (Symbol (Score.control_name c))
    to_type _ = TControlName

instance Typecheck Score.PControl where
    from_val (VSymbol (Symbol s))
        | Just name <- Text.stripPrefix "#" s, Id.valid name =
            Just $ Score.PControl name
    from_val _ = Nothing
    to_val c = VSymbol (Symbol (Score.pcontrol_name c))
    to_type _ = TPitchControlName

-- *** enum

-- | This is for text strings which are parsed to call-specific types.  You
-- can declare an instance and the default Typecheck instance will allow you
-- to incorporate the type directly into the signature of the call.
--
-- If your type is a Bounded Enum, you get a default parser, and the enum
-- values go in the 'TSymbol' so the docs can mention them.
--
-- So the type needs to be in (Bounded, Enum, ShowVal, TypecheckSymbol,
-- Typecheck), though all of these can use default implementations.
class ShowVal a => TypecheckSymbol a where
    parse_symbol :: Text -> Maybe a
    default parse_symbol :: (Bounded a, Enum a) => Text -> Maybe a
    parse_symbol = make_parse_enum [minBound :: a .. maxBound]

    symbol_values :: Proxy a -> Maybe [Text]
    default symbol_values :: (Bounded a, Enum a) => Proxy a -> Maybe [Text]
    symbol_values _ = Just $ map show_val [minBound :: a .. maxBound]

make_parse_enum :: ShowVal a => [a] -> (Text -> Maybe a)
make_parse_enum vals = flip Map.lookup m
    where m = Map.fromList (zip (map show_val vals) vals)

-- | Make a ShowVal from a Show instance.
default_show_val :: Show a => a -> Text
default_show_val = Text.toLower . showt

instance Typecheck Bool
instance TypecheckSymbol Bool
instance ShowVal Bool where show_val b = if b then "t" else "f"

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

instance Typecheck Pitch.Pitch where
    from_val (VNotePitch a) = Just a
    from_val _ = Nothing
    to_val = VNotePitch
    to_type _ = TNotePitch

instance Typecheck Score.Instrument where
    from_val (VInstrument a) = Just a
    from_val _ = Nothing
    to_val = VInstrument
    to_type _ = TInstrument

instance Typecheck ControlFunction where
    from_val (VControlFunction a) = Just a
    from_val _ = Nothing
    to_val = VControlFunction
    to_type _ = TControlFunction

instance Typecheck Quoted where
    from_val (VQuoted a) = Just a
    from_val (VSymbol sym) = Just (Quoted (call0 sym :| []))
    from_val _ = Nothing
    to_val = VQuoted
    to_type _ = TQuoted

-- * environ

-- | Insert a new val, but return Left if it changes the type of an existing
-- one, so once you put a key of a given type into the environ, it can only
-- ever be overwritten by a Val of the same type.  The idea is that being
-- inconsistent with types will just lead to confusion.
--
-- 'VNotGiven' is another special case, it deletes the given key.
put_val :: Typecheck a => ValName -> a -> Environ -> Either Type Environ
put_val name val environ
    | VNotGiven <- new_val = Right $ delete_val name environ
    | otherwise = case lookup_val name environ of
        Nothing -> case Map.lookup name hardcoded_types of
            Just expected | not $ types_match expected (type_of new_val) ->
                Left expected
            _ -> Right $ BaseTypes.insert_val name new_val environ
        Just old_val -> case val_types_match old_val new_val of
            Just expected -> Left expected
            Nothing -> Right $ BaseTypes.insert_val name new_val environ
    where new_val = to_val val

-- | Like 'put_val', but format the error msg.
put_val_error :: Typecheck a => ValName -> a -> Environ -> Either Text Environ
put_val_error name val = first fmt . put_val name val
    where
    fmt typ = "can't set " <> pretty name <> " to "
        <> show_val (to_val val) <> ", expected " <> pretty typ

-- | Insert a val without typechecking.
insert_val :: Typecheck a => ValName -> a -> Environ -> Environ
insert_val name = BaseTypes.insert_val name . to_val

-- | If a standard val gets set to the wrong type, it will cause confusing
-- errors later on.
hardcoded_types :: Map.Map ValName Type
hardcoded_types = Map.fromList
    [ (Environ.attributes, TAttributes)
    , (Environ.block_end, TNum TScoreTime TAny)
    , (Environ.control, TSymbol Nothing)
    , (Environ.instrument, TInstrument)
    , (Environ.key, TSymbol Nothing)
    , (Environ.merge, TSymbol Nothing)
    , (Environ.scale, TSymbol Nothing)
    , (Environ.seed, TNum TUntyped TAny)
    , (Environ.srate, TNum TUntyped TAny)
    , (Environ.suppress_until, TNum TRealTime TAny)
    , (Environ.tuning, TSymbol Nothing)
    , (Environ.voice, TNum TUntyped TAny)
    ]

data LookupError = NotFound | WrongType !Type deriving (Show)

get_val :: Typecheck a => ValName -> Environ -> Either LookupError a
get_val name environ = case lookup_val name environ of
    Nothing -> Left NotFound
    Just val -> case from_val val of
        Nothing -> Left (WrongType (type_of val))
        Just v -> Right v

-- | Like 'get_val', except that type errors and not found both turn into
-- Nothing.
maybe_val :: Typecheck a => ValName -> Environ -> Maybe a
maybe_val name = from_val <=< lookup_val name

-- | Like 'get_val' but format a WrongType nicely.
checked_val :: forall a. Typecheck a => ValName -> Environ
    -> Either Text (Maybe a)
checked_val name environ = case get_val name environ of
        Left NotFound -> return Nothing
        Left (WrongType typ) ->
            Left $ showt name <> ": expected " <> pretty return_type
                <> " but val type is " <> pretty typ
        Right v -> return (Just v)
    where return_type = to_type (Proxy :: Proxy a)

-- | Like 'checked_val', but juggle the return type around so NotFound is just
-- Nothing, which is more convenient in some cases.
checked_val2 :: Typecheck a => ValName -> Environ -> Maybe (Either Text a)
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
map_args :: (Val -> Val) -> Expr -> Expr
map_args f = fmap call
    where
    call (Call sym terms) = Call sym (map term terms)
    term (ValCall c) = ValCall (call c)
    term (Literal lit) = Literal (f lit)

map_call_id :: (CallId -> CallId) -> Call -> Call
map_call_id f (Call call args) = Call (f call) args

-- | Transform only the CallId in the generator position.
map_generator :: (CallId -> CallId) -> Expr -> Expr
map_generator f (call1 :| calls) = case calls of
    [] -> map_call_id f call1 :| []
    _ : _ -> call1 :| Seq.map_last (map_call_id f) calls

-- | Convenient constructor for Call.
call :: Symbol -> [Term] -> Call
call sym = Call sym

call0 :: Symbol -> Call
call0 sym = Call sym []

literal_call :: Symbol -> [Val] -> Call
literal_call sym args = call sym (map Literal args)

inst :: Text -> Term
inst = Literal . VInstrument . Score.Instrument

val_call :: Symbol -> [Val] -> Term
val_call sym args = ValCall (literal_call sym args)


-- * ControlFunction

call_control_function :: ControlFunction -> Score.Control -> Dynamic
    -> RealTime -> Score.TypedVal
call_control_function (ControlFunction _ f) = f

-- | Modify the underlying function, presumably to compose something onto the
-- input or output.
modify_control_function ::
    ((RealTime -> Score.TypedVal) -> (RealTime -> Score.TypedVal))
    -> ControlFunction -> ControlFunction
modify_control_function modify (ControlFunction name f) =
    ControlFunction name (\dyn control -> modify (f dyn control))
