{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables #-}
{- | The event text in a note track forms a simple language.

    Notes with text are interpreted as function calls.  The function will be
    sought in a function namespace which has global defaults merged with static
    config.  Also, each block defines a function with its name, and may be
    called without the namespace from a block in the same namespace.

    Type checking: functions must be defined with a type signature.  Since this
    is a first-order language, signatures are simply lists of types.

    Type inference: look at the positions of variables in the block and figure
    out what the type of the block is.

    Function arguments:

    - The only automatic coercion is from a number to a constant signal of that
    value.

    - An argument of @_@ is treated as not given, so you can pass positional
    arguments after it.

    Control track: @+, cont@ is the same as @cont | add %cont2@

    call syntax:
    echo delay=%echo-delay,1 feedback=.4 times=1

    echo _ %echo-delay,1

    %note-pitch,*5c
-}
module Derive.TrackLang (
    module Derive.TrackLang, module Derive.BaseTypes, show_val
) where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Ui.ScoreTime as ScoreTime
import qualified Derive.BaseTypes as Score
import qualified Derive.BaseTypes as PitchSignal
import Derive.BaseTypes
       (Environ, make_environ, environ_to_list, insert_val, lookup_val,
        null_environ, ValName, Val(..), Symbol(..), RelativeAttrs(..),
        ControlRef(..), PitchControl, ValControl, Note(..),
        show_call_val)
import Derive.ShowVal (ShowVal(..))

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import Types


-- | Symbols used in function call position.
type CallId = Symbol

apply_attr :: RelativeAttrs -> Score.Attributes -> Score.Attributes
apply_attr rel attrs = case rel of
    Add as -> attrs <> as
    Remove as -> Score.attrs_diff attrs as
    Set as -> as

apply_attrs :: [RelativeAttrs] -> Score.Attributes -> Score.Attributes
apply_attrs = List.foldl' (.) id . map apply_attr

-- | An empty instrument literal is a no-op, see 'VInstrument'.
is_null_instrument :: Score.Instrument -> Bool
is_null_instrument (Score.Instrument "") = True
is_null_instrument _ = False

-- | Call used by the infix @=@ syntax.
c_equal :: CallId
c_equal = Symbol "="

-- | Make an untyped VNum.
num :: Double -> Val
num = VNum . Score.untyped

score_time :: ScoreTime -> Val
score_time = VNum . Score.Typed Score.Score . ScoreTime.to_double

real_time :: RealTime -> Val
real_time = VNum . Score.Typed Score.Real . RealTime.to_seconds

transposition :: Pitch.Transpose -> Val
transposition (Pitch.Diatonic d) = VNum $ Score.Typed Score.Diatonic d
transposition (Pitch.Chromatic d) = VNum $ Score.Typed Score.Chromatic d

-- * time

-- | Some calls can operate in either RealTime or ScoreTime.
data RealOrScore = Real RealTime | Score ScoreTime deriving (Eq, Show)

-- | Normally Transpose will default to Chromatic if the val is untyped,
-- but some calls would prefer to default to Diatonic.
newtype DefaultDiatonic = DefaultDiatonic Pitch.Transpose deriving (Show)
-- | Either RealTime or ScoreTime, but untyped defaults to RealTime.
newtype DefaultReal = DefaultReal RealOrScore deriving (Eq, Show)
newtype DefaultScore = DefaultScore RealOrScore deriving (Eq, Show)

-- | Create DefaultReal and DefaultScores for use in "Derive.Sig" signatures
-- for default values.  It would be nice to use literals and let type
-- inference do its thing, but there's no good definition for the rest of
-- the methods in Integral and Fractional.
real :: RealTime -> DefaultReal
real = DefaultReal . Real

score :: ScoreTime -> DefaultScore
score = DefaultScore . Score

-- * show val

instance (ShowVal a) => ShowVal (Maybe a) where
    show_val Nothing = "<no default>"
    show_val (Just a) = show_val a

instance (ShowVal a, ShowVal b) => ShowVal (Either a b) where
    show_val = either show_val show_val

instance ShowVal DefaultReal where show_val (DefaultReal x) = show_val x
instance ShowVal DefaultScore where show_val (DefaultScore x) = show_val x
instance ShowVal DefaultDiatonic where show_val (DefaultDiatonic x) = show_val x
instance ShowVal RealOrScore where
    show_val (Real x) = show_val x
    show_val (Score x) = show_val x

-- * types

data Type = TNum NumType
    | TRelativeAttrs | TAttributes
    | TControl | TPitchControl | TScaleId | TPitch | TInstrument | TSymbol
    | TNotGiven | TMaybe Type | TEither Type Type | TVal
    deriving (Eq, Ord, Show)

data NumType = TUntyped | TTranspose | TDefaultDiatonic | TDefaultChromatic
    | TTime | TDefaultReal | TDefaultScore | TRealTime | TScoreTime
    | TInt
    deriving (Eq, Ord, Show)

to_num_type :: Score.Type -> NumType
to_num_type typ = case typ of
    Score.Untyped -> TUntyped
    Score.Real -> TTime
    Score.Score -> TTime
    Score.Diatonic -> TTranspose
    Score.Chromatic -> TTranspose

instance Pretty.Pretty Type where
    pretty (TMaybe typ) = "Maybe " ++ Pretty.pretty typ
    pretty (TEither a b) = Pretty.pretty a ++ " or " ++ Pretty.pretty b
    pretty (TNum typ) = join "Num" $ case typ of
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
        where join x y = if null y then x else x ++ " (" ++ y ++ ")"
    pretty typ = drop 1 (show typ)

type_of :: Val -> Type
type_of val = case val of
    -- Yes, it's duplicated with 'to_type', and I can merge them by adding
    -- a VMaybe and doing @type_of . to_val@, but 'to_val' and 'type_of' both
    -- promising to not evaluate the value seems even more hacky than just
    -- 'to_type' making that promise.
    VNum num -> TNum (to_num_type (Score.type_of num))
    VRelativeAttrs {} -> TRelativeAttrs
    VAttributes {} -> TAttributes
    VControl {} -> TControl
    VPitchControl {} -> TPitchControl
    VScaleId {} -> TScaleId
    VPitch {} -> TPitch
    VInstrument {} -> TInstrument
    VSymbol {} -> TSymbol
    VNotGiven -> TNotGiven

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
    to_type val = TEither (to_type left) (to_type right)
        where
        Right right = val
        Left left = val

instance Typecheck Double where
    from_val (VNum (Score.Typed _ a)) = Just a
    from_val _ = Nothing
    to_val = VNum . Score.untyped
    to_type _ = TNum TUntyped

instance Typecheck Int where
    from_val (VNum (Score.Typed _ a))
        | frac == 0 = Just int
        | otherwise = Nothing
        where (int, frac) = properFraction a
    from_val _ = Nothing
    to_val = VNum . Score.untyped . fromIntegral
    to_type _ = TNum TInt

-- | VNums can also be coerced into chromatic transposition, so you can write
-- a plain number if you don't care about diatonic.
instance Typecheck Pitch.Transpose where
    from_val (VNum (Score.Typed typ val)) = case typ of
        Score.Untyped -> Just (Pitch.Chromatic val)
        Score.Chromatic -> Just (Pitch.Chromatic val)
        Score.Diatonic -> Just (Pitch.Diatonic val)
        _ -> Nothing
    from_val _ = Nothing
    to_val (Pitch.Chromatic a) = to_val a
    to_val (Pitch.Diatonic a) = to_val a
    to_type _ = TNum TTranspose

-- | But some calls want to default to diatonic, not chromatic.
instance Typecheck DefaultDiatonic where
    from_val (VNum (Score.Typed typ val)) = case typ of
        Score.Untyped -> Just (DefaultDiatonic (Pitch.Diatonic val))
        Score.Chromatic -> Just (DefaultDiatonic (Pitch.Chromatic val))
        Score.Diatonic -> Just (DefaultDiatonic (Pitch.Diatonic val))
        _ -> Nothing
    from_val _ = Nothing
    to_val (DefaultDiatonic a) = to_val a
    to_type _ = TNum TDefaultDiatonic

instance Typecheck ScoreTime where
    from_val (VNum (Score.Typed typ val)) = case typ of
        Score.Untyped -> Just (ScoreTime.double val)
        Score.Score -> Just (ScoreTime.double val)
        _ -> Nothing
    from_val _ = Nothing
    to_val a = VNum $ Score.Typed Score.Score (ScoreTime.to_double a)
    to_type _ = TNum TScoreTime

instance Typecheck RealTime where
    from_val (VNum (Score.Typed typ val)) = case typ of
        Score.Untyped -> Just (RealTime.seconds val)
        Score.Real -> Just (RealTime.seconds val)
        _ -> Nothing
    from_val _ = Nothing
    to_val a = VNum $ Score.Typed Score.Real (RealTime.to_seconds a)
    to_type _ = TNum TRealTime

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
    to_type _ = TNum TTime

instance Typecheck DefaultReal where
    from_val (VNum (Score.Typed typ val)) = case typ of
        Score.Untyped -> Just $ DefaultReal $ Real (RealTime.seconds val)
        Score.Score -> Just $ DefaultReal $ Score (ScoreTime.double val)
        Score.Real -> Just $ DefaultReal $ Real (RealTime.seconds val)
        _ -> Nothing
    from_val _ = Nothing
    to_val (DefaultReal a) = to_val a
    to_type _ = TNum TDefaultReal

instance Typecheck DefaultScore where
    from_val (VNum (Score.Typed typ val)) = case typ of
        Score.Untyped -> Just $ DefaultScore $ Score (ScoreTime.double val)
        Score.Score -> Just $ DefaultScore $ Score (ScoreTime.double val)
        Score.Real -> Just $ DefaultScore $ Real (RealTime.seconds val)
        _ -> Nothing
    from_val _ = Nothing
    to_val (DefaultScore a) = to_val a
    to_type _ = TNum TDefaultScore

instance Typecheck RelativeAttrs where
    from_val (VRelativeAttrs a) = Just a
    from_val _ = Nothing
    to_val = VRelativeAttrs
    to_type _ = TRelativeAttrs

instance Typecheck Score.Attributes where
    from_val (VAttributes a) = Just a
    from_val _ = Nothing
    to_val = VAttributes
    to_type _ = TAttributes

instance Typecheck ValControl where
    from_val (VControl a) = Just a
    from_val (VNum a) = Just $ ConstantControl a
    from_val _ = Nothing
    to_val = VControl
    to_type _ = TControl

instance Typecheck PitchControl where
    from_val (VPitchControl a) = Just a
    from_val _ = Nothing
    to_val = VPitchControl
    to_type _ = TPitchControl

instance Typecheck Pitch.ScaleId where
    from_val (VScaleId a) = Just a
    from_val _ = Nothing
    to_val = VScaleId
    to_type _ = TScaleId

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

instance Typecheck String where
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
-- 'RelativeAttrs's are never inserted, they combine with existing Attributes or
-- create new ones.
put_val :: (Typecheck val) => ValName -> val -> Environ -> Either Type Environ
put_val name val environ = case maybe_old of
    Nothing -> case Map.lookup name hardcoded_types of
        Just expected | type_of new_val /= expected -> Left expected
        _ -> Right $ insert_val name new_val environ
    Just old_val
        | type_of old_val == type_of new_val ->
            Right $ insert_val name (environ_val name val environ) environ
        | otherwise -> Left (type_of old_val)
    where
    maybe_old = lookup_val name environ
    new_val = case to_val val of
        VRelativeAttrs rel_attr -> VAttributes $
            case maybe_old of
                Just (VAttributes attrs) -> apply_attr rel_attr attrs
                _ -> apply_attr rel_attr Score.no_attrs
        _ -> to_val val
    environ_val name val environ = case to_val val of
        VRelativeAttrs rel_attr -> VAttributes $
            case lookup_val name environ of
                Just (VAttributes attrs) -> apply_attr rel_attr attrs
                _ -> apply_attr rel_attr Score.no_attrs
        new_val -> new_val

-- | If a standard val gets set to the wrong type, it will cause confusing
-- errors later on.
hardcoded_types :: Map.Map ValName Type
hardcoded_types = Map.fromList
    [ (v_attributes, TAttributes)
    , (v_instrument, TInstrument)
    , (v_key, TSymbol)
    , (v_scale, TScaleId)
    , (v_seed, TNum TUntyped)
    , (v_srate, TNum TUntyped)
    , (v_tuning, TSymbol)
    , (v_voice, TNum TUntyped)
    ]

data LookupError = NotFound | WrongType Type deriving (Show)

get_val :: (Typecheck a) => ValName -> Environ -> Either LookupError a
get_val name environ = case lookup_val name environ of
    Nothing -> Left NotFound
    Just val -> case from_val val of
        Nothing -> Left (WrongType (type_of val))
        Just v -> Right v

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


-- Define a few inhabitants of Environ which are used by the built-in set
-- of calls.

-- | Default set of attrs.
v_attributes :: ValName
v_attributes = Score.v_attributes

-- | Default instrument.
v_instrument :: ValName
v_instrument = Symbol "inst"

-- | Diatonic transposition often requires a key.  The interpretation of the
-- value depends on the scale.
v_key :: ValName
v_key = Symbol "key"

-- | Default scale, used by pitch tracks with a @*@ title.
v_scale :: ValName
v_scale = Symbol "scale"

-- | Random seed used by randomization functions.  Can be explicitly
-- initialized to capture a certain \"random\" variation.
--
-- This is rounded to an integer, so only integral values make sense.
v_seed :: ValName
v_seed = Symbol "seed"

-- | Sampling rate used by signal interpolators.
v_srate :: ValName
v_srate = Symbol "srate"

-- | Kind of tuning for the scale in scope.  The meaning is dependent on the
-- scale, e.g. ngumbang ngisep for Balinese scales.
v_tuning :: ValName
v_tuning = Symbol "tuning"

-- | Separate notes into different voices.  This is used by integrate to put
-- them on their own tracks, and by the lilypond backend to split them into
-- their own voices.
v_voice :: ValName
v_voice = Symbol "voice"


-- * expressions

-- | The only operator is @|@, so a list suffices for an AST.
type Expr = NonEmpty Call
data Call = Call CallId [Term] deriving (Show)
data Term = ValCall Call | Literal Val deriving (Show)

instance ShowVal Expr where
    show_val expr = Seq.join " | " (map show_val (NonEmpty.toList expr))
instance ShowVal Call where
    show_val (Call (Symbol sym) terms) =
        sym ++ if null terms then "" else ' ' : unwords (map show_val terms)
instance ShowVal Term where
    show_val (ValCall call) = "(" ++ show_val call ++ ")"
    show_val (Literal val) = show_val val

instance Pretty.Pretty Call where
    pretty = show_val

instance DeepSeq.NFData Call where
    rnf (Call call_id terms) = call_id `seq` DeepSeq.rnf terms
instance DeepSeq.NFData Term where
    rnf (ValCall call) = DeepSeq.rnf call
    rnf (Literal val) = DeepSeq.rnf val

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

-- | Convenient constructor for Call.  Not to be confused with 'call0'--calln.
call :: String -> [Term] -> Call
call sym = Call (Symbol sym)

inst :: String -> Term
inst = Literal . VInstrument . Score.Instrument

val_call :: String -> [Val] -> Term
val_call sym args = ValCall (Call (Symbol sym) (map Literal args))

note :: String -> [Val] -> Note
note sym args = Note (Pitch.Note sym) args

note_call :: Note -> Term
note_call (Note note args) = val_call (Pitch.note_text note) args
