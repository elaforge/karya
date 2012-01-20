{-# LANGUAGE TypeSynonymInstances #-} -- for instance Typecheck String
{-# LANGUAGE FlexibleInstances #-} -- for instance Typecheck (Control X)
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
    module Derive.TrackLang, module Derive.BaseTypes
) where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Derive.BaseTypes as Score
import qualified Derive.BaseTypes as PitchSignal
import Derive.BaseTypes (Environ, ValName)
import Derive.BaseTypes
       (Val(..), Symbol(..), AttrMode(..), RelativeAttr(..),
        ControlRef(..), PitchControl, ValControl, show_num)

import qualified Perform.Pitch as Pitch



-- | Symbols used in function call position.
type CallId = Symbol

set_attr :: RelativeAttr -> Score.Attributes -> Score.Attributes
set_attr (RelativeAttr (mode, attr)) attrs = Score.Attributes $ case mode of
    Add -> Set.insert attr (Score.attrs_set attrs)
    Remove -> Set.delete attr (Score.attrs_set attrs)
    Set -> Set.singleton attr
    Clear -> Set.empty

-- | An empty instrument literal is a no-op, see 'VInstrument'.
is_null_instrument :: Score.Instrument -> Bool
is_null_instrument (Score.Instrument "") = True
is_null_instrument _ = False

-- | Call used by the infix @=@ syntax.
c_equal :: CallId
c_equal = Symbol "="

-- * types

data Type = TNum | TString | TRelativeAttr | TAttributes | TControl
    | TPitchControl | TScaleId | TPitch | TInstrument | TSymbol
    | TNotGiven | TMaybe Type | TVal
    deriving (Eq, Show)

instance Pretty.Pretty Type where
    pretty (TMaybe typ) = "Maybe " ++ Pretty.pretty typ
    pretty typ = drop 1 (show typ)

type_of :: Val -> Type
type_of val = case val of
    -- Yes, it's duplicated with 'to_type', and I can merge them by adding
    -- a VMaybe and doing @type_of . to_val@, but 'to_val' and 'type_of' both
    -- promising to not evaluate the value seems even more hacky than just
    -- 'to_type' making that promise.
    VNum {} -> TNum
    VString {} -> TString
    VRelativeAttr {} -> TRelativeAttr
    VAttributes {} -> TAttributes
    VControl {} -> TControl
    VPitchControl {} -> TPitchControl
    VScaleId {} -> TScaleId
    VPitch {} -> TPitch
    VInstrument {} -> TInstrument
    VSymbol {} -> TSymbol
    VNotGiven -> TNotGiven

class Show a => Typecheck a where
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
-- defaults.  Further docs in 'CallSig.optional'.
instance (Typecheck a) => Typecheck (Maybe a) where
    from_val VNotGiven = Just Nothing
    from_val a = case from_val a of
        Nothing -> Nothing
        Just v -> Just (Just v)
    to_val Nothing = VNotGiven
    to_val (Just a) = to_val a
    to_type val = TMaybe (to_type (maybe undefined id val))

instance Typecheck Double where
    from_val (VNum a) = Just a
    from_val _ = Nothing
    to_val = VNum
    to_type _ = TNum

instance Typecheck String where
    from_val (VString s) = Just s
    from_val _ = Nothing
    to_val = VString
    to_type _ = TString

instance Typecheck RelativeAttr where
    from_val (VRelativeAttr a) = Just a
    from_val _ = Nothing
    to_val = VRelativeAttr
    to_type _ = TRelativeAttr

instance Typecheck Score.Attributes where
    from_val (VAttributes a) = Just a
    from_val _ = Nothing
    to_val = VAttributes
    to_type _ = TAttributes

instance Typecheck ValControl where
    from_val (VControl a) = Just a
    from_val (VNum a) = Just (ConstantControl a)
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

-- * dynamic environment

-- | Return either the modified environ or the type expected if the type of the
-- argument was wrong.  Once you put a key of a given type into the environ, it
-- can only ever be overwritten by a Val of the same type.
--
-- 'RelativeAttr's are never inserted, they combine with existing Attributes or
-- create new ones.
put_val :: (Typecheck val) => ValName -> val -> Environ -> Either Type Environ
put_val name val environ = case maybe_old of
    Nothing -> case Map.lookup name hardcoded_types of
        Just expected | type_of new_val /= expected -> Left expected
        _ -> Right $ Map.insert name new_val environ
    Just old_val
        | type_of old_val == type_of new_val ->
            Right $ Map.insert name (environ_val environ name val) environ
        | otherwise -> Left (type_of old_val)
    where
    maybe_old = Map.lookup name environ
    new_val = case to_val val of
        VRelativeAttr rel_attr -> VAttributes $
            case maybe_old of
                Just (VAttributes attrs) -> set_attr rel_attr attrs
                _ -> set_attr rel_attr Score.no_attrs
        _ -> to_val val

environ_val :: (Typecheck val) => Environ -> ValName -> val -> Val
environ_val environ name val = case to_val val of
    VRelativeAttr rel_attr -> VAttributes $
        case Map.lookup name environ of
            Just (VAttributes attrs) -> set_attr rel_attr attrs
            _ -> set_attr rel_attr Score.no_attrs
    new_val -> new_val

-- | If a standard val gets set to the wrong type, it will cause confusing
-- errors later on.
hardcoded_types :: Map.Map ValName Type
hardcoded_types = Map.fromList
    [ (v_attributes, TAttributes)
    , (v_key, TString)
    , (v_instrument, TInstrument)
    , (v_scale, TScaleId)
    , (v_srate, TNum)
    , (v_seed, TNum)
    ]

data LookupError = NotFound | WrongType Type deriving (Show)

lookup_val :: (Typecheck a) => ValName -> Environ -> Either LookupError a
lookup_val name environ = case Map.lookup name environ of
    Nothing -> Left NotFound
    Just val -> case from_val val of
        Nothing -> Left (WrongType (type_of val))
        Just v -> Right v

-- Define a few inhabitants of Environ which are used by the built-in set
-- of calls.

-- | Default set of attrs.
v_attributes :: ValName
v_attributes = Symbol "attr"

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

-- | Sampling rate used by signal interpolators.
v_srate :: ValName
v_srate = Symbol "srate"

-- | Random seed used by randomization functions.  Can be explicitly
-- initialized to capture a certain \"random\" variation.
v_seed :: ValName
v_seed = Symbol "seed"


-- * parsing

-- | The only operator is @|@, so a list suffices for an AST.
type Expr = [Call]
data Call = Call CallId [Term] deriving (Show)
data Term = ValCall Call | Literal Val deriving (Show)

instance Pretty.Pretty Call where
    pretty (Call call_id terms) =
        Pretty.pretty call_id ++ if null terms then ""
            else " " ++ Seq.join " " (map Pretty.pretty terms)
instance Pretty.Pretty Term where
    pretty (ValCall call) = "(" ++ Pretty.pretty call ++ ")"
    pretty (Literal val) = Pretty.pretty val

instance DeepSeq.NFData Call where
    rnf (Call call_id terms) = call_id `seq` DeepSeq.rnf terms
instance DeepSeq.NFData Term where
    rnf (ValCall call) = DeepSeq.rnf call
    rnf (Literal val) = DeepSeq.rnf val
instance DeepSeq.NFData Val where
    rnf (VNum d) = DeepSeq.rnf d
    rnf (VSymbol (Symbol s)) = DeepSeq.rnf s
    rnf _ = ()

-- | Convenient constructor for Call.  Not to be confused with 'call0'--calln.
--
-- TODO I should be able to have different types of vals, but I think I need an
-- existential wrapper for that, or an infix operator.
call :: String -> [Term] -> Call
call sym = Call (Symbol sym)

inst :: String -> Term
inst = Literal . VInstrument . Score.Instrument

val_call :: String -> Term
val_call sym = ValCall (Call (Symbol sym) [])

