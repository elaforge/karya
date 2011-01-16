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
module Derive.TrackLang where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Derive.Score as Score
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal


data Val =
    -- | Literal: @42.23@, @-.4@
    VNum Double
    -- | Escape a quote by doubling it.
    --
    -- Literal: @'hello'@, @'quinn''s hat'@
    | VString String

    -- | Relative attribute adjustment.
    --
    -- This is a bit of a hack, but means I can do attr adjustment without +=
    -- or -= operators.
    --
    -- Literal: @+attr@, @-attr@, @=attr@, @=-@ (to clear attributes).
    | VRelativeAttr RelativeAttr
    -- | A set of Attributs for an instrument.  No literal, since you can use
    -- VRelativeAttr.
    | VAttributes Score.Attributes

    -- | A control name.  An optional value gives a default if the control
    -- isn't present.
    --
    -- Literal: @%control@, @%control,.4@
    | VControl Control
    -- | If a control name starts with a *, it denotes a pitch signal and the
    -- scale is taken from the environ.  Unlike a control signal, the empty
    -- string is a valid signal name and means the default pitch signal.
    --
    -- Literal: @#pitch,4c@, @#,4@, @#@
    | VPitchControl PitchControl

    -- | The literal names a ScaleId, and will probably result in an exception
    -- if the lookup fails.  The empty scale is taken to mean the relative
    -- scale.
    --
    -- Literal: @*scale@, @*@.
    | VScaleId Pitch.ScaleId
    -- | No literal yet, but is returned from val calls.
    | VDegree Pitch.Degree
    -- | Sets the instrument in scope for a note.  An empty instrument doesn't
    -- set the instrument, but can be used to mark a track as a note track.
    --
    -- Literal: @>@, @>inst@
    | VInstrument Score.Instrument

    -- | A call to a function.  Symbol parsing is special in that the first
    -- word is always parsed as a symbol.  So you can have symbols of numbers
    -- or other special characters.  This means that a symbol can't be in the
    -- middle of an expression, but if you surround a word with parens like
    -- @(42)@, it will be interpreted as a call.  So the only characters a
    -- symbol can't have are space and parens.
    --
    -- Literal: @func@
    | VSymbol Symbol
    -- | An explicit not-given arg for functions so you can use positional args
    -- with defaults.
    --
    -- Literal: @_@
    | VNotGiven
    deriving (Eq, Show)

instance Pretty.Pretty Val where
    pretty val = case val of
            VNum d -> show_num d
            VString s -> "'" ++ Seq.replace "'" "''" s ++ "'"
            VRelativeAttr (RelativeAttr (mode, attr)) -> case mode of
                Add -> '+' : attr
                Remove -> '-' : attr
                Set -> '=' : attr
                Clear -> "=-"
            VAttributes attrs -> Seq.join "+" (Score.attrs_list attrs)
            VControl control -> show_control '%' Pretty.pretty control
            VPitchControl control -> show_control '#' Pitch.note_text control
            VScaleId (Pitch.ScaleId scale_id) -> '*' : scale_id
            VDegree (Pitch.Degree d) -> "<degree: " ++ show_num d ++ ">"
            VInstrument (Score.Instrument inst) -> '>' : inst
            VSymbol sym -> Pretty.pretty sym
            VNotGiven -> "_"
        where
        show_control prefix show_val control = case control of
            ConstantControl val -> show_val val
            DefaultedControl (Score.Control cont) deflt ->
                prefix : cont ++ ',' : show_val deflt
            Control (Score.Control cont) -> prefix : cont

-- | Convert a haskell number into a tracklang number.
show_num :: (RealFloat a) => a -> String
show_num = Pretty.show_float (Just 2)

data AttrMode = Add | Remove | Set | Clear deriving (Eq, Show)

-- | Symbols used in function call position.
type CallId = Symbol
-- | Symbols to look up a val in the 'ValMap'.
type ValName = Symbol

newtype Symbol = Symbol String deriving (Eq, Ord, Show)
instance Pretty.Pretty Symbol where
    pretty (Symbol "") = "<null>"
    pretty (Symbol s) = s

type Control = ControlRef Signal.Y
type PitchControl = ControlRef Pitch.Note

data ControlRef val =
    -- | A constant signal.  For 'Control', this is coerced from a VNum
    -- literal.
    ConstantControl val
    -- | If the control isn't present, use the constant.
    | DefaultedControl Score.Control val
    -- | Throw an exception if the control isn't present.
    | Control Score.Control
    deriving (Eq, Show)

newtype RelativeAttr = RelativeAttr (AttrMode, Score.Attribute)
    deriving (Eq, Show)

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

-- * constants

c_equal :: CallId
c_equal = Symbol "="

-- | Define a few inhabitants of Environ which are used by the built-in set
-- of calls.
v_instrument, v_attributes, v_scale, v_srate :: ValName
v_instrument = Symbol "inst"
v_attributes = Symbol "attr"
v_scale = Symbol "scale"
v_srate = Symbol "srate"

-- * types

data Type = TNum | TString | TRelativeAttr | TAttributes | TControl
    | TPitchControl | TScaleId | TDegree | TInstrument | TSymbol
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
    VDegree {} -> TDegree
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

instance Typecheck Control where
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

instance Typecheck Pitch.Degree where
    from_val (VDegree a) = Just a
    from_val _ = Nothing
    to_val = VDegree
    to_type _ = TDegree

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

type Environ = Map.Map ValName Val

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
            Right $ Map.insert name new_val environ
        | otherwise -> Left (type_of old_val)
    where
    maybe_old = Map.lookup name environ
    new_val = case (to_val val) of
        VRelativeAttr rel_attr -> VAttributes $
            case maybe_old of
                Just (VAttributes attrs) -> set_attr rel_attr attrs
                _ -> set_attr rel_attr Score.no_attrs
        _ -> to_val val

-- | If a standard val gets set to the wrong type, it will cause confusing
-- errors later on.
hardcoded_types :: Map.Map ValName Type
hardcoded_types = Map.fromList
    [ (v_instrument, TInstrument)
    , (v_attributes, TAttributes)
    , (v_scale, TScaleId)
    , (v_srate, TNum)
    ]

data LookupError = NotFound | WrongType Type deriving (Show)

lookup_val :: (Typecheck a) => ValName -> Environ -> Either LookupError a
lookup_val name environ = case Map.lookup name environ of
    Nothing -> Left NotFound
    Just val -> case from_val val of
        Nothing -> Left (WrongType (type_of val))
        Just v -> Right v


-- * parsing

-- | The only operator is @|@, so a list of lists suffices for an AST.
type Expr = [Call]
data Call = Call CallId [Term] deriving (Eq, Show)
data Term = ValCall Call | Literal Val deriving (Eq, Show)

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
call :: String -> Call
call sym = Call (Symbol sym) []

val_call :: String -> Term
val_call sym = ValCall (Call (Symbol sym) [])
