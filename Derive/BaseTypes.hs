{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE OverlappingInstances #-} -- I want a special Pretty for TypedVal
{-# OPTIONS_HADDOCK not-home #-}
{- | This is a bit of song and dance to avoid circular imports.

    "Derive.Score", "Derive.PitchSignal", and "Derive.TrackLang" all define
    basic types.  They also refer to each others types, which means they must
    all be defined in the same module.  But each set of types also comes with
    its own set of functions, and it would make for a giant messy module to
    put them all together.

    So the basic types are defined here, and re-exported from their intended
    modules.  All importers should access the symbols from the higher-level
    modules if at all possible.  Even the ones that must import BaseTypes
    (which should be only the modules collected in BasyTypes itself) should
    use @import qualified as@ to make clear the module that the symbols
    *should* be coming from.

    It's a little grody but still nicer than hs-boot.

    TODO some haddock flags to make sure the docs are collected in the high
    level modules?
-}
module Derive.BaseTypes where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set

import qualified Text.Read as Read

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Ui.ScoreTime as ScoreTime
import qualified Derive.ShowVal as ShowVal
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal


-- * Derive.Score

-- | An Instrument is identified by a plain string.  This will be looked up in
-- the instrument db to get the backend specific Instrument type as well as
-- the backend itself, but things at the Derive layer and above don't care
-- about all that.
newtype Instrument = Instrument String
    deriving (DeepSeq.NFData, Eq, Ord, Show, Read)

instance Pretty.Pretty Instrument where pretty = ShowVal.show_val
instance ShowVal.ShowVal Instrument where
    show_val (Instrument inst) = '>' : inst

newtype Control = Control String
    deriving (Eq, Ord, Read, Show, DeepSeq.NFData)

-- | Tag for the type of the values in a control signal.
data Type = Untyped | Chromatic | Diatonic | Score | Real
    deriving (Eq, Ord, Read, Show)

instance Pretty.Pretty Type where pretty = show

type_to_code :: Type -> String
type_to_code typ = case typ of
    Untyped -> ""
    Chromatic -> "c"
    Diatonic -> "d"
    Score -> ScoreTime.suffix : "" -- t for time
    Real -> RealTime.suffix : "" -- s for seconds

code_to_type :: String -> Maybe Type
code_to_type s = case s of
    "c" -> Just Chromatic
    "d" -> Just Diatonic
    "t" -> Just Score
    "s" -> Just Real
    "" -> Just Untyped
    _ -> Nothing

instance Monoid.Monoid Type where
    mempty = Untyped
    mappend Untyped typed = typed
    mappend typed _ = typed

instance Pretty.Pretty Control where pretty = ShowVal.show_val
instance ShowVal.ShowVal Control where
    show_val (Control c) = '%' : c

data Typed a = Typed {
    type_of :: !Type
    , typed_val :: !a
    } deriving (Eq, Ord, Read, Show)

instance (DeepSeq.NFData a) => DeepSeq.NFData (Typed a) where
    rnf (Typed typ val) = typ `seq` DeepSeq.rnf val

instance Functor Typed where
    fmap f (Typed typ val) = Typed typ (f val)

instance (Pretty.Pretty a) => Pretty.Pretty (Typed a) where
    format (Typed typ val) =
        Pretty.text (if null c then "" else c ++ ":") <> Pretty.format val
        where c = type_to_code typ

merge_typed :: (a -> a -> a) -> Typed a -> Typed a -> Typed a
merge_typed f (Typed typ1 v1) (Typed typ2 v2) = Typed (typ1<>typ2) (f v1 v2)

untyped :: a -> Typed a
untyped = Typed Untyped

type TypedSignal = Typed Signal.Control
type TypedVal = Typed Signal.Y

instance ShowVal.ShowVal TypedVal where
    show_val (Typed typ val) = ShowVal.show_val val ++ type_to_code typ

-- ** Attributes

-- | Instruments can have a set of attributes along with them.  These are
-- propagated dynamically down the derivation stack.  They function like
-- arguments to an instrument, and will typically select an articulation, or
-- a drum from a drumset, or something like that.
type Attribute = String
newtype Attributes = Attributes (Set.Set Attribute)
    deriving (Monoid.Monoid, Eq, Ord, Read, Show)

instance Pretty.Pretty Attributes where pretty = ShowVal.show_val
instance ShowVal.ShowVal Attributes where
    show_val attrs = if null alist then "-" else '+' : Seq.join "+" alist
        where alist = attrs_list attrs

attr :: String -> Attributes
attr = Attributes . Set.singleton

attrs :: [String] -> Attributes
attrs = Attributes . Set.fromList

set_to_attrs :: Set.Set Attribute -> Attributes
set_to_attrs = Attributes

attrs_diff :: Attributes -> Attributes -> Attributes
attrs_diff (Attributes x) (Attributes y) = Attributes (Set.difference x y)

-- | True if the first argument contains the attributes in the second.
attrs_contain :: Attributes -> Attributes -> Bool
attrs_contain (Attributes super) (Attributes sub) = sub `Set.isSubsetOf` super

attrs_set :: Attributes -> Set.Set Attribute
attrs_set (Attributes attrs) = attrs

attrs_remove :: Attributes -> Attributes -> Attributes
attrs_remove (Attributes remove) (Attributes attrs) =
    Attributes $ attrs `Set.difference` remove

attrs_list :: Attributes -> [Attribute]
attrs_list = Set.toList . attrs_set

no_attrs :: Attributes
no_attrs = Attributes Set.empty


-- * Derive.PitchSignal

-- | A pitch is an abstract value that can turn a map of values into
-- a NoteNumber.  The values are expected to contain transpositions that this
-- Pitch understands, for example 'Derive.Score.c_chromatic' and
-- 'Derive.Score.c_diatonic'.
data Pitch = Pitch !(PitchCall Pitch.NoteNumber) !(PitchCall Pitch.Note)
type PitchCall a = Controls -> Either PitchError a
type Controls = Map.Map Control Signal.Y

instance Eq Pitch where
    Pitch p1 _ == Pitch p2 _ = p1 Map.empty == p2 Map.empty

instance Show Pitch where
    show (Pitch p _) = show (p Map.empty)

-- | This is just for debugging convenience, since it doesn't preserve the
-- structure of the pitch.
instance Read Pitch where
    readPrec = mk <$> Read.readPrec
        where
        mk nn = Pitch (const (Right nn)) (const (Right (Pitch.Note (show nn))))

instance Pretty.Pretty Pitch where
    pretty (Pitch p n) = either show Pretty.pretty (p Map.empty)
        <> "(" <> either show Pitch.note_text (n Map.empty) <> ")"

newtype PitchError = PitchError String deriving (Eq, Ord, Read, Show)
instance Pretty.Pretty PitchError where pretty (PitchError s) = s


-- * Derive.TrackLang

newtype Environ = Environ (Map.Map ValName Val)
    deriving (Show, Monoid.Monoid, Pretty.Pretty, DeepSeq.NFData)

make_environ :: [(ValName, Val)] -> Environ
make_environ = Environ . Map.fromList

-- | Insert a val directly, with no typechecking.
insert_val :: ValName -> Val -> Environ -> Environ
insert_val name val (Environ env) = Environ $ Map.insert name val env

lookup_val :: ValName -> Environ -> Maybe Val
lookup_val name (Environ env) = Map.lookup name env

null_environ :: Environ -> Bool
null_environ (Environ env) = Map.null env

-- | Symbols to look up a val in the 'ValMap'.
type ValName = Symbol

-- | Environ key for the default attributes.
--
-- This is defined here so 'Derive.Score.event_attributes' can use it.
v_attributes :: ValName
v_attributes = Symbol "attr"

-- ** Val

data Val =
    -- | A number with an optional type suffix.
    --
    -- Literal: @42.23@, @-.4@, @1c@, @-2.4d@
    VNum !TypedVal

    -- | Relative attribute adjustment.
    --
    -- This is a bit of a hack, but means I can do attr adjustment without +=
    -- or -= operators.
    --
    -- Literal: @+attr@, @-attr@, @=attr@, @=-@ (to clear attributes).
    | VRelativeAttrs !RelativeAttrs
    -- | A set of Attributes for an instrument.  No literal, since you can use
    -- VRelativeAttrs.
    | VAttributes !Attributes

    -- | A control name.  An optional value gives a default if the control
    -- isn't present.
    --
    -- Literal: @%control@, @%control,.4@
    | VControl !ValControl
    -- | If a control name starts with a *, it denotes a pitch signal and the
    -- scale is taken from the environ.  Unlike a control signal, the empty
    -- string is a valid signal name and means the default pitch signal.
    --
    -- Literal: @\#pitch,4c@, @\#,4@, @\#@
    | VPitchControl !PitchControl

    -- | The literal names a ScaleId, and will probably result in an exception
    -- if the lookup fails.  The empty scale is taken to mean the relative
    -- scale.
    --
    -- Literal: @*scale@, @*@.
    | VScaleId !Pitch.ScaleId
    -- | No literal yet, but is returned from val calls.
    | VPitch Pitch
    -- | Sets the instrument in scope for a note.  An empty instrument doesn't
    -- set the instrument, but can be used to mark a track as a note track.
    --
    -- Literal: @>@, @>inst@
    | VInstrument !Instrument

    -- | A string, which is interpreted as a call if it's at the front of an
    -- expression.  Parsing a symbol is somewhat complicated.  If it occurs
    -- at the front of an expression, it can have anything in it except
    -- spaces or parens: 'Derive.ParseBs.p_call_symbol'.  If it's in the
    -- argument position, it can be surrounded with single quotes and contain
    -- anything, and a single quote is encoded as two single quotes:
    -- 'Derive.ParseBs.p_string'.  Or if it starts with a letter and has no
    -- spaces it can be written bare: 'Derive.ParseBs.p_symbol'.
    --
    -- Literal: @func@, @\'hello\'@, @\'quinn\'\'s hat\'@
    | VSymbol !Symbol
    -- | An explicit not-given arg for functions so you can use positional
    -- args with defaults.
    --
    -- Literal: @_@
    | VNotGiven
    deriving (Show)

instance ShowVal.ShowVal Val where
    show_val val = case val of
        VNum d -> ShowVal.show_val d
        VRelativeAttrs rel -> ShowVal.show_val rel
        VAttributes attrs -> ShowVal.show_val attrs
        VControl control -> ShowVal.show_val control
        VPitchControl control -> ShowVal.show_val control
        VScaleId scale_id -> ShowVal.show_val scale_id
        VPitch pitch -> ShowVal.show_val pitch
        VInstrument inst -> ShowVal.show_val inst
        VSymbol sym -> ShowVal.show_val sym
        VNotGiven -> "_"

instance DeepSeq.NFData Val where
    rnf (VNum d) = DeepSeq.rnf d
    rnf (VSymbol (Symbol s)) = DeepSeq.rnf s
    rnf _ = ()

-- | Pitchas have no literal syntax, but I have to print something.
instance ShowVal.ShowVal Pitch where
    show_val pitch = "<pitch: " ++ Pretty.pretty pitch ++ ">"

instance Pretty.Pretty Val where pretty = ShowVal.show_val
instance ShowVal.ShowVal Pitch.ScaleId where
    show_val (Pitch.ScaleId s) = '*' : s

newtype Symbol = Symbol String deriving (Eq, Ord, Show, DeepSeq.NFData)
instance Pretty.Pretty Symbol where pretty = ShowVal.show_val

instance ShowVal.ShowVal Symbol where
    show_val (Symbol s)
        | ' ' `elem` s = '\'' : concatMap quote s ++ "'"
        | otherwise = s
        where
        quote '\'' = "''"
        quote c = [c]

data RelativeAttrs = Add Attributes | Remove Attributes | Set Attributes
    deriving (Eq, Show)

instance ShowVal.ShowVal RelativeAttrs where
    show_val rel = case rel of
            Add attrs -> '+' : str attrs
            Remove attrs -> '-' : str attrs
            Set attrs -> '=' : str attrs
        where
        str attrs = case attrs_list attrs of
            [] -> "-"
            as -> Seq.join "+" as


data ControlRef val =
    -- | A constant signal.  For 'Control', this is coerced from a VNum
    -- literal.
    ConstantControl val
    -- | If the control isn't present, use the constant.
    | DefaultedControl Control val
    -- | Throw an exception if the control isn't present.
    | LiteralControl Control
    deriving (Eq, Show)

type PitchControl = ControlRef Note
type ValControl = ControlRef TypedVal

instance Pretty.Pretty PitchControl where pretty = ShowVal.show_val
instance ShowVal.ShowVal PitchControl where
    -- The PitchControl syntax doesn't support args for the signal default yet.
    show_val = show_control '#' (Pitch.note_text . note_sym)

instance Pretty.Pretty ValControl where pretty = ShowVal.show_val
instance ShowVal.ShowVal ValControl where
    show_val = show_control '%' $ \(Typed typ num) ->
        ShowVal.show_val num ++ type_to_code typ

show_control :: Char -> (val -> String) -> ControlRef val -> String
show_control prefix show_val control = case control of
    ConstantControl val -> show_val val
    DefaultedControl (Control cont) deflt ->
        prefix : cont ++ ',' : show_val deflt
    LiteralControl (Control cont) -> prefix : cont

-- ** Note

-- | Pitch.Note is just the name of the pitch, but the TrackLang Note carries
-- args, and should be used in preference to Pitch.Note where appropriate.
data Note = Note {
    note_sym :: Pitch.Note
    , note_args :: [Val]
    } deriving (Show)
