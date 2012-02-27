{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
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

import Util.Control
import qualified Util.Functor0 as Functor0
import Util.Functor0 (Elem)
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal


-- * Derive.Score

-- | An Instrument is identified by a plain string.  This will be looked up in
-- the instrument db to get the backend specific Instrument type as well as
-- the backend itself, but things at the Derive layer and above don't care
-- about all that.
newtype Instrument = Instrument String
    deriving (DeepSeq.NFData, Eq, Ord, Show, Read)

instance Pretty.Pretty Instrument where
    pretty (Instrument inst) = '>' : inst

newtype Control = Control String
    deriving (Eq, Ord, Show, DeepSeq.NFData)

-- | Tag for the type of the values in a control signal.
data Type = Untyped | Chromatic | Diatonic | Score | Real
    deriving (Eq, Ord, Show)

instance Pretty.Pretty Type

type_to_code :: Type -> String
type_to_code typ = case typ of
    Untyped -> ""
    Chromatic -> "c"
    Diatonic -> "d"
    Score -> "s"
    Real -> "r"

code_to_type :: String -> Maybe Type
code_to_type s = case s of
    "c" -> Just Chromatic
    "d" -> Just Diatonic
    "s" -> Just Score
    "r" -> Just Real
    "" -> Just Untyped
    _ -> Nothing

instance Monoid.Monoid Type where
    mempty = Untyped
    mappend Untyped typed = typed
    mappend typed _ = typed

instance Pretty.Pretty Control where
    pretty (Control c) = '%' : c

data Typed a = Typed {
    type_of :: !Type
    , typed_val :: !a
    } deriving (Eq, Show)

instance (DeepSeq.NFData a) => DeepSeq.NFData (Typed a) where
    rnf (Typed typ val) = typ `seq` DeepSeq.rnf val

instance Functor Typed where
    fmap f (Typed typ val) = Typed typ (f val)

merge_typed :: (a -> a -> a) -> Typed a -> Typed a -> Typed a
merge_typed f (Typed typ1 v1) (Typed typ2 v2) = Typed (typ1<>typ2) (f v1 v2)

untyped :: a -> Typed a
untyped = Typed Untyped

type TypedControl = Typed Signal.Control
type TypedVal = Typed Signal.Y

instance Pretty.Pretty TypedVal where
    pretty (Typed typ val) = show_num val ++ type_to_code typ

-- ** Attributes

-- | Instruments can have a set of attributes along with them.  These are
-- propagated dynamically down the derivation stack.  They function like
-- arguments to an instrument, and will typically select an articulation, or
-- a drum from a drumset, or something like that.
type Attribute = String
newtype Attributes = Attributes (Set.Set Attribute)
    deriving (Monoid.Monoid, Eq, Ord, Show)

instance Pretty.Pretty Attributes where
    pretty attrs = "{" ++ Seq.join ", " (attrs_list attrs) ++ "}"

attrs_set :: Attributes -> Set.Set Attribute
attrs_set (Attributes attrs) = attrs

attrs_list :: Attributes -> [Attribute]
attrs_list = Set.toList . attrs_set

no_attrs :: Attributes
no_attrs = Attributes Set.empty


-- * Derive.PitchSignal

newtype Pitch = Pitch PitchCall
type PitchCall = Map.Map Control TypedVal -> Either PitchError Pitch.NoteNumber

instance Eq Pitch where
    Pitch p1 == Pitch p2 = p1 Map.empty == p2 Map.empty

instance Show Pitch where
    show (Pitch p) = "((Pitch " ++ nn ++ "))"
        where
        nn = either (show . Pretty.pretty) (show_num . un_nn) (p Map.empty)
        un_nn (Pitch.NoteNumber nn) = nn

instance Pretty.Pretty Pitch where
    pretty = show

instance Functor0.Functor0 Pitch where
    type Elem Pitch = PitchCall
    fmap0 f (Pitch p) = Pitch (f p)

newtype PitchError = PitchError String deriving (Eq, Ord, Show)

instance Pretty.Pretty PitchError where
    pretty (PitchError s) = s


-- * Derive.TrackLang

type Environ = Map.Map ValName Val

-- | Symbols to look up a val in the 'ValMap'.
type ValName = Symbol

-- ** Val

data Val =
    -- | A number with an optional type suffix.
    --
    -- Literal: @42.23@, @-.4@, @1c@, @-2.4d@
    VNum TypedVal

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
    | VAttributes Attributes

    -- | A control name.  An optional value gives a default if the control
    -- isn't present.
    --
    -- Literal: @%control@, @%control,.4@
    | VControl ValControl
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
    | VPitch Pitch
    -- | Sets the instrument in scope for a note.  An empty instrument doesn't
    -- set the instrument, but can be used to mark a track as a note track.
    --
    -- Literal: @>@, @>inst@
    | VInstrument Instrument

    -- | A call to a function.  Symbol parsing is special in that the first
    -- word is always parsed as a symbol.  So you can have symbols of numbers
    -- or other special characters.  This means that a symbol can't be in the
    -- middle of an expression, but if you surround a word with parens like
    -- @(42)@, it will be interpreted as a call.  So the only characters a
    -- symbol can't have are space and parens.
    --
    -- Literal: @func@
    | VSymbol Symbol
    -- | An explicit not-given arg for functions so you can use positional
    -- args with defaults.
    --
    -- Literal: @_@
    | VNotGiven
    deriving (Show)

-- | The Pretty instance for val should, like the haskell-level (Show, Read)
-- pair, produce a string that the parser can turn back into the original
-- value.  Except for values which have no literal syntax, such as VPitch.
--
-- The reason why is documented in 'Derive.Call.Note.inverting'.
instance Pretty.Pretty Val where
    pretty val = case val of
        VNum d -> Pretty.pretty d
        VString s -> "'" ++ Seq.replace "'" "''" s ++ "'"
        VRelativeAttr (RelativeAttr (mode, attr)) -> case mode of
            Add -> '+' : attr
            Remove -> '-' : attr
            Set -> '=' : attr
            Clear -> "=-"
        VAttributes (Attributes attrs) -> Seq.join "+" (Set.toList attrs)
        VControl control -> Pretty.pretty control
        VPitchControl control -> Pretty.pretty control
        VScaleId (Pitch.ScaleId scale_id) -> '*' : scale_id
        VPitch pitch -> "<pitch: " ++ Pretty.pretty pitch ++ ">"
        VInstrument (Instrument inst) -> '>' : inst
        VSymbol sym -> Pretty.pretty sym
        VNotGiven -> "_"

-- | Convert a haskell number into a tracklang number.
show_num :: (RealFloat a) => a -> String
show_num = Pretty.show_float (Just 2)

newtype Symbol = Symbol String deriving (Eq, Ord, Show)
instance Pretty.Pretty Symbol where pretty (Symbol s) = s

data AttrMode = Add | Remove | Set | Clear deriving (Eq, Show)
newtype RelativeAttr = RelativeAttr (AttrMode, Attribute) deriving (Eq, Show)

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

instance Pretty.Pretty PitchControl where
    -- The PitchControl syntax doesn't support args for the signal default yet.
    pretty = show_control '#' (Pitch.note_text . note_sym)

instance Pretty.Pretty ValControl where
    pretty = show_control '%'
        (\(Typed typ num) -> show_num num ++ type_to_code typ)

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
