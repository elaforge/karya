-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

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
import qualified Data.Char as Char
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set
import qualified Data.String as String
import qualified Data.Text as Text

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Serialize as Serialize
import qualified Util.TimeVector as TimeVector

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
newtype Instrument = Instrument Text
    deriving (DeepSeq.NFData, Eq, Ord, Show, Read)

instance Pretty.Pretty Instrument where pretty = untxt . ShowVal.show_val
instance ShowVal.ShowVal Instrument where
    show_val (Instrument inst) = Text.cons '>' inst

-- | A control is an abstract parameter that influences derivation.  Some of
-- them affect performance and will be rendered as MIDI controls or note
-- parameters or whatever, while others may affect derivation (e.g. tempo) and
-- won't be seen by the backend at all.
newtype Control = Control Text
    deriving (Eq, Ord, Read, Show, DeepSeq.NFData, Serialize.Serialize,
        String.IsString)

-- | Tag for the type of the values in a control signal.
data Type = Untyped | Chromatic | Diatonic | Nn | Score | Real
    deriving (Eq, Ord, Read, Show)

instance Pretty.Pretty Type where pretty = show

all_types :: [Type]
all_types = [Chromatic, Diatonic, Nn, Score, Real, Untyped]
    -- Untyped goes last because the parser tries them in order.

type_to_code :: Type -> String
type_to_code typ = case typ of
    Untyped -> ""
    Chromatic -> "c"
    Diatonic -> "d"
    Nn -> "nn"
    Score -> ScoreTime.suffix : "" -- t for time
    Real -> RealTime.suffix : "" -- s for seconds

code_to_type :: String -> Maybe Type
code_to_type s = case s of
    "c" -> Just Chromatic
    "d" -> Just Diatonic
    "nn" -> Just Nn
    "t" -> Just Score
    "s" -> Just Real
    "" -> Just Untyped
    _ -> Nothing

instance Monoid.Monoid Type where
    mempty = Untyped
    mappend Untyped typed = typed
    mappend typed _ = typed

instance Pretty.Pretty Control where pretty = untxt . ShowVal.show_val
instance ShowVal.ShowVal Control where
    show_val (Control c) = Text.cons '%' c

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

type TypedControl = Typed Signal.Control
type TypedVal = Typed Signal.Y

instance ShowVal.ShowVal TypedVal where
    show_val (Typed typ val) = ShowVal.show_val val <> txt (type_to_code typ)

-- ** Attributes

-- | Instruments can have a set of attributes along with them.  These are
-- propagated dynamically down the derivation stack.  They function like
-- arguments to an instrument, and will typically select an articulation, or
-- a drum from a drumset, or something like that.
type Attribute = Text
newtype Attributes = Attributes (Set.Set Attribute)
    deriving (Monoid.Monoid, Eq, Ord, Read, Show)

instance Pretty.Pretty Attributes where pretty = untxt . ShowVal.show_val
instance ShowVal.ShowVal Attributes where
    show_val = ("+"<>) . Text.intercalate "+" . attrs_list

attr :: Text -> Attributes
attr = Attributes . Set.singleton

attrs :: [Text] -> Attributes
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

-- | A pitch signal is similar to a 'Signal.Control', except that its values
-- are 'Pitch'es instead of plain floating point values.
newtype Signal = Signal { sig_vec :: TimeVector.Boxed Pitch }
    deriving (Show, Pretty.Pretty)

instance DeepSeq.NFData Signal where
    rnf (Signal vec) = vec `seq` ()

-- | A PitchConfig is the data that can continue to influence the pitch's
-- frequency.
--
-- The ControlValMap could be embedded in the Environ, but firstly they come
-- from different places, and secondly the ControlValMap is combined
-- additively.
data PitchConfig = PitchConfig !Environ !ControlValMap
    deriving (Show)

type ControlValMap = Map.Map Control Signal.Y

instance Monoid.Monoid PitchConfig where
    mempty = PitchConfig mempty mempty
    mappend (PitchConfig env1 c1) (PitchConfig env2 c2) =
        PitchConfig (env1 <> env2) (Map.unionWith (+) c1 c2)

-- | A pitch is an abstract value that can turn a map of values into
-- a NoteNumber.  The values are expected to contain transpositions that this
-- Pitch understands, for example 'Controls.chromatic' and 'Controls.diatonic'.
data Pitch = Pitch {
    pitch_eval_nn :: !(PitchConfig -> Either PitchError Pitch.NoteNumber)
    , pitch_eval_note :: !(PitchConfig -> Either PitchError Pitch.Note)
    , pitch_scale :: !Scale
    }

-- | Signal can't take a Scale because that would be a circular import.
-- Fortunately it only needs a few fields.  However, because of the
-- circularity, the Scale.Scale -> PitchSignal.Scale constructor is in
-- "Derive.Derive".
data Scale = Scale {
    -- | It can be useful to see the scale of a pitch, e.g. to create more
    -- pitches in the same scale as an existing pitch.
    pscale_scale_id :: !Pitch.ScaleId
    -- | The set of transposer signals for this scale, as documented in
    -- 'Derive.Scale.scale_transposers'.
    --
    -- They are stored here because they're needed by 'to_nn'.  I could
    -- store them separately, e.g. in the 'Score.Event' alongside the
    -- event_pitch, but the scale at event creation time is not guaranteed to
    -- be the same as the one when the pitch was created, so the safest thing
    -- to do is keep it with the pitch itself.
    , pscale_transposers :: !(Set.Set Control)
    } deriving (Show)

instance Pretty.Pretty Scale where
    pretty = Pretty.pretty . pscale_scale_id

-- | It can't be reduced since it has lambdas, but at least this way you can
-- easily rnf things that contain it.
instance DeepSeq.NFData Pitch where
    rnf _ = ()

instance Show Pitch where
    -- Show just the NN, so this is parseable by Util.PPrint.
    show p = either show Pretty.pretty (pitch_eval_nn p mempty)

-- | Will look like: 62.95nn,4i(*wayang)
instance Pretty.Pretty Pitch where
    pretty p = either show Pretty.pretty (pitch_eval_nn p mempty) <> ","
        <> either show (untxt . Pitch.note_text) (pitch_eval_note p mempty)
        <> "(" <> Pretty.pretty (pitch_scale p) <> ")"

-- | Pitches have no literal syntax, but I have to print something.
instance ShowVal.ShowVal Pitch where
    show_val pitch = "<pitch: " <> txt (Pretty.pretty pitch) <> ">"

newtype PitchError = PitchError Text deriving (Eq, Ord, Read, Show)
instance Pretty.Pretty PitchError where pretty (PitchError s) = untxt s


-- * Derive.TrackLang

newtype Environ = Environ (Map.Map ValName Val)
    deriving (Show, Monoid.Monoid, Pretty.Pretty, DeepSeq.NFData)

make_environ :: [(ValName, Val)] -> Environ
make_environ = Environ . Map.fromList

environ_to_list :: Environ -> [(ValName, Val)]
environ_to_list (Environ env) = Map.toList env

-- | Insert a val directly, with no typechecking.
insert_val :: ValName -> Val -> Environ -> Environ
insert_val name val (Environ env) = Environ $ Map.insert name val env

delete_val :: ValName -> Environ -> Environ
delete_val name (Environ env) = Environ $ Map.delete name env

lookup_val :: ValName -> Environ -> Maybe Val
lookup_val name (Environ env) = Map.lookup name env

null_environ :: Environ -> Bool
null_environ (Environ env) = Map.null env

-- | Symbols to look up a val in the 'ValMap'.
type ValName = Symbol

-- ** Val

-- | Pitches have to be evaluated, but the parser has to return something
-- unevaluated.
type RawVal = ValType RawPitchControl

-- | Val with VPitchControl evaluated.
type Val = ValType PitchControl

data ValType pitch =
    -- | A number with an optional type suffix.  It also has a ratio style
    -- literal, though the output is still a floating point value, not a true
    -- ratio.
    --
    -- Literal: @42.23@, @-.4@, @1c@, @-2.4d@, @3/2@, @-3/2@, @0x7f@.
    VNum !TypedVal
    -- | A set of Attributes for an instrument.
    --
    -- Literal: @+attr@, @+attr1+attr2@.
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
    | VPitchControl !pitch

    -- | No literal, but is returned from val calls, notably scale calls.
    | VPitch !Pitch
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
    -- 'Derive.ParseBs.p_string'.  Or if it starts with a hyphen, letter, or
    -- @*@, it doesn't need quotes at all: 'Derive.ParseBs.p_symbol'.
    --
    -- Literal: @func@, @\'hello\'@, @\'quinn\'\'s hat\'@
    | VSymbol !Symbol
    -- | An explicit not-given arg for functions so you can use positional
    -- args with defaults.
    --
    -- Literal: @_@
    | VNotGiven
    deriving (Show)

-- | This instance is actually invalid due to showing VPitch, which has no
-- literal, and for 'Val', showing 'PitchControl', which amounts to the same
-- thing.  I use this to treat any Val as a Symbol to re-evaluate it.  Being
-- invalid means that a VPitch or VPitchControl with a default will cause
-- a parse failure, but I'll have to see if this becomes a problem in practice.
instance (ShowVal.ShowVal pitch) => ShowVal.ShowVal (ValType pitch) where
    show_val val = case val of
        VNum d -> ShowVal.show_val d
        VAttributes attrs -> ShowVal.show_val attrs
        VControl control -> ShowVal.show_val control
        VPitchControl control -> ShowVal.show_val control
        VPitch pitch -> ShowVal.show_val pitch
        VInstrument inst -> ShowVal.show_val inst
        VSymbol sym -> ShowVal.show_val sym
        VNotGiven -> "_"

instance (ShowVal.ShowVal pitch) => Pretty.Pretty (ValType pitch) where
    pretty = untxt . ShowVal.show_val

instance DeepSeq.NFData (ValType pitch) where
    rnf (VNum d) = DeepSeq.rnf d
    rnf (VSymbol (Symbol s)) = DeepSeq.rnf s
    rnf _ = ()

newtype Symbol = Symbol Text
    deriving (Eq, Ord, Show, DeepSeq.NFData, String.IsString)
instance Pretty.Pretty Symbol where pretty = untxt . ShowVal.show_val

instance ShowVal.ShowVal Symbol where
    -- TODO This is actually kind of error prone.  The problem is that symbols
    -- at the beginning of an expression are parsed as-is and cannot have
    -- quotes.  Only ones as arguments need quotes.  Symbols are rarely
    -- arguments, but strings frequently are.  Maybe I should go back to
    -- separate types for symbols and strings?
    show_val (Symbol s)
        | parseable = s
        | otherwise = "'" <> Text.concatMap quote s <> "'"
        where
        -- This should be the same as ParseBs.p_symbol.  I can't use it
        -- directly because that would be a circular import.
        parseable = case Text.uncons s of
            Just (c, cs) -> (Char.isAlpha c || c == '-' || c == '*')
                && Text.all (\c -> c /= ' ' && c /= ')') cs
            Nothing -> False
        quote '\'' = "''"
        quote c = Text.singleton c

-- | Show a symbol intended for call position.  Call position is special in
-- that it can contain any character except space without quoting.
show_call_val :: Val -> Text
show_call_val (VSymbol (Symbol sym)) = sym
show_call_val val = ShowVal.show_val val

instance ShowVal.ShowVal Text where
    show_val = ShowVal.show_val . Symbol

data ControlRef val =
    -- | A signal literal.
    ControlSignal val
    -- | If the control isn't present, use the constant.
    | DefaultedControl Control val
    -- | Throw an exception if the control isn't present.
    | LiteralControl Control
    deriving (Eq, Show)

type PitchControl = ControlRef Signal
-- | Pitches have to be evaluated, but the parser has to return something
-- unevaluated.
type RawPitchControl = ControlRef PitchCall
type ValControl = ControlRef TypedControl

instance Pretty.Pretty PitchControl where pretty = untxt . ShowVal.show_val

-- | There's no way to convert a pitch back into the expression that produced
-- it, so this is the best I can do.
--
-- Similar to ShowVal 'ValControl', there's no signal literal so I use the
-- value at 0.  A pitch can be turned into an expression, but not necessarily
-- accurately since it doesn't take things like pitch interpolation into
-- account.
instance ShowVal.ShowVal PitchControl where
    show_val = show_control '#' $ \sig -> case TimeVector.at 0 (sig_vec sig) of
        Nothing -> "<none>"
        Just p -> ShowVal.show_val p

instance ShowVal.ShowVal RawPitchControl where
    show_val = show_control '#' (\c -> "(" <> ShowVal.show_val c <> ")")

instance Pretty.Pretty ValControl where pretty = untxt . ShowVal.show_val

-- | This can only represent constant signals, since there's no literal for an
-- arbitrary signal.  Non-constant signals will turn into a constant of
-- whatever was at 0.
instance ShowVal.ShowVal ValControl where
    show_val = show_control '%' $ \(Typed typ sig) ->
        ShowVal.show_val (Signal.at 0 sig) <> txt (type_to_code typ)

show_control :: Char -> (val -> Text) -> ControlRef val -> Text
show_control prefix val_text control = case control of
    ControlSignal val -> val_text val
    DefaultedControl (Control cont) deflt -> mconcat
        [Text.singleton prefix, cont, ",", val_text deflt]
    LiteralControl (Control cont) -> Text.cons prefix cont

-- ** Call

-- | Symbols used in function call position.  This is just to document that
-- a symbol is expected to be looked up in the scope.
type CallId = Symbol

-- | The only operator is @|@, so a list suffices for an AST.
type Expr = NonEmpty Call
data Call = Call CallId [Term] deriving (Show)
data Term = ValCall Call | Literal RawVal deriving (Show)

-- | This is just a 'Call', but it's expected to return a VPitch.
type PitchCall = Call

instance ShowVal.ShowVal Expr where
    show_val expr =
        Text.intercalate " | " (map ShowVal.show_val (NonEmpty.toList expr))
instance ShowVal.ShowVal Call where
    show_val (Call (Symbol sym) terms) =
        sym <> if null terms then ""
            else " " <> Text.unwords (map ShowVal.show_val terms)
instance ShowVal.ShowVal Term where
    show_val (ValCall call) = "(" <> ShowVal.show_val call <> ")"
    show_val (Literal val) = ShowVal.show_val val

instance Pretty.Pretty Call where
    pretty = untxt . ShowVal.show_val

instance DeepSeq.NFData Call where
    rnf (Call call_id terms) = call_id `seq` DeepSeq.rnf terms
instance DeepSeq.NFData Term where
    rnf (ValCall call) = DeepSeq.rnf call
    rnf (Literal val) = DeepSeq.rnf val
