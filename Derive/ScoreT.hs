-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# OPTIONS_HADDOCK not-home #-}
-- | Low-dependency basic types for derivation.
module Derive.ScoreT (
    -- * Instrument
    Instrument(..)
    , instrument_name
    , empty_instrument

    -- * Control
    , Control(..)
    , control_name
    , control, unchecked_control

    -- * PControl
    , PControl(..)
    , pcontrol_name
    , default_pitch
    , pcontrol, unchecked_pcontrol
    , parse_generic_control

    -- * Type
    , Type(..)
    , all_types
    , type_to_code, code_to_type
    , TimeT(..), TransposeT(..), Duration(..)
    , time_t, transpose_t, duration
    , Typed(..)
    , merge_typed
    , untyped
    , type_to_transpose

    -- * type aliases
    , ControlValMap
    , TypedControlValMap
    , ControlMap
    , FunctionMap
    , Function
    , TypedFunction
    , TypedSignal
) where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Hashable as Hashable
import qualified Data.Map as Map
import qualified Data.String as String
import qualified Data.Text as Text

import qualified Util.Lists as Lists
import qualified Util.Pretty as Pretty
import qualified Util.Serialize as Serialize

import qualified Derive.ShowVal as ShowVal
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import qualified Ui.Id as Id
import qualified Ui.ScoreTime as ScoreTime

import           Global
import           Types


-- | An Instrument is identified by a plain string.  This will be looked up in
-- the instrument db to get the backend specific Instrument type as well as
-- the backend itself, but things at the Derive layer and above don't care
-- about all that.
--
-- This should be a valid symbol as defined by 'Ui.Id.valid_symbol'.  This
-- way it can be parsed without quotes.
newtype Instrument = Instrument Text
    deriving (Eq, Ord, Show, Read, Hashable.Hashable, DeepSeq.NFData,
        Serialize.Serialize, String.IsString)

instrument_name :: Instrument -> Text
instrument_name (Instrument s) = s

empty_instrument :: Instrument
empty_instrument = Instrument ""

instance Pretty Instrument where pretty = ShowVal.show_val
instance ShowVal.ShowVal Instrument where
    show_val (Instrument inst) = ShowVal.show_val inst

-- * Control

-- | A control is an abstract parameter that influences derivation.  Some of
-- them affect performance and will be rendered as MIDI controls or note
-- parameters or whatever, while others may affect derivation (e.g. tempo) and
-- won't be seen by the backend at all.
--
-- A Control should be a valid identifier as defined by 'Ui.Id.valid_symbol'.
newtype Control = Control Text
    deriving (Eq, Ord, Read, Show, DeepSeq.NFData, Serialize.Serialize,
        String.IsString, ShowVal.ShowVal)

instance Pretty Control where pretty = ShowVal.show_val

control_name :: Control -> Text
control_name (Control name) = name

-- | Use this constructor when making a Control from user input.  Literals
-- can use the IsString instance.
control :: Text -> Either Text Control
control name
    | Text.null name = Left "empty control name"
    | Id.valid_symbol name = Right $ Control name
    | otherwise = Left $ "invalid characters in control: " <> showt name

unchecked_control :: Text -> Control
unchecked_control = Control

-- * PControl

-- | The pitch control version of 'Control'.  Unlike Control, this is allowed
-- to be null, which is the name of the default pitch signal.
--
-- A PControl should be a valid identifier as defined by 'Ui.Id.valid_symbol',
-- except that its literal tracklang form starts with a @#@, to differentiate
-- from a Control.
newtype PControl = PControl Text
    deriving (Eq, Ord, Read, Show, DeepSeq.NFData, Serialize.Serialize,
        String.IsString)

pcontrol_name :: PControl -> Text
pcontrol_name (PControl name) = name

default_pitch :: PControl
default_pitch = ""

instance Pretty PControl where pretty = ShowVal.show_val
instance ShowVal.ShowVal PControl where
    show_val p@(PControl s)
        -- Invert the # -> default pitch mapping in
        -- Typecheck.lookup_pitch_signal.  This is kind of bogus because the #
        -- syntax belongs to the PControlRef, not the control name itself.
        -- But otherwise I get ''s which look confusing in errors?  I guess
        -- the issue is that there actually is a separate state_pitch, it's
        -- not in state_environ, so '' is not its actual name.
        | p == default_pitch = "#"
        | otherwise = ShowVal.show_val s


-- | Use this constructor when making a PControl from user input.  Literals
-- can use the IsString instance.
pcontrol :: Text -> Either Text PControl
pcontrol name
    | Text.null name || Id.valid_symbol name = Right $ PControl name
    | otherwise = Left $ "invalid characters in pitch control: " <> showt name

unchecked_pcontrol :: Text -> PControl
unchecked_pcontrol = PControl

-- | Parse either a Control or PControl.
parse_generic_control :: Text
    -> Either Text (Either Control PControl)
parse_generic_control name = case Text.uncons name of
    Just ('#', rest) -> Right <$> pcontrol rest
    _ -> Left <$> control name


-- * Type

-- | Tag for the type of the values in a control signal.
-- Untyped goes last because the parser tries them in order.
data Type = Chromatic | Diatonic | Nn | Score | Real | Untyped
    deriving (Eq, Enum, Ord, Bounded, Show)

all_types :: [Type]
all_types = [minBound ..]

type_to_code :: Type -> Text
type_to_code = \case
    Untyped -> ""
    Chromatic -> "c"
    Diatonic -> "d"
    Nn -> "nn"
    Score -> Text.singleton ScoreTime.suffix -- t for time
    Real -> Text.singleton RealTime.suffix -- s for seconds

code_to_type :: Text -> Maybe Type
code_to_type = (`Map.lookup` enum_map)
    where enum_map = Map.fromList $ Lists.keyOn ShowVal.show_val [minBound ..]

instance Semigroup Type where
    Untyped <> typed = typed
    typed <> _ = typed
instance Monoid Type where
    mempty = Untyped
    mappend = (<>)

instance Pretty Type where pretty = showt

instance Serialize.Serialize Type where
    put = Serialize.put . fromEnum
    get = toEnum <$> Serialize.get

instance ShowVal.ShowVal Type where
    show_val = type_to_code

-- This feels clumsy.
-- What I want to express is subtyping.

data TimeT = TReal | TScore
    deriving (Eq, Ord, Show)

instance Pretty TimeT where pretty = showt

data TransposeT = TDiatonic | TChromatic | TNn
    deriving (Eq, Ord, Show)

instance Pretty TransposeT where pretty = showt

-- | Some calls can operate in either RealTime or ScoreTime.
data Duration = RealDuration RealTime.RealTime
    | ScoreDuration ScoreTime.ScoreTime
    deriving (Eq, Show)

time_t :: TimeT -> Type -> Maybe TimeT
time_t deflt = \case
    Untyped -> Just deflt
    Real -> Just TReal
    Score -> Just TScore
    _ -> Nothing

transpose_t :: TransposeT -> Type -> Maybe TransposeT
transpose_t deflt = \case
    Untyped -> Just deflt
    Diatonic -> Just TDiatonic
    Chromatic -> Just TChromatic
    Nn -> Just TNn
    _ -> Nothing

duration :: TimeT -> Typed Signal.Y -> Maybe Duration
duration deflt (Typed typ val) = case typ of
    Score -> Just $ ScoreDuration (ScoreTime.from_double val)
    Real -> Just $ RealDuration (RealTime.seconds val)
    Untyped -> case deflt of
        TScore -> Just $ ScoreDuration (ScoreTime.from_double val)
        TReal -> Just $ RealDuration (RealTime.seconds val)
    _ -> Nothing

-- The transpose equilavents are in Perform.Pitch.Transpose

{-
data Type = Time TimeT | Transpose TransposeT | Untyped
data TimeT = Real | Score
data TransposeT = Chromatic | Diatonic | Nn

data Duration = RealD RealTime | ScoreD ScoreTime

duration :: TimeT ->
duration deflt (Typed typ val) = case typ of
    Time ttype -> to_duration ttype val
    Untyped -> to_duration deflt val

data Category = TimeT TimeT | TransposeT TransposeT | TUntyped
    deriving (Eq, Ord, Show)

category :: Type -> Category
category = \case
    Chromatic -> TTranspose TChromatic
    Diatonic -> TTranspose TDiatonic
    Nn -> TTranspose TNn
    Real -> TTime TReal
    Score -> TTime TScore
    Untyped -> TUntyped
-}

data Typed a = Typed {
    type_of :: !Type
    , val_of :: !a
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance DeepSeq.NFData a => DeepSeq.NFData (Typed a) where
    rnf (Typed typ val) = typ `seq` DeepSeq.rnf val

instance Semigroup a => Semigroup (Typed a) where
    Typed t1 v1 <> Typed t2 v2 = Typed (t1<>t2) (v1<>v2)
instance Monoid a => Monoid (Typed a) where
    mempty = Typed mempty mempty
    mappend = (<>)

instance Pretty a => Pretty (Typed a) where
    format (Typed typ val) =
        Pretty.text (if Text.null c then "" else c <> ":") <> Pretty.format val
        where c = type_to_code typ

instance ShowVal.ShowVal (Typed Control) where
    show_val (Typed typ c) = ShowVal.show_val c <> ":" <> type_to_code typ

instance Serialize.Serialize a => Serialize.Serialize (Typed a) where
    put (Typed a b) = Serialize.put a >> Serialize.put b
    get = Typed <$> Serialize.get <*> Serialize.get

merge_typed :: (a -> a -> a) -> Typed a -> Typed a -> Typed a
merge_typed f (Typed typ1 v1) (Typed typ2 v2) = Typed (typ1<>typ2) (f v1 v2)

untyped :: a -> Typed a
untyped = Typed Untyped

type_to_transpose :: Typed Signal.Y -> Maybe Pitch.Transpose
type_to_transpose (Typed typ val) = case typ of
    Diatonic -> Just $ Pitch.Diatonic val
    Chromatic -> Just $ Pitch.Chromatic val
    Nn -> Just $ Pitch.Nn val
    _ -> Nothing


-- * type aliases

instance ShowVal.ShowVal (Typed Signal.Y) where
    show_val (Typed typ val) = ShowVal.show_val val <> type_to_code typ

-- | This is a snapshot of the control signals at a certain point in time.
-- It's meant for 'Derive.PSignal.PitchConfig', so the values are expected to
-- be transpositions, and hence untyped.
type ControlValMap = Map Control Signal.Y
type TypedControlValMap = Map Control (Typed Signal.Y)

type ControlMap = Map Control TypedSignal
type FunctionMap = Map Control TypedFunction
type Function = RealTime -> Signal.Y

type TypedFunction = Typed (RealTime -> Signal.Y)
type TypedSignal = Typed Signal.Control
