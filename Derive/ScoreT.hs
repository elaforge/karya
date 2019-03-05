-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# OPTIONS_HADDOCK not-home #-}
-- | Low-dependency basic types for derivation.
module Derive.ScoreT where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.String as String
import qualified Data.Text as Text

import qualified Util.Pretty as Pretty
import qualified Util.Serialize as Serialize
import qualified Ui.ScoreTime as ScoreTime
import qualified Derive.ShowVal as ShowVal
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import Global


-- | An Instrument is identified by a plain string.  This will be looked up in
-- the instrument db to get the backend specific Instrument type as well as
-- the backend itself, but things at the Derive layer and above don't care
-- about all that.
--
-- This should be a valid symbol as defined by 'Ui.Id.valid_symbol'.  This
-- way it can be parsed without quotes.
newtype Instrument = Instrument Text
    deriving (Eq, Ord, Show, Read, DeepSeq.NFData, Serialize.Serialize,
        String.IsString)

instrument_name :: Instrument -> Text
instrument_name (Instrument s) = s

empty_instrument :: Instrument
empty_instrument = Instrument ""

instance Pretty Instrument where pretty = ShowVal.show_val
instance ShowVal.ShowVal Instrument where
    show_val (Instrument inst) = ShowVal.show_val inst

-- | A control is an abstract parameter that influences derivation.  Some of
-- them affect performance and will be rendered as MIDI controls or note
-- parameters or whatever, while others may affect derivation (e.g. tempo) and
-- won't be seen by the backend at all.
--
-- A Control should be a valid identifier as defined by 'Ui.Id.valid_symbol'.
newtype Control = Control Text
    deriving (Eq, Ord, Read, Show, DeepSeq.NFData, Serialize.Serialize,
        String.IsString)

control_name :: Control -> Text
control_name (Control name) = name

instance Pretty Control where pretty = ShowVal.show_val
instance ShowVal.ShowVal Control where show_val (Control c) = Text.cons '%' c

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

instance Pretty PControl where pretty = ShowVal.show_val
instance ShowVal.ShowVal PControl where show_val (PControl c) = Text.cons '#' c


-- ** Type

-- | Tag for the type of the values in a control signal.
data Type = Untyped | Chromatic | Diatonic | Nn | Score | Real
    deriving (Eq, Enum, Ord, Read, Show)

instance Pretty Type where pretty = showt

instance Serialize.Serialize Type where
    put = Serialize.put . fromEnum
    get = toEnum <$> Serialize.get

all_types :: [Type]
all_types = [Chromatic, Diatonic, Nn, Score, Real, Untyped]
    -- Untyped goes last because the parser tries them in order.

type_to_code :: Type -> Text
type_to_code typ = case typ of
    Untyped -> ""
    Chromatic -> "c"
    Diatonic -> "d"
    Nn -> "nn"
    Score -> Text.singleton ScoreTime.suffix -- t for time
    Real -> Text.singleton RealTime.suffix -- s for seconds

code_to_type :: Text -> Maybe Type
code_to_type s = case s of
    "c" -> Just Chromatic
    "d" -> Just Diatonic
    "nn" -> Just Nn
    "t" -> Just Score
    "s" -> Just Real
    "" -> Just Untyped
    _ -> Nothing

instance Semigroup Type where
    Untyped <> typed = typed
    typed <> _ = typed
instance Monoid Type where
    mempty = Untyped
    mappend = (<>)

data Typed a = Typed {
    type_of :: !Type
    , typed_val :: !a
    } deriving (Eq, Ord, Read, Show)

instance DeepSeq.NFData a => DeepSeq.NFData (Typed a) where
    rnf (Typed typ val) = typ `seq` DeepSeq.rnf val

instance Functor Typed where
    fmap f (Typed typ val) = Typed typ (f val)

instance Semigroup a => Semigroup (Typed a) where
    Typed t1 v1 <> Typed t2 v2 = Typed (t1<>t2) (v1<>v2)
instance (Semigroup a, Monoid a) => Monoid (Typed a) where
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

-- * ControlMap

instance ShowVal.ShowVal (Typed Signal.Y) where
    show_val (Typed typ val) = ShowVal.show_val val <> type_to_code typ

-- | This is a snapshot of the control signals at a certain point in time.
-- It's meant for 'PitchConfig', so the values are expected to be
-- transpositions, and hence untyped.
type ControlValMap = Map Control Signal.Y
type TypedControlValMap = Map Control (Typed Signal.Y)
