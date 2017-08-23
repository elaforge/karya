-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS_HADDOCK not-home #-}
-- | These are types that belong in "Derive.Score", but are here to avoid
-- circular imports.
module Derive.ScoreTypes where
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
import Types


-- | An Instrument is identified by a plain string.  This will be looked up in
-- the instrument db to get the backend specific Instrument type as well as
-- the backend itself, but things at the Derive layer and above don't care
-- about all that.
--
-- This should be a valid symbol as defined by 'Ui.Id.valid_symbol'.  This
-- it can be parsed without quotes.
newtype Instrument = Instrument Text
    deriving (Eq, Ord, Show, Read, DeepSeq.NFData, Serialize.Serialize)

instrument :: Text -> Instrument
instrument = Instrument

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

-- ** Warp

-- | A tempo warp signal.  The shift and stretch are an optimization hack
-- stolen from nyquist.  The idea is to make composed shifts and stretches more
-- efficient if only the shift and stretch are changed.  The necessary magic
-- is in 'compose_warps'.
--
-- The order of operation is: stretch -> shift -> signal.  That is, if the
-- signal is \"f\": f(t*stretch + shift).
data Warp = Warp {
    warp_signal :: !Signal.Warp
    , warp_shift :: !RealTime
    , warp_stretch :: !RealTime
    } deriving (Eq, Show)

id_warp :: Warp
id_warp = Warp id_warp_signal 0 1

id_warp_signal :: Signal.Warp
id_warp_signal = Signal.signal
    [(0, 0), (RealTime.large, RealTime.to_seconds RealTime.large)]
    -- This could be Signal.empty and 'warp_pos' would still treat it as 1:1,
    -- but then I'd need complicated special cases for 'warp_to_signal' and
    -- 'compose_warps', so don't bother.

instance Pretty Warp where
    format (Warp sig shift stretch) =
        Pretty.record (Pretty.text "Warp"
                Pretty.<+> Pretty.format (shift, stretch))
            [("signal", Pretty.format sig)]

instance DeepSeq.NFData Warp where
    rnf (Warp sig shift stretch) =
        DeepSeq.rnf sig `seq` DeepSeq.rnf shift `seq` DeepSeq.rnf stretch

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

instance Monoid Type where
    mempty = Untyped
    mappend Untyped typed = typed
    mappend typed _ = typed

data Typed a = Typed {
    type_of :: !Type
    , typed_val :: !a
    } deriving (Eq, Ord, Read, Show)

instance DeepSeq.NFData a => DeepSeq.NFData (Typed a) where
    rnf (Typed typ val) = typ `seq` DeepSeq.rnf val

instance Functor Typed where
    fmap f (Typed typ val) = Typed typ (f val)

instance Monoid a => Monoid (Typed a) where
    mempty = Typed mempty mempty
    mappend (Typed t1 v1) (Typed t2 v2) = Typed (t1<>t2) (v1<>v2)

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

type_to_transpose :: TypedVal -> Maybe Pitch.Transpose
type_to_transpose (Typed typ val) = case typ of
    Diatonic -> Just $ Pitch.Diatonic val
    Chromatic -> Just $ Pitch.Chromatic val
    Nn -> Just $ Pitch.Nn val
    _ -> Nothing

-- * ControlMap

type TypedControl = Typed Signal.Control
type TypedVal = Typed Signal.Y

instance ShowVal.ShowVal TypedVal where
    show_val (Typed typ val) = ShowVal.show_val val <> type_to_code typ

-- | This is a snapshot of the control signals at a certain point in time.
-- It's meant for 'PitchConfig', so the values are expected to be
-- transpositions, and hence untyped.
type ControlValMap = Map Control Signal.Y
type TypedControlValMap = Map Control (Typed Signal.Y)
