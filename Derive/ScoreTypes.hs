-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK not-home #-}
-- | These are types that belong in "Derive.Score", but are here to avoid
-- circular imports.
module Derive.ScoreTypes where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set
import qualified Data.String as String
import qualified Data.Text as Text

import qualified Util.Pretty as Pretty
import qualified Util.Serialize as Serialize
import qualified Ui.ScoreTime as ScoreTime
import qualified Derive.ShowVal as ShowVal
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Global
import Types


-- | An Instrument is identified by a plain string.  This will be looked up in
-- the instrument db to get the backend specific Instrument type as well as
-- the backend itself, but things at the Derive layer and above don't care
-- about all that.
newtype Instrument = Instrument Text
    deriving (Eq, Ord, Show, Read, DeepSeq.NFData, Serialize.Serialize)

instance Pretty.Pretty Instrument where pretty = ShowVal.show_val
instance ShowVal.ShowVal Instrument where
    show_val (Instrument inst) = Text.cons '>' inst

-- | A control is an abstract parameter that influences derivation.  Some of
-- them affect performance and will be rendered as MIDI controls or note
-- parameters or whatever, while others may affect derivation (e.g. tempo) and
-- won't be seen by the backend at all.
--
-- A Control should be a valid identifier as defined by 'Ui.Id.valid'.
newtype Control = Control Text
    deriving (Eq, Ord, Read, Show, DeepSeq.NFData, Serialize.Serialize,
        String.IsString)

control_name :: Control -> Text
control_name (Control name) = name

instance Pretty.Pretty Control where pretty = Text.cons '%' . ShowVal.show_val
instance ShowVal.ShowVal Control where show_val (Control c) = c

-- | The pitch control version of 'Control'.  Unlike Control, this is allowed
-- to be null, which is the name of the default pitch signal.
--
-- A PControl should be a valid identifier as defined by 'Ui.Id.valid', except
-- that its literal tracklang form starts with a @#@, to differentiate from
-- a Control.
newtype PControl = PControl Text
    deriving (Eq, Ord, Read, Show, DeepSeq.NFData, Serialize.Serialize,
        String.IsString)

pcontrol_name :: PControl -> Text
pcontrol_name (PControl name) = name

instance Pretty.Pretty PControl where pretty = ShowVal.show_val
instance ShowVal.ShowVal PControl where
    show_val (PControl c) = Text.cons '#' c

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

instance Pretty.Pretty Warp where
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

instance Pretty.Pretty Type where pretty = showt

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

data Typed a = Typed {
    type_of :: !Type
    , typed_val :: !a
    } deriving (Eq, Ord, Read, Show)

instance DeepSeq.NFData a => DeepSeq.NFData (Typed a) where
    rnf (Typed typ val) = typ `seq` DeepSeq.rnf val

instance Functor Typed where
    fmap f (Typed typ val) = Typed typ (f val)

instance Monoid.Monoid a => Monoid.Monoid (Typed a) where
    mempty = Typed mempty mempty
    mappend (Typed t1 v1) (Typed t2 v2) = Typed (t1<>t2) (v1<>v2)

instance Pretty.Pretty a => Pretty.Pretty (Typed a) where
    format (Typed typ val) =
        Pretty.text (if Text.null c then "" else c <> ":") <> Pretty.format val
        where c = type_to_code typ

instance Serialize.Serialize a => Serialize.Serialize (Typed a) where
    put (Typed a b) = Serialize.put a >> Serialize.put b
    get = Typed <$> Serialize.get <*> Serialize.get

merge_typed :: (a -> a -> a) -> Typed a -> Typed a -> Typed a
merge_typed f (Typed typ1 v1) (Typed typ2 v2) = Typed (typ1<>typ2) (f v1 v2)

untyped :: a -> Typed a
untyped = Typed Untyped

-- * ControlMap

type TypedControl = Typed Signal.Control
type TypedVal = Typed Signal.Y

instance ShowVal.ShowVal TypedVal where
    show_val (Typed typ val) = ShowVal.show_val val <> type_to_code typ

-- | This is a snapshot of the control signals at a certain point in time.
-- It's meant for 'PitchConfig', so the values are expected to be
-- transpositions, and hence untyped.
type ControlValMap = Map.Map Control Signal.Y
type TypedControlValMap = Map.Map Control (Typed Signal.Y)

-- ** Attributes

-- | Instruments can have a set of attributes along with them.  These are
-- propagated dynamically down the derivation stack.  They function like
-- arguments to an instrument, and will typically select an articulation, or
-- a drum from a drumset, or something like that.
type Attribute = Text
newtype Attributes = Attributes (Set.Set Attribute)
    deriving (Monoid.Monoid, Eq, Ord, Read, Show, Serialize.Serialize,
        DeepSeq.NFData)

instance Pretty.Pretty Attributes where pretty = ShowVal.show_val
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
