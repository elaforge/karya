-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Type descriptions of the 'Val'.
--
-- This is in its own module so "Derive.Deriver.Monad" can import it without
-- importing "Derive.Typecheck".
module Derive.ValType (
    Type(..)
    , NumType(..)
    , NumValue(..)
    , general_type_of, specific_type_of
) where
import qualified Data.List as List
import qualified Data.Text as Text

import qualified Util.Texts as Texts
import           Derive.DeriveT (Val(..))
import qualified Derive.ScoreT as ScoreT
import qualified Ui.Id as Id

import           Global


data Type =
    -- | This is the \"any\" type.
    TVal
    | TNum NumType NumValue
    | TSignal NumType
    | TPSignal
    | TAttributes | TControlRef | TPControlRef | TPitch | TNotePitch
    -- | Text string, with enum values if it's an enum.
    | TStr (Maybe [Text]) | TControl | TPControl
    | TNotGiven | TSeparator | TMaybe Type | TEither Type Type
    -- | Two types in sequence.  This has no corresponding Typecheck instance
    -- since it doesn't correspond to a single Val, but is used by "Derive.Sig"
    -- for documentation.
    | TPair Type Type
    -- | A 'VQuoted'.  This has no Typecheck instance so it should never show
    -- up as a call argument.
    | TQuoted
    | TControlFunction
    | TList !Type
    | TDeriver !Text
    -- | Typecheck instances that don't correspond directly to a Val type
    -- get this, as a plain description.
    | TOther !Text
    deriving (Eq, Ord, Show)

-- | Get the intersection of the types.  If there's no intersection, it winds
-- up at TVal.  This is for 'infer_type_of'
instance Semigroup Type where
    TNum nt1 vt1 <> TNum nt2 vt2 = TNum (nt1<>nt2) (vt1<>vt2)
    TSignal t1 <> TSignal t2 = TSignal (t1<>t2)
    TMaybe t1 <> TMaybe t2 = TMaybe (t1 <> t2)
    TEither t1 u1 <> TEither t2 u2 = TEither (t1<>t2) (u1<>u2)
    TPair t1 u1 <> TPair t2 u2 = TPair (t1<>t2) (u1<>u2)
    TList t1 <> TList t2 = TList (t1<>t2)
    t1 <> t2
        | t1 == t2 = t1
        | otherwise = TVal

-- | Some of these are subtypes of others (TTranspose includes
-- TDefaultDiatonic), but since they're just documentation, it shouldn't
-- matter.
data NumType = TUntyped | TInt
    | TTranspose | TDefaultDiatonic | TDefaultChromatic | TNoteNumber
    | TTime | TDefaultReal | TDefaultScore | TRealTime | TScoreTime
    deriving (Eq, Ord, Show)

instance Semigroup NumType where
    t1 <> t2
        | t1 == t2 = t1
        | otherwise = TUntyped

-- | Numeric subtypes.
data NumValue =
    TAny
    -- | >=0
    | TNonNegative
    -- | >0
    | TPositive
    -- | 0 <= a <= 1
    | TNormalized
    -- | -1 <= a <= 1
    | TNormalizedBipolar
    deriving (Eq, Ord, Show)

instance Semigroup NumValue where
    t1 <> t2
        | t1 == t2 = t1
        | otherwise = TAny

instance Pretty Type where
    pretty = \case
        TMaybe typ -> "Maybe " <> pretty typ
        TEither a b -> pretty a <> " or " <> pretty b
        TPair a b -> "(" <> pretty a <> ", " <> pretty b <> ")"
        TNum typ val -> append_parens "Num" $
            Texts.join2 ", " (pretty typ) (pretty val)
        TSignal typ -> append_parens "Signal" (pretty typ)
        TStr enums -> append_parens "Str" $ maybe "" Text.unwords enums
        -- There is no corresponding Val type for these, so I might as well be
        -- clear about what they mean.
        TControl -> append_parens "Control" Id.symbol_description
        TPControl -> append_parens "PControl" ("#" <> Id.symbol_description)
        TList typ -> "list of " <> pretty typ
        TOther text -> text
        TNotGiven -> "_"
        TDeriver name -> name <> " deriver"
        typ -> Text.drop 1 (showt typ)

append_parens :: Text -> Text -> Text
append_parens name desc
    | Text.null desc = name
    | otherwise = name <> " (" <> desc <> ")"

instance Pretty NumType where
    pretty = \case
        TUntyped -> ""
        TInt -> "integral"
        TTranspose -> "Transposition"
        TDefaultDiatonic -> "Transposition, default diatonic"
        TDefaultChromatic -> "Transposition, default chromatic"
        TTime -> "Time"
        TDefaultReal -> "Time, default real"
        TDefaultScore -> "Time, default score"
        TRealTime -> "RealTime"
        TScoreTime -> "ScoreTime"
        TNoteNumber -> "NN"

instance Pretty NumValue where
    pretty = \case
        TAny -> ""
        TNonNegative -> ">=0"
        TPositive -> ">0"
        TNormalized -> "0 <= x <= 1"
        TNormalizedBipolar -> "-1 <= x <= 1"

-- | Infer the most specific type possible, looking at the value inside.  This
-- is for documentation (e.g. type error messages) for values.
specific_type_of :: Val -> Type
specific_type_of = infer_type_of True

-- | Infer a general type.  This is also for type errors, but for env type
-- check errors, which use 'DeriveT.types_equal', which doesn't check the
-- value.
general_type_of :: Val -> Type
general_type_of = infer_type_of False

infer_type_of :: Bool -> Val -> Type
infer_type_of specific = \case
    VNum (ScoreT.Typed typ val) -> TNum (to_num_type typ) $ if specific
        then (if val > 0 then TPositive
            else if val >= 0 then TNonNegative else TAny)
        else TAny
    VSignal sig -> TSignal (to_num_type (ScoreT.type_of sig))
    VPSignal {} -> TPSignal
    VAttributes {} -> TAttributes
    VControlRef {} -> TControlRef
    VPControlRef {} -> TPControlRef
    VPitch {} -> TPitch
    VNotePitch {} -> TNotePitch
    VStr {} -> TStr Nothing
    VQuoted {} -> TQuoted
    VControlFunction {} -> TControlFunction
    VNotGiven -> TNotGiven
    VSeparator -> TSeparator
    VList [] -> TList TVal
    -- Could use NonEmpty and sconcat, but too much bother.
    VList vs -> TList (List.foldl1' (<>) (map (infer_type_of specific) vs))

to_num_type :: ScoreT.Type -> NumType
to_num_type = \case
    ScoreT.Untyped -> TUntyped
    ScoreT.Real -> TRealTime
    ScoreT.Score -> TScoreTime
    ScoreT.Diatonic -> TTranspose
    ScoreT.Chromatic -> TTranspose
    ScoreT.Nn -> TNoteNumber
