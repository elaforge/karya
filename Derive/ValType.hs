-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Type descriptions of the 'Val'.
--
-- This is in its own module so "Derive.Deriver.Monad" can import it without
-- importing "Derive.Typecheck".
module Derive.ValType where
import qualified Data.Text as Text

import qualified Util.Pretty as Pretty
import qualified Util.TextUtil as TextUtil
import qualified Ui.Id as Id
import qualified Derive.Score as Score
import Derive.BaseTypes (Val(..))
import Global


data Type =
    TNum NumType NumValue
    | TAttributes | TControlRef | TPControlRef | TPitch | TNotePitch
    | TInstrument
    -- | Text string, with enum values if it's an enum.
    | TSymbol (Maybe [Text]) | TControl | TPControl
    | TNotGiven | TSeparator | TMaybe Type | TEither Type Type | TVal
    -- | A 'VQuoted'.  This has no Typecheck instance so it should never show
    -- up as a call argument.
    | TQuoted
    | TControlFunction
    | TList !Type
    -- | Typecheck instances that don't correspond directly to a Val type
    -- get this, as a plain description.
    | TOther !Text
    deriving (Eq, Ord, Show)

-- | These are kind of muddled.  This is because they were originally just
-- documentation, so the more specific the better, but are also used for
-- typechecking in 'put_val', so the subtype relations need to be respected.
-- But since some are just documentation (e.g. TDefaultReal), they should never
-- show up on the LHS of a put_val typecheck.
data NumType = TUntyped | TInt
    | TTranspose | TDefaultDiatonic | TDefaultChromatic | TNoteNumber
    | TTime | TDefaultReal | TDefaultScore | TRealTime | TScoreTime
    deriving (Eq, Ord, Show)

-- | Numeric subtypes, from most general to most specific.
data NumValue = TAny
    -- | >=0
    | TNatural
    -- | >0
    | TPositive
    -- | 0 <= a <= 1
    | TNormalized
    deriving (Eq, Ord, Show)

-- | This typechecking already exists in the Typecheck instances, but all it
-- can do is go from a Val to a @Typecheck a => Maybe a@.  So I can't reuse it
-- to check a type against a type, so it has to be duplicated, similar to how
-- 'type_of' can't reuse 'to_type'.
--
-- The result is I have redundant functions like 'subtypes_of' and 'type_of'
-- and 'to_num_type', and a mistake or inconsistency with 'to_type' or 'to_val'
-- will cause typechecking to fail in some subtle case.  Fortunately there are
-- relatively few types and hopefully won't be many more, and it only affects
-- 'put_val'.  It could all do with a cleanup.  I'm sure there's a right way
-- to do this sort of thing.
types_match :: Type -> Type -> Bool
types_match t1 t2 = case (t1, t2) of
    (TNum n1 v1, TNum n2 v2) -> num_types_match n1 n2 && num_vals_match v1 v2
    (TMaybe t1, TMaybe t2) -> types_match t1 t2
    (TEither t1 u1, TEither t2 u2) -> types_match t1 t2 && types_match u1 u2
    (TList t1, TList t2) -> types_match t1 t2
    (t1, t2) -> t1 == t2
    where
    num_types_match n1 n2 = n2 `elem` subtypes_of n1
    num_vals_match v1 v2 = v1 <= v2

-- | Nothing if the type of the rhs matches the lhs, otherwise the expected
-- type.
val_types_match :: Val -> Val -> Maybe Type
val_types_match lhs rhs
    | types_match expected (type_of rhs) = Nothing
    | otherwise = Just expected
    where expected = infer_type_of False lhs

subtypes_of :: NumType -> [NumType]
subtypes_of n
    | n `elem` [TTime, TDefaultReal, TDefaultScore] =
        [TTime, TDefaultReal, TDefaultScore, TRealTime, TScoreTime]
    | n `elem` transpose = transpose
    | otherwise = [n]
    where
    transpose = [TTranspose, TDefaultDiatonic, TDefaultChromatic, TNoteNumber]

instance Pretty.Pretty Type where
    pretty (TMaybe typ) = "Maybe " <> pretty typ
    pretty (TEither a b) = pretty a <> " or " <> pretty b
    pretty (TNum typ val) = append_parens "Num" $
        TextUtil.joinWith ", " (pretty typ) (pretty val)
    pretty (TSymbol enums) =
        append_parens "Symbol" $ maybe "" Text.unwords enums
    -- There is no corresponding Val type for these, so I might as well be
    -- clear about what they mean.
    pretty TControl = append_parens "Control" Id.valid_description
    pretty TPControl = append_parens "PControl" ("#" <> Id.valid_description)
    pretty (TList typ) = "list of " <> pretty typ
    pretty (TOther text) = text
    pretty typ = Text.drop 1 (showt typ)

append_parens :: Text -> Text -> Text
append_parens name desc
    | Text.null desc = name
    | otherwise = name <> " (" <> desc <> ")"

instance Pretty.Pretty NumType where
    pretty t = case t of
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

instance Pretty.Pretty NumValue where
    pretty t = case t of
        TAny -> ""
        TNatural -> ">=0"
        TPositive -> ">0"
        TNormalized -> "0 <= x <= 1"

type_of :: Val -> Type
type_of = infer_type_of True

infer_type_of :: Bool -- ^ If True, infer the most specific type possible.
    -- Otherwise, infer a general type.  This is because if 'put_val' gets a
    -- 1 it doesn't mean it's intended to be a TPositive.
    -> Val -> Type
infer_type_of specific val = case val of
    VNum (Score.Typed typ val) -> TNum (to_num_type typ) $ if specific
        then (if val > 0 then TPositive
            else if val >= 0 then TNatural else TAny)
        else TAny
    VAttributes {} -> TAttributes
    VControlRef {} -> TControlRef
    VPControlRef {} -> TPControlRef
    VPitch {} -> TPitch
    VNotePitch {} -> TNotePitch
    VInstrument {} -> TInstrument
    VSymbol {} -> TSymbol Nothing
    VQuoted {} -> TQuoted
    VControlFunction {} -> TControlFunction
    VNotGiven -> TNotGiven
    VSeparator -> TSeparator
    VList {} -> TList TVal

to_num_type :: Score.Type -> NumType
to_num_type typ = case typ of
    Score.Untyped -> TUntyped
    Score.Real -> TRealTime
    Score.Score -> TScoreTime
    Score.Diatonic -> TTranspose
    Score.Chromatic -> TTranspose
    Score.Nn -> TNoteNumber
