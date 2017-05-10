-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
-- | This is a serializable subset of 'BaseTypes.Val' and 'BaseTypes.Environ'.
-- It omits pitches, which are code and can't be serialized.
module Derive.RestrictedEnviron where
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map

import qualified Util.Pretty as Pretty
import qualified Util.Serialize as Serialize
import Util.Serialize (get, put)

import qualified Ui.ScoreTime as ScoreTime
import qualified Derive.Attrs as Attrs
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Expr as Expr
import qualified Derive.ScoreTypes as ScoreTypes
import qualified Derive.ShowVal as ShowVal

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import Global
import Types


newtype Environ = Environ (Map BaseTypes.Key Val)
    deriving (Read, Show, Eq, Monoid, Pretty.Pretty, Serialize.Serialize)

make :: [(BaseTypes.Key, Val)] -> Environ
make = Environ . Map.fromList

convert :: Environ -> BaseTypes.Environ
convert (Environ env) = BaseTypes.Environ $ Map.map convert_val env

-- * val

data Val =
    VNum !ScoreTypes.TypedVal
    | VAttributes !Attrs.Attributes
    | VControlRef !BaseTypes.ControlRef
    | VNotePitch !Pitch.Pitch
    | VStr !Expr.Str
    | VQuoted !Expr
    | VList ![Val]
    deriving (Eq, Read, Show)

convert_val :: Val -> BaseTypes.Val
convert_val val = case val of
    VNum v -> BaseTypes.VNum v
    VAttributes v -> BaseTypes.VAttributes v
    VControlRef v -> BaseTypes.VControlRef v
    VNotePitch v -> BaseTypes.VNotePitch v
    VStr v -> BaseTypes.VStr v
    VQuoted v -> BaseTypes.VQuoted $ BaseTypes.Quoted $ convert_expr v
    VList v -> BaseTypes.VList $ map convert_val v

instance Pretty.Pretty Val where
    format v = Pretty.format (convert_val v)

-- | This duplicates 'TrackLang.Typecheck', but then so does this whole module.
-- In any case, it's convenient for creaing 'Environ's.
--
-- TODO But I wish I could reuse Typecheck.ToVal TypecheckSymbol TovAl,
-- otherwise I have to add an extra instance declaration for each type.
class ToVal a where
    to_val :: a -> Val
    default to_val :: ShowVal.ShowVal a => a -> Val
    to_val = VStr . Expr.Str . ShowVal.show_val

-- ** VNum

instance ToVal Val where to_val = id
instance ToVal Double where to_val = VNum . ScoreTypes.untyped
instance ToVal Int where to_val = VNum . ScoreTypes.untyped . fromIntegral
instance ToVal Pitch.NoteNumber where
    to_val = VNum . ScoreTypes.Typed ScoreTypes.Nn . Pitch.nn_to_double
instance ToVal Pitch.Transpose where
    to_val n = case n of
        Pitch.Diatonic n -> VNum $ ScoreTypes.Typed ScoreTypes.Diatonic n
        Pitch.Chromatic n -> VNum $ ScoreTypes.Typed ScoreTypes.Chromatic n
        Pitch.Nn n -> VNum $ ScoreTypes.Typed ScoreTypes.Nn n
instance ToVal ScoreTime where
    to_val = VNum . ScoreTypes.Typed ScoreTypes.Score . ScoreTime.to_double
instance ToVal RealTime where
    to_val = VNum . ScoreTypes.Typed ScoreTypes.Real . RealTime.to_seconds

-- ** rest

instance ToVal Attrs.Attributes where to_val = VAttributes
instance ToVal BaseTypes.ControlRef where to_val = VControlRef
instance ToVal Pitch.Pitch where to_val = VNotePitch
instance ToVal ScoreTypes.Instrument where
    to_val (ScoreTypes.Instrument a) = VStr (Expr.Str a)
instance ToVal Expr.Str where to_val = VStr
instance ToVal Text where to_val = VStr . Expr.Str
instance ToVal Expr where to_val = VQuoted

-- * call

type Expr = NonEmpty Call
data Call = Call Expr.Symbol [Term] deriving (Eq, Read, Show)
data Term = ValCall Call | Literal Val deriving (Eq, Read, Show)

convert_expr :: Expr -> BaseTypes.Expr
convert_expr = NonEmpty.map convert_call

convert_call :: Call -> BaseTypes.Call
convert_call (Call sym terms) = BaseTypes.Call sym (map convert_term terms)

convert_term :: Term -> BaseTypes.Term
convert_term (ValCall call) = BaseTypes.ValCall (convert_call call)
convert_term (Literal val) = BaseTypes.Literal (convert_val val)

instance Serialize.Serialize Val where
    put val = case val of
        VNum v -> Serialize.put_tag 0 >> put v
        VAttributes v -> Serialize.put_tag 1 >> put v
        VControlRef v -> Serialize.put_tag 2 >> put v
        VNotePitch v -> Serialize.put_tag 3 >> put v
        -- tag 4 was VInstrument
        VStr v -> Serialize.put_tag 5 >> put v
        VQuoted v -> Serialize.put_tag 6 >> put v
        VList v -> Serialize.put_tag 7 >> put v
    get = do
        tag <- Serialize.get_tag
        case tag of
            0 -> VNum <$> get
            1 -> VAttributes <$> get
            2 -> VControlRef <$> get
            3 -> VNotePitch <$> get
            4 -> VStr <$> get -- tag 4 was VInstrument
            5 -> VStr <$> get
            6 -> VQuoted <$> get
            7 -> VList <$> get
            _ -> Serialize.bad_tag "RestrictedEnviron.Val" tag

instance Serialize.Serialize Call where
    put (Call a b) = put a >> put b
    get = Call <$> get <*> get

instance Serialize.Serialize Term where
    put term = case term of
        ValCall v -> Serialize.put_tag 0 >> put v
        Literal v -> Serialize.put_tag 1 >> put v
    get = Serialize.get_tag >>= \x -> case x of
        0 -> ValCall <$> get
        1 -> Literal <$> get
        n -> Serialize.bad_tag "RestrictedEnviron.Term" n
