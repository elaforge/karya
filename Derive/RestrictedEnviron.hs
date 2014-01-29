-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- | This is a serializable subset of 'TrackLang.Val' and 'TrackLang.Environ'.
-- It omits pitches, which are code and can't be serialized.
module Derive.RestrictedEnviron where
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Serialize as Serialize
import Util.Serialize (get, put)

import qualified Ui.ScoreTime as ScoreTime
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import Types


newtype Environ = Environ (Map.Map TrackLang.ValName Val)
    deriving (Read, Show, Eq, Monoid.Monoid, Pretty.Pretty, Serialize.Serialize)

make :: [(TrackLang.ValName, Val)] -> Environ
make = Environ . Map.fromList

convert :: Environ -> TrackLang.Environ
convert (Environ env) = BaseTypes.Environ $ Map.map convert_val env

-- * val

data Val =
    VNum !Score.TypedVal
    | VAttributes !Score.Attributes
    | VControl !TrackLang.ValControl
    | VNotePitch !Pitch.Pitch
    | VInstrument !Score.Instrument
    | VSymbol !TrackLang.Symbol
    | VQuoted !Expr
    deriving (Eq, Read, Show)

convert_val :: Val -> TrackLang.Val
convert_val val = case val of
    VNum v -> TrackLang.VNum v
    VAttributes v -> TrackLang.VAttributes v
    VControl v -> TrackLang.VControl v
    VNotePitch v -> TrackLang.VNotePitch v
    VInstrument v -> TrackLang.VInstrument v
    VSymbol v -> TrackLang.VSymbol v
    VQuoted v -> TrackLang.VQuoted $ TrackLang.Quoted $ convert_expr v

instance Pretty.Pretty Val where
    format v = Pretty.format (convert_val v)

-- | This duplicates 'TrackLang.Typecheck', but then so does this whole module.
-- In any case, it's convenient for creaing 'Environ's.
class ToVal a where to_val :: a -> Val

-- ** VNum

instance ToVal Val where to_val = id
instance ToVal Double where to_val = VNum . Score.untyped
instance ToVal Int where to_val = VNum . Score.untyped . fromIntegral
instance ToVal Pitch.Transpose where
    to_val n = case n of
        Pitch.Diatonic n -> VNum $ Score.Typed Score.Diatonic n
        Pitch.Chromatic n -> VNum $ Score.Typed Score.Chromatic n
        Pitch.Nn n -> VNum $ Score.Typed Score.Nn n
instance ToVal ScoreTime where
    to_val = VNum . Score.Typed Score.Score . ScoreTime.to_double
instance ToVal RealTime where
    to_val = VNum . Score.Typed Score.Real . RealTime.to_seconds

-- ** rest

instance ToVal Score.Attributes where to_val = VAttributes
instance ToVal TrackLang.ValControl where to_val = VControl
instance ToVal Pitch.Pitch where to_val = VNotePitch
instance ToVal Score.Instrument where to_val = VInstrument
instance ToVal TrackLang.Symbol where to_val = VSymbol
instance ToVal Text where to_val = VSymbol . TrackLang.Symbol
instance ToVal Expr where to_val = VQuoted

-- * call

type Expr = NonEmpty Call
data Call = Call TrackLang.CallId [Term] deriving (Eq, Read, Show)
data Term = ValCall Call | Literal Val deriving (Eq, Read, Show)

convert_expr :: Expr -> TrackLang.Expr
convert_expr = NonEmpty.map convert_call

convert_call :: Call -> TrackLang.Call
convert_call (Call call_id terms) =
    TrackLang.Call call_id (map convert_term terms)

convert_term :: Term -> TrackLang.Term
convert_term (ValCall call) = TrackLang.ValCall (convert_call call)
convert_term (Literal val) = TrackLang.Literal (convert_val val)

instance Serialize.Serialize Val where
    put val = case val of
        VNum v -> Serialize.put_tag 0 >> put v
        VAttributes v -> Serialize.put_tag 1 >> put v
        VControl v -> Serialize.put_tag 2 >> put v
        VNotePitch v -> Serialize.put_tag 3 >> put v
        VInstrument v -> Serialize.put_tag 4 >> put v
        VSymbol v -> Serialize.put_tag 5 >> put v
        VQuoted v -> Serialize.put_tag 6 >> put v
    get = do
        tag <- Serialize.get_tag
        case tag of
            0 -> VNum <$> get
            1 -> VAttributes <$> get
            2 -> VControl <$> get
            3 -> VNotePitch <$> get
            4 -> VInstrument <$> get
            5 -> VSymbol <$> get
            6 -> VQuoted <$> get
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
