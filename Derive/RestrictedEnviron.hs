-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DefaultSignatures #-}
-- | This is a serializable subset of 'DeriveT.Val' and 'DeriveT.Environ'.
-- It omits pitches, which are code and can't be serialized.
module Derive.RestrictedEnviron where
import           Prelude hiding (lookup, null)
import qualified Data.Map as Map

import qualified Util.Pretty as Pretty
import qualified Util.Serialize as Serialize
import           Util.Serialize (get, put)

import qualified Derive.Attrs as Attrs
import qualified Derive.DeriveT as DeriveT
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Expr as Expr
import qualified Derive.PSignal as PSignal
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import qualified Ui.ScoreTime as ScoreTime

import           Global
import           Types


-- * Environ

newtype Environ = Environ (Map EnvKey.Key Val)
    deriving (Show, Eq, Semigroup, Monoid, Serialize.Serialize)

-- Environ keys are always Text, and it's annoying to have quotes on them.
instance Pretty Environ where
    format (Environ env) = Pretty.formatMap
        . map (bimap Pretty.text Pretty.format) . Map.toList $ env

from_list :: [(EnvKey.Key, Val)] -> Environ
from_list = Environ . Map.fromList

convert :: Environ -> DeriveT.Environ
convert (Environ env) = DeriveT.Environ $ convert_val <$> env

lookup :: EnvKey.Key -> Environ -> Maybe Val
lookup key (Environ env) = Map.lookup key env

null :: Environ -> Bool
null (Environ env) = Map.null env

-- * val

-- | This is like 'DeriveT.Val', except missing fields that can't be
-- serialized, or require Deriver and hence couldn't go in a module below
-- Deriver without incurring a circular dependency.
--
-- Namely: 'DeriveT.VPitch', 'DeriveT.VControlFunction'.
-- NOTE [val-and-minival].
data Val =
    VNum !(ScoreT.Typed Signal.Y)
    | VAttributes !Attrs.Attributes
    | VControlRef !DeriveT.ControlRef
    | VConstantPitch !ConstantPitch
    | VNotePitch !Pitch.Pitch
    | VStr !Expr.Str
    | VQuoted !Expr
    | VList ![Val]
    deriving (Eq, Show)

convert_val :: Val -> DeriveT.Val
convert_val val = case val of
    VNum v -> DeriveT.VNum v
    VAttributes v -> DeriveT.VAttributes v
    VControlRef v -> DeriveT.VControlRef v
    VConstantPitch (ConstantPitch scale_id note nn) ->
        DeriveT.VPitch $ PSignal.constant_pitch scale_id note nn
    VNotePitch v -> DeriveT.VNotePitch v
    VStr v -> DeriveT.VStr v
    VQuoted v -> DeriveT.VQuoted $ DeriveT.Quoted $
        Expr.map_literals convert_val v
    VList v -> DeriveT.VList $ map convert_val v

instance Pretty Val where format = Pretty.format . convert_val

-- | This duplicates 'TrackLang.Typecheck', but then so does this whole module.
-- In any case, it's convenient for creaing 'Environ's.
--
-- TODO But I wish I could reuse Typecheck.ToVal, otherwise I have to add an
-- extra instance declaration for each type.
class ToVal a where
    to_val :: a -> Val
    default to_val :: ShowVal.ShowVal a => a -> Val
    to_val = VStr . Expr.Str . ShowVal.show_val

-- ** VNum

instance ToVal Val where to_val = id
instance ToVal Double where to_val = VNum . ScoreT.untyped
instance ToVal Int where to_val = VNum . ScoreT.untyped . fromIntegral
instance ToVal Pitch.NoteNumber where
    to_val = VNum . ScoreT.Typed ScoreT.Nn . Pitch.nn_to_double
instance ToVal Pitch.Transpose where
    to_val n = case n of
        Pitch.Diatonic n -> VNum $ ScoreT.Typed ScoreT.Diatonic n
        Pitch.Chromatic n -> VNum $ ScoreT.Typed ScoreT.Chromatic n
        Pitch.Nn n -> VNum $ ScoreT.Typed ScoreT.Nn n
instance ToVal ScoreTime where
    to_val = VNum . ScoreT.Typed ScoreT.Score . ScoreTime.to_double
instance ToVal RealTime where
    to_val = VNum . ScoreT.Typed ScoreT.Real . RealTime.to_seconds

instance ToVal a => ToVal [a] where to_val = VList . map to_val

-- ** rest

instance ToVal Attrs.Attributes where to_val = VAttributes
instance ToVal DeriveT.ControlRef where to_val = VControlRef
instance ToVal Pitch.Pitch where to_val = VNotePitch
instance ToVal ScoreT.Instrument where
    to_val (ScoreT.Instrument a) = VStr (Expr.Str a)
instance ToVal Expr.Str where to_val = VStr
instance ToVal Text where to_val = VStr . Expr.Str
instance ToVal Expr where to_val = VQuoted

data ConstantPitch = ConstantPitch !Pitch.ScaleId !Pitch.Note !Pitch.NoteNumber
    deriving (Show, Eq)

instance ToVal ConstantPitch where to_val = VConstantPitch

-- * call

type Expr = Expr.Expr Val
type Call = Expr.Call Val
type Term = Expr.Term Val

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
        VConstantPitch (ConstantPitch scale_id note nn) -> Serialize.put_tag 8
            >> put scale_id >> put note >> put nn
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
            8 -> fmap VConstantPitch $ ConstantPitch <$> get <*> get <*> get
            _ -> Serialize.bad_tag "RestrictedEnviron.Val" tag

instance Serialize.Serialize Call where
    put (Expr.Call a b) = put a >> put b
    get = Expr.Call <$> get <*> get

instance Serialize.Serialize Term where
    put term = case term of
        Expr.ValCall v -> Serialize.put_tag 0 >> put v
        Expr.Literal v -> Serialize.put_tag 1 >> put v
    get = Serialize.get_tag >>= \case
        0 -> Expr.ValCall <$> get
        1 -> Expr.Literal <$> get
        n -> Serialize.bad_tag "RestrictedEnviron.Term" n
