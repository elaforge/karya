-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DefaultSignatures #-}
-- | This is a serializable subset of 'DeriveT.Val' and 'DeriveT.Environ'.
-- It omits pitches, which are code and can't be serialized.
module Derive.REnv (
    Environ(..)
    , toMap, fromList, convert, lookup, null
    -- * val
    , promote, demote
    , Val(..)
    , ConstantPitch(..)
    , ToVal(..)
    , Expr, Call, Term
) where
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

toMap :: Environ -> Map EnvKey.Key Val
toMap (Environ m) = m

fromList :: [(EnvKey.Key, Val)] -> Environ
fromList = Environ . Map.fromList

convert :: Environ -> DeriveT.Environ
convert (Environ env) = DeriveT.Environ $ promote <$> env

lookup :: EnvKey.Key -> Environ -> Maybe Val
lookup key (Environ env) = Map.lookup key env

null :: Environ -> Bool
null (Environ env) = Map.null env

-- * val

-- | This is like 'DeriveT.Val', except missing fields that can't be
-- serialized, or require Deriver and hence couldn't go in a module below
-- Deriver without incurring a circular dependency.
--
-- Namely: 'DeriveT.VPSignal', 'DeriveT.VCFunction'.
-- NOTE [val-and-minival].
data Val
    = VNum !(ScoreT.Typed Signal.Y)
    | VAttributes !Attrs.Attributes
    | VControlRef !DeriveT.ControlRef
    | VConstantPitch !ConstantPitch
    | VNotePitch !Pitch.Pitch
    | VStr !Expr.Str
    | VQuoted !Expr
    | VList ![Val]
    deriving (Eq, Show)

promote :: Val -> DeriveT.Val
promote = \case
    VNum a -> DeriveT.VSignal $ Signal.constant <$> a
    VAttributes a -> DeriveT.VAttributes a
    VControlRef a -> DeriveT.VControlRef a
    VConstantPitch (ConstantPitch scaleId note nn) ->
        DeriveT.VPSignal $ PSignal.constant $
            PSignal.constantPitch scaleId note nn
    VNotePitch a -> DeriveT.VNotePitch a
    VStr a -> DeriveT.VStr a
    VQuoted a -> DeriveT.VQuoted $ DeriveT.Quoted $ Expr.mapLiterals promote a
    VList a -> DeriveT.VList $ map promote a

demote :: DeriveT.Val -> Maybe Val
demote = \case
    DeriveT.VSignal (ScoreT.Typed ty sig)
        | Just a <- Signal.constantVal sig -> Just $ VNum (ScoreT.Typed ty a)
        | otherwise -> Nothing
    DeriveT.VPSignal _ -> Nothing
    DeriveT.VAttributes a -> Just $ VAttributes a
    DeriveT.VControlRef a -> Just $ VControlRef a
    DeriveT.VPControlRef _ -> Nothing
    DeriveT.VNotePitch a -> Just $ VNotePitch a
    DeriveT.VStr a -> Just $ VStr a
    DeriveT.VQuoted (DeriveT.Quoted a) ->
        VQuoted <$> traverse (traverse demote) a
    DeriveT.VCFunction _ -> Nothing
    DeriveT.VPFunction _ -> Nothing
    DeriveT.VNotGiven -> Nothing
    DeriveT.VSeparator -> Nothing
    DeriveT.VList as -> VList <$> mapM demote as

instance Pretty Val where format = Pretty.format . promote
instance ShowVal.ShowVal Val where showVal = ShowVal.showVal . promote

-- | This duplicates 'TrackLang.Typecheck', but then so does this whole module.
-- In any case, it's convenient for creaing 'Environ's.
--
-- TODO But I wish I could reuse Typecheck.ToVal, otherwise I have to add an
-- extra instance declaration for each type.
class ToVal a where
    toVal :: a -> Val
    default toVal :: ShowVal.ShowVal a => a -> Val
    toVal = VStr . Expr.Str . ShowVal.showVal

-- ** VNum

instance ToVal Val where toVal = id
instance ToVal Double where toVal = VNum . ScoreT.untyped
instance ToVal Int where toVal = VNum . ScoreT.untyped . fromIntegral
instance ToVal Pitch.NoteNumber where
    toVal = VNum . ScoreT.Typed ScoreT.Nn . Pitch.nnToDouble
instance ToVal Pitch.Transpose where
    toVal n = case n of
        Pitch.Diatonic n -> VNum $ ScoreT.Typed ScoreT.Diatonic n
        Pitch.Chromatic n -> VNum $ ScoreT.Typed ScoreT.Chromatic n
        Pitch.Nn n -> VNum $ ScoreT.Typed ScoreT.Nn n
instance ToVal ScoreTime where
    toVal = VNum . ScoreT.Typed ScoreT.Score . ScoreTime.toDouble
instance ToVal RealTime where
    toVal = VNum . ScoreT.Typed ScoreT.Real . RealTime.toSeconds

instance ToVal a => ToVal [a] where toVal = VList . map toVal

-- ** rest

instance ToVal Attrs.Attributes where toVal = VAttributes
instance ToVal DeriveT.ControlRef where toVal = VControlRef
instance ToVal Pitch.Pitch where toVal = VNotePitch
instance ToVal ScoreT.Instrument where
    toVal (ScoreT.Instrument a) = VStr (Expr.Str a)
instance ToVal Expr.Str where toVal = VStr
instance ToVal Text where toVal = VStr . Expr.Str
instance ToVal Expr where toVal = VQuoted

data ConstantPitch = ConstantPitch !Pitch.ScaleId !Pitch.Note !Pitch.NoteNumber
    deriving (Show, Eq)

instance ToVal ConstantPitch where toVal = VConstantPitch

-- * call

type Expr = Expr.Expr Val
type Call = Expr.Call Val
type Term = Expr.Term Val

instance Serialize.Serialize Val where
    put val = case val of
        VNum v -> Serialize.putTag 0 >> put v
        VAttributes v -> Serialize.putTag 1 >> put v
        VControlRef v -> Serialize.putTag 2 >> put v
        VNotePitch v -> Serialize.putTag 3 >> put v
        -- tag 4 was VInstrument
        VStr v -> Serialize.putTag 5 >> put v
        VQuoted v -> Serialize.putTag 6 >> put v
        VList v -> Serialize.putTag 7 >> put v
        VConstantPitch (ConstantPitch scaleId note nn) -> Serialize.putTag 8
            >> put scaleId >> put note >> put nn
    get = do
        tag <- Serialize.getTag
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
            _ -> Serialize.badTag "REnv.Val" tag

instance Serialize.Serialize Call where
    put (Expr.Call a b) = put a >> put b
    get = Expr.Call <$> get <*> get

instance Serialize.Serialize Term where
    put term = case term of
        Expr.ValCall v -> Serialize.putTag 0 >> put v
        Expr.Literal v -> Serialize.putTag 1 >> put v
    get = Serialize.getTag >>= \case
        0 -> Expr.ValCall <$> get
        1 -> Expr.Literal <$> get
        n -> Serialize.badTag "REnv.Term" n
