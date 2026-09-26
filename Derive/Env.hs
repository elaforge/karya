-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Env (
    Key, Environ, null, lookup, insert
    , fromList, toList
    , toMap, fromMap
    , delete
    , isSet
    , map
    , fromControls
    -- * put
    , putVal
    , modifySignal
    , insertVal
    , LookupError(..)
    , getVal
    , maybeVal
    , checkedVal, checkedVal2
) where
import qualified Prelude
import           Prelude hiding (map, null, lookup)
import qualified Data.Map as Map

import qualified Derive.DeriveT as DeriveT
import           Derive.DeriveT (Environ(..), insert, lookup, null)
import qualified Derive.EnvKey as EnvKey
import           Derive.EnvKey (Key)
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Typecheck as Typecheck
import qualified Derive.ValType as ValType

import           Global


-- * basic functions

fromList :: [(Key, DeriveT.Val)] -> Environ
fromList = Environ . Map.fromList

toList :: Environ -> [(Key, DeriveT.Val)]
toList (Environ env) = Map.toList env

toMap :: Environ -> Map Key DeriveT.Val
toMap (Environ env) = env

fromMap :: Map Key DeriveT.Val -> Environ
fromMap = Environ

delete :: Key -> Environ -> Environ
delete key (Environ env) = Environ $ Map.delete key env

isSet :: Key -> Environ -> Bool
isSet key (Environ env) = Map.member key env

map :: (DeriveT.Val -> DeriveT.Val) -> Environ -> Environ
map f (Environ env) = Environ $ f <$> env

fromControls :: ScoreT.ControlMap -> Environ
fromControls = fromMap . Map.fromAscList . Prelude.map convert . Map.toAscList
    where
    convert (ScoreT.Control control, sig) = (control, DeriveT.VSignal sig)

-- * typechecking

type Error = Text

-- | Insert a new val, but return Left if it changes the type of an existing
-- one, so once you put a key of a given type into the environ, it can only
-- ever be overwritten by a Val of the same type.  The idea is that being
-- inconsistent with types will just lead to confusion.
--
-- 'DeriveT.VNotGiven' is another special case, it deletes the given key.
putVal :: Typecheck.ToVal a => Key -> a -> Environ -> Either Error Environ
putVal key val environ = case lookup key environ of
    Nothing -> case Map.lookup key hardcodedTypes of
        Just expected | not $ DeriveT.typesEqual expected rhs ->
            Left $ typeError key rhs (ValType.generalTypeOf expected)
        _ -> Right $ case rhs of
            DeriveT.VNotGiven -> environ
            _ -> insert key rhs environ
    Just lhs -> assign (lhs, rhs)
    where
    rhs = Typecheck.toVal val
    assign = \case
        (_, DeriveT.VNotGiven) -> Right $ delete key environ
        (DeriveT.VCFunction cf, rhs)
            | Just cf <- mergeCf cf rhs -> Right $ add cf
        (lhs, DeriveT.VCFunction cf)
            | Just cf <- mergeCf cf lhs -> Right $ add cf
        (lhs, rhs)
            | DeriveT.typesEqual lhs rhs -> Right $ add rhs
            | otherwise ->
                Left $ typeError key rhs (ValType.generalTypeOf lhs)
    mergeCf cf = \case
        DeriveT.VSignal sig ->
            Just $ DeriveT.VCFunction $ cf { DeriveT.cfSignal = sig }
        -- Anything else gets type checked and replaced.
        _ -> Nothing
    add rhs = insert key rhs environ

modifySignal :: (Maybe ScoreT.TypedSignal -> ScoreT.TypedSignal) -> Key
    -> Environ -> Either Error DeriveT.Val
modifySignal modify key environ = case lookup key environ of
    Nothing -> Right $ Typecheck.toVal $ modify Nothing
    Just val -> modifySignalVal (modify . Just) val

modifySignalVal :: (ScoreT.TypedSignal -> ScoreT.TypedSignal) -> DeriveT.Val
    -> Either Error DeriveT.Val
modifySignalVal modify = \case
    DeriveT.VSignal sig -> Right $ DeriveT.VSignal $ modify sig
    DeriveT.VCFunction cf -> Right $ DeriveT.VCFunction $
        cf { DeriveT.cfSignal = modify (DeriveT.cfSignal cf) }
    val -> Left $ "can't modify " <> pretty (ValType.generalTypeOf val)
        <> " as a signal"

typeError :: Key -> DeriveT.Val -> ValType.Type -> Error
typeError key val expected = mconcat
    [ "can't set ", pretty key, " to ", ShowVal.showVal val, ", expected "
    , pretty expected, " but got ", pretty (ValType.generalTypeOf val)
    ]

-- | Insert a val without typechecking.
insertVal :: Typecheck.ToVal a => Key -> a -> Environ -> Environ
insertVal key = insert key . Typecheck.toVal

-- | If a standard val gets set to the wrong type, it will cause confusing
-- errors later on.
hardcodedTypes :: Map Key DeriveT.Val
hardcodedTypes = Map.fromList
    [ (EnvKey.attributes,  DeriveT.VAttributes mempty)
    , (EnvKey.blockEnd,   DeriveT.constant ScoreT.Score 0)
    , (EnvKey.control,     str)
    , (EnvKey.instrument,  str)
    , (EnvKey.key,         str)
    , (EnvKey.merge,       str)
    , (EnvKey.scale,       str)
    , (EnvKey.seed,        num)
    , (EnvKey.srate,       num)
    , (EnvKey.suppressUntil, DeriveT.constant ScoreT.Real 0)
    , (EnvKey.tuning,      str)
    , (EnvKey.voice,       num)
    ]
    where
    str = DeriveT.VStr ""
    num = DeriveT.num 0

data LookupError = NotFound | WrongType !ValType.Type deriving (Show)

getVal :: Typecheck.Typecheck a => Key -> Environ -> Either LookupError a
getVal key environ = case lookup key environ of
    Nothing -> Left NotFound
    Just val -> case Typecheck.fromValSimple val of
        Nothing -> Left (WrongType (ValType.generalTypeOf val))
        Just a -> Right a

-- | Like 'get_val', except that type errors and not found both turn into
-- Nothing.
maybeVal :: Typecheck.Typecheck a => Key -> Environ -> Maybe a
maybeVal key = Typecheck.fromValSimple <=< lookup key

-- | Like 'get_val' but format a WrongType nicely.
checkedVal :: forall a. Typecheck.Typecheck a => Key -> Environ
    -> Either Text (Maybe a)
checkedVal key environ = case getVal key environ of
    Left NotFound -> pure Nothing
    Left (WrongType typ) ->
        Left $ pretty key <> ": expected " <> pretty returnType
            <> " but env val is " <> pretty typ
    Right a -> pure (Just a)
    where returnType = Typecheck.toType (Proxy :: Proxy a)

-- | Like 'checked_val', but juggle the return type around so NotFound is just
-- Nothing, which is more convenient in some cases.
checkedVal2 :: Typecheck.Typecheck a => Key -> Environ
    -> Maybe (Either Text a)
checkedVal2 key environ = case checkedVal key environ of
    Right Nothing -> Nothing
    Right (Just val) -> Just (Right val)
    Left err -> Just (Left err)
