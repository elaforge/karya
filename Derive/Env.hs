-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Env (
    module Derive.Env
    , Key, Environ, null, lookup, insert
) where
import           Prelude hiding (map, null, lookup)
import qualified Data.Map as Map

import qualified Derive.DeriveT as DeriveT
import           Derive.DeriveT (Environ(..), insert, lookup, null)
import qualified Derive.EnvKey as EnvKey
import           Derive.EnvKey (Key)
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Typecheck as Typecheck
import qualified Derive.ValType as ValType

import           Global


-- * basic functions

from_list :: [(Key, DeriveT.Val)] -> Environ
from_list = Environ . Map.fromList

to_list :: Environ -> [(Key, DeriveT.Val)]
to_list (Environ env) = Map.toList env

to_map :: Environ -> Map Key DeriveT.Val
to_map (Environ env) = env

from_map :: Map Key DeriveT.Val -> Environ
from_map = Environ

delete :: Key -> Environ -> Environ
delete key (Environ env) = Environ $ Map.delete key env

is_set :: Key -> Environ -> Bool
is_set key (Environ env) = Map.member key env

map :: (DeriveT.Val -> DeriveT.Val) -> Environ -> Environ
map f (Environ env) = Environ $ f <$> env

-- * typechecking

type Error = Text

-- | Insert a new val, but return Left if it changes the type of an existing
-- one, so once you put a key of a given type into the environ, it can only
-- ever be overwritten by a Val of the same type.  The idea is that being
-- inconsistent with types will just lead to confusion.
--
-- 'DeriveT.VNotGiven' is another special case, it deletes the given key.
put_val :: Typecheck.ToVal a => Key -> a -> Environ -> Either Error Environ
put_val key val environ = case lookup key environ of
    Nothing -> case Map.lookup key hardcoded_types of
        Just expected | not $
            ValType.types_match expected (ValType.type_of rhs) ->
                Left $ type_error key rhs expected
        _ -> Right $ case rhs of
            DeriveT.VNotGiven -> environ
            _ -> insert key rhs environ
    Just lhs -> assign (lhs, rhs)
    where
    rhs = Typecheck.to_val val
    assign = \case
        (_, DeriveT.VNotGiven) -> Right $ delete key environ
        (DeriveT.VControlFunction cf, rhs)
            | Just cf <- merge_cf cf rhs -> add <$> cf
        -- As a special hack, while `lhs=(cf) | lhs=1` is fine because
        -- the num coerces to a constant signal, let's disallow the other
        -- way.  This is because numeric constants are common and turning
        -- them into a function means plain Derive.get_val will stop working,
        -- e.g. Call.get_srate.
        (lhs, DeriveT.VControlFunction cf)
            | Just cf <- merge_cf cf lhs -> add <$> cf
        (lhs, rhs) -> case ValType.val_types_match lhs rhs of
            Just expected -> Left $ type_error key rhs expected
            Nothing -> Right $ add rhs
    merge_cf cf = \case
        DeriveT.VSignal sig -> Just $ DeriveT.VControlFunction <$>
            cf_set_control sig cf
        -- Anything else gets type checked and replaced.
        _ -> Nothing
    add rhs = insert key rhs environ

cf_set_control :: DeriveT.TypedSignal -> DeriveT.ControlFunction
    -> Either Error DeriveT.ControlFunction
cf_set_control sig cf = case DeriveT.cf_function cf of
    DeriveT.CFBacked _ f -> Right $
        cf { DeriveT.cf_function = DeriveT.CFBacked sig f }
    -- TODO If I make CFPure a separate type, this goes away.
    DeriveT.CFPure {} -> Left $ "can't merge "
        <> ShowVal.show_val sig <> " into pure ControlFunction"

type_error :: Key -> DeriveT.Val -> ValType.Type -> Error
type_error key val expected = mconcat
    [ "can't set ", pretty key, " to ", ShowVal.show_val val, ", expected "
    , pretty expected, " but got ", pretty (ValType.type_of val)
    ]

-- | Insert a val without typechecking.
insert_val :: Typecheck.ToVal a => Key -> a -> Environ -> Environ
insert_val key = insert key . Typecheck.to_val

-- | If a standard val gets set to the wrong type, it will cause confusing
-- errors later on.
hardcoded_types :: Map Key ValType.Type
hardcoded_types = Map.fromList
    [ (EnvKey.attributes,  ValType.TAttributes)
    , (EnvKey.block_end,   ValType.TNum ValType.TScoreTime ValType.TAny)
    , (EnvKey.control,     ValType.TStr Nothing)
    , (EnvKey.instrument,  ValType.TStr Nothing)
    , (EnvKey.key,         ValType.TStr Nothing)
    , (EnvKey.merge,       ValType.TStr Nothing)
    , (EnvKey.scale,       ValType.TStr Nothing)
    , (EnvKey.seed,        ValType.TNum ValType.TUntyped ValType.TAny)
    , (EnvKey.srate,       ValType.TNum ValType.TUntyped ValType.TAny)
    , (EnvKey.suppress_until, ValType.TNum ValType.TRealTime ValType.TAny)
    , (EnvKey.tuning,      ValType.TStr Nothing)
    , (EnvKey.voice,       ValType.TNum ValType.TUntyped ValType.TAny)
    ]

data LookupError = NotFound | WrongType !ValType.Type deriving (Show)

get_val :: Typecheck.Typecheck a => Key -> Environ -> Either LookupError a
get_val key environ = case lookup key environ of
    Nothing -> Left NotFound
    Just val -> case Typecheck.from_val_simple val of
        Nothing -> Left (WrongType (ValType.type_of val))
        Just a -> Right a

-- | Like 'get_val', except that type errors and not found both turn into
-- Nothing.
maybe_val :: Typecheck.Typecheck a => Key -> Environ -> Maybe a
maybe_val key = Typecheck.from_val_simple <=< lookup key

-- | Like 'get_val' but format a WrongType nicely.
checked_val :: forall a. Typecheck.Typecheck a => Key -> Environ
    -> Either Text (Maybe a)
checked_val key environ = case get_val key environ of
    Left NotFound -> return Nothing
    Left (WrongType typ) ->
        Left $ pretty key <> ": expected " <> pretty return_type
            <> " but env val is " <> pretty typ
    Right a -> return (Just a)
    where return_type = Typecheck.to_type (Proxy :: Proxy a)

-- | Like 'checked_val', but juggle the return type around so NotFound is just
-- Nothing, which is more convenient in some cases.
checked_val2 :: Typecheck.Typecheck a => Key -> Environ
    -> Maybe (Either Text a)
checked_val2 key environ = case checked_val key environ of
    Right Nothing -> Nothing
    Right (Just val) -> Just (Right val)
    Left err -> Just (Left err)
