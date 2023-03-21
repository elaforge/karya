-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Env (
    Key, Environ, null, lookup, insert
    , from_list, to_list
    , to_map, from_map
    , delete
    , is_set
    , map
    , put_val
    , modify_signal
    , insert_val
    , LookupError(..)
    , get_val
    , maybe_val
    , checked_val, checked_val2
) where
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

import qualified Perform.Signal as Signal

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
        Just expected | not $ DeriveT.types_equal expected rhs ->
            Left $ type_error key rhs (ValType.general_type_of expected)
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
        (lhs, DeriveT.VControlFunction cf)
            | Just cf <- merge_cf cf lhs -> add <$> cf
        (lhs, rhs)
            | DeriveT.types_equal lhs rhs -> Right $ add rhs
            | otherwise ->
                Left $ type_error key rhs (ValType.general_type_of lhs)
    merge_cf cf = \case
        DeriveT.VNum num -> Just $ DeriveT.VControlFunction <$>
            cf_set_control (Signal.constant <$> num) cf
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

modify_signal :: (Maybe DeriveT.TypedSignal -> DeriveT.TypedSignal) -> Key
    -> Environ -> Either Error DeriveT.Val
modify_signal modify key environ = case lookup key environ of
    Nothing -> Right $ Typecheck.to_val $ modify Nothing
    Just val -> modify_signal_val (modify . Just) val

modify_signal_val :: (DeriveT.TypedSignal -> DeriveT.TypedSignal) -> DeriveT.Val
    -> Either Error DeriveT.Val
modify_signal_val modify = \case
    DeriveT.VNum num ->
        Right $ DeriveT.VSignal $ modify $ Signal.constant <$> num
    DeriveT.VSignal sig -> Right $ DeriveT.VSignal $ modify sig
    DeriveT.VControlFunction cf -> case DeriveT.cf_function cf of
        DeriveT.CFBacked sig f ->
            Right $ DeriveT.VControlFunction $
                cf { DeriveT.cf_function = DeriveT.CFBacked (modify sig) f }
        DeriveT.CFPure {} ->
            Left "can't modify pure ControlFunction as a signal"
    val -> Left $ "can't modify " <> pretty (ValType.general_type_of val)
        <> " as a signal"

type_error :: Key -> DeriveT.Val -> ValType.Type -> Error
type_error key val expected = mconcat
    [ "can't set ", pretty key, " to ", ShowVal.show_val val, ", expected "
    , pretty expected, " but got ", pretty (ValType.general_type_of val)
    ]

-- | Insert a val without typechecking.
insert_val :: Typecheck.ToVal a => Key -> a -> Environ -> Environ
insert_val key = insert key . Typecheck.to_val

-- | If a standard val gets set to the wrong type, it will cause confusing
-- errors later on.
hardcoded_types :: Map Key DeriveT.Val
hardcoded_types = Map.fromList
    [ (EnvKey.attributes,  DeriveT.VAttributes mempty)
    , (EnvKey.block_end,   tnum ScoreT.Score)
    , (EnvKey.control,     str)
    , (EnvKey.instrument,  str)
    , (EnvKey.key,         str)
    , (EnvKey.merge,       str)
    , (EnvKey.scale,       str)
    , (EnvKey.seed,        num)
    , (EnvKey.srate,       num)
    , (EnvKey.suppress_until, tnum ScoreT.Real)
    , (EnvKey.tuning,      str)
    , (EnvKey.voice,       num)
    ]
    where
    str = DeriveT.VStr ""
    tnum typ = DeriveT.VNum (ScoreT.Typed typ 0)
    num = DeriveT.VNum (ScoreT.untyped 0)

data LookupError = NotFound | WrongType !ValType.Type deriving (Show)

get_val :: Typecheck.Typecheck a => Key -> Environ -> Either LookupError a
get_val key environ = case lookup key environ of
    Nothing -> Left NotFound
    Just val -> case Typecheck.from_val_simple val of
        Nothing -> Left (WrongType (ValType.general_type_of val))
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
