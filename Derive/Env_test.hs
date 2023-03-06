-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Env_test where
import qualified Data.Map as Map

import qualified Derive.DeriveT as DeriveT
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.ScoreT as ScoreT
import           Derive.TestInstances ()
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Signal as Signal

import           Global
import           Types
import           Util.Test


test_put_val :: Test
test_put_val = do
    right_equal (put1 (0 :: ScoreTime) (1 :: ScoreTime))
        (Typecheck.to_val (1 :: ScoreTime))
    left_like (put1 (0 :: ScoreTime) (1 :: RealTime))
        "expected Num (ScoreTime) but got Num (RealTime"
    -- Don't infer that just because someone put in a positive value that
    -- the type must be positive.
    right_equal (put1 (1 :: Double) (-1 :: Double)) (DeriveT.num (-1))
    right_equal (put1 (-1 :: Double) (1 :: Double)) (DeriveT.num 1)
    let sig = DeriveT.VSignal $ ScoreT.untyped $ Signal.from_pairs [(0, 4)]
    -- Num and Signal are compatible, as long as the types are compatible.
    right_equal (put1 (1 :: Double) sig) sig
    right_equal (put1 sig (1 :: Double)) (DeriveT.num 1)
    left_like (put1
            (DeriveT.VSignal (ScoreT.Typed ScoreT.Real (Signal.constant 1)))
            (2 :: ScoreTime))
        "expected Signal (RealTime) but got Num (ScoreTime"
    -- remove with VNotGiven
    right_equal (put_env "k" DeriveT.VNotGiven []) mempty
    right_equal (put_env "k" DeriveT.VNotGiven [("k", DeriveT.num 1)]) mempty
    -- untested: hardcoded_types

test_put_val_cf :: Test
test_put_val_cf = do
    let e_cf (DeriveT.VControlFunction cf) = case DeriveT.cf_function cf of
            DeriveT.CFBacked sig _ -> Just
                ( DeriveT.cf_name cf
                , Just $ ScoreT.typed_val sig
                )
            DeriveT.CFPure {} -> Just (DeriveT.cf_name cf, Nothing)
        e_cf _ = Nothing
    let put old new = e_cf <$> put1 old new
    let cfp = make_cfp "cfp"
        cf0 = make_cf "cf0" (ScoreT.untyped (Signal.constant 0))
        cf1 = make_cf "cf1" (ScoreT.untyped (Signal.constant 1))
    let sig = Signal.from_pairs [(0, 4)]
        vsig = DeriveT.VSignal $ ScoreT.untyped sig
    -- Pure cf can be replaced by impure and vice versa.  TODO but they are
    -- kind of different types, if I make them so then this will fail.
    right_equal (put cf0 cfp) (Just ("cfp", Nothing))
    left_like (put cfp (DeriveT.num 1)) "expected ControlFunction but got Num"
    left_like (put cfp vsig)
        "can't merge (signal 0 4) into pure ControlFunction"
    right_equal (put cf0 vsig) (Just ("cf0", Just sig))
    right_equal (put vsig cf0) (Just ("cf0", Just sig))
    right_equal (put cf1 vsig) (Just ("cf1", Just sig))

put1 :: (Typecheck.ToVal old, Typecheck.ToVal new) => old -> new
    -> Either Text DeriveT.Val
put1 old new =
    Map.findWithDefault (error "lhs not in env")  "lhs" <$>
        put_env "lhs" new [("lhs", Typecheck.to_val old)]

put_env :: Typecheck.ToVal val => EnvKey.Key -> val
    -> [(EnvKey.Key, DeriveT.Val)]
    -> Either Text (Map EnvKey.Key DeriveT.Val)
put_env key val = fmap Env.to_map . Env.put_val key val . Env.from_list


make_cf :: Text -> DeriveT.TypedSignal -> DeriveT.Val
make_cf name sig = DeriveT.VControlFunction $
    DeriveT.ControlFunction name (DeriveT.CFBacked sig (\_ _ _ -> 0))

make_cfp :: Text -> DeriveT.Val
make_cfp name = DeriveT.VControlFunction $
    DeriveT.ControlFunction name (DeriveT.CFPure ScoreT.Untyped (const 0))
