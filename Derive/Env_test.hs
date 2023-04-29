-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Env_test where
import qualified Data.Map as Map

import qualified Derive.DeriveT as DeriveT
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.PSignal as PSignal
import qualified Derive.ScoreT as ScoreT
import           Derive.TestInstances ()
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Signal as Signal

import           Global
import           Types
import           Util.Test


test_put_val :: Test
test_put_val = do
    let numt typ = DeriveT.constant typ
    right_equal (put1 (0 :: ScoreTime) (1 :: ScoreTime))
        (Typecheck.to_val (1 :: ScoreTime))
    -- I used to disallow this but simplified checking.
    -- left_like (put1 (0 :: ScoreTime) (1 :: RealTime))
    --     "expected Num (ScoreTime) but got Num (RealTime"
    right_equal (put1 (0 :: ScoreTime) (1 :: RealTime)) (numt ScoreT.Real 1)

    -- Don't infer that just because someone put in a positive value that
    -- the type must be positive.
    right_equal (put1 (1 :: Double) (-1 :: Double)) (DeriveT.num (-1))
    right_equal (put1 (-1 :: Double) (1 :: Double)) (DeriveT.num 1)
    let sig = DeriveT.VSignal $ ScoreT.untyped $ Signal.from_pairs [(0, 4)]
    -- Num and Signal are compatible, as long as the types are compatible.
    right_equal (put1 (1 :: Double) sig) sig
    right_equal (put1 sig (1 :: Double)) (DeriveT.num 1)

    right_equal
        (put1
            (DeriveT.VSignal (ScoreT.Typed ScoreT.Real (Signal.constant 1)))
            (2 :: ScoreTime))
        (numt ScoreT.Score 2)
    -- left_like (put1
    --         (DeriveT.VSignal (ScoreT.Typed ScoreT.Real (Signal.constant 1)))
    --         (2 :: ScoreTime))
    --     "expected Signal (RealTime) but got Num (ScoreTime"

    -- remove with VNotGiven
    right_equal (put_env "k" DeriveT.VNotGiven []) mempty
    right_equal (put_env "k" DeriveT.VNotGiven [("k", DeriveT.num 1)]) mempty
    -- untested: hardcoded_types

test_checked_val :: Test
test_checked_val = do
    let f v = Env.checked_val "k" $ Env.from_list [("k", v)]
    let nn = DeriveT.constant ScoreT.Nn
    left_like (fmap (pretty @(Maybe PSignal.Pitch)) (f (DeriveT.num 45)))
        "expected Pitch but env val is Signal"
    right_equal (fmap (pretty @(Maybe PSignal.Pitch)) (f (nn 45)))
        "45nn,45nn(no-scale)"
    left_like (fmap (pretty @(Maybe [PSignal.Pitch]))
        (f (DeriveT.VList [DeriveT.num 45])))
        "expected list of Pitch but env val is list of Signal"
    right_equal
        (fmap (pretty @(Maybe [PSignal.Pitch])) (f (DeriveT.VList [nn 45])))
        "[45nn,45nn(no-scale)]"
    left_like (fmap (pretty @(Maybe Int)) (f (DeriveT.VList [nn 45])))
        "expected Signal (integral) but env val is list of Signal"
    right_equal (fmap (pretty @(Maybe [Int])) (f (DeriveT.VList [nn 45])))
        "[45]"

test_put_val_cf :: Test
test_put_val_cf = do
    let e_cf (DeriveT.VCFunction cf) = Just
            ( DeriveT.cf_name cf
            , Just $ ScoreT.typed_val (DeriveT.cf_signal cf)
            )
        e_cf _ = Nothing
    let put old new = e_cf <$> put1 old new
    let cfp = make_cfp "cfp"
        cf0 = make_cf "cf0" (ScoreT.untyped (Signal.constant 0))
        cf1 = make_cf "cf1" (ScoreT.untyped (Signal.constant 1))
    let sig = Signal.from_pairs [(0, 4)]
        vsig = DeriveT.VSignal $ ScoreT.untyped sig
    left_like (put cf0 cfp) "expected CFunction but got PFunction"
    left_like (put cfp (DeriveT.num 1)) "expected PFunction but got Signal"
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
make_cf name sig = DeriveT.VCFunction $ DeriveT.CFunction
    { cf_name = name
    , cf_signal = sig
    , cf_function = \_ _ _ -> 0
    }

make_cfp :: Text -> DeriveT.Val
make_cfp name = DeriveT.VPFunction $
    DeriveT.PFunction name (ScoreT.untyped (const 0))
