-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.ValType_test where
import qualified Derive.DeriveT as DeriveT
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Typecheck as Typecheck
import qualified Derive.ValType as ValType
import           Derive.ValType (NumType(..), NumValue(..), Type(..))

import qualified Perform.Pitch as Pitch

import           Types
import           Util.Test


test_types_match :: Test
test_types_match = do
    let f t1 v2 = ValType.types_match t1 (to_type v2)
    equal (f (TNum TScoreTime TAny) (0 :: RealTime)) False
    equal (f (TNum TScoreTime TAny) (0 :: ScoreTime)) True
    -- equal (f (TNum TScoreTime TAny) (TNum TScoreTime TNatural)
    -- TTime has subtypes.
    equal (f (TNum TTime TAny) (-1 :: RealTime)) True
    equal (f (TNum TDefaultReal TAny) (-1 :: ScoreTime)) True
    -- NumValue also has subtypes.
    equal (f (TNum TTime TPositive) (-1 :: RealTime)) False
    equal (f (TNum TTime TPositive) (0 :: RealTime)) False
    equal (f (TNum TTime TPositive) (1 :: RealTime)) True
    equal (f (TNum TTime TNonNegative) (0 :: RealTime)) True
    equal (f (TNum TTime TNonNegative) (1 :: RealTime)) True

    equal (f (TNum TTime TNormalized) (1 :: RealTime)) True
    equal (f (TNum TTime TNormalized) (2 :: RealTime)) False
    equal (f (TNum TUntyped TNormalizedBipolar) (-1 :: RealTime)) True
    equal (f (TNum TUntyped TNormalizedBipolar) (2 :: RealTime)) False

    -- TTranspose has subtypes.
    equal (f (TNum TTranspose TAny) (1 :: RealTime)) False
    equal (f (TNum TTranspose TAny) (Pitch.Chromatic 1)) True
    equal (f (TNum TTranspose TAny) (Pitch.Diatonic 1)) True
    equal (f (TNum TTranspose TAny) (Pitch.nn 1)) True
    equal (f (TNum TTranspose TAny) (Pitch.Nn 1)) True

    equal (f (TList TVal) [1 :: Int]) True

test_type_of :: Test
test_type_of = do
    let f = ValType.type_of
    let nn = DeriveT.VNum . ScoreT.Typed ScoreT.Nn
    equal (f (DeriveT.VList [])) $ TList TVal
    equal (f (DeriveT.VList [DeriveT.num 1])) $ TList (TNum TUntyped TPositive)
    equal (f (DeriveT.VList [nn 45])) $ TList (TNum TNoteNumber TPositive)
    equal (f (DeriveT.VList [DeriveT.num 1, DeriveT.VAttributes mempty])) $
        TList TVal
    equal (f (DeriveT.VList [DeriveT.num 1, DeriveT.num (-1)])) $
        TList (TNum TUntyped TAny)

to_type :: Typecheck.ToVal a => a -> Type
to_type = ValType.type_of . Typecheck.to_val
