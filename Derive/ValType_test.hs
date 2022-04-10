-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.ValType_test where
import Util.Test
import qualified Derive.Typecheck as Typecheck
import qualified Derive.ValType as ValType
import Derive.ValType (Type(..), NumType(..), NumValue(..))

import qualified Perform.Pitch as Pitch
import Types


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
    equal (f (TNum TTime TPositive) (1 :: RealTime)) True

    -- TTranspose has subtypes.
    equal (f (TNum TTranspose TAny) (1 :: RealTime)) False
    equal (f (TNum TTranspose TAny) (Pitch.Chromatic 1)) True
    equal (f (TNum TTranspose TAny) (Pitch.Diatonic 1)) True
    equal (f (TNum TTranspose TAny) (Pitch.nn 1)) True
    equal (f (TNum TTranspose TAny) (Pitch.Nn 1)) True

to_type :: Typecheck.ToVal a => a -> Type
to_type = ValType.type_of . Typecheck.to_val
