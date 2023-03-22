-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.ValType_test where
import qualified Derive.DeriveT as DeriveT
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ValType as ValType
import           Derive.ValType (NumType(..), NumValue(..), Type(..))

import           Util.Test


test_type_of :: Test
test_type_of = do
    let f = ValType.specific_type_of
    let nn = DeriveT.constant ScoreT.Nn
    equal (f (DeriveT.VList [])) $ TList TVal
    equal (f (DeriveT.VList [DeriveT.num 1])) $
        TList (TSignal TUntyped TPositive)
    equal (f (DeriveT.VList [nn 45])) $ TList (TSignal TNoteNumber TPositive)
    equal (f (DeriveT.VList [DeriveT.num 1, DeriveT.VAttributes mempty])) $
        TList TVal
    equal (f (DeriveT.VList [DeriveT.num 1, DeriveT.num (-1)])) $
        TList (TSignal TUntyped TAny)
