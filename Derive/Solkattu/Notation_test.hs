-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Solkattu.Notation_test where
import Util.Test
import qualified Derive.Solkattu.Dsl as Dsl
import Derive.Solkattu.Dsl (faster, nadai)
import qualified Derive.Solkattu.Notation as Notation

import Global


di, ta, ka, ki :: Notation.Sequence ()
(di, ta, ka, ki) = (Dsl.di, Dsl.ta, Dsl.ka, Dsl.ki)

test_matras_of = do
    let f = Notation.matrasOfE
    equal (f Dsl.p5) (Right 5)
    equal (f (ta <> ka)) (Right 2)
    equal (f (di <> faster (ta <> ka))) (Right 2)
    -- If the whole thing is is set to the nadai, that's ok.
    equal (f (nadai 5 (ta <> ka))) (Right 2)
    left_like (f (nadai 5 (ta <> ka) <> di)) "nadai change"
    left_like (f (faster di)) "non-integral matras"

test_splitD = do
    let f dur = (map pretty *** map pretty) . Notation.splitD dur
    equal (f (1/4) (ta <> ka)) (["ta"], ["ka"])
    equal (f (1/4) (faster (ta <> ka) <> di)) (["s+1(ta ka)"], ["di"])
    equal (f (1/4) (faster (ta <> di <> ki <> ta) <> di))
        (["s+1(ta di)"], ["s+1(ki ta)", "di"])

test_rdropM = do
    let f m = map pretty . Notation.rdropM m
    equal (f 1 (ta <> ka)) ["ta"]
    equal (f 1 (di <> faster (ta <> ka) <> di)) ["di", "s+1(ta ka)"]
    equal (f 1 (di <> faster (ta <> di <> ki <> ta))) ["di", "s+1(ta di)"]
