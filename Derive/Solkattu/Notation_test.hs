-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Solkattu.Notation_test where
import Util.Test
import qualified Derive.Solkattu.Dsl as Dsl
import Derive.Solkattu.Dsl (su, sd, nadai, __)
import qualified Derive.Solkattu.DslSollu as DslSollu
import qualified Derive.Solkattu.Notation as Notation
import qualified Derive.Solkattu.Sequence as S
import qualified Derive.Solkattu.Solkattu as Solkattu
import qualified Derive.Solkattu.Tala as Tala

import Global


di, ta, ka, ki :: Notation.Sequence ()
(di, ta, ka, ki) = (DslSollu.di, DslSollu.ta, DslSollu.ka, DslSollu.ki)

test_matras_of = do
    let f = Notation.matrasOfE
    equal (f Dsl.p5) (Right 5)
    equal (f (ta <> ka)) (Right 2)
    equal (f (di <> su (ta <> ka))) (Right 2)
    -- If the whole thing is is set to the nadai, that's ok.
    equal (f (nadai 5 (ta <> ka))) (Right 2)
    left_like (f (nadai 5 (ta <> ka) <> di)) "nadai change"
    left_like (f (su di)) "non-integral matras"

test_splitD = do
    let f dur = (map pretty *** map pretty) . Notation.splitD dur
    equal (f (1/4) (ta <> ka)) (["ta"], ["ka"])
    equal (f (1/4) (su (ta <> ka) <> di)) (["s+1(ta ka)"], ["di"])
    equal (f (1/4) (su (ta <> di <> ki <> ta) <> di))
        (["s+1(ta di)"], ["s+1(ki ta)", "di"])
    throws (f (1/4) (sd ta <> ka)) "can't split"

    -- split rests
    equal (f (1/4) (sd __ <> ka)) (["__"], ["__", "ka"])
    equal (f (3/4) (sd (sd __) <> ka)) (["s-1(__)", "__"], ["__", "ka"])

flatten :: S.HasMatras a => [S.Note a] -> [S.Stroke a]
flatten = map snd . S.normalize_speed Tala.adi_tala . S.flatten

speed :: S.Speed -> [S.Note a] -> S.Note a
speed = S.change_speed

test_spaceD = do
    let f tempo = sum . map (S.note_duration tempo)
            . Notation.spaceD Solkattu.Rest tempo
    equal (f S.default_tempo 0) 0
    equal (f S.default_tempo 1) 1
    equal (f S.default_tempo (3/4)) (3/4)
    throws (f S.default_tempo (1/3)) "not a binary multiple"
    equal (f (S.Tempo 0 6) (1/3)) (1/3)

test_rdropM = do
    let f m = map pretty . Notation.rdropM m
    equal (f 1 (ta <> ka)) ["ta"]
    equal (f 1 (di <> su (ta <> ka) <> di)) ["di", "s+1(ta ka)"]
    equal (f 1 (di <> su (ta <> di <> ki <> ta))) ["di", "s+1(ta di)"]

test_replaceStart = do
    let f prefix = map pretty . Notation.replaceStart prefix
    equal (f di (ta<>ki<>ta)) ["di", "ki", "ta"]
    equal (f di (su (ta<>ka) <> ki)) ["di", "ki"]
    -- split rests
    throws (f di (sd ta)) "can't split"

test_align = do
    let f dur = map pretty . Notation.__a dur
    equal (f 1 ta) ["__", "s-1(__)", "ta"]
