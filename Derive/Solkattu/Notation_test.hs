-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
module Derive.Solkattu.Notation_test where
import qualified Data.Tuple as Tuple

import Util.Test
import Derive.Solkattu.Dsl (su, sd, __)
import qualified Derive.Solkattu.DslSollu as DslSollu
import qualified Derive.Solkattu.Instrument.Mridangam as Mridangam
import qualified Derive.Solkattu.Korvai as Korvai
import Derive.Solkattu.Notation
import qualified Derive.Solkattu.Sequence as S
import qualified Derive.Solkattu.Solkattu as Solkattu
import qualified Derive.Solkattu.SolkattuGlobal as SolkattuGlobal
import qualified Derive.Solkattu.Tala as Tala

import Global


di, ta, ka, ki :: SequenceT Solkattu.Sollu
(di, ta, ka, ki) = (DslSollu.di, DslSollu.ta, DslSollu.ka, DslSollu.ki)

taka :: SequenceT Solkattu.Sollu
taka = ta <> ka

-- Many of the Notation functions are indirectly tested in Realize_test.

test_splitM = do
    equal ((map pretty *** map pretty) $ splitM 1 taka)
        (["(1, After)(ta ka)"], ["(1, Before)(ta ka)"])

test_splitM_ = do
    let f matras = fmap (extract *** extract) . splitM_either matras
        extract = map pretty . flatten_groups
    equal (f 1 (su taka <> di)) $ Right (["s+1(ta ka)"], ["di"])
    equal (f 1 (su (ta <> di <> ki <> ta) <> di)) $
        Right (["s+1(ta di)"], ["s+1(ki ta)", "di"])
    left_like (f 1 (sd ta <> ka)) "can't split"

    -- split rests
    equal (f 1 (sd __ <> ka)) $ Right (["__"], ["__", "ka"])
    equal (f 3 (sd (sd __) <> ka)) $ Right (["s-1(__)", "__"], ["__", "ka"])

test_spaceM = do
    let f = sum . map (S.note_fmatra S.default_tempo) . spaceM Solkattu.Rest
    equal (f 0) 0
    equal (f 1) 1
    equal (f 3) 3
    equal (f (3/4)) (3/4)
    throws (f (1/3)) "not a binary multiple"

test_replaceStart = do
    let f prefix = map pretty . replaceStart prefix
    equal (f di (ta<>ki<>ta)) ["di", "ki", "ta"]
    equal (f di (su taka <> ki)) ["di", "ki"]
    -- split rests
    throws (f di (sd ta)) "can't split"

test_align = do
    let f dur = map pretty . __a dur
    equal (f 1 ta) ["s-1(__)", "__", "ta"]

flatten_groups :: [S.Note g a] -> [S.Note () a]
flatten_groups = S.flatten_groups

realize_korvai :: SolkattuGlobal.StrokeMap Mridangam.Stroke
    -> SolkattuGlobal.Sequence -> Either Text [(Text, S.Duration)]
realize_korvai strokes = realize . make_korvai strokes

make_korvai :: SolkattuGlobal.StrokeMap Mridangam.Stroke
    -> SolkattuGlobal.Sequence -> SolkattuGlobal.Korvai
make_korvai strokes seq = korvai
    where
    korvai = SolkattuGlobal.korvai1 Tala.adi_tala
        (SolkattuGlobal.make_mridangam0 strokes)
        seq

realize :: SolkattuGlobal.Korvai -> Either Text [(Text, S.Duration)]
realize = extract . head . Korvai.realize Korvai.mridangam False
    where
    extract (Left err) = Left err
    extract (Right (strokes, _err)) = Right $ extract_strokes strokes
    extract_strokes = map (Tuple.swap . second pretty) . S.flattened_notes
        . S.with_durations
