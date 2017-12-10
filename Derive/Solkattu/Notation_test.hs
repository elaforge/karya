-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
module Derive.Solkattu.Notation_test where
import qualified Data.Tuple as Tuple

import Util.Test
import Derive.Solkattu.Dsl (su, sd, nadai, __)
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

test_splitD = do
    equal ((map pretty *** map pretty) $ splitD (1/4) (ta <> ka))
        (["([ka], Back)(ta)"], ["([ta], Front)(ka)"])

    let f dur = (extract *** extract) . splitD dur
        extract = map pretty . flatten_groups
    equal (f (1/4) (su (ta <> ka) <> di)) (["s+1(ta ka)"], ["di"])
    equal (f (1/4) (su (ta <> di <> ki <> ta) <> di))
        (["s+1(ta di)"], ["s+1(ki ta)", "di"])
    throws (f (1/4) (sd ta <> ka)) "can't split"

    -- split rests
    equal (f (1/4) (sd __ <> ka)) (["__"], ["__", "ka"])
    equal (f (3/4) (sd (sd __) <> ka)) (["s-1(__)", "__"], ["__", "ka"])

test_splitS_ = do
    let f dur = (extract *** extract) . splitS_ dur
        extract = map pretty . flatten_groups
    let seq = su (ta <> ka) <> di
    equal (f 0 seq) ([], ["s+1(ta ka)", "di"])
    equal (f 1 seq) (["s+1(ta)"], ["s+1(ka)", "di"])
    equal (f 2 seq) (["s+1(ta ka)"], ["di"])
    equal (f 3 seq) (["s+1(ta ka)", "di"], [])

test_spaceD = do
    let f tempo = sum . map (S.note_duration tempo)
            . spaceD Solkattu.Rest tempo
    equal (f S.default_tempo 0) 0
    equal (f S.default_tempo 1) 1
    equal (f S.default_tempo (3/4)) (3/4)
    throws (f S.default_tempo (1/3)) "not a binary multiple"
    equal (f (S.Tempo 0 6 1) (1/3)) (1/3)

test_replaceStart = do
    let f prefix = map pretty . replaceStart prefix
    equal (f di (ta<>ki<>ta)) ["di", "ki", "ta"]
    equal (f di (su (ta<>ka) <> ki)) ["di", "ki"]
    -- split rests
    throws (f di (sd ta)) "can't split"

test_align = do
    let f dur = map pretty . __a dur
    equal (f 1 ta) ["s-1(__)", "__", "ta"]

test_groups = do
    let Mridangam.Strokes {..} = Mridangam.notes
    let run = realize_korvai
            [ (taka, [k, t])
            , (takita, [n, p, k])
            ]
        taka = ta <> ka
        takita = ta <> ki <> ta
    equal (run $ taka) $ Right [("k", 1/4), ("t", 1/4)]
    equal (run $ dropM 1 $ taka) $ Right [("t", 1/4)]
    -- TODO requires nested groups
    -- equal (run $ dropM 1 $ dropM 1 $ takita) $ Right [("k", 1/4)]

    equal (run $ su $ dropM 1 taka) $ Right [("t", 1/8)]
    equal (run $ dropM 1 $ su $ taka <> taka) $ Right [("k", 1/8), ("t", 1/8)]
    equal (run $ dropM 1 $ nadai 8 $ taka <> taka) $
        Right [("k", 1/8), ("t", 1/8)]
    equal (run $ dropM 2 $ nadai 6 $ takita <> takita) $
        Right [("n", 1/6), ("p", 1/6), ("k", 1/6)]

-- reduce tested in Realize_test

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
    extract_strokes = map (Tuple.swap . second pretty) . S.tempo_to_duration
        . map (first S._tempo)
