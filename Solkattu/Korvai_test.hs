-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
module Solkattu.Korvai_test where
import qualified Data.Text as Text

import Util.Test
import qualified Solkattu.Dsl as Dsl
import Solkattu.DslSollu (ta, ka, din, na)
import qualified Solkattu.Instrument.Mridangam as Mridangam
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Realize as Realize
import qualified Solkattu.Sequence as Sequence
import qualified Solkattu.Solkattu as Solkattu
import qualified Solkattu.Tala as Tala

import Global


test_realize = do
    let f realizePatterns = fmap (first extract) . head
            . Korvai.realize Korvai.mridangam realizePatterns
            . korvai (Tala.Tala "eka" Tala.eka 2) . (:[])
        extract notes =
            [ pretty tempo <> ":" <> pretty stroke
            | (tempo, stroke) <- Sequence.tempoNotes notes
            ]
        tkdn = cycle $ mconcat [ta, ka, din, na]
        p4s = cycle $ Dsl.pat 4
    equal (f False (take 4 tkdn)) $ Right
        ( map ("s0n4:"<>) (chars "kook")
        , "4: korvai should end on or before sam: avartanam 1, akshara 1 + 0"
        )
    equal (f False (take 2 p4s)) $ Right (map ("s0n4:"<>) ["p4", "p4"], "")
    equal (f True (take 2 p4s)) $ Right (map ("s0n4:"<>) (chars "pkonpkon"), "")
    equal (f False (Dsl.sd (take 1 p4s))) $ Right (["s-1n4:p4"], "")
    equal (f False (Dsl.nadai 2 (take 1 p4s))) $ Right (["s0n2:p4"], "")
    equal (f True (Dsl.nadai 2 (take 1 p4s))) $
        Right (map ("s0n2:"<>) (chars "pkon"), "")

test_realizeTechnique = do
    let f = fmap extract . head
            . Korvai.realize Korvai.mridangam False
            . korvai Tala.adi_tala . (:[])
        extract = Text.unwords . map pretty . Sequence.flattenedNotes . fst
        takatakadinna = mconcat [ta, ka, ta, ka, din, na]
    equal (f takatakadinna) (Right "k t k o o k")
    equal (f (Dsl.dropM 1 takatakadinna)) (Right "k k o o k")
    equal (f (Dsl.dropM 1 (mconcat [ta, ka, ta, ta, ka]))) (Right "t k k o")

chars :: [Char] -> [Text]
chars = map Text.singleton

korvai :: Tala.Tala -> [Korvai.Sequence] -> Korvai.Korvai
korvai tala = Korvai.korvai tala mridangam

mridangam :: Korvai.StrokeMaps
mridangam = mempty
    { Korvai.instMridangam = Dsl.check $ Realize.instrument strokes patterns }
    where
    strokes =
        [ (ta <> ka <> din <> na, [k, o, o, k])
        , (ta <> ka <> ta <> ka <> din <> na, [k, t, k, o, o, k])
        , (ta <> ka <> ta <> ta <> ka, [k, t, k, k, o])
        ]
    patterns = Solkattu.check $ Realize.patterns $
        map (first Solkattu.PatternM) [(4, [p, k, o, n])]
        where Mridangam.Strokes {..} = Mridangam.notes
    Mridangam.Strokes {..} = Mridangam.notes
