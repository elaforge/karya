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
import qualified Solkattu.Notation as Notation
import qualified Solkattu.Realize as Realize
import qualified Solkattu.Sequence as Sequence
import qualified Solkattu.Solkattu as Solkattu
import qualified Solkattu.Tala as Tala

import Global


test_realize = do
    let f realizePatterns = fmap (first extract) . head
            . Korvai.realize Korvai.mridangam realizePatterns
            . korvai [] (Tala.Tala "eka" Tala.eka 2) . (:[])
        extract notes =
            [ pretty tempo <> ":" <> pretty stroke
            | (tempo, stroke) <- Sequence.tempoNotes notes
            ]
        tkdn = cycle $ mconcat [ta, ka, din, na]
        p4s = cycle $ Dsl.pat 4
    equal (f False (take 4 tkdn)) $ Right
        ( map ("s0n4:"<>) (chars "kook")
        , "4: should end on sam, actually ends on 1:1, or sam - 1"
        )
    equal (f False (take 2 p4s)) $ Right (map ("s0n4:"<>) ["p4", "p4"], "")
    equal (f True (take 2 p4s)) $ Right (map ("s0n4:"<>) (chars "pkonpkon"), "")
    equal (f False (Dsl.sd (take 1 p4s))) $ Right (["s-1n4:p4"], "")
    equal (f False (Dsl.nadai 2 (take 1 p4s))) $ Right (["s0n2:p4"], "")
    equal (f True (Dsl.nadai 2 (take 1 p4s))) $
        Right (map ("s0n2:"<>) (chars "pkon"), "")

test_realizeTechnique = do
    let f strokes = fmap extract . head
            . Korvai.realize Korvai.mridangam False
            . korvai strokes Tala.adi_tala . (:[])
        extract = Text.unwords . map pretty . Sequence.flattenedNotes . fst
    let strokes1 =
            [ (takatakadinna, [k, t, k, o, o, k])
            , (takatataka, [k, t, k, k, o])
            ]
            where Mridangam.Strokes {..} = Mridangam.notes
        takatakadinna = mconcat [ta, ka, ta, ka, din, na]
        takatataka = mconcat [ta, ka, ta, ta, ka]
    equal (f strokes1 takatakadinna) (Right "k t k o o k")
    equal (f strokes1 (Dsl.dropM 1 takatakadinna)) (Right "k k o o k")

    let strokes2 = [(ta_din, [k, t, k, k, t, o])]
            where Mridangam.Strokes {..} = Mridangam.notes
        ta_din = mconcat [ta, din, ta, Dsl.su (ta <> ka), din]
    equal (f strokes2 $ mconcat $ Notation.expand 3 1 ta_din) $
        Right $ "k k t o\ \ k k k t o\ \ k t k k t o"

chars :: [Char] -> [Text]
chars = map Text.singleton

korvai :: [(Korvai.Sequence, [Mridangam.SNote])] -> Tala.Tala
    -> [Korvai.Sequence] -> Korvai.Korvai
korvai strokes tala = Korvai.korvaiInferSections tala (makeMridangam strokes)

makeMridangam :: [(Korvai.Sequence, [Mridangam.SNote])] -> Korvai.StrokeMaps
makeMridangam strokes = mempty
    { Korvai.instMridangam = Dsl.check $
        Realize.instrument (takadinna : strokes) patterns
    }
    where
    takadinna = (ta <> ka <> din <> na, [k, o, o, k])
    patterns = Solkattu.check $ Realize.patterns $
        map (first Solkattu.PatternM) [(4, [p, k, o, n])]
        where Mridangam.Strokes {..} = Mridangam.notes
    Mridangam.Strokes {..} = Mridangam.notes
