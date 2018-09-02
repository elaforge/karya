-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
module Solkattu.Korvai_test where
import qualified Data.Text as Text

import qualified Solkattu.Dsl as Dsl
import qualified Solkattu.Instrument.Mridangam as Mridangam
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Notation as Notation
import qualified Solkattu.Realize as Realize
import qualified Solkattu.S as S
import qualified Solkattu.Score.Mridangam2018 as Mridangam2018
import qualified Solkattu.Score.Solkattu2018 as Solkattu2018
import qualified Solkattu.SolkattuGlobal as SolkattuGlobal
import Solkattu.SolkattuGlobal (ta, ka, din, na)
import qualified Solkattu.Tala as Tala

import Global
import Util.Test


test_realize = do
    let f = fmap (first extract) . head
            . Korvai.realize Korvai.mridangam
            . korvai [] (Tala.Tala "eka" Tala.eka 2) . (:[])
        extract notes =
            [ pretty tempo <> ":" <> pretty stroke
            | (tempo, stroke) <- S.tempoNotes notes
            ]
        tkdn n = mconcat $ take n $ cycle [ta, ka, din, na]
        p5s n = mconcat $ replicate n Dsl.p5
    equal (f (tkdn 4)) $ Right
        ( map ("s0n4:"<>) (chars "kook")
        , "4: should end on sam, actually ends on 1:1, or sam - 1"
        )
    equal (f (Dsl.nadai 5 (p5s 2))) $
        Right (map ("s0n5:"<>) (chars "ktknoktkno"), "")
    equal (f (Dsl.sd (tkdn 8))) $
        Right (map ("s-1n4:"<>) (chars "kookkook"), "")

test_realizeTechnique = do
    let f strokes = fmap extract . head
            . Korvai.realize Korvai.mridangam
            . korvai strokes Tala.adi_tala . (:[])
        extract = Text.unwords . map pretty . S.flattenedNotes . fst
    let strokes1 =
            [ (takatakadinna, mconcat [k, t, k, o, o, k])
            , (takatataka, mconcat [k, t, k, k, o])
            ]
            where Mridangam.Strokes {..} = Mridangam.notes
        takatakadinna = mconcat [ta, ka, ta, ka, din, na]
        takatataka = mconcat [ta, ka, ta, ta, ka]
    equal (f strokes1 takatakadinna) (Right "k t k o o k")
    equal (f strokes1 (Dsl.dropM 1 takatakadinna)) (Right "k k o o k")

    let strokes2 = [(ta_din, mconcat [k, t, k, k, t, o])]
            where Mridangam.Strokes {..} = Mridangam.notes
        ta_din = mconcat [ta, din, ta, Dsl.su (ta <> ka), din]
    equal (f strokes2 $ mconcat $ Notation.expand 3 1 ta_din) $
        Right $ "k k t o\ \ k k k t o\ \ k t k k t o"

test_korvaiInstruments = do
    let f = map fst . Korvai.korvaiInstruments
    equal (f Solkattu2018.misra_to_mohra1a)
        ["kendang tunggal", "konnakol", "mridangam"]
    -- No konnakol!
    equal (f Mridangam2018.e_323_1) ["mridangam"]

chars :: [Char] -> [Text]
chars = map Text.singleton

korvai :: SolkattuGlobal.StrokeMap Mridangam.Stroke
    -> Tala.Tala -> [Korvai.Sequence] -> Korvai.Korvai
korvai strokes tala = Korvai.korvaiInferSections tala (makeMridangam strokes)

makeMridangam :: SolkattuGlobal.StrokeMap Mridangam.Stroke -> Korvai.StrokeMaps
makeMridangam strokes = mempty
    { Korvai.smapMridangam =
        Realize.strokeMap Mridangam.defaultPatterns (defaults ++ strokes) }
    where
    defaults = map (bimap mconcat mconcat)
        [ ([ta, ka, din, na], [k, o, o, k])
        -- , ([Dsl.pat 4], [p, k, o, n])
        ]
        where Mridangam.Strokes {..} = Mridangam.notes
