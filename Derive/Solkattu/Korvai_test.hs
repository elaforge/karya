-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
module Derive.Solkattu.Korvai_test where
import qualified Data.Text as Text

import Util.Test
import qualified Derive.Solkattu.Dsl as Dsl
import Derive.Solkattu.DslSollu (ta, ka, din, na)
import qualified Derive.Solkattu.Instrument.Mridangam as Mridangam
import qualified Derive.Solkattu.Korvai as Korvai
import qualified Derive.Solkattu.Realize as Realize
import qualified Derive.Solkattu.Solkattu as Solkattu
import qualified Derive.Solkattu.Tala as Tala

import Global


test_realize = do
    let f realize_patterns = second (first (map extract)) . head
            . Korvai.realize Korvai.mridangam realize_patterns
            . korvai (Tala.Tala "eka" Tala.eka 2) . (:[])
        extract (tempo, stroke) = pretty tempo <> ":" <> pretty stroke
        tkdn = cycle $ mconcat [ta, ka, din, na]
        p4s = cycle $ Dsl.pat 4
    equal (f False (take 4 tkdn)) $ Right
        ( map ("s0n4:"<>) (chars "kook")
        , "korvai should end on or before sam: avartanam 1, akshara 1, matra 0"
        )
    equal (f False (take 2 p4s)) $ Right (map ("s0n4:"<>) ["p4", "p4"], "")
    equal (f True (take 2 p4s)) $ Right (map ("s0n4:"<>) (chars "pkonpkon"), "")
    equal (f False (Dsl.sd (take 1 p4s))) $ Right (["s-1n4:p4"], "")
    equal (f False (Dsl.nadai 2 (take 1 p4s))) $ Right (["s0n2:p4"], "")
    equal (f True (Dsl.nadai 2 (take 1 p4s))) $
        Right (map ("s0n2:"<>) (chars "pkon"), "")

chars :: [Char] -> [Text]
chars = map Text.singleton

korvai :: Tala.Tala -> [Korvai.Sequence] -> Korvai.Korvai
korvai tala = Korvai.korvai tala mridangam

mridangam :: Korvai.StrokeMaps
mridangam = mempty
    { Korvai.inst_mridangam = Dsl.check $
        Mridangam.instrument [(ta <> ka <> din <> na, [k, o, o, k])] patterns
    }
    where
    Mridangam.Strokes {..} = Mridangam.notes
    patterns = Solkattu.check $ Realize.patterns $
        map (first Solkattu.PatternM) [(4, [p, k, o, n])]
        where Mridangam.Strokes {..} = Mridangam.notes
