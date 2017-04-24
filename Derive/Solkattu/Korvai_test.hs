-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
module Derive.Solkattu.Korvai_test where
import qualified Data.Text as Text

import Util.Test
import qualified Derive.Solkattu.Dsl as Dsl
import Derive.Solkattu.Dsl (ta, ka, din, na)
import qualified Derive.Solkattu.Korvai as Korvai
import qualified Derive.Solkattu.Mridangam as Mridangam
import qualified Derive.Solkattu.Realize as Realize
import qualified Derive.Solkattu.Score as Score
import qualified Derive.Solkattu.Solkattu as Solkattu
import qualified Derive.Solkattu.Tala as Tala

import Global


test_realize = do
    let f realize_patterns = second (first (map extract))
            . Korvai.realize Korvai.mridangam realize_patterns
            . korvai (Tala.Tala Tala.eka 2)
        extract (tempo, stroke) = pretty tempo <> ":" <> pretty stroke
        tkdn = cycle $ mconcat [ta, ka, din, na]
        p4s = cycle $ Dsl.pat 4
    equal (f False (take 4 tkdn)) $ Right
        ( map ("s0n4:"<>) (chars "kook")
        , "expected akshara 0, but at avartanam 1, akshara 1, matra 0"
        )
    equal (f False (take 2 p4s)) $
        Right (map ("s0n4:"<>) ["p4", "p4"], "")
    equal (f True (take 2 p4s)) $
        Right (map ("s0n4:"<>) (chars "pkonpkon"), "")
    equal (f False (Dsl.slower (take 1 p4s))) $ Right (["s-1n4:p4"], "")
    equal (f False (Dsl.nadai 2 (take 1 p4s))) $ Right (["s0n2:p4"], "")
    equal (f True (Dsl.nadai 2 (take 1 p4s))) $
        Right (map ("s0n2:"<>) (chars "pkon"), "")

chars :: [Char] -> [Text]
chars = map Text.singleton

korvai :: Tala.Tala -> Korvai.Sequence -> Korvai.Korvai
korvai tala = Korvai.korvai tala mridangam

mridangam :: Korvai.Instruments
mridangam = mempty
    { Korvai.inst_mridangam = Dsl.check $
        Mridangam.instrument Score.standard_strokes patterns
    }
    where
    patterns = Solkattu.check $ Realize.patterns [(4, [p, k, o, n])]
        where Mridangam.Strokes {..} = Mridangam.notes