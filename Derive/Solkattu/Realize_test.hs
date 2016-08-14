-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Solkattu.Realize_test where
import qualified Data.Map as Map
import qualified Data.Text as Text

import Util.Test
import qualified Derive.Solkattu.Dsl as Dsl
import Derive.Solkattu.Dsl (ta, di, __)
import qualified Derive.Solkattu.Mridangam as M
import qualified Derive.Solkattu.Realize as Realize
import qualified Derive.Solkattu.Solkattu as Solkattu
import Derive.Solkattu.Solkattu (Note(..), Sollu(..))

import Global


test_realize = do
    let f = (Text.unlines *** show_strokes)
            . Realize.realize True mridangam
        mridangam = Realize.Instrument smap M.defaults
        smap = Realize.StrokeMap $ Map.fromList
            [ ([Ta, Din], map Just [k, od])
            , ([Na, Din], map Just [n, od])
            , ([Ta], map Just [t])
            , ([Din, Ga], [Just od, Nothing])
            ]
        k = M.Valantalai M.Ki
        t = M.Valantalai M.Ta
        od = M.Both M.Thom M.Din
        n = M.Valantalai M.Nam
    equal (f [Rest, Sollu Ta Nothing, Rest, Rest, Sollu Din Nothing])
        (Right "__ k __ __ D")
    equal (f [Pattern 5, Rest, Sollu Ta Nothing, Sollu Din Nothing])
        (Right "k t k n o __ k D")
    equal (f [Sollu Ta Nothing, Sollu Ta Nothing]) (Right "t t")
    equal (f [Sollu Din Nothing, Sollu Ga Nothing]) (Right "D __")
    equal (f [Sollu Din Nothing, Rest, Sollu Ga Nothing]) (Right "D __ __")
    left_like (f [Sollu Din Nothing, Sollu Din Nothing]) "sequence not found"

    -- An explicit stroke will replace just that stroke.
    equal (f [Sollu Na Nothing, Sollu Din (Just (M.Valantalai M.Chapu))])
        (Right "n u")

show_strokes :: [Realize.Note M.Stroke] -> Text
show_strokes = Text.unwords . map pretty

test_stroke_map = do
    let f = fmap (\(Realize.StrokeMap smap) -> Map.toList smap)
            . Realize.stroke_map
        (k, t) = (Dsl.k, Dsl.t)
    equal (f []) (Right [])
    equal (f [(ta <> di, [k, t])])
        (Right [([Ta, Di],
            [Just $ M.Valantalai M.Ki, Just $ M.Valantalai M.Ta])])
    left_like (f (replicate 2 (ta <> di, [k, t]))) "duplicate StrokeMap keys"
    left_like (f [(ta <> di, [k])]) "have differing lengths"
    left_like (f [(Dsl.tang <> Dsl.ga, [Dsl.u, __, __])]) "differing lengths"
    left_like (f [(ta <> [Pattern 5], [k])]) "only have plain sollus"

test_format = do
    let f = Text.replace "\ESC[0m" "!" . Text.replace "\ESC[1m" "!"
            . Realize.format (Solkattu.adi_tala 4)
        n4 = [Dsl.k, Dsl.t, Dsl.__, Dsl.n]
    -- Emphasize every 4.
    equal (f n4) "!k! t _ n"
    equal (f (n4 <> n4)) "!k! t _ n !k! t _ n"
    -- Emphasis works in patterns.
    equal (f (n4 <> [Realize.Pattern 5] <> n4))
        "!k! t _ n !p5!------!--!k t _ !n!"
    -- Patterns are wrapped properly.
    equal (f (n4 <> [Realize.Pattern 5] <> n4 <> [Realize.Pattern 5]))
        "!k! t _ n !p5!------!--!k t _ !n! p5----\n\
        \!--!--"
