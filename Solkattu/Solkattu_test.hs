-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Solkattu_test where
import qualified Data.Text as Text

import Util.Test
import Global
import qualified Solkattu.Dsl as Dsl
import Solkattu.Dsl (__)
import Solkattu.DslSollu (ta, di, ki, thom)
import qualified Solkattu.Notation as Notation
import qualified Solkattu.S as S
import qualified Solkattu.Solkattu as Solkattu



test_matrasOf = do
    let f = Solkattu.matrasOf
        t s n = S.Tempo s n 1
    equal (f (t 0 4) ta) 1
    equal (f (t 0 6) ta) 1
    equal (f (t 1 4) ta) (1/2)
    equal (f (t 1 6) ta) (1/2)

    let ta4 = ta <> ta <> ta <> ta
    let f2 = f S.defaultTempo
    equal (f2 (Notation.takeM 0 ta4)) 0
    equal (f2 (Notation.takeM 1 ta4)) 1
    equal (f2 (Notation.takeM 4 ta4)) 4
    equal (f2 (Notation.takeM 5 ta4)) 4
    equal (f2 (Dsl.su (Notation.takeM 2 ta4))) 1

    equal (f2 (Notation.dropM 0 ta4)) 4
    equal (f2 (Notation.dropM 1 ta4)) 3
    equal (f2 (Notation.dropM 4 ta4)) 0
    equal (f2 (Notation.dropM 5 ta4)) 0

test_durationOf = do
    let f = Solkattu.durationOf S.defaultTempo
    let tas n = mconcat $ replicate n ta
    equal (f (tas 4)) 1
    equal (f (Dsl.nadai 7 (tas 7))) 1
    equal (f (Dsl.nadai 7 $ Notation.dropM 2 (tas 9))) 1

test_cancelKarvai = do
    let f = Text.unwords . map pretty . S.flattenedNotes
            . Solkattu.cancelKarvai . S.flatten
        k = Dsl.karvai
        group = Notation.group
    equal (f (ta <> thom)) "ta thom"
    equal (f (ta <> k thom)) "ta"
    equal (f (ta <> thom <> __)) "ta thom __"
    equal (f (ta <> k thom <> __)) "ta thom"
    equal (f (ta <> k thom <> di)) "ta di"
    equal (f (ta <> k thom <> group di)) "ta di"
    equal (f (ta <> k thom <> group __)) "ta thom"
    equal (f (ta <> group (k thom) <> __)) "ta thom"

test_vary = do
    let f notes = map (Text.unwords . map pretty) $
            Solkattu.vary
                (Solkattu.variations [Solkattu.standard, Solkattu.ascending])
                notes
    equal (f (ta <> di)) ["ta di"]
    equal (f (ta <> Dsl.p6 <> di <> Dsl.p6)) ["ta 6p di 6p"]
    equal (f (ta <> Dsl.p6 <> di <> Dsl.p6 <> Dsl.p6)) ["ta 5p di 6p 7p"]
    equal (f (Dsl.tri_ ta Dsl.p6 <> di <> Dsl.tri_ ki Dsl.p7))
        [ "5p ta 6p mid^ta 7p di 6p ki 7p mid^ki 8p"
        , "5p ta 6p mid^ta 7p di 5p ki 7p mid^ki 9p"
        ]

-- * utils

test_applyModifications = do
    let f = Solkattu.applyModifications (+)
    equal (f [] [1]) [1]
    let mods = [(0, 10), (2, 20)]
    equal (f mods [1]) [11]
    equal (f mods [1..2]) [11, 2]
    equal (f mods [1..3]) [11, 2, 23]
    equal (f mods [1..4]) [11, 2, 23, 4]

test_permuteFst = do
    let f = Solkattu.permuteFst (\x -> [x, x+1])
    equal (f ([] :: [(Int, Char)])) []
    equal (f [(0, 'a')]) [[(0, 'a')], [(1, 'a')]]
    equal (f [(0, 'a'), (10, 'b')])
        [ [(0, 'a'), (10, 'b')]
        , [(0, 'a'), (11, 'b')]
        , [(1, 'a'), (10, 'b')]
        , [(1, 'a'), (11, 'b')]
        ]
