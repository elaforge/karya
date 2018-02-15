-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.India.Gamakam6_test where
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.C.India.Gamakam6 as Gamakam
import Derive.C.India.Gamakam6
       (Call(..), Alias(..), Movement(..), Pitch(..), From(..), Duration(..))
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch
import Global
import Types


test_sequence = do
    let run c = derive_tracks e_nns $ pitch_gamakam_note
                [(0, "4c"), (4, "4d"), (6, "4e")]
                [(4, c)]
                [(0, 4), (4, 2), (6, 2)]
        output nns = ([[(0, NN.c4), (4, NN.c4)], nns, [(6, NN.e4)]], [])
    equal (run "--|") (output [(4, NN.d4), (6, NN.d4)])
    equal (run "!=") (output [(4, NN.c4), (6, NN.c4)])
    equal (run "!0") (output [(4, NN.c4), (5, NN.cs4), (6, NN.d4)])
    equal (run "!-1") (output [(4, NN.c4), (6, NN.c4)])
    equal (run "!C=") (output [(4, NN.d4), (6, NN.d4)])
    equal (run "!]10") (output [(4, 64), (5, 63), (6, 62)])
    equal (run "!]+0") (output [(4, 63), (5, 62.5), (6, 62)])
    equal (run "!]^0") (output [(4, 62.5), (5, 62.25), (6, 62)])
    equal (run "!]a0") (output [(4, 60), (5, 61), (6, 62)])
    equal (run "!]a=") (output [(4, NN.c4), (6, NN.c4)])
    equal (run "!]<0") (output [(4, NN.c4), (5, NN.cs4), (6, NN.d4)])

e_nns :: Score.Event -> [(RealTime, Pitch.NoteNumber)]
e_nns e = drop_last_dups fst $ takeWhile ((<= Score.event_end e) . fst) $
    DeriveTest.e_nns_literal e

drop_last_dups :: Eq k => (a -> k) -> [a] -> [a]
drop_last_dups key = go
    where
    go (x:xs)
        | all ((== key x) . key) xs = [x]
        | otherwise = x : go xs
    go [] = []

test_parse = do
    let f = Gamakam.parse
    equal (f "") $ Right []
    equal (f "=") $ Right
        [Right $ Move (Movement (Pitch From 0 0) (Relative 1))]
    equal (f "<=>") $ Right
        [ Right $ Move $ Movement (Pitch Prev 0 0) (Relative 1)
        , Right $ Move $ Movement (Pitch From 0 0) (Relative 1)
        , Right $ Move $ Movement (Pitch Next 0 0) (Relative 1)
        ]
    equal (f "]0a+") $ Right
        [ Right $ SetFrom (Pitch Current 0 0)
        , Right $ Move $ Movement (Pitch Current (-1) 1) (Relative 1)
        ]
    equal (f "]<>") $ Right
        [ Right $ SetFrom (Pitch Prev 0 0)
        , Right $ Move $ Movement (Pitch Next 0 0) (Relative 1)
        ]
    equal (f "]<0") $ Right
        [ Right $ SetFrom (Pitch Prev 0 0)
        , Right $ Move $ Movement (Pitch Current 0 0) (Relative 1)
        ]
    equal (f "<^<") $ Right
        [ Right $ Move $ Movement (Pitch Prev 0 0.5) (Relative 1)
        , Right $ Move $ Movement (Pitch Prev 0 0) (Relative 1)
        ]
    equal (f "#^.#v.") $ Right
        [ Right $ Move $ Movement (Pitch From 0 0.5) (Relative 0.5)
        , Right $ Move $ Movement (Pitch From 0 (-0.5)) (Relative 0.5)
        ]
    equal (f "-5__") $ Right
        [ Right $ Move $ Movement (Pitch Current (-5) 0) (Relative 3)
        ]
    equal (f "-5..") $ Right
        [ Right $ Move $ Movement (Pitch Current (-5) 0) (Relative 0.25)
        ]
    equal (f "=n=") $ Right
        [ Right $ Move $ Movement (Pitch From 0 0) (Relative 1)
        , Left (Alias 'n')
        , Right $ Move $ Movement (Pitch From 0 0) (Relative 1)
        ]

-- * util

derive_tracks :: (Score.Event -> a) -> [UiTest.TrackSpec]
    -> ([a], [Text])
derive_tracks extract = DeriveTest.extract extract
    . DeriveTest.derive_tracks
        "import india.gamakam6 | transition=1 | dyn-transition=1"

pitch_gamakam_note :: [(ScoreTime, Text)] -> [(ScoreTime, Text)]
    -> [(ScoreTime, ScoreTime)] -> [UiTest.TrackSpec]
pitch_gamakam_note pitches gamakams notes =
    [ ("*", [(s, 0, p) | (s, p) <- pitches])
    , ("t-nn | gamak", [(s, 0, p) | (s, p) <- gamakams])
    , (">", [(s, d, "") | (s, d) <- notes])
    ]
