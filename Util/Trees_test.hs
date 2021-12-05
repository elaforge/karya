-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Trees_test where
import           Data.Tree (Tree(..))

import qualified Util.Trees as Trees

import           Util.Test


test_findAll :: Test
test_findAll = do
    let f = Trees.findAll (/=0)
    equal (f [Node 1 []]) [Node 1 []]
    equal (f [Node 1 [Node 0 []]]) [Node 1 [Node 0 []]]
    equal (f [Node 0 [Node 1 []]]) [Node 1 []]
    equal (f
        [ Node 0
          [ Node 0 [Node 3 []]
          , Node 1 [Node 4 []]
          , Node 0 [Node 5 []]
          ]
        ])
        [ Node 3 []
        , Node 1 [Node 4 []]
        , Node 5 []
        ]

test_find :: Test
test_find = do
    let f = Trees.find (/=0)
    equal (f [Node 1 []]) $ Just $ Node 1 []
    equal (f [Node 1 [Node 0 []]]) $ Just $ Node 1 [Node 0 []]
    equal (f [Node 0 [Node 1 []]]) $ Just $ Node 1 []
    equal (f
        [ Node 0
          [ Node 0 [Node 3 []]
          , Node 1 [Node 4 []]
          , Node 0 [Node 5 []]
          ]
        ])
        (Just $ Node 1 [Node 4 []])

test_filter :: Test
test_filter = do
    let f = Trees.filter (/=0)
    equal (f $ Node 1 []) $ Just $ Node 1 []
    equal (f $ Node 1 [Node 0 []]) $ Just $ Node 1 []
    equal (f $ Node 1 [Node 0 [Node 2 []], Node 3 []]) $
        Just $ Node 1 [Node 3 []]
