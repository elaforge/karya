-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Pretty_test where
import qualified Data.Map as Map

import qualified Util.Pretty as Pretty
import qualified Perform.RealTime as RealTime
import Global


-- Some hand tests for the pretty printer.  Not sure how to automate these...

pformat :: (Pretty.Pretty a) => a -> IO ()
pformat = putStrLn . Pretty.render 30 . Pretty.format

format :: String -> Pretty.Doc
format = Pretty.format

str :: String -> String
str = id

test_list = do
    let max = 10 :: Int
        pp :: (Pretty.Pretty a) => a -> IO ()
        pp val = forM_ [40, 35, 30, 15] $ \width ->
            putStrLn $ Pretty.render width $ Pretty.format val
    putStrLn $ Pretty.pretty [0..max]
    pp [0..max]

test_record = do
    let num = 30 :: Int
    let f = pformat . Pretty.record (Pretty.text "Rec")
    -- fit on one line
    f [("hi", format "there")]
    f (replicate 3 ("hi", format "there"))
    f (replicate 2 ("label", Pretty.format [0..num]))
    f [(str "lab", format "short"), (str "label", Pretty.format [0..num])]
    f (replicate 2 (str "really long label",
        format "really long data value"))

test_map = do
    let f = pformat . Map.fromList
    f [(k, "abcdef" :: String) | k <- ['a'..'d']]

test_tuple = do
    pformat (("hi", "there", "really long string and stuff")
        :: (String, String, String))
    pformat (RealTime.seconds 3, RealTime.seconds 4)
    putStrLn $ Pretty.pretty ('a', 'b')
