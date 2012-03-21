module Util.Pretty_test where
import qualified Data.Map as Map
import qualified Text.PrettyPrint as PP

import Util.Control
import Util.Pretty
import qualified Perform.RealTime as RealTime


-- Some hand tests for the pretty printer.  Not sure how to automate these...

pprint :: (Pretty a) => a -> IO ()
pprint = putStrLn . render 30 . format

test_list = do
    let max = 10 :: Int
        pp :: Pretty a => a -> IO ()
        pp val = forM_ [40, 35, 30, 15] $ \width ->
            putStrLn $ render width $ format val
    putStrLn $ pretty [0..max]
    pp [0..max]
    -- pp "some string"

test_record = do
    let num = 30 :: Int
    let f = pprint . record (PP.text "Rec")
    -- fit on one line
    f [("hi", format "there")]
    f (replicate 3 ("hi", format "there"))
    f (replicate 2 ("label", format [0..num]))
    f [("lab", format "short"), ("label", format [0..num])]
    f (replicate 2 ("really long label", format "really long data value"))

test_map = do
    let f = pprint . Map.fromList
    f [(k, "abcdef") | k <- ['a'..'z']]

test_tuple = do
    pprint ("hi", "there", "really long string and stuff")
    pprint (RealTime.seconds 3, RealTime.seconds 4)
    putStrLn $ pretty ('a', 'b')
