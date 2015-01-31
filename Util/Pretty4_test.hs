-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Pretty4_test where
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Lazy as Lazy

import qualified Util.Pretty4 as Pretty
import Util.Test
import Global


-- Some hand tests for the pretty printer.  Not sure how to automate these...

render :: Pretty.Pretty a => Int -> a -> Text
render width = Lazy.toStrict . Pretty.render "  " width . Pretty.format

formats :: String -> Pretty.Doc
formats = Pretty.format

write = Text.IO.putStrLn

test_list = do
    let f width = Text.lines $ render width (Pretty.format ns)
        ns = [0 .. 4 :: Int]
    equal (f 10)
        [ "[ 0, 1, 2"
        , ", 3, 4"
        , "]"
        ]
    equal (f 16)
        [ "[ 0, 1, 2, 3, 4"
        , "]"
        ]
    -- TODO not going to work until I have a way to render differently
    -- depending on wrapping.
    -- equal (f 17) ["[ 0, 1, 2, 3, 4 ]"]

test_record = do
    let f = render 30 . Pretty.record (Pretty.text "Rec")
    -- fit on one line
    equal (f [("hi", formats "there")]) "Rec { hi = \"there\" }\n"
    -- -- TODO need special bracket support
    -- equal (f (replicate 2 ("hi", formats "there")))
    --     "Rec\n  { hi = \"there\", hi = \"there\" }\n"
    equal (f (replicate 3 ("hi", formats "there")))
        "Rec\n  { hi = \"there\", hi = \"there\"\n  , hi = \"there\"\n  }\n"

ntest_map = do
    let f width = render width val
        val = Map.fromList [(k, "1234" :: String) | k <- ['a'..'b']]
    write (f 30)
    write (f 15)
    write (f 10)

ntest_tuple = do
    write $ render 30 (("hi", "there", "really long string and stuff")
        :: (String, String, String))
    write $ Pretty.pretty ('a', 'b')
