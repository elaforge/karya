-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Pretty_test where
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy

import Util.Format ((</>), (<//>), (<+>), withIndent)
import qualified Util.Pretty as Pretty
import Util.Test

import Global


render :: Pretty a => Int -> a -> [String]
render width = map untxt . Text.lines . Lazy.toStrict
    . Pretty.render "  " width . Pretty.format

formats :: String -> Pretty.Doc
formats = Pretty.format


test_list = do
    let f width = render width (Pretty.format ns)
        ns = [0 .. 4 :: Int]
    equal (f 10)
        [ "[ 0, 1, 2"
        , ", 3, 4"
        , "]"
        ]
    equal (f 16) ["[0, 1, 2, 3, 4]"]

test_record = do
    let f = render 32 . make
        make = Pretty.record "Rec"

    -- fit on one line
    equal (f [("hi", formats "there")])
        ["Rec { hi = \"there\" }"]
    equal (f (replicate 2 ("hi", formats "there")))
        [ "Rec"
        , "  { hi = \"there\", hi = \"there\" }"
        ]
    equal (f (replicate 3 ("hi", formats "there")))
        [ "Rec"
        , "  { hi = \"there\", hi = \"there\""
        , "  , hi = \"there\""
        , "  }"
        ]

    equal (f $ replicate 2 ("sub", make (replicate 2 ("field", "value"))))
        [ "Rec"
        , "  { sub ="
        , "    Rec"
        , "      { field = value"
        , "      , field = value"
        , "      }"
        , "  , sub ="
        , "    Rec"
        , "      { field = value"
        , "      , field = value"
        , "      }"
        , "  }"
        ]

    let record = Pretty.record ("Record " <> Pretty.format t)
            [("field1", "val1")]
        t = ["abc", "def"] :: [String]
    equal (render 30 [record, "tail"])
        [ "[ Record [\"abc\", \"def\"]"
        , "    { field1 = val1 }"
        , ", tail"
        , "]"
        ]

test_map = do
    let f width = render width val
        val = Map.fromList [(k, "1234" :: String) | k <- ['a'..'b']]
    equal (f 30) ["{'a': \"1234\", 'b': \"1234\"}"]
    equal (f 15)
        [ "{ 'a': \"1234\""
        , ", 'b': \"1234\""
        , "}"
        ]
    equal (f 10)
        [ "{ 'a'"
        , "  : \"1234\""
        , ", 'b'"
        , "  : \"1234\""
        , "}"
        ]

    let doc = ("{" <+> withIndent ">u")
            </> ("," <+> withIndent sub)
            <//> "}\n"
        sub = ("{" <+> withIndent (">i" </> (": >kontakt/wayang-isep")))
            </> ("," <+> withIndent
                (">u" </> (": >kontakt/wayang-umbang")))
            <//> "}\n"
    equal (render 30 doc)
        [ "{ >u"
        , ", { >i: >kontakt/wayang-isep"
        , "  , >u: >kontakt/wayang-umbang"
        , "  }"
        , "}"
        ]

test_tuple = do
    equal (Pretty.pretty ('a', 'b')) "('a', 'b')"
    let t1 :: (String, String, String)
        t1 = ("hi", "there", "really long string and stuff")
    equal (render 15 t1)
        [ "( \"hi\", \"there\""
        , ", \"really long string and stuff\""
        , ")"
        ]
    let t2 :: (String, [String])
        t2 = ("hi there", words "it wraps")
    equal (render 10 t2)
        [ "( \"hi there\""
        , ", [ \"it\""
        , "  , \"wraps\""
        , "  ]"
        , ")"
        ]

test_delimitedList = do
    let f = Pretty.delimitedList False '[' ']'
    -- shortForm omits the newlines and spaces.
    equal (unlines $ render 8 $ f ["a", "b"]) "[a, b]\n"
    equal (render 8 $ f ["abc", "def"])
        [ "[ abc"
        , ", def"
        , "]"
        ]

    equal (render 8 $ f ["hi", f ["nest", "wrap"]])
        [ "[ hi"
        , ", [ nest"
        , "  , wrap"
        , "  ]"
        , "]"
        ]

    equal (render 8 $ f ["hi", f ["nest", "wrap"], "x"])
        [ "[ hi"
        , ", [ nest"
        , "  , wrap"
        , "  ]"
        , ", x"
        , "]"
        ]

    equal (render 15 $ Pretty.delimitedList False '[' ']'
            [Pretty.record "Record" [("field1", "val1")]])
        [ "[ Record"
        , "    { field1 ="
        , "      val1"
        , "    }"
        , "]"
        ]
