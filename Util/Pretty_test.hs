-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveGeneric #-}
module Util.Pretty_test where
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy

import qualified GHC.Generics as Generics

import           Util.Format (withIndent, (<+>), (<//>), (</>))
import qualified Util.Pretty as Pretty

import           Global
import           Util.Test


test_list :: Test
test_list = do
    let f width = render width (Pretty.format ns)
        ns = [0 .. 4 :: Int]
    equal (f 10)
        [ "[ 0, 1, 2"
        , ", 3, 4"
        , "]"
        ]
    equal (f 16) ["[0, 1, 2, 3, 4]"]

test_record :: Test
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

    -- nested record
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

test_map :: Test
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

test_tuple :: Test
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

test_delimitedList :: Test
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

test_formatG :: Test
test_formatG = do
    let rec1 = Rec1 1 2.5 'c' (Map.fromList [("hi", "there")])
    let rec2 = Rec2 rec1 "hi"
    equal_fmt id (renderText 25 rec1)
        "Rec1\n\
        \  { int = 1, double = 2.5\n\
        \  , char = 'c'\n\
        \  , map = {\"hi\": \"there\"}\n\
        \  }\n"
    equal_fmt id (renderText 25 rec2)
        "Rec2\n\
        \  { sub =\n\
        \    Rec1\n\
        \      { int = 1\n\
        \      , double = 2.5\n\
        \      , char = 'c'\n\
        \      , map =\n\
        \        {\"hi\": \"there\"}\n\
        \      }\n\
        \  , string = \"hi\"\n\
        \  }\n"

_test_bug :: Test
_test_bug = do
    let rec1 = Rec1 1 2.5 'c' (Map.fromList [("hi", "there")])
    -- TODO: bug:
    -- should wrap after "", not after Rec1
    equal_fmt id (renderText 60 ("" :: String, rec1))
        "( \"\"\n\
        \, Rec1\n\
        \    { int = 1, double = 2.5, char = 'c'\n\
        \    , map = {\"hi\": \"there\"}\n\
        \    }\n\
        \)\n"
    -- parens are misaligned
    equal_fmt id (renderText 60 [("" :: String, prettyRec1 rec1)])
        "[ ( \"\"\n\
        \  , Rec1\n\
        \      { int = 1, double = 2.5, char = 'c'\n\
        \      , map = {\"hi\": \"there\"}\n\
        \      }\n\
        \  )\n\
        \]\n"
    where
    prettyRec1 :: Rec1 -> Pretty.Doc
    prettyRec1 (Rec1 int double char map) = Pretty.record "Rec1"
        [ ("rec1Int", Pretty.format int)
        , ("rec1Double", Pretty.format double)
        , ("rec1Char", Pretty.format char)
        , ("rec1Map", Pretty.format map)
        ]

data Rec1 = Rec1 {
    rec1Int :: Int
    , rec1Double :: Double
    , rec1Char :: Char
    , rec1Map :: Map String String
    } deriving (Show, Generics.Generic)

instance Pretty.Pretty Rec1 where
    format = Pretty.formatGCamel

data Rec2 = Rec2 {
    rec2Sub :: Rec1
    , rec2String :: String
    } deriving (Show, Generics.Generic)

instance Pretty.Pretty Rec2 where
    format = Pretty.formatGCamel


-- * util

render :: Pretty a => Int -> a -> [String]
render width = map untxt . Text.lines . renderText width

renderText :: Pretty a => Int -> a -> Text
renderText width = Lazy.toStrict . Pretty.render "  " width . Pretty.format

formats :: String -> Pretty.Doc
formats = Pretty.format
