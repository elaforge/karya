-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Format_test where
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Builder as Builder

import qualified Util.Format as Format
import Util.Format (indent_, indent, withIndent, (<+/>), (</>), BreakType(..))
import Util.Format (Doc(..))
import Util.Test

import Global


test_render :: Test
test_render = do
    let f = Format.render "  " 10
    equal (f $ "a" <+/> "b") "a b\n"
    equal (f $ "a" </> "b") "ab\n"
    equal (f $ "1234" <+/> "6789" <+/> "ab") "1234 6789\nab\n"
    equal (f $ "12345" <> indent_ ("f =" <> indent_ "1234"))
        "12345\n  f = 1234\n"

    -- It always prefers to break on a lower indent.
    equal (f $ "12345" <> indent_
            ("f" <> indent_ "1"
            <+/> "g" <> indent_ "1"
            <+/> "h" <> indent_ "1"))
        "12345\n  f 1 g 1\n  h 1\n"
        -- 12345
        --   f 1 g 1
        --   h 1

        -- 12345 f 1
        --   g 1 h 1

    -- But will break on a higher indent if there's no choice.
    equal (f $ "12345" <> indent_
            ("f =" <> indent_ "12345"
            <+/> "g =" <> indent_ "12345"))
        "12345\n  f =\n    12345\n  g =\n    12345\n"
    -- Multiple newlines preserved.  They are merged, where more newlines wins.
    equal (f $ "hi\n\nthere\n") "hi\n\nthere\n"
    equal (f $ "hi" <> Format.newline 1 <> Format.newline 1 <> "there")
        "hi\nthere\n"
    equal (f $ "hi" <> Format.newline 1 <> Format.newline 2 <> "there")
        "hi\n\nthere\n"
    equal (f $ "hi" <> Format.newline 2 <> Format.newline 1 <> "there")
        "hi\n\nthere\n"

    -- A dedent triggers a line break.
    equal (render 8 $ withIndent ("12345" </> "1234") </> "12")
        [ "12345"
        , "  1234"
        , "12"
        ]

test_render_double_indent :: Test
test_render_double_indent = do
    let f = map untxt . Text.lines . Lazy.toStrict . Format.render "  " 10
    let fmt = "{ " <> withIndent
            ( "[ " <> withIndent ("k1" </> ": v1")
            </> ", " <> withIndent ("k2" </> ": v2")
            )
    -- I get different sectionStartIndent and sectionEndIndent.
    equal (flatten fmt)
        [ S 0 1 "{ [ k1" NoSpace []
        , S 2 2 ": v1" NoSpace []
        , S 1 1 ", k2" NoSpace []
        , S 2 1 ": v2" (Hard 1) []
        ]
    equal (f fmt)
        [ "{ [ k1: v1"
        , "  , k2: v2"
        ]

test_render_full_width :: Test
test_render_full_width = do
    equal (render 8 $ "1234" </> "1234") ["12341234"]
    equal (render 8 $ "1234" </> "一二三四") ["1234", "一二三四"]

test_text :: Test
test_text = do
    let f = Format.text
    equal (f "hi\nthere") ("hi" <> Format.newline 1 <> "there")
    equal (f "hi\n\nthere") ("hi" <> Format.newline 2 <> "there")

test_shortForm :: Test
test_shortForm = do
    let f = Format.render "  "
        sf = Format.shortForm
    let abc = sf "[abc]" ("a," </> "b," </> "c")
    equal (f 5 abc) "[abc]\n"
    equal (f 4 abc) "a,b,\nc\n"
    equal (f 10 $ "xyz" <> indent_ abc) "xyz [abc]\n"
    equal (f 8 $ "xyz" <> indent_ abc) "xyz\n  [abc]\n"
    equal (f 6 $ "xyz" <> indent_ abc) "xyz\n  a,b,\n  c\n"
    equal (flatten $ sf "abc" "123" <> sf "def" "456")
        [S 0 0 "abcdef" (Hard 1) [S 0 0 "abc456" (Hard 1) []]]
        -- I would like this to be:
        -- [S 0 "abcdef" (Hard 1) [S 0 "123456" (Hard 1) []]]
        -- Unfortunately I can't figure out how to get it to work that way.

test_flatten :: Test
test_flatten = do
    let f = flatten
    -- Interaction with indent.
    equal (f $ "a" <> indent_ "b" </> "c")
        [S 0 0 "a" Space [], S 1 1 "b" NoSpace [], S 0 0 "c" (Hard 1) []]
    equal (f $ "a" <> indent "b" </> "c")
        [S 0 0 "a" NoSpace [], S 1 1 "b" NoSpace [], S 0 0 "c" (Hard 1) []]
    -- withIndent only takes effect on the next break.
    equal (f $ "a" </> withIndent ("b" </> "c") </> "d")
        [ S 0 0 "a" NoSpace [], S 0 0 "b" NoSpace [], S 1 1 "c" NoSpace []
        , S 0 0 "d" (Hard 1) []
        ]

    equal (f $ ("a" <> indent_ "b") </> "c")
        [S 0 0 "a" Space [], S 1 1 "b" NoSpace [], S 0 0 "c" (Hard 1) []]
    equal (f $ "a" <> indent_ ("b" <> indent "c") <+/> "d")
        [ S 0 0 "a" Space [], S 1 1 "b" NoSpace [], S 2 1 "c" Space []
        , S 0 0 "d" (Hard 1) []
        ]
    equal (f $ "a" <> indent_ (indent "b") </> "c")
        [S 0 0 "a" Space [], S 2 1 "b" NoSpace [], S 0 0 "c" (Hard 1) []]

toLines :: Lazy.Text -> [String]
toLines = map untxt . Text.lines . Lazy.toStrict

{-

Text "{ " :+
  Indented 1
    ((Text "[ " :+ Indented 1 (Text "k1" :+ Break NoSpace :+ Text ": v1"))
       :+
       Break NoSpace :+
          Text ", " :+
             Indented 1 (Text "k2" :+ Break NoSpace :+ Text ": v2"))

"{ " :+ Indent :+ "[ " :+ Indent :+ "k1" :+ Break :+ ": v1" :+ Dedent
       :+ Break :+ ", " :+ Indented :+ "k2" :+ Break :+ ": v2" :+ Dedent
    :+ Dedent

b0 p0       p1    b2p2     p2 b1p1   p1     b2p2     p2 p1
0    1      2               1        2               1 0
"{ " + "[ " + "k1" / ": v1" - / ", " + "k2" / ": v2" - - /
                  (01)       (22)          (11)          (21)

0 1 2 /  1/ 2 /
0     2   1   2
{ [ k1: v1
          , k2: v2

desired:
0 1 2 |  1| 2 |
01    22  11  22
{ [ k1: v1
          , k2: v2

got:
0 1 2 |  1| 2 |
02    21  12  20
{ [ k1: v1
          , k2: v2

0123456789a
{ [ k1: v1
  , k2: v2

{ [ k1
    : v1
  , k2
    : v2

{ [ long1: v1
  , k2: v2

{ [ long1
    : v1
  , k2: v2



[ S 0 2 "{ [ k1" NoSpace []
, S 2 1 ": v1" NoSpace []
, S 1 2 ", k2" NoSpace []
, S 2 0 ": v2" (Hard 1) []
]

-}

test_flatten_shortForm :: Test
test_flatten_shortForm = do
    let f = flatten
    equal (f $ Format.shortForm "short" ("long" </> "form"))
        [ S 0 0 "short" (Hard 1)
            [S 0 0 "long" NoSpace [], S 0 0 "form" (Hard 1) []]
        ]

    equal (f $ "a" <> indent (Format.shortForm "short" ("long"</>"form")))
        [ S 0 0 "a" NoSpace []
        , S 1 1 "short" (Hard 1) [S 1 1 "long" NoSpace []
        , S 1 1 "form" (Hard 1) []]
        ]
    -- Merging a value distributes over both short and long forms.
    equal (f $ "a-" <> Format.shortForm "short" "long")
        [S 0 0 "a-short" (Hard 1) [S 0 0 "a-long" (Hard 1) []]]

    -- Doubly nested ShortForm, relies on propagating stateBreakIndent for
    -- the last text chunk to get the right indent.
    let doc = "a" <> indent_ ((Format.shortForm "sf1")
            ("b" <> indent_ ((Format.shortForm "sf2") "c")))
    equal (f doc)
        [ S 0 0 "a" Space []
        , S 1 1 "sf1" (Hard 1)
            [ S 1 1 "b" Space []
            , S 2 2 "sf2" (Hard 1)
                [ S 2 2 "c" (Hard 1) [] ]
            ]
        ]

    -- The last sub section has the same break as the section itself.
    equal (f $ Format.shortForm "sf" "a" </> "d")
        [ S 0 0 "sf" NoSpace
            [ S 0 0 "a" NoSpace []]
        , S 0 0 "d" (Hard 1) []
        ]

    let doc = Format.shortForm "short-form"
            ("before " <> withIndent "after") </> "tail"
    equal (f doc)
        [ S 0 0 "short-form" NoSpace [S 0 1 "before after" NoSpace []]
        , S 0 0 "tail" (Hard 1) []
        ]

    let doc = Format.shortForm "Record [abc, def] tail"
            ("Record " <> withIndent
                (Format.shortForm "[abc, def]" "[ abc, def ]"))
            </> "tail"
    equal (f doc)
        [ S 0 0 "Record [abc, def] tail" NoSpace
            [ S 0 1 "Record [abc, def]" NoSpace
                [ S 0 0 "Record [ abc, def ]" NoSpace [] ]
            ]
        , S 0 0 "tail" (Hard 1) []
        ]

    -- The indent doesn't take effect until after a break.
    let doc = Format.shortForm "short" $ Format.withIndent ("a" </> "b")
    equal (f doc)
        [ S 0 0 "short" (Hard 1)
            [ S 0 0 "a" NoSpace []
            , S 1 1 "b" (Hard 1) []
            ]
        ]

    -- Even when nested, "inner" has a indent of 0.
    let doc = Format.shortForm "outer" $
            Format.withIndent $ Format.shortForm "inner" ("a" </> "b")
    equal (f doc)
        [ S 0 0 "outer" (Hard 1)
            [ S 0 1 "inner" (Hard 1)
                [ S 0 0 "a" NoSpace []
                , S 1 1 "b" (Hard 1) []
                ]
            ]
        ]

    let doc = ShortForm "short form is too long" $ mconcat
            [ indent
            , ShortForm "short form" $ mconcat
                [ "[ ", indent, "abc", dedent, Break NoSpace
                , ", ", indent, "def", dedent, Break (Hard 1)
                ]
            , dedent
            , Break Space, ", ", indent, "tail", dedent, Break (Hard 1)
            , dedent
            ]
        indent = Indent 1
        dedent = Indent (-1)
    equal (f doc)
        [ S 0 0 "short form is too long" (Hard 1)
            [ S 0 1 "short form" Space
                [ S 0 2 "[ abc" NoSpace []
                -- first indent takes effect, second indent doesn't
                , S 1 2 ", def" (Hard 1) []
                ]
            , S 0 1 ", tail" (Hard 1) []
            ]
        ]

    let doc = "xyz" <> indent_
            (Format.shortForm "[abc]" ("a," </> "b," </> "c"))
    equal (f doc)
        [ S 0 0 "xyz" Space []
        , S 1 1 "[abc]" (Hard 1)
            [ S 1 1 "a," NoSpace []
            , S 1 1 "b," NoSpace []
            , S 1 1 "c" (Hard 1) []
            ]
        ]

test_spanLine :: Test
test_spanLine = do
    let f = extract . Format.spanLine 2 6 . map section
        extract (_log, b, pre, post) =
            (b, map sectionText pre, map sectionText post)
    -- I get one line plus the next.
    equal (f [(0, 0, "123", Space), (2, 2, "567", Space), (2, 2, "z", Space)])
        (True, ["123", "567"], ["z"])
    -- No space means it fits on one line.
    equal (f [(0, 0, "123", NoSpace), (2, 2, "456", Space), (2, 2, "z", Space)])
        (True, ["123", "456", "z"], [])
    -- Indent makes it not fit.
    equal (f [(1, 1, "123", NoSpace), (2, 2, "456", Space), (2, 2, "z", Space)])
        (True, ["123", "456"], ["z"])
    -- Hard break.
    equal (f [(0, 0, "123", Hard 1), (2, 2, "456", Space), (2, 2, "z", Space)])
        (False, ["123"], ["456", "z"])

test_findBreak :: Test
test_findBreak = do
    let f = extract . Format.findBreak . map section
        extract (pre, post) = (map sectionText pre, map sectionText post)
    equal (f [(0, 0, "a", NoSpace), (1, 1, "b", NoSpace)])
        ([], ["a", "b"])
    equal (f [(1, 1, "a", NoSpace), (2, 2, "b", NoSpace), (1, 1, "c", NoSpace)])
        (["a", "b"], ["c"])
    equal (f [(1, 1, "a", NoSpace), (0, 0, "b", NoSpace), (1, 1, "c", NoSpace)])
        (["a"], ["b", "c"])

section :: (Int, Int, Text, BreakType) -> Format.Section
section (startIndent, endIndent, text, break) = Format.Section
    { sectionStartIndent = startIndent
    , sectionEndIndent = endIndent
    , sectionB = Format.bFromText text
    , sectionSubs = []
    , sectionBreak = break
    }

data S = S Int Int Text BreakType [S] deriving (Show, Eq)

unsection :: Format.Section -> S
unsection (Format.Section startIndent endIndent b subs break) =
    S startIndent endIndent (bText b) break (map unsection subs)

sectionText :: Format.Section -> Text
sectionText = bText . Format.sectionB

bText :: Format.B -> Text
bText = Lazy.toStrict . Builder.toLazyText . Format.bBuilder

render :: Int -> Doc -> [String]
render width = map untxt . Text.lines . Lazy.toStrict
    . Format.render "  " width

flatten :: Doc -> [S]
flatten = map unsection . Format.flatten
