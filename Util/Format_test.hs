module Util.Format_test where
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Builder as Builder

import qualified Util.Format as Format
import Util.Format (indent_, indent, withIndent, (<+/>), (</>), BreakType(..))
import Util.Format (Doc(..))
import Util.Test

import Global


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

test_text = do
    let f = Format.text
    equal (f "hi\nthere") ("hi" <> Format.newline 1 <> "there")
    equal (f "hi\n\nthere") ("hi" <> Format.newline 2 <> "there")

test_shortForm = do
    let f = Format.render "  "
        sf = Format.shortForm
    let abc = sf "[abc]" ("a," </> "b," </> "c")
    equal (f 5 abc) "[abc]\n"
    equal (f 4 abc) "a,b,\nc\n"
    equal (f 10 $ "xyz" <> indent_ abc) "xyz [abc]\n"
    equal (f 8 $ "xyz" <> indent_ abc) "xyz\n  [abc]\n"
    equal (f 6 $ "xyz" <> indent_ abc) "xyz\n  a,b,\n  c\n"

test_flatten = do
    let f = flatten
    -- Interaction with indent.
    equal (f $ "a" <> indent_ "b" </> "c")
        [S 0 "a" Space [], S 1 "b" NoSpace [], S 0 "c" (Hard 1) []]
    equal (f $ "a" <> indent "b" </> "c")
        [S 0 "a" NoSpace [], S 1 "b" NoSpace [], S 0 "c" (Hard 1) []]
    -- withIndent only takes effect on the next break.
    equal (f $ "a" </> withIndent ("b" </> "c") </> "d")
        [ S 0 "a" NoSpace [], S 0 "b" NoSpace [], S 1 "c" NoSpace []
        , S 0 "d" (Hard 1) []
        ]

    equal (f $ ("a" <> indent_ "b") </> "c")
        [S 0 "a" Space [], S 1 "b" NoSpace [], S 0 "c" (Hard 1) []]
    equal (f $ "a" <> indent_ ("b" <> indent "c") <+/> "d")
        [ S 0 "a" Space [], S 1 "b" NoSpace [], S 2 "c" Space []
        , S 0 "d" (Hard 1) []
        ]
    equal (f $ "a" <> indent_ (indent "b") </> "c")
        [S 0 "a" Space [], S 2 "b" NoSpace [], S 0 "c" (Hard 1) []]

test_flatten_shortForm = do
    let f = flatten
    equal (f $ Format.shortForm "short" ("long" </> "form"))
        [ S 0 "short" (Hard 1)
            [S 0 "long" NoSpace [], S 0 "form" (Hard 1) []]
        ]

    equal (f $ "a" <> indent (Format.shortForm "short" ("long"</>"form")))
        [ S 0 "a" NoSpace []
        , S 1 "short" (Hard 1) [S 1 "long" NoSpace [], S 1 "form" (Hard 1) []]
        ]
    -- Merging a value distributes over both short and long forms.
    equal (f $ "a-" <> Format.shortForm "short" "long")
        [S 0 "a-short" (Hard 1) [S 0 "a-long" (Hard 1) []]]

    -- Doubly nested ShortForm, relies on propagating stateBreakIndent for
    -- the last text chunk to get the right indent.
    let doc = "a" <> indent_ ((Format.shortForm "sf1")
            ("b" <> indent_ ((Format.shortForm "sf2") "c")))
    equal (f doc)
        [ S 0 "a" Space []
        , S 1 "sf1" (Hard 1)
            [ S 1 "b" Space []
            , S 2 "sf2" (Hard 1)
                [ S 2 "c" (Hard 1) [] ]
            ]
        ]

    -- The last sub section has the same break as the section itself.
    equal (f $ Format.shortForm "sf" "a" </> "d")
        [ S 0 "sf" NoSpace
            [ S 0 "a" NoSpace []]
        , S 0 "d" (Hard 1) []
        ]

    let doc = Format.shortForm "short-form"
            ("before " <> withIndent "after") </> "tail"
    equal (f doc)
        [ S 0 "short-form" NoSpace [S 0 "before after" NoSpace []]
        , S 0 "tail" (Hard 1) []
        ]

    let doc = Format.shortForm "Record [abc, def] tail"
            ("Record " <> withIndent
                (Format.shortForm "[abc, def]" "[ abc, def ]"))
            </> "tail"
    equal (f doc)
        [ S 0 "Record [abc, def] tail" NoSpace
            [ S 0 "Record [abc, def]" NoSpace
                [ S 0 "Record [ abc, def ]" NoSpace [] ]
            ]
        , S 0 "tail" (Hard 1) []
        ]

    -- The indent doesn't take effect until after a break.
    let doc = Format.shortForm "short" $ Format.withIndent ("a" </> "b")
    equal (f doc)
        [ S 0 "short" (Hard 1)
            [ S 0 "a" NoSpace []
            , S 1 "b" (Hard 1) []
            ]
        ]

    -- Even when nested, "inner" has a indent of 0.
    let doc = Format.shortForm "outer" $
            Format.withIndent $ Format.shortForm "inner" ("a" </> "b")
    equal (f doc)
        [ S 0 "outer" (Hard 1)
            [ S 0 "inner" (Hard 1)
                [ S 0 "a" NoSpace []
                , S 1 "b" (Hard 1) []
                ]
            ]
        ]

    let doc = ShortForm "short form is too long" $
            Indented 1
                (ShortForm "short form"
                    (Text "[ " :+ Indented 1 "abc"
                        :+ Break NoSpace :+ ", " :+ Indented 1 "def"
                        :+ Break (Hard 1)))
            :+ Break Space :+ ", " :+ Indented 1 "tail" :+ Break (Hard 1)
    equal (f doc)
        [ S 0 "short form is too long" (Hard 1)
            [ S 0 "short form" Space
                [ S 0 "[ abc" NoSpace []
                -- first indent takes effect, second indent doesn't
                , S 1 ", def" (Hard 1) []
                ]
            , S 0 ", tail" (Hard 1) []
            ]
        ]

test_spanLine = do
    let f = extract . Format.spanLine 2 10 . map section
        extract (b, pre, post) = (b, map sectionText pre, map sectionText post)
    -- I get one line plus the next.
    equal (f [(0, "12345", Space), (2, "789ab", Space), (2, "z", Space)])
        (True, ["12345", "789ab"], ["z"])
    -- No space means it fits on one line.
    equal (f [(0, "12345", NoSpace), (2, "789ab", Space), (2, "z", Space)])
        (True, ["12345", "789ab", "z"], [])
    -- Indent makes it not fit.
    equal (f [(2, "12345", NoSpace), (2, "789ab", Space), (2, "z", Space)])
        (True, ["12345", "789ab"], ["z"])
    -- Hard break.
    equal (f [(0, "12345", Hard 1), (2, "789ab", Space), (2, "z", Space)])
        (False, ["12345"], ["789ab", "z"])

test_findBreak = do
    let f = extract . Format.findBreak . map section
        extract (pre, post) = (map sectionText pre, map sectionText post)
    equal (f [(0, "a", NoSpace), (1, "b", NoSpace)])
        ([], ["a", "b"])
    equal (f [(1, "a", NoSpace), (2, "b", NoSpace), (1, "c", NoSpace)])
        (["a", "b"], ["c"])
    equal (f [(1, "a", NoSpace), (0, "b", NoSpace), (1, "c", NoSpace)])
        (["a"], ["b", "c"])

section :: (Int, Text, BreakType) -> Format.Section
section (indent, text, break) = Format.Section
    { sectionIndent = indent
    , sectionB = Format.bFromText text
    , sectionSubs = []
    , sectionBreak = break
    }

data S = S Int Text BreakType [S] deriving (Show, Eq)

unsection :: Format.Section -> S
unsection (Format.Section indent b subs break) =
    S indent (bText b) break (map unsection subs)

sectionText :: Format.Section -> Text
sectionText = bText . Format.sectionB

bText :: Format.B -> Text
bText = Lazy.toStrict . Builder.toLazyText . Format.bBuilder

render :: Int -> Doc -> [String]
render width = map untxt . Text.lines . Lazy.toStrict
    . Format.render "  " width

flatten :: Doc -> [S]
flatten = map unsection . Format.flatten
