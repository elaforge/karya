module Util.Format_test where
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Builder as Builder

import qualified Util.Format as Format
import Util.Format (indented, (<+/>), (</>), BreakType(..))
import Util.Test

import Global


test_render = do
    let f = Format.render "  " 10
    equal (f $ "a" <+/> "b") "a b\n"
    equal (f $ "a" </> "b") "ab\n"
    equal (f $ "1234" <+/> "6789" <+/> "ab") "1234 6789\nab\n"
    equal (f $ "12345" <+/> indented ("f =" <+/> indented "1234"))
        "12345\n  f = 1234\n"
    -- It always prefers to break on a lower indent.
    equal (f $ "12345" <+/> indented
            ("f" <+/> indented "1"
            <+/> "g" <+/> indented "1"
            <+/> "h" <+/> indented "1"))
        "12345\n  f 1 g 1\n  h 1\n"
    -- But break on a higher indent if there's no choice.
    equal (f $ "12345" <+/> indented
            ("f =" <+/> indented "12345"
            <+/> "g =" <+/> indented "12345"))
        "12345\n  f =\n    12345\n  g =\n    12345\n"
    equal (f $ "hi\n\nthere\n") "hi\n\nthere\n"

test_shortForm = do
    let f = Format.render "  "
        sf = Format.shortForm
    let abc = sf "[abc]" ("a," </> "b," </> "c")
    equal (f 5 abc) "[abc]\n"
    equal (f 4 abc) "a,b,\nc\n"
    equal (f 10 $ "xyz" <+/> indented abc) "xyz [abc]\n"
    equal (f 8 $ "xyz" <+/> indented abc) "xyz\n  [abc]\n"
    equal (f 6 $ "xyz" <+/> indented abc) "xyz\n  a,b,\n  c\n"

test_flatten = do
    let f = map unsection . Format.flatten
    -- Interaction with 'indented'.
    equal (f $ "a" <+/> indented "b" </> "c")
        [S 0 "a" Space [], S 1 "b" NoSpace [], S 0 "c" Hard []]
    equal (f $ ("a" <+/> indented "b") </> "c")
        [S 0 "a" Space [], S 1 "b" NoSpace [], S 0 "c" Hard []]
    equal (f $ "a" <+/> indented ("b" </> indented "c") <+/> "d")
        [ S 0 "a" Space [], S 1 "b" NoSpace [], S 2 "c" Space []
        , S 0 "d" Hard []
        ]
    equal (f $ "a" <+/> indented (indented "b") </> "c")
        [S 0 "a" Space [], S 2 "b" NoSpace [], S 0 "c" Hard []]
    -- <> merges text, and inherits the highest indent.  This isn't really
    -- on purpose, but is the easiest to implement.
    equal (f $ "a" <> indented "b" <> "c")
        [S 1 "abc" Hard []]

    -- ShortForm
    equal (f $ Format.shortForm "short" ("long" </> "form"))
        [ S 0 "short" Hard
            [S 0 "long" NoSpace [], S 0 "form" Hard []]
        ]

    equal (f $ "a" </> indented (Format.shortForm "short" ("long" </> "form")))
        [ S 0 "a" NoSpace []
        , S 1 "short" Hard [S 1 "long" NoSpace [], S 1 "form" Hard []]
        ]

    -- Doubly nested ShortForm, relies on propagating stateBreakIndent for
    -- the last text chunk to get the right indent.
    let doc = "a" <+/> indented ((Format.shortForm "sf1")
            ("b" <+/> indented ((Format.shortForm "sf2") "c")))
    equal (f doc)
        [ S 0 "a" Space []
        , S 1 "sf1" Hard
            [ S 1 "b" Space []
            , S 2 "sf2" Hard
                [ S 2 "c" Hard [] ]
            ]
        ]

    -- The last sub section has the same break as the section itself.
    equal (f $ Format.shortForm "sf" "a" </> "d")
        [ S 0 "sf" NoSpace
            [ S 0 "a" NoSpace []]
        , S 0 "d" Hard []
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
    equal (f [(0, "12345", Hard), (2, "789ab", Space), (2, "z", Space)])
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
