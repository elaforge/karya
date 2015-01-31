module Util.Format4_test where
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Builder as Builder

import qualified Util.Format4 as Format
import Util.Format4 (_indented_, _indented, (<+/>), (</>), BreakType(..))
import Util.Test

import Global


test_all = do
    let f = Format.render "  " 10
    equal (f $ "a" <+/> "b") "a b\n"
    equal (f $ "a" </> "b") "ab\n"
    equal (f $ "1234" <+/> "6789" <+/> "ab") "1234 6789\nab\n"
    equal (f $ "12345" <> _indented_ ("f =" <> _indented "1234"))
        "12345\n  f = 1234\n"
    -- It always prefers to break on a lower indent.
    equal (f $ "12345" <> _indented_
            ("f" <> _indented "1"
            <+/> "g" <> _indented "1"
            <+/> "h" <> _indented "1"))
        "12345\n  f 1 g 1\n  h 1\n"
    -- But break on a higher indent if there's no choice.
    equal (f $ "12345" <> _indented_
            ("f =" <> _indented "12345"
            <+/> "g =" <> _indented "12345"))
        "12345\n  f =\n    12345\n  g =\n    12345\n"

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
section (indent, text, break) =
    Format.Section indent (Format.bFromText text) break

unsection :: Format.Section -> (Int, Text, BreakType)
unsection (Format.Section indent b break) = (indent, bText b, break)

sectionText :: Format.Section -> Text
sectionText = bText . Format.sectionB

bText :: Format.B -> Text
bText = Lazy.toStrict . Builder.toLazyText . Format.bBuilder
