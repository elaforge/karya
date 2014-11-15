-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Format3_test where
import qualified Data.Text.Lazy as Lazy

import qualified Util.Format3 as Format
import Util.Format3 ((</>), (<+/>), indented, break)
import Util.Test

import Global


ntest_render = do
    let f = Lazy.toStrict . Format.render "  " 10
    equal (f "") ""
    equal (f $ "" <> "") ""
    -- "c = d" triggers two breaks.
    equal (f $ "Rec" <> indented ("a = b" <+/> "c = d") <+/> "Record2")
        "Rec\n  a = b\n  c = d\nRecord2"

    -- NoSpace break.
    equal (f $ "123" </> "456" </> "789" </> "0123")
        "123456789\n0123"

    -- Multiple indent levels.
    equal (f $ "Record" <> indented ("ab =" <> indented "blah") <+/> "Record2")
        "Record\n  ab =\n    blah\nRecord2"

    -- Multiple indent levels on one line.
    equal (f $ "Record" <> indented ("a =" <> indented "b" <+/> "c") <+/> "Rec2")
        "Record\n  a = b c\nRec2"

    -- Line too long, can't be broken.
    equal (f $ "Record" <> indented "0123456789" <+/> "R")
        "Record\n  0123456789\nR"

    equal (f $ ("Record" <> indented ("01234" <+/> "56789") <+/> "R"))
        "Record\n  01234\n  56789\nR"
    -- Embedded newline forces a break.
    equal (f $ ("Record" <> indented "01234\n56789") <+/> "R")
        "Record\n  01234\n  56789\nR"

    -- Multiple indent levels can go on line if they all fit.
    equal (f $ "A" <+/> indented ("B" <+/> indented "C") <+/> "D")
        "A B C D"

ntest_render2 = do
    let f = Lazy.toStrict . Format.render "  " 30
    pprint (f $ "Rec" <> indented
            ("{ hi =" <> indented "there"
            </> ", hi =" <> indented "there"
            </> ", hi =" <> indented "there"
            </> "}"))

{-
    123456789012345678901234567890
    Rec { hi = there, hi = there, hi =

    Rec
      { hi = there, hi = there, hi =

    123456789012345678901234567890
    Rec
      { hi = there, hi = there
      , hi = there
      }
-}

-- Rec
--   { hi = there, hi =
--      there
--   , hi = there
--   }
--
--      BAD
--              (0, "Rec"), (1, "{ hi ="), (2, "there") collect: (1, ", hi =")
-- Break 1      (0, "Rec"), (1, "{ hi = there, hi =")
--
-- It sees (1, ", hi =")  and figures 'hi = there' won't break, which is
-- correct.  But it also concludes ", hi =" won't break, which it can't
-- know because it hasn't seen "there" yet.  So it could join
--
--              (0, "Rec"), (1, "{ hi = there") collect: (1, ", hi =")
-- Break 1      (0, "Rec"), (1, "{ hi = there") collect: (1, ", hi =")
-- "there"      "Rec\n" (1, "{ hi = there")12 collect: (1, ", hi ="6)
--
-- 

ntest_renderFlat = do
    let f = Lazy.toStrict . Format.renderFlat
    equal (f $ "Rec" <> indented ("a = b" <+/> "c = d") <+/> "Record2")
        "Rec a = b c = d Record2"
    equal (f "a\nb") "a b"
    equal (f $ "a" </> "b") "ab"

{-
    Rec 1 a 1 b 0 rec
    "b"         (0, "Rec"), (1, "a") collect=1, "b"
    "rec"       (0, "Rec"), (1, "a b") collect=0, "rec"
    flushCollect (0, "Rec a b rec")
    GOOD

    Rec 1 a 1 bcdefg 0 rec
    "bcdefg"    (0, "Rec"), (1, "a") collect=1, "bcdefg"
                "Rec\n" (1, "a") collect=1, "bcdefg"
    "rec"       (1, "a bcdefg") collect=0, "rec"
    flushCollect (0, "a bcdefg rec")
    -> "Rec\na bcdefg rec"
    BAD

    GOOD is ok because it goes 0, 1, 0
    BAD isn't because it goes 1, 0
    So if stateCollectIndent is < the first indent, I have to emit that
    first.  If it's >=, 
-}


{-
    "Record" <> indented ("abcde\nfg" </> "hi") </> "R"

    Record 1 abcde _ fg 1 hi 0 R

    Record
      abcde
      fg hi
    R

    (0, "Record"), collect="abcde"

    When I get to a hard break, it flushes collect and forces breaks until
    there are none left.

    (0, "Record"), (1, "abcde")
    output: "Record\n  abcde\n"

    R 

    Record 01234 56
    R

    Record
      01234 56
    R

    Record
      01234
      56
    R

    This is a hard break.
-}

-- record3 = "Record" <+/- (("a =" <+/- "b") <+/> "c")
--     <+/> "Record2"
-- record4 = "Record" <+/- (("a =" <+/- "b") <> "c")
--     <+/> "Record2"
-- __________|
-- Record    |
--   ab =    |
--     blah  |
-- Record2   |


-- 0123456789a
-- __________|
-- Record    |
--   a =/b/c |
-- Record2   |
--
-- "Record" </+- (("a =" </+- "b") </+> "c")
-- </+> "Record2"
--
-- 0 "Record" 6<=10
-- 1 "a =" 6+2+3 = 11>10 ---> "Record\n" (1, "a =") 2+3 = 5<=10
-- 2 "b" 2+3+1+1 = 7<=10 ---> (1, "a ="), (2, "b")
-- 1 indent decrease to 1, so flush >1: (1, "a = b" NoSpace)
-- 1 "c" 2+5+1+1 = 9<=10 --> (1, "a = b c")
-- 1 "cde" 2+5+1+3 = 11>10 -> "  a = b c\n" (1, "cde")
--
-- 0 "Record" 6<=10
-- 1 "a =" 6+2+3 = 11>10 ---> "Record\n" (1, "a =") 2+3 = 5<=10
-- 2 "b" 2+3+1+1 = 7<=10 ---> (1, "a ="+) collect = "b"
-- 1 indent decrease to 1
-- 1 Break Space 0 - collect goes to indent 1: 
-- 1 "c" 2+5+1+1 = 9<=10 --> (1, "a = b c")
-- 1 "cde" 2+5+1+3 = 11>10 -> "  a = b c\n" (1, "cde")

-- 0123456789a
-- __________|
-- Record    |
--   ab =    |
--     blah  |
-- Record2   |
--
-- 0 "Record" 6<=10
-- 1 "ab =" 0+6+1+4 = 11>10 ---> "Record\n", (1, "ab =") 2+4 = 6<10
-- 2 "blah" 2+4+1+4 = 11>10 ---> "Record\n  ab =\n", (2, "blah") 4+4 = 8<10
-- 0 Record2, indent went down so flush: "Record\n  ab =\n    blah\n" []
--   "Record2" 8<=10
--
-- 0123456789a
-- __________|
-- Rec       |
--   a = b   |
--   c = d   |
-- Record2   |
--
-- 0 "Rec" 3<=10
-- 1 "a = b" 3+2+5 = 10<=10 ---> (0, "Rec"), (1, "a = b")
-- 1 "c = d" 3+2+5+1+5 = 16>10 -> "Rec\n", (1, "a = b") 2+5+1+5 = 13>10
--      break again: "Rec\n  a = b\n" [] 2+5 = 7<=10
-- 0 Record2, indent decrease, so flush: "Rec\n  a = b\n  c = d\n" []
--      "Record2"

-- So the algorithm is:
-- on text: append to collect, and check for overflow:
--      sum breaks + collect, sum breaks is lowest indent + lengths, counting
--      Space, except last one
-- if overflow, flush lowest break, try again
-- otherwise, append to break at current (highest) indent
--
-- on break:
-- if indent increases, collect goes to old indent, stateIndent++
-- if indent decreases, flush all >indents: concat breaks <> collect
--      But I can't do this because I don't know Space yet.  So I need
--      Collect B Space, and flush on text append.
-- if indent is the same, collect goes to indent

-- After a dedent I need to know Space, because dedent implies a break too.
-- But that's not represented: ("Record" <+/- "abcdef") <> "what?"
-- Since what will definitely wrap after abcdef, maybe it doesn't matter.
