-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Some terminal display hacks.
module Solkattu.Terminal where
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Seq as Seq
import Global


-- | iTerm on OSX supports all the extended "disable mode" codes like [24m,
-- except the one for bold.  Which is the one I need.  Work around that by
-- replacing unbold with reset to normal, and then re-enable bg color if that's
-- set.
--
-- This is specific to iTerm, so if I want to support other terminals I might
-- need a more general purpose fix.
fix_for_iterm :: Text -> Text
fix_for_iterm = mconcat . snd . List.mapAccumL go "" . split
    where
    go bg (code, text)
        | code `Set.member` all_set_bgs = (code, code <> fix code text)
        | code == bold_off = (bg, normal <> bg <> fix bg text)
        | code == bg_default = ("", code <> text)
        | otherwise = (bg, code <> fix bg text)
        where fix = fix_newline_bg

fix_newline_bg :: Text -> Text -> Text
fix_newline_bg bg text
    | Text.null bg = text
    | not ("\n" `Text.isInfixOf` text) = text
    | otherwise = Text.intercalate "\n" $ fix $ Text.splitOn "\n" text
    where fix = Seq.map_tail (bg<>) . Seq.map_init (<>bg_default)

split :: Text -> [(Text, Text)]
split = fix . Text.splitOn "\ESC["
    where
    fix (x : xs) = ("", x) : map split xs
    fix [] = []
    split t = ("\ESC[" <> pre <> "m", Text.drop 1 post)
        where (pre, post) = Text.breakOn "m" t

data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
    deriving (Enum, Eq, Show)
data Bright = Normal | Bright
    deriving (Eq, Show)

all_set_bgs :: Set Text
all_set_bgs = Set.fromList
    [set_bg b c | c <- [Black .. White], b <- [Normal, Bright]]

set_fg, set_bg :: Bright -> Color -> Text
set_fg b c = esc (fromEnum c + (if b == Normal then 30 else 90))
set_bg b c = esc (fromEnum c + (if b == Normal then 40 else 100))

bg_default :: Text
bg_default = esc 49

normal :: Text
normal = esc 0

bold_on, bold_off :: Text
bold_on = esc 1
bold_off = esc 21

underline_on, underline_off :: Text
underline_on = esc 4
underline_off = esc 24

esc :: Int -> Text
esc n = "\ESC[" <> showt n <> "m"

-- * tests

-- Should look right on iterm.
test_fix_for_iterm :: IO ()
test_fix_for_iterm = putStrLn $ untxt $ fix_for_iterm $ Text.unwords
    [ "before", set_bg Normal Cyan, "w", bold_on, "b", bold_off
    , "still w", bg_default, "done"
    ]

test_fix_newline_bg :: IO ()
test_fix_newline_bg = putStrLn $ untxt $ fix_for_iterm $ Text.unwords
    [ "before", set_bg Normal Cyan, "during\nnewline", bg_default, "last\nline"
    ]
