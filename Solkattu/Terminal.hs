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
fixForIterm :: Text -> Text
fixForIterm = mconcat . snd . List.mapAccumL go "" . split
    where
    go bg (code, text)
        | code `Set.member` allSetBgs = (code, code <> fix code text)
        | code == boldOff = (bg, normal <> bg <> fix bg text)
        | code == bgDefault = ("", code <> text)
        | otherwise = (bg, code <> fix bg text)
        where fix = fixNewlineBg

fixNewlineBg :: Text -> Text -> Text
fixNewlineBg bg text
    | Text.null bg = text
    | not ("\n" `Text.isInfixOf` text) = text
    | otherwise = Text.intercalate "\n" $ fix $ Text.splitOn "\n" text
    where fix = Seq.map_tail (bg<>) . Seq.map_init (<>bgDefault)

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

allSetBgs :: Set Text
allSetBgs = Set.fromList
    [setBg b c | c <- [Black .. White], b <- [Normal, Bright]]

setFg, setBg :: Bright -> Color -> Text
setFg b c = esc (fromEnum c + (if b == Normal then 30 else 90))
setBg b c = esc (fromEnum c + (if b == Normal then 40 else 100))

bgDefault :: Text
bgDefault = esc 49

normal :: Text
normal = esc 0

boldOn, boldOff :: Text
boldOn = esc 1
boldOff = esc 21

underlineOn, underlineOff :: Text
underlineOn = esc 4
underlineOff = esc 24

esc :: Int -> Text
esc n = "\ESC[" <> showt n <> "m"

-- * tests

-- Should look right on iterm.
testFixForIterm :: IO ()
testFixForIterm = putStrLn $ untxt $ fixForIterm $ Text.unwords
    [ "before", setBg Normal Cyan, "w", boldOn, "b", boldOff
    , "still w", bgDefault, "done"
    ]

testFixNewlineBg :: IO ()
testFixNewlineBg = putStrLn $ untxt $ fixForIterm $ Text.unwords
    [ "before", setBg Normal Cyan, "during\nnewline", bgDefault, "last\nline"
    ]
