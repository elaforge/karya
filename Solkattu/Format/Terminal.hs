-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Some terminal display hacks.
module Solkattu.Format.Terminal where
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Seq as Seq
import Global


-- | Tweak escape codes to get around terminal bugs \/ features.
fix :: Text -> Text
fix = mconcat . snd . List.mapAccumL go "" . split
    where
    go bg (code, text)
        | code `Set.member` allSetBgs = (code, code <> fixNewlineBg code text)
        | code == bgDefault = ("", code <> text)
        | otherwise = (bg, code <> fixNewlineBg bg text)

-- | If I don't turn off the bg color before a newline, the color goes all the
-- way to the right margin, which looks really ugly.
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
boldOff = esc 22
    -- Some docs say it's 21, but in practice xterm and iterm use 22.

underlineOn, underlineOff :: Text
underlineOn = esc 4
underlineOff = esc 24

esc :: Int -> Text
esc n = "\ESC[" <> showt n <> "m"

-- * tests

-- Ensure text style state works.
testTextStyle :: IO ()
testTextStyle = putStrLn $ untxt $ Text.unwords
    [ "normal", setBg Normal Cyan, "cyan-bg", boldOn, "bold", boldOff
    , "bold off, still cyan", bgDefault, "normal"
    ]

-- Ensure the background color doesn't get stuck on across the newline.
testFixNewlineBg :: IO ()
testFixNewlineBg = putStrLn $ untxt $ fix $ Text.unwords
    [ "before", setBg Normal Cyan, "during\nnewline", bgDefault, "last\nline"
    ]
