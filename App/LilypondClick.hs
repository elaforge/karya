module App.LilypondClick where
import qualified Data.Maybe as Maybe
import qualified System.Environment as Environment

import Util.Control
import qualified Util.Regex as Regex
import qualified App.SendCmd as SendCmd


{-
    Unfortunately handling clicks in previous is encrusted with apple gunk as
    usual.  You can't just tell it what to do with clicks, you have to have
    a running app with CFBundleURLName in Info.plist and then a magic function
    which is called by the OS runloop.  I'd probably have to make a small
    python or applescript process that gets the URL and runs this program.

    Put in Info.plist:

        <key>CFBundleURLTypes</key>
        <array>
            <dict>
                <key>CFBundleTypeRole</key>
                <string>Editor</string>
                <key>CFBundleURLName</key>
                <string>text editor via url</string>
                <key>CFBundleURLSchemes</key>
                <array>
                    <string>textedit</string>
                </array>
                <key>NSDocumentClass</key>
                <string>TinyTinyDocument</string>
            </dict>
        </array>
-}


main :: IO ()
main = do
    args <- Environment.getArgs
    (fname, charnum) <- return $ case args of
        [url] -> Maybe.fromMaybe usage $ parse_url url
        _ -> usage
    response <- SendCmd.send $
        unwords ["lilypond_click", show fname, show charnum]
    unless (null response) $
        putStrLn response

usage :: a
usage = error "usage: lilypond_click textedit://FILE:LINE:CHAR:COLUMN"

-- | Example:
-- textedit:///home/quinn/s/ly/test.ly:3:4:4 -> ("test.ly", 4)
parse_url :: String -> Maybe (FilePath, Int)
parse_url url = case matches of
    (_, [file, _line, char, _column]) : _
        | Just charnum <- read_maybe char -> Just (file, charnum)
    _ -> Nothing
    where
    matches = Regex.find_groups
        (Regex.make "textedit://(.*):([^:]+):([^:]+):(.*)") url

read_maybe :: (Read a) => String -> Maybe a
read_maybe s = case reads s of
    (val, "") : _ -> Just val
    _ -> Nothing
