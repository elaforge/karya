-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Derive.JScore.Format_test where
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified Derive.JScore.Format as Format
import qualified Derive.JScore.Parse as Parse
import qualified Derive.JScore.T as T

import           Global
import           Util.Test.Global


-- * format

test_format_score :: Test
test_format_score = do
    let pr = either (Text.IO.putStrLn . ("error: "<>)) (mapM_ Text.IO.putStrLn)
    rightEqual (formatScore "") []
    pr $ formatScore "..35 name [ > 1235 | 65321 ]" -- error: not a power of 2
    pr $ formatScore "12.5) name [ > 1235 | 6532 ]"

test_format_score2 :: Test
test_format_score2 = do
    let pr = either (Text.IO.putStrLn . ("error: "<>)) (mapM_ Text.IO.putStrLn)
    pr $ formatScore "1235 [ > 2 126  56 | 3 5 3 5 ]"
    pr $ formatScore "1235 [ > .5.6.5.3 | 3 5 3 5 ]"
    pr $ formatScore
        "1235 [\n\
        \    > .5. 6..1.\n\
        \    > ...,5.3.5\n\
        \]\n"
    pr $ formatScore
        "1235 [\n\
        \    > .5 .6 .1.\n\
        \    >  . ,5 3 5\n\
        \]\n"

formatScore :: Text -> Either Text [Text]
formatScore source = case Parse.parseScore source of
    Left err -> Left err
    Right score
        | not (null errs) ->
            Left $ Text.unlines $ map (T.showError "fn" source) errs
        | otherwise -> Right lines
        where (lines, errs) = Format.formatScore id score

_parseFile :: FilePath -> IO ()
_parseFile fname = (Parse.parseScore <$> Text.IO.readFile fname) >>= \case
    Left err -> Text.IO.putStrLn err
    Right score -> Text.IO.putStrLn $ Parse.unparse score
