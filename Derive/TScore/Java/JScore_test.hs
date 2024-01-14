-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Derive.TScore.Java.JScore_test where
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified Derive.TScore.Java.JScore as JScore
import qualified Derive.TScore.Java.T as T
import qualified Derive.TScore.Parse as Parse

import           Global
import           Util.Test.Global

-- * parse

everything_score :: Text
everything_score =
    "%irama = tanggung\n\
    \%inst = gender-barung -- comment\n\
    \3231^ dualolo cilik [\n\
    \    > '5653 | .6.56.1.\n\
    \    >  .12_ | 6.2.321.\n\
    \]\n\
    \'1235 ranbatan -- comment\n\
    \1235) tumurun gede -- comment\n"

test_roundtrip :: Test
test_roundtrip = do
    let trip = fmap unparse . JScore.parse_score
    right_equal (trip "%piece = b") "%piece = b\n"
    right_equal (trip "1235 [ > 5 6/. _ ]") "1235 [ > 5 6/. _ ]\n"
    let normalized = trip everything_score
    right_equal (const () <$> normalized) ()
    equal normalized (trip =<< normalized)
    Text.IO.putStrLn $ either txt id normalized

test_parse :: Test
test_parse = do
    let f = JScore.parse_score
    pprint (f "1235 tumurun\n1235 am")

unparse :: Parse.Element a => a -> Text
unparse = Parse.unparse Parse.default_config

-- * format

test_format_score :: Test
test_format_score = do
    let pr = either (Text.IO.putStrLn . ("error: "<>)) (mapM_ Text.IO.putStrLn)
    right_equal (format_score "") []
    pr $ format_score "..35 name [ > 1235 | 65321 ]" -- error: not a power of 2
    pr $ format_score "12.5) name [ > 1235 | 6532 ]"

test_format_score2 :: Test
test_format_score2 = do
    let pr = either (Text.IO.putStrLn . ("error: "<>)) (mapM_ Text.IO.putStrLn)
    pr $ format_score "1235 [ > 2 126  56 | 3 5 3 5 ]"
    pr $ format_score "1235 [ > .5.6.5.3 | 3 5 3 5 ]"
    pr $ format_score
        "1235 [\n\
        \    > .5. 6..1.\n\
        \    > ...,5.3.5\n\
        \]\n"
    pr $ format_score
        "1235 [\n\
        \    > .5 .6 .1.\n\
        \    >  . ,5 3 5\n\
        \]\n"

format_score :: Text -> Either Text [Text]
format_score source = case JScore.parse_score source of
    Left err -> Left (txt err)
    Right score
        | not (null errs) ->
            Left $ Text.unlines $ map (T.show_error source) errs
        | otherwise -> Right lines
        where (lines, errs) = JScore.format_score id score

_parse_file :: FilePath -> IO ()
_parse_file fname = (JScore.parse_score <$> Text.IO.readFile fname) >>= \case
    Left err -> putStrLn err
    Right score -> Text.IO.putStrLn $ unparse score