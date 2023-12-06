-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Derive.TScore.Java_test where
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified Util.Logger as Logger
import qualified Util.Test.Testing as Testing
import qualified Derive.TScore.Java as Java
import           Derive.TScore.Java (Pitch(..), PitchClass(..))
import qualified Derive.TScore.Parse as Parse
import qualified Derive.TScore.T as T

import           Global
import           Util.Test.Global

-- * parse

everything_score :: Text
everything_score =
    "%irama = tanggung\n\
    \%inst = gb -- gender-barung\n\
    \dualolo = %gatra=3231 [\n\
    \    > '5653 | .6.56.1.\n\
    \    >  .12_ | 6.2.321.\n\
    \]\n"

_parse_file :: FilePath -> IO ()
_parse_file fname = (Java.parse_score <$> Text.IO.readFile fname) >>= \case
    Left err -> putStrLn err
    Right score -> Text.IO.putStrLn $ unparse score

test_roundtrip :: Test
test_roundtrip = do
    let trip = fmap unparse . Java.parse_score
    right_equal (trip "%a = b") "%a = b\n"
    right_equal (trip "x = [ > 5 6/. _ ]") "x = [ > 5 6/. _ ]\n"
    let normalized = trip everything_score
    right_equal (const () <$> normalized) ()
    equal normalized (trip =<< normalized)
    Text.IO.putStrLn $ either txt id normalized

test_infer_octave :: Test
test_infer_octave = do
    let f = Java.infer_octave
    let o = Java.RelativeOctave
    equal (f (1, Just P1) (Pitch (o 0) P2)) (Pitch 1 P2)
    equal (f (1, Just P1) (Pitch (o 0) P7)) (Pitch 0 P7)
    -- First ' forces upward motion.
    equal (f (1, Just P1) (Pitch (o 1) P2)) (Pitch 1 P2)
    equal (f (1, Just P1) (Pitch (o 1) P7)) (Pitch 1 P7)
    -- Additional 's add octaves.
    equal (f (1, Just P1) (Pitch (o 2) P2)) (Pitch 2 P2)
    -- Same for ,
    equal (f (1, Just P1) (Pitch (o (-1)) P2)) (Pitch 0 P2)
    equal (f (1, Just P1) (Pitch (o (-1)) P7)) (Pitch 0 P7)
    equal (f (1, Just P1) (Pitch (o (-2)) P2)) (Pitch (-1) P2)

-- ** util

parse :: Parse.Element a => Text -> Either String a
parse = Parse.parse_text (Parse.parse Parse.default_config)

unparse :: Parse.Element a => a -> Text
unparse = Parse.unparse Parse.default_config


-- * check

test_resolve_duration_bias_start :: Test
test_resolve_duration_bias_start = do
    let f = fmap (map extract) . resolve_tokens Java.BiasStart
        extract (t, n) = (t, T.note_duration n)
    right_equal (f "1234") [(0, 1/4), (1/4, 1/4), (2/4, 1/4), (3/4, 1/4)]
    right_equal (f ".1.2.3.4")
        [(1/8, 1/4), (3/8, 1/4), (5/8, 1/4), (7/8, 1/8)]
    right_equal (f "1.2.3.4.")
        [(0, 1/4), (1/4, 1/4), (2/4, 1/4), (3/4, 1/4)]
    right_equal (f "1") [(0, 1)]
    right_equal (f "1 | 23") [(0, 1), (1, 1/2), (1+1/2, 1/2)]
    right_equal (f "12..") [(0/4, 1/4), (1/4, 3/4)]
    right_equal (f "12_.") [(0/4, 1/4), (1/4, 1/4)]
    right_equal (f "1_.2") [(0/4, 1/4), (3/4, 1/4)]

test_resolve_duration_bias_end :: Test
test_resolve_duration_bias_end = do
    let f = fmap (map extract) . resolve_tokens Java.BiasEnd
        extract (t, n) = (t, T.note_duration n)
    right_equal (f "1234") [(1/4, 1/4), (2/4, 1/4), (3/4, 1/4), (4/4, 0)]
    right_equal (f ".1.2.3.4") [(1/4, 1/4), (2/4, 1/4), (3/4, 1/4), (4/4, 0)]
    right_equal (f "1.2.3.4.")
        [(1/8, 1/4), (3/8, 1/4), (5/8, 1/4), (7/8, 1/8)]
    right_equal (f "1") [(1, 0)]
    right_equal (f "1 | 23") [(1, 1/2), (1+1/2, 1/2), (2, 0)]
    right_equal (f "12..") [(1/4, 1/4), (2/4, 2/4)]
    right_equal (f "12_.") [(1/4, 1/4), (2/4, 1/4)]
    right_equal (f "1_.2") [(1/4, 1/4), (4/4, 0)]

test_infer_rests :: Test
test_infer_rests = do
    let f bias = fmap (map extract) . resolve_tokens bias
        extract (t, n) = (t * 8, T.note_duration n * 8)
    -- Last 6 is actually after the 4/4, it belongs to the next measure.
    -- 1st 2 should be at 1/4 though.
    -- This means the beats still have to be aligned at the ends?
    let expected end =
            [ (2, 2), (4, 1), (5, 1), (6, 2), (8, 1), (9, 1)
            , (10, 2), (12, 2), (14, 2), (16, end)
            ]
    let expectedS = map (first (subtract 2)) (expected 2)
        expectedE = expected 0
    {-
        should be 4 beats:
        1 2 3 4
        2 126 56

        Can't have notes before 2 because it has to be from previous measure.

        0  1  2  3  4
           2. 12 6. 56 | 1
           2  12 6  56 | 1

        0  1  2  3  4    1
          .2 .1 26 .5 | 61
    -}
    right_equal (f Java.BiasStart "2 126 56 | 3 5 3 5") expectedS
    right_equal (f Java.BiasStart "2. 12 6. 56 | 3 5 3 5") expectedS

    right_equal (f Java.BiasEnd ".2 .1 26 .5 | 63 5 3 5") expectedE
    right_equal (f Java.BiasEnd "2 126 5 | 63 5 3 5") expectedE

    -- This is .235, even though it looks misleading.
    -- The problem is rests are only inferred when it's not already a power of
    -- 2.
    right_equal (const () <$> f Java.BiasStart ". 235") ()
    right_equal (const () <$> f Java.BiasStart "12345 6") ()

test_resolve_pitch :: Test
test_resolve_pitch = do
    let f = fmap (map extract) . resolve_tokens Java.BiasStart
        extract = T.note_pitch . snd
    right_equal (f "1471")
        [Pitch 0 P1, Pitch 0 P4, Pitch 0 P7, Pitch 1 P1]
    right_equal (f "11") [Pitch 0 P1, Pitch 0 P1]
    right_equal (f "147,1") [Pitch 0 P1, Pitch 0 P4, Pitch 0 P7, Pitch 0 P1]

err :: Int -> Text -> T.Error
err pos = T.Error (T.Pos pos)

parse_tokens :: Text -> [Java.Token]
parse_tokens = Testing.expect_right . fmap Java.track_tokens . parse . ("> "<>)

resolve_tokens :: Java.Bias -> Text
    -> Either [T.Error] [(T.Time, T.Note () (Pitch Int) T.Time)]
resolve_tokens bias source
    | null errs = Right lines
    | otherwise = Left errs
    where
    (lines, errs) = Logger.runId $ Java.resolve_tokens bias $
        parse_tokens source


-- * format

test_format_score :: Test
test_format_score = do
    let pr = either (Text.IO.putStrLn . ("error: "<>)) (mapM_ Text.IO.putStrLn)
    right_equal (format_score "") []
    -- pr $ format_score "a = [ > 1235 | 65321 ]"
    -- pr $ format_score "a = [ > 1235 | 6.5.3..2 ]"
    -- pr $ format_score "a = [ > 1 2 321 ]"
    pr $ format_score "a = [ > 1234 > 1 123 .4 ]"
    pr $ format_score "a = [ > 1234 > 1 123 .4 | 1 ]"

test_format_score2 :: Test
test_format_score2 = do
    let pr = either (Text.IO.putStrLn . ("error: "<>)) (mapM_ Text.IO.putStrLn)
    pr $ format_score "a = [ > 2 126  56 | 3 5 3 5 ]"
    pr $ format_score "a = [ > .5.6.5.3 | 3 5 3 5 ]"
    pr $ format_score
        "a = [\n\
        \    > .5. 6..1.\n\
        \    > ...,5.3.5\n\
        \]\n"
    pr $ format_score
        "a = [\n\
        \    > .5 .6 .1.\n\
        \    >  . ,5 3 5\n\
        \]\n"

format_score :: Text -> Either Text [Text]
format_score source = case Java.parse_score source of
    Left err -> Left (txt err)
    Right score
        | not (null errs) ->
            Left $ Text.unlines $ map (T.show_error source) errs
        | otherwise -> Right lines
        where (lines, errs) = Java.format_score score

print_file :: FilePath -> IO ()
print_file fname = print_score =<< Text.IO.readFile fname

print_score :: Text -> IO ()
print_score source = case Java.parse_score source of
    Left err -> putStrLn err
    Right score -> do
        let (lines, errs) = Java.format_score score
        mapM_ Text.IO.putStrLn lines
        mapM_ (Text.IO.putStrLn . T.show_error source) errs
