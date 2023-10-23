-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Derive.TScore.Java_test where
import qualified Data.Text.IO as Text.IO

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
    right_equal (trip "x = [ > 56/._ ]") "x = [ > 5 6/ . _ ]\n"
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

test_resolve_duration :: Test
test_resolve_duration = do
    let f = map (fmap extract) . Java.resolve_tokens . parse_tokens
        extract (t, n) = (t, T.note_duration n)
    equal (f "1") $ map Right [(0, 1)]
    equal (f "1 | 23") $ map Right [(0, 1), (1, 1/2), (1+1/2, 1/2)]
    equal (f "123") [err 2 "group not a power of 2: 3"]
    equal (f "12..") $ map Right [(0, 1/4), (1/4, 3/4)]
    equal (f "12_.") $ map Right [(0, 1/4), (1/4, 1/4)]
    equal (f "1_.2") $ map Right [(0, 1/4), (3/4, 1/4)]

test_resolve_pitch :: Test
test_resolve_pitch = do
    let f = extract . Java.resolve_tokens . parse_tokens
        extract = map (fmap (T.note_pitch . snd))
    equal (f "1471") $ map Right
        [Pitch 0 P1, Pitch 0 P4, Pitch 0 P7, Pitch 1 P1]
    equal (f "11") $ map Right [Pitch 0 P1, Pitch 0 P1]
    equal (f "147,1") $ map Right
        [Pitch 0 P1, Pitch 0 P4, Pitch 0 P7, Pitch 0 P1]

err :: Int -> Text -> Either T.Error a
err pos = Left . T.Error (T.Pos pos)

parse_tokens :: Text -> [Java.Token]
parse_tokens = Testing.expect_right . fmap Java.track_tokens . parse . ("> "<>)


-- * format

print_file :: FilePath -> IO ()
print_file fname = do
    source <- Text.IO.readFile fname
    case Java.parse_score source of
        Left err -> putStrLn err
        Right score -> do
            mapM_ Text.IO.putStrLn $ Java.format_score score
            mapM_ Text.IO.putStrLn $ Java.check source score
