-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions to compare a performance against a previous \"known good\" one.
-- This is used to detect when code changes cause a performance to change.
module Cmd.DiffPerformance (
    -- * save and load
    load_midi, save_midi, midi_magic
    -- * diff lilypond
    , diff_lilypond
    -- * diff midi
    , diff_midi_performance
    -- * util
    , show_midi
    , diff_lines
) where
import qualified Control.Exception as Exception
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import qualified System.Directory as Directory
import System.FilePath ((</>))
import qualified System.IO.Error as IO.Error
import qualified System.Process as Process

import qualified Util.File as File
import qualified Util.Seq as Seq
import qualified Util.Serialize as Serialize

import qualified Midi.Encode as Encode
import Midi.Instances ()
import qualified Midi.Midi as Midi

import qualified Ui.Ui as Ui
import qualified Perform.RealTime as RealTime
import Global


type Messages = Vector.Vector Midi.WriteMessage

-- * save and load

load_midi :: FilePath -> IO (Either Text Messages)
load_midi fname =
    first ((("loading " <> showt fname <> ": ") <>) . pretty) <$>
        Serialize.unserialize midi_magic fname

-- | Perform the input score and save the midi msgs to the output file.
-- This creates the -perf files.
save_midi :: FilePath -> Messages -> IO ()
save_midi fn = void . Serialize.serialize midi_magic fn

-- | Saved MIDI performance.
midi_magic :: Serialize.Magic (Vector.Vector Midi.WriteMessage)
midi_magic = Serialize.Magic 'm' 'i' 'd' 'i'


-- * diff lilypond

diff_lilypond :: String -> FilePath -> Ui.LilypondPerformance -> Text
    -> IO (Maybe Text, [FilePath])
diff_lilypond name dir performance ly_code =
    first (fmap (info<>)) <$> diff_lines name dir
        (Text.lines (Ui.perf_performance performance)) (Text.lines ly_code)
    where info = diff_info performance <> "\n"

-- * diff midi

diff_midi_performance :: String -> FilePath
    -> Ui.MidiPerformance -> [Midi.WriteMessage] -> IO (Maybe Text, [FilePath])
diff_midi_performance name dir performance msgs =
    first (fmap (info<>)) <$> diff_lines name dir
        (show_midi $ Vector.toList $ Ui.perf_performance performance)
        (show_midi msgs)
    where info = diff_info performance <> "\n"

-- | Write files in the given directory and run the @diff@ command on them.
diff_lines :: String -> FilePath -> [Text] -> [Text]
    -> IO (Maybe Text, [FilePath])
    -- ^ (abbreviated_diff, wrote_files)
diff_lines name dir expected got = do
    Directory.createDirectoryIfMissing True dir
    File.writeLines expected_fn expected
    File.writeLines got_fn got
    (_code, diff, stderr) <- Process.readProcessWithExitCode
        "diff" [expected_fn, got_fn] ""
    unless (null stderr) $
        Exception.throwIO $ IO.Error.userError $ "diff failed: " ++ stderr
    let abbreviated
            | null diff = Nothing
            | otherwise = Just $ show_diffs (txt diff)
    return (abbreviated, [expected_fn, got_fn])
    where
    expected_fn = dir </> name ++ ".expected"
    got_fn = dir </> name ++ ".got"

diff_info :: Ui.Performance a -> Text
diff_info perf =
    "Diffs from " <> pretty (Ui.perf_creation perf)
    <> "\nPatch: " <> Ui.perf_patch perf

show_diffs :: Text -> Text
show_diffs diff = Text.unlines (limit 50 (Text.lines diff))

limit :: Int -> [Text] -> [Text]
limit n xs = pre ++ if null post then [] else [msg]
    where
    msg = "... trimmed (" <> showt (length xs) <> " lines)"
    (pre, post) = splitAt n xs

show_midi :: [Midi.WriteMessage] -> [Text]
show_midi = map pretty . normalize

-- | To better approximate audible differences, I strip excessive time
-- precision and ensure notes happening at the same time are in a consistent
-- order.
normalize :: [Midi.WriteMessage] -> [Midi.WriteMessage]
normalize = concatMap List.sort . Seq.group_adjacent Midi.wmsg_ts . map strip
    where
    strip wmsg = wmsg
        { Midi.wmsg_ts = strip_time (Midi.wmsg_ts wmsg)
        , Midi.wmsg_msg = strip_msg (Midi.wmsg_msg wmsg)
        }
    -- It'll be rounded again by the pretty instance, since I actually diff
    -- pretty output, so this is likely unnecessary.
    strip_time = RealTime.seconds . round_to 3 . RealTime.to_seconds
    -- PitchBends are serialized as 14-bit numbers, so when they get
    -- deserialized they change.
    strip_msg = Encode.decode . Encode.encode

round_to :: RealFrac d => Int -> d -> d
round_to n = (/ 10^n) . fromIntegral . round . (* 10^n)
