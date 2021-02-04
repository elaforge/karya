-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions to compare a performance against a previous \"known good\" one.
-- This is used to detect when code changes cause a performance to change.
module Cmd.DiffPerformance (
    -- * save and load
    load_midi, save_midi, midi_magic
    -- * diff
    , diff_lilypond
    , diff_im
    , diff_midi
    -- * util
    , show_midi
    , diff_lines
) where
import qualified Control.Exception as Exception
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import qualified System.Directory as Directory
import           System.FilePath ((</>))
import qualified System.IO.Error as IO.Error
import qualified System.Process as Process

import qualified Util.File as File
import qualified Util.Seq as Seq
import qualified Util.Serialize as Serialize

import qualified Midi.Encode as Encode
import           Midi.Instances ()
import qualified Midi.Midi as Midi

import qualified Perform.RealTime as RealTime
import qualified Synth.Shared.Note as Shared.Note
import qualified Ui.UiConfig as UiConfig

import           Global


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


-- * diff

diff_lilypond :: String -> FilePath -> UiConfig.LilypondPerformance -> Text
    -> IO (Maybe Text, [FilePath])
diff_lilypond = diff_performance Text.lines

diff_im :: String -> FilePath -> UiConfig.ImPerformance -> [Shared.Note.Note]
    -> IO (Maybe Text, [FilePath])
diff_im name dir performance =
    diff_performance show_im name dir (Vector.toList <$> performance)
    where show_im = map pretty

diff_midi :: String -> FilePath -> UiConfig.MidiPerformance
    -> [Midi.WriteMessage] -> IO (Maybe Text, [FilePath])
diff_midi name dir performance =
    diff_performance show_midi name dir (Vector.toList <$> performance)

diff_performance :: (events -> [Text]) -> String -> FilePath
    -> UiConfig.Performance events -> events -> IO (Maybe Text, [FilePath])
diff_performance show_events name dir performance events =
    first (fmap (info<>)) <$> diff_lines name dir
        (show_events (UiConfig.perf_events performance))
        (show_events events)
    where
    info = Text.unlines
        [ "Diffs from " <> pretty (UiConfig.perf_creation performance)
        , "Commit: " <> UiConfig.perf_commit performance
        ]

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
