-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions to compare a performance against a previous \"known good\" one.
-- This is used to detect when code changes cause a performance to change.
module Cmd.DiffPerformance (
    -- * save and load
    loadMidi, saveMidi, midiMagic
    -- * diff
    , diffLilypond
    , diffIm
    , diffMidi
    -- * util
    , showMidi
    , diffLines
) where
import qualified Control.Exception as Exception
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import qualified System.Directory as Directory
import           System.FilePath ((</>))
import qualified System.IO.Error as IO.Error
import qualified System.Process as Process

import qualified Util.Files as Files
import qualified Util.Lists as Lists
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

loadMidi :: FilePath -> IO (Either Text Messages)
loadMidi fname =
    first ((("loading " <> showt fname <> ": ") <>) . pretty) <$>
        Serialize.unserialize midiMagic fname

-- | Perform the input score and save the midi msgs to the output file.
-- This creates the -perf files.
saveMidi :: FilePath -> Messages -> IO ()
saveMidi fn = void . Serialize.serialize midiMagic fn

-- | Saved MIDI performance.
midiMagic :: Serialize.Magic (Vector.Vector Midi.WriteMessage)
midiMagic = Serialize.Magic 'm' 'i' 'd' 'i'


-- * diff

diffLilypond :: String -> FilePath -> UiConfig.LilypondPerformance -> Text
    -> IO (Maybe Text, [FilePath])
diffLilypond = diffPerformance Text.lines

diffIm :: String -> FilePath -> UiConfig.ImPerformance -> [Shared.Note.Note]
    -> IO (Maybe Text, [FilePath])
diffIm name dir performance =
    diffPerformance showIm name dir (Vector.toList <$> performance)
    where showIm = map pretty

diffMidi :: String -> FilePath -> UiConfig.MidiPerformance
    -> [Midi.WriteMessage] -> IO (Maybe Text, [FilePath])
diffMidi name dir performance =
    diffPerformance showMidi name dir (Vector.toList <$> performance)

diffPerformance :: (events -> [Text]) -> String -> FilePath
    -> UiConfig.Performance events -> events -> IO (Maybe Text, [FilePath])
diffPerformance showEvents name dir performance events =
    first (fmap (info<>)) <$> diffLines name dir
        (showEvents (UiConfig.perfEvents performance))
        (showEvents events)
    where
    info = Text.unlines
        [ "Diffs from " <> pretty (UiConfig.perfCreation performance)
        , "Commit: " <> UiConfig.perfCommit performance
        ]

-- | Write files in the given directory and run the @diff@ command on them.
diffLines :: String -> FilePath -> [Text] -> [Text]
    -> IO (Maybe Text, [FilePath])
    -- ^ (abbreviated_diff, wrote_files)
diffLines name dir expected got = do
    Directory.createDirectoryIfMissing True dir
    Files.writeLines expectedFn expected
    Files.writeLines gotFn got
    (_code, diff, stderr) <- Process.readProcessWithExitCode
        "diff" [expectedFn, gotFn] ""
    unless (null stderr) $
        Exception.throwIO $ IO.Error.userError $ "diff failed: " ++ stderr
    let abbreviated
            | null diff = Nothing
            | otherwise = Just $ showDiffs (txt diff)
    pure (abbreviated, [expectedFn, gotFn])
    where
    expectedFn = dir </> name ++ ".expected"
    gotFn = dir </> name ++ ".got"

showDiffs :: Text -> Text
showDiffs diff = Text.unlines (limit 50 (Text.lines diff))

limit :: Int -> [Text] -> [Text]
limit n xs = pre ++ if null post then [] else [msg]
    where
    msg = "... trimmed (" <> showt (length xs) <> " lines)"
    (pre, post) = splitAt n xs

showMidi :: [Midi.WriteMessage] -> [Text]
showMidi = map pretty . normalize

-- | To better approximate audible differences, I strip excessive time
-- precision and ensure notes happening at the same time are in a consistent
-- order.
normalize :: [Midi.WriteMessage] -> [Midi.WriteMessage]
normalize = concatMap List.sort . Lists.groupAdjacent Midi.wmsgTs . map strip
    where
    strip wmsg = wmsg
        { Midi.wmsgTs = stripTime (Midi.wmsgTs wmsg)
        , Midi.wmsgMsg = stripMsg (Midi.wmsgMsg wmsg)
        }
    -- It'll be rounded again by the pretty instance, since I actually diff
    -- pretty output, so this is likely unnecessary.
    stripTime = RealTime.seconds . roundTo 3 . RealTime.toSeconds
    -- PitchBends are serialized as 14-bit numbers, so when they get
    -- deserialized they change.
    stripMsg = Encode.decode . Encode.encode

roundTo :: RealFrac d => Int -> d -> d
roundTo n = (/ 10^n) . fromIntegral . round . (* 10^n)
