-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions to compare a performance against a previous \"known good\" one.
-- This is used to detect when code changes cause a performance to change.
module Cmd.DiffPerformance where
import qualified Data.Algorithm.Diff as Diff
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import Util.Control
import qualified Util.Seq as Seq
import qualified Midi.Encode as Encode
import qualified Midi.Midi as Midi
import qualified Ui.State as State
import qualified Cmd.Serialize as Serialize
import qualified Perform.RealTime as RealTime


type Messages = Vector.Vector Midi.WriteMessage

-- * save and load

midi_magic :: Serialize.Magic
midi_magic = Serialize.Magic 'm' 'i' 'd' 'i'

load_midi :: FilePath -> IO Messages
load_midi fname = Serialize.unserialize midi_magic fname >>= \x -> case x of
    Left err -> errorIO $ "loading " ++ fname ++ ": " ++ err
    Right Nothing -> errorIO $ "not found: " ++ fname
    Right (Just msgs) -> return msgs

-- | Perform the input score and save the midi msgs to the output file.
-- This creates the -perf files.
save_midi :: FilePath -> Messages -> IO ()
save_midi = Serialize.serialize midi_magic


-- * diff lilypond

diff_lilypond :: State.LilypondPerformance -> Text -> Maybe Text
diff_lilypond prev ly_code
    | null diffs = Nothing
    | otherwise = Just $ show_diffs prev diffs
    where diffs = diff_lines (State.perf_performance prev) ly_code

diff_lines :: Text -> Text -> [[Text]]
diff_lines expected got = mapMaybe (show_diff id) $
    Diff.getGroupedDiffBy (==) (Text.lines expected) (Text.lines got)

-- * diff

diff_midi_performance :: State.MidiPerformance -> [Midi.WriteMessage]
    -> Maybe Text
diff_midi_performance prev midi
    | null diffs = Nothing
    | otherwise = Just $ show_diffs prev diffs
    where diffs = diff_midi (Vector.toList (State.perf_performance prev)) midi

show_diffs :: State.Performance a -> [[Text]] -> Text
show_diffs perf diffs =
    "Diffs from " <> prettyt (State.perf_creation perf)
    <> "\nPatch: " <> State.perf_patch perf
    <> "\n" <> Text.unlines (limit 50 (List.intercalate [""] diffs))

limit :: Int -> [Text] -> [Text]
limit n xs = ok ++ if more then ["... (trimmed)"] else []
    where (ok, more) = take_more n xs

take_more :: Int -> [a] -> ([a], Bool)
take_more n xs
    | n <= 0 = ([], not (null xs))
    | otherwise = case xs of
        [] -> ([], False)
        x : xs -> first (x:) $ take_more (n-1) xs

diff_midi :: [Midi.WriteMessage] -> [Midi.WriteMessage] -> [[Text]]
diff_midi expected got =
    mapMaybe (show_diff prettyt) $ Diff.getGroupedDiffBy (==)
        (normalize expected) (normalize got)

-- | To better approximate audible differences, I strip excessive time
-- precision and ensure notes happening at the same time are in a consistent
-- order.
normalize :: [Midi.WriteMessage] -> [Midi.WriteMessage]
normalize = concatMap List.sort . Seq.group Midi.wmsg_ts . map strip
    where
    strip wmsg = wmsg
        { Midi.wmsg_ts = strip_time (Midi.wmsg_ts wmsg)
        , Midi.wmsg_msg = strip_msg (Midi.wmsg_msg wmsg)
        }
    strip_time = RealTime.seconds . round_to 3 . RealTime.to_seconds
    -- PitchBends are serialized as 14-bit numbers, so when they get
    -- deserialized they change.
    strip_msg = Encode.decode . Encode.encode

round_to :: RealFrac d => Int -> d -> d
round_to n = (/ 10^n) . fromIntegral . round . (* 10^n)

show_diff :: (a -> Text) -> Diff.Diff [a] -> Maybe [Text]
show_diff _ (Diff.Both {}) = Nothing
show_diff to_text (Diff.First msgs) = Just $ map (("- " <>) . to_text) msgs
show_diff to_text (Diff.Second msgs) = Just $ map (("+ " <>) . to_text) msgs
