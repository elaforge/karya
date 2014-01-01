-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions to compare a performance against a previous \"known good\" one.
-- This is used to detect when code changes cause a performance to change.
module Cmd.DiffPerformance where
import qualified Data.Algorithm.Diff as Diff
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import qualified Util.ApproxEq as ApproxEq
import Util.Control
import qualified Util.Pretty as Pretty

import qualified Midi.Midi as Midi
import qualified Ui.State as State
import qualified Cmd.Serialize as Serialize


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

-- * diff

verify :: State.Performance -> Messages -> Text
verify prev midi
    | null diffs = "valid"
    | otherwise = show_diffs prev diffs
    where
    diffs = diff_midi (State.perf_midi prev) midi
    show_diffs perf diffs =
        "Diffs from " <> Pretty.prettytxt (State.perf_creation perf)
        <> "\nPatch: " <> State.perf_patch perf
        <> "\n" <> Text.unlines (take 50 diffs)

-- Faster diff:
-- Zip msgs and compare each one.

diff_midi :: Messages -> Messages -> [Text]
diff_midi expected got =
    mapMaybe show_diff $ Diff.getGroupedDiffBy wmsgs_equal
        (Vector.toList expected) (Vector.toList got)
    where
    show_diff (Diff.Both {}) = Nothing
    show_diff (Diff.First msgs) = Just $ Text.unlines $
        map (("- " <>) . Pretty.prettytxt) msgs
    show_diff (Diff.Second msgs) = Just $ Text.unlines $
        map (("+ " <>) . Pretty.prettytxt) msgs

wmsgs_equal :: Midi.WriteMessage -> Midi.WriteMessage -> Bool
wmsgs_equal (Midi.WriteMessage dev1 t1 m1) (Midi.WriteMessage dev2 t2 m2) =
    dev1 == dev2 && ApproxEq.approx_eq 0.001 t1 t2 && msgs_equal m1 m2

msgs_equal :: Midi.Message -> Midi.Message -> Bool
msgs_equal (Midi.ChannelMessage chan1 m1) (Midi.ChannelMessage chan2 m2) =
    chan1 == chan2 && chan_msgs_equal m1 m2
msgs_equal m1 m2 = m1 == m2

chan_msgs_equal :: Midi.ChannelMessage -> Midi.ChannelMessage -> Bool
chan_msgs_equal (Midi.PitchBend v1) (Midi.PitchBend v2) =
    ApproxEq.approx_eq 0.01 v1 v2
    -- PitchBends are serialized as 14-bit numbers, so when they get
    -- deserialized they change.
chan_msgs_equal m1 m2 = m1 == m2
