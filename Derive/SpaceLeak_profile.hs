-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Try to find space leaks in specific functions.
module Derive.SpaceLeak_profile where
import qualified Util.Seq as Seq
import qualified Util.Thread as Thread
import qualified Derive.C.Post.Postproc as Postproc
import qualified Derive.Call.Post as Post
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Stream as Stream


profile_cancel = do
    let f = Postproc.group_and_cancel
            (Postproc.cancel_strong_weak Postproc.infer_duration_merged)
            Post.hand_key 2
        make = Stream.from_sorted_events . map DeriveTest.mkevent
        inst = ScoreT.Instrument "i1"
    let events = make [(s, 1, "4c", [], inst) | s <- Seq.range 0 (1024 * 50) 1]
    -- With force, run with -K2905K
    Thread.force events
    Thread.printTimer "force" ("done: "<>) $
        return $! either show (show . length . Stream.to_list) $ f events
