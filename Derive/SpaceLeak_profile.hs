-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Try to find space leaks in specific functions.
module Derive.SpaceLeak_profile where
import qualified Util.Lists as Lists
import qualified Util.Thread as Thread
import qualified Derive.C.Post.Postproc as Postproc
import qualified Derive.Call.Post as Post
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Stream as Stream


profileCancel = do
    let f = Postproc.groupAndCancel
            (Postproc.cancelStrongWeak Postproc.inferDurationMerged)
            Post.handKey 2
        make = Stream.fromSortedEvents . map DeriveTest.mkevent
        inst = ScoreT.Instrument "i1"
    let events = make
            [(s, 1, "4c", [], inst) | s <- Lists.range 0 (1024 * 50) 1]
    -- With force, run with -K2905K
    Thread.force events
    Thread.printTimer "force" ("done: "<>) $
        pure $! either show (show . length . Stream.toList) $ f events
