-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Args_test where
import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest

import qualified Ui.UiTest as UiTest

import           Util.Test


test_lookup_next_pitch :: Test
test_lookup_next_pitch = do
    let run = DeriveTest.extract (DeriveTest.eEnviron "next")
            . DeriveTest.deriveTracksSetup
                (CallTest.withNoteGenerator "g" cGen) "tempo=.5"
            . UiTest.noteTrack1
        cGen = CallTest.generator $ \args -> do
            next <- Args.lookupNextPitch args
            Derive.withVal "next" next Call.note
    equal (run ["g -- 4c", "g -- 4d"])
        ([Just "<pitch: 62nn,4d(twelve)>", Nothing], [])
