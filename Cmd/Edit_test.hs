-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Edit_test where
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Edit as Edit

import Types


test_split_events = do
    let run events = CmdTest.e_tracks
            . run_sel [(">", events)] Edit.cmd_split_events 1
    equal (run [(0, 4, "")] 0) $
        Right ([(">", [(0, 4, "")])], [])
    equal (run [(0, 4, "")] 2) $
        Right ([(">", [(0, 2, ""), (2, 2, "")])], [])
    equal (run [(2, 2, "")] 3) $
        Right ([(">", [(2, 1, ""), (3, 1, "")])], [])
    equal (run [(0, 4, "")] 4) $
        Right ([(">", [(0, 4, "")])], [])

    equal (run [(4, -4, "")] 0) $
        Right ([(">", [(4, -4, "")])], [])
    equal (run [(4, -4, "")] 2) $
        Right ([(">", [(2, -2, ""), (4, -2, "")])], [])
    equal (run [(4, -4, "")] 4) $
        Right ([(">", [(4, -4, "")])], [])

run_sel :: [UiTest.TrackSpec] -> Cmd.CmdId a -> TrackNum -> ScoreTime
    -> CmdTest.Result a
run_sel track_specs cmd tracknum pos = CmdTest.run_tracks track_specs $ do
    CmdTest.set_sel tracknum pos tracknum pos
    cmd

test_record_recent = do
    let f recent = Edit.record_recent recent
        n txt = Cmd.RecentGenerator txt False
        t txt = Cmd.RecentTransform txt False
    -- Generators get replaced, and are always in slot 1.
    equal (f (n "a") []) [(1, n "a")]
    equal (f (n "b") [(1, n "a"), (2, t "t")]) [(1, n "b"), (2, t "t")]

    -- Transformers go in 2..4 and cycle.
    equal (f (t "a") []) [(2, t "a")]
    equal (f (t "b") [(2, t "a")]) [(3, t "b"), (2, t "a")]

    -- The oldest one is bumped off, and its key reused.
    equal (f (t "a") [(4, t "x"), (3, t "y"), (2, t "z")])
        [(2, t "a"), (4, t "x"), (3, t "y")]
    -- Unless an existing one matches.
    equal (f (t "a 2") [(4, t "x"), (3, t "a 1"), (2, t "z")])
        [(3, t "a 2"), (4, t "x"), (2, t "z")]

test_record_recent_replace = do
    -- "similar" recent notes should replace existing ones
    let f = Edit.record_recent
    let note = Cmd.RecentGenerator
    equal (f (note "a" False) [(1, (note "a" True))])
        [(1, (note "a" False))]
    equal (f (note "a .2" True) [(1, (note "a .5" True))])
        [(1, (note "a .2" True))]
