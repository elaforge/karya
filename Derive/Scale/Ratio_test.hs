-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.Ratio_test where
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.DeriveTest as DeriveTest


test_ratio = do
    let e_nns = DeriveTest.extract DeriveTest.e_nns
        e_tsig r =
            ( lookup (UiTest.default_block_id, UiTest.mk_tid 3)
                (DeriveTest.e_tsigs r)
            , DeriveTest.e_tsig_logs r
            )
    let run ratio base = DeriveTest.derive_tracks_setup
            (DeriveTest.with_tsig_tracknums [3]) ""
            [ (">i1", [(0, 1, "")])
            , ("*twelve #ratio-source", [(0, 0, base)])
            , ("*ratio", [(0, 0, ratio)])
            ]
    -- Bah, 'hz_to_nn . nn_to_hz' introduces imprecision.
    equalf 0.001 (e_nns $ run "1/1" "4c") ([[(0, 60)]], [])
    equalf 0.001 (e_nns $ run "2/1" "4c") ([[(0, 72)]], [])
    equalf 0.001 (e_nns $ run "-2/1" "4c") ([[(0, 48)]], [])
    let tracks =
            [ (">i1", [(0, 1, "")])
            , ("*ratio", [(0, 0, "1/1")])
            , ("*twelve #ratio-source", [(0, 0, "4c")])
            ]
    equal (e_nns $ DeriveTest.derive_tracks "" tracks)
        ([[]], ["ratio scale requires #ratio-source"])

    -- This actually tests that Control.eval_signal sets the scale properly.
    equalf 0.001 (e_tsig $ run "2/1" "4c") (Just [(0, 72)], [])
