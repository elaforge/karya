-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.Legong_test where
import qualified Util.Seq as Seq
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Scale.Legong as Legong
import qualified Derive.Score as Score


test_note_to_call = do
    let run ps = DeriveTest.extract Score.initial_nn $
            DeriveTest.derive_tracks "scale=legong" $ --  | key=selesir"
            UiTest.note_track [(t, 1, p) | (t, p) <- zip (Seq.range_ 0 1) ps]
    -- Default to umbang.
    equal (run ["1i"]) ([Just $ head Legong.umbang], [])
    equal (run ["tuning=isep | -- 1i"]) ([Just $ head Legong.isep], [])
    let nns = drop 9 Legong.umbang
    equal (run ["2e", "2e#", "2u", "2a"]) (map Just (take 4 nns), [])
