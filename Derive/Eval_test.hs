-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Eval_test where
import Util.Test
import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Eval as Eval
import qualified Derive.Instrument.DUtil as DUtil
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig

import Global


test_recursive_call = do
    let extract = DeriveTest.extract DeriveTest.e_event
    let result = extract $ DeriveTest.derive_tracks_with with_recur ""
            [(">", [(0, 1, "recur")])]
        with_recur = CallTest.with_note_generator "recur" recursive
    equal result ([], ["Error: call stack too deep: recursive"])
    where
    recursive :: Derive.Generator Derive.Note
    recursive = Derive.generator "test-module" "recursive" mempty "doc" $
        Sig.call0 $ \args ->
            Eval.reapply_call (Derive.passed_info args) "recur" []

test_reapply_generator = do
    let run = DeriveTest.extract DeriveTest.e_attributes .
            DeriveTest.derive_tracks_with with ""
        dyn = ("dyn", [(0, 0, "1")])
    equal (run [(">", [(0, 0, "a")]), dyn]) (["+a"], [])
    -- If Eval.reapply_generator isn't replacing the info_expr, the inversion
    -- will wind up with 'ab' as the call again, and 'ab' will be called
    -- multiple times.
    equal (run [(">", [(0, 0, "ab")]), dyn]) (["+a", "+b"], [])
    where
    with = CallTest.with_note_generators $
        ("ab", DUtil.multiple_call "ab" ["a", "b"])
        : CUtil.drum_calls Nothing
            [ Drums.note 'a' "a" (Score.attr "a")
            , Drums.note 'b' "b" (Score.attr "b")
            ]
