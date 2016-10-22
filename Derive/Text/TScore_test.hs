-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Derive.Text.TScore_test where
import qualified Text.Parsec as Parsec

import Util.Test
import qualified Derive.Text.TScore as TScore
import Global


test_p_score = do
    let f = fmap (\(TScore.Score notes) -> notes) . parse TScore.p_score
    let note p = TScore.TNote $
            TScore.Note Nothing p 0 (TScore.Duration Nothing 0 False)
    equal (f "") $ Right []
    equal (f "||") $ Right [TScore.TBarline 2]
    equal (f " | || ") $ Right [TScore.TBarline 1, TScore.TBarline 2]
    equal (f "a") $ Right [note "a"]
    equal (f "a -- hi") $ Right [note "a"]
    equal (f " a b c") $ Right [note "a", note "b", note "c"]

test_p_note = do
    let f = parse TScore.p_note
    equal (f "b") $ Right $
        TScore.Note Nothing "b" 0 (TScore.Duration Nothing 0 False)
    equal (f "a/b2.~") $ Right $
        TScore.Note (Just "a") "b" 0 (TScore.Duration (Just 2) 1 True)
    equal (f "a/b,~") $ Right $
        TScore.Note (Just "a") "b" (-1) (TScore.Duration Nothing 0 True)

parse :: TScore.Parser a -> Text -> Either String a
parse p = first show . Parsec.parse (p <* Parsec.eof) ""
