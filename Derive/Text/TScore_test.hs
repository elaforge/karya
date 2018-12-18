-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Derive.Text.TScore_test where
import qualified Text.Parsec as Parsec

import Util.Test
import qualified Derive.Text.TScore as TScore
import Global


test_score = do
    let f = fmap (\(TScore.Score notes) -> notes) . parse TScore.p_score
    let bar = TScore.TBarline . TScore.Barline
        no_dur = TScore.Duration Nothing 0 False
        no_oct = TScore.Relative 0
    -- TODO test roundtrip
    equal (f "") $ Right []
    equal (f "||") $ Right [bar 2]
    equal (f " | || ") $ Right [bar 1, bar 2]
    equal (f "a") $ Right [note "" no_oct "a" no_dur]
    equal (f "a -- hi") $ Right [note "" no_oct "a" no_dur]
    let rest = TScore.TRest . TScore.Rest
    equal (f "_4 | _.") $ Right
        [ rest (TScore.Duration (Just 4) 0 False)
        , bar 1
        , rest (TScore.Duration Nothing 1 False)
        ]

    equal (f "a b/") $ Right
        [ note "" no_oct "a" no_dur
        , note "b" no_oct "" no_dur
        ]
    equal (f "\"a b\"/") $ Right [note "a b" no_oct "" no_dur]
    equal (f "\"a \"() b\"/") $ Right [note "a \"() b" no_oct "" no_dur]

test_note = do
    let f = parse TScore.parse
    let no_dur = TScore.Duration Nothing 0 False
    left_like (f "") "unexpected end of input"
    equal (f "a") $ Right $ note "" (TScore.Relative 0) "a" no_dur
    equal (f "+pizz/") $ Right $ note "+pizz" (TScore.Relative 0) "" no_dur
    equal (f "a/'b1.~") $ Right $
        note "a" (TScore.Relative 1) "b" (TScore.Duration (Just 1) 1 True)

note :: Text -> TScore.Octave -> Text -> TScore.Duration -> TScore.Token
note call oct pitch dur = TScore.TNote $
    TScore.Note (TScore.Call call) (TScore.Pitch oct pitch) dur

parse :: TScore.Parser a -> Text -> Either String a
parse p = first show . Parsec.parse (p <* Parsec.eof) ""
