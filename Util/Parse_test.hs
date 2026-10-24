-- Copyright 2022 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Parse_test where
import qualified Data.Set as Set
import qualified Text.Megaparsec as P

import qualified Util.Parse as Parse
import qualified Util.ParseText as ParseText

import           Global
import           Util.Test


test_attoparse :: Test
test_attoparse = do
    let nat = Parse.pNat
    let anat = Parse.attoparse "anat" ParseText.pNat
    let parse = Parse.parse
    let comma p1 p2 = parse ((,) <$> p1 <*> ("," *> p2))
    rightEqual (parse nat "42") 42
    rightEqual (parse anat "42") 42
    rightEqual (comma nat nat "1,2") (1, 2)
    rightEqual (comma anat nat "1,2") (1, 2)
    rightEqual (comma nat anat "1,2") (1, 2)
    rightEqual (comma anat anat "1,2") (1, 2)
    leftLike (comma nat nat "a,2") "expecting nat"
    leftLike (comma anat nat "a,2") "expected anat"
    leftLike (comma nat anat "1,a") "expected anat"

test_errorBundle :: Test
test_errorBundle = do
    match (render multiple "") "fname:1:1:*fname:1:1:*"
    match (render multiToken "abcde") "fname:1:1:*^^^*"
    where
    render parser input = case Parse.parseE "fname" parser input of
        Left bundle -> ParseText.showError $ ParseText.fromMegaparsec bundle
        Right _ -> "parser unexpectedly succeeded"
    multiple = do
        P.registerFailure Nothing Set.empty
        P.registerFailure Nothing Set.empty
    multiToken = P.failure (Just $ P.Tokens ('a' :| "bc")) Set.empty
