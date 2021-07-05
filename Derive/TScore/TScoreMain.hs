-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Standalone driver for tscore.
module Derive.TScore.TScoreMain where
import qualified Data.Text.IO as Text.IO
import qualified System.Environment as Environment

import qualified Util.Pretty as Pretty
import qualified Cmd.Simple as Simple
import qualified Derive.TScore.TScore as TScore
import qualified Ui.Ui as Ui

import           Global


main :: IO ()
main = do
    [fname] <- Environment.getArgs
    score <- Text.IO.readFile fname
    ui_state <- either errorIO return $ TScore.parse_score score
    dump <- either (errorIO . pretty) return $
        Ui.eval ui_state $ Simple.dump_state
    Pretty.pprint dump
