-- Copyright 2024 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions to integrate javanese text score.
module Cmd.Repl.LJScore where
import qualified Data.Text as Text

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Derive.JScore.JScore as JScore

import           Global


integrate_file :: FilePath -> Cmd.CmdL Text
integrate_file fname = do
    (new_blocks, errors) <- JScore.integrate_file fname
    mapM_ Create.view new_blocks
    pure $ Text.unlines errors
