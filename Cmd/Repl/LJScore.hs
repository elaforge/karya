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


integrateFile :: FilePath -> Cmd.CmdL Text
integrateFile fname = JScore.integrateFile fname >>= \case
    Left errors -> pure $ Text.unlines errors
    Right (mbToplevel, newBlocks) -> do
        whenJust mbToplevel (void . Create.view)
        pure $ Text.unwords (map pretty (maybe id (:) mbToplevel newBlocks))
