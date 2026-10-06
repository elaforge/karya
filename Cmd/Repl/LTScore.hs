-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions to deal with text score, as stored in 'UiConfig.config_tscore'.
module Cmd.Repl.LTScore where
import qualified App.ReplProtocol as ReplProtocol
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Derive.TScore.TScore as TScore
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global


edit :: Ui.M m => m ReplProtocol.Result
edit = do
    tscore <- get
    pure $ ReplProtocol.Edit $ (:| []) $ ReplProtocol.Editor
        { _file = ReplProtocol.Text ReplProtocol.TScore tscore
        , _lineNumber = 0
        , _onSave = onSet
        , _onSend = onSet
        }
    where
    onSet = Just "LTScore.set %s"

get :: Ui.M m => m Text
get = Ui.config#UiConfig.tscore <#> Ui.get

set :: Cmd.M m => Text -> m ()
set tscore = do
    Ui.modifyConfig $ UiConfig.tscore #= tscore
    integrate

integrate :: Cmd.M m => m ()
integrate = do
    newBlocks <- TScore.cmdIntegrate =<< get
    mapM_ Create.view newBlocks
