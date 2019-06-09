-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions to deal with text score, as stored in 'UiConfig.config_tscore'.
module Cmd.Repl.LTScore where
import qualified App.ReplProtocol as ReplProtocol
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Repl.LState as LState

import qualified Derive.TScore.TScore as TScore
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global


-- | Edit both ky and tscore together.
both :: Ui.M m => m ReplProtocol.Result
both = do
    tscore <- edit
    ky <- LState.ky
    case (ky, tscore) of
        (ReplProtocol.Edit ky, ReplProtocol.Edit tscore) ->
            return $ ReplProtocol.Edit (tscore <> ky)
        _ -> Ui.throw "expected ReplProtocol.Edit"

edit :: Ui.M m => m ReplProtocol.Result
edit = do
    tscore <- get
    return $ ReplProtocol.Edit $ (:| []) $ ReplProtocol.Editor
        { _file = ReplProtocol.Text ReplProtocol.TScore tscore
        , _line_number = 0
        , _on_save = on_set
        , _on_send = on_set
        }
    where
    on_set = Just "LTScore.set %s"

get :: Ui.M m => m Text
get = Ui.config#UiConfig.tscore <#> Ui.get

set :: Cmd.M m => Text -> m ()
set tscore = do
    Ui.modify_config $ UiConfig.tscore #= tscore
    integrate

integrate :: Cmd.M m => m ()
integrate = do
    new_blocks <- TScore.cmd_integrate =<< get
    mapM_ Create.view new_blocks
