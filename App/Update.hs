-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Load and save files to update them to the latest version.  Useful when
-- a non-versioned datatype changes.
--
-- Git saves are flattened into a plain saves.
--
-- @tools/update_all.py@ can update a whole directory of saves.
module App.Update where
import qualified Data.List as List
import qualified Data.Text.IO as Text.IO
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO

import qualified Util.Git as Git
import qualified Util.Regex as Regex
import qualified Util.Test.Testing as Testing

import qualified Cmd.Save as Save
import qualified Ui.Transform as Transform
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global


main :: IO ()
main = Git.initialize $ do
    args <- Environment.getArgs
    case args of
        [from_fn, to_fn] -> update from_fn to_fn
        _ -> fail_with "usage: update from_fn to_fn"

update :: FilePath -> FilePath -> IO ()
update from_fn to_fn
    | ".ky" `List.isSuffixOf` from_fn = do
        old <- Text.IO.readFile from_fn
        let new = upgrade_controls old
        when (old /= new) $ do
            putStrLn $ "ky file: " <> from_fn
            Text.IO.putStrLn $ Testing.colored_diff old new
        Text.IO.writeFile to_fn new
    | otherwise = do
        result <- Save.read_ from_fn
        case result of
            Left err -> fail_with $ "Reading " <> showt from_fn <> ": " <> err
            Right state -> do
                state <- upgrade state
                Save.write_state to_fn state

upgrade :: Ui.State -> IO Ui.State
upgrade state = do
    (state, changes) <- return $ Transform.map_code upgrade_controls state
    mapM_ Text.IO.putStrLn changes
    (state, diff) <- return $ map_ky upgrade_controls state
    when (diff /= "") $
        Text.IO.putStrLn $ "ky:\n" <> diff
    (state, diff) <- return $ map_ky upgrade_controls state
    when (diff /= "") $
        Text.IO.putStrLn $ "tscore:\n" <> diff
    return state

map_ky :: (Text -> Text) -> Ui.State -> (Ui.State, Text)
map_ky modify state =
    ( Ui.config#UiConfig.ky #= new $ state
    , if new == old then "" else Testing.colored_diff old new
    )
    where
    old = UiConfig.config_ky (Ui.state_config state)
    new = modify old

map_tscore :: (Text -> Text) -> Ui.State -> (Ui.State, Text)
map_tscore modify state =
    ( Ui.config#UiConfig.tscore #= new $ state
    , if new == old then "" else Testing.colored_diff old new
    )
    where
    old = UiConfig.config_tscore (Ui.state_config state)
    new = modify old

upgrade_controls :: Text -> Text
upgrade_controls = Regex.substituteGroups controls_re (\_ gs -> mconcat gs)

{-
    I could parse but maybe regex is enough?
    '%([0-9a-z._-] *=)' -> '\1'
    . tscore already uses leading %ky=''...'', and 'score = %default',
      don't match those.  I want track titles and event text and inside
      ky.
-}

controls_re :: Regex.Regex
controls_re = Regex.compileUnsafe "%([a-z0-9.-]+)\\b"

{-
    search: config_ky, config_tscore
    I could parse but maybe regex is enough?
    '%([0-9a-z._-] *=)' -> '\1'
    . tscore already uses leading %ky=''...'', and 'score = %default',
      don't match those.  I want track titles and event text and inside
      ky.
    . Show suggested changes, make changes, save backup.
-}

err_msg :: Text -> IO ()
err_msg = Text.IO.hPutStrLn IO.stdout

fail_with :: Text -> IO ()
fail_with msg = do
    err_msg msg
    Exit.exitWith (Exit.ExitFailure 1)
