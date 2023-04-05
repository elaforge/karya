-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Load and save files to update them to the latest version.  Useful when
-- a non-versioned datatype changes.
--
-- Git saves are flattened into a plain saves.
--
-- @tools/update_all.py@ can update a whole directory of saves.
module App.Update (main) where
import qualified Data.List as List
import qualified Data.Text.IO as Text.IO
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO

import qualified Util.Diffs as Diffs
import qualified Util.Git as Git
import qualified Util.Regex as Regex

import qualified Cmd.Save as Save
import qualified Ui.Transform as Transform
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global


main :: IO ()
main = Git.initialize $ do
    args <- Environment.getArgs
    case args of
        [from_fn, to_fn] -> update Nothing from_fn to_fn
        ["unidy-env", from_fn, to_fn] ->
            update (Just upgrade_controls) from_fn to_fn
        _ -> fail_with "usage: update from_fn to_fn"

update :: Maybe (Text -> Text) -> FilePath -> FilePath -> IO ()
update mb_modify from_fn to_fn
    | ".ky" `List.isSuffixOf` from_fn = do
        old <- Text.IO.readFile from_fn
        new <- case mb_modify of
            Nothing -> return old
            Just modify -> do
                let new = modify old
                when (old /= new) $ do
                    putStrLn $ "ky file: " <> from_fn
                    Text.IO.putStrLn $ Diffs.colored1 old new
                return new
        Text.IO.writeFile to_fn new
    | otherwise = do
        result <- Save.read_ from_fn
        case result of
            Left err -> fail_with $ "Reading " <> showt from_fn <> ": " <> err
            Right state -> do
                state <- maybe (return state) (\m -> upgrade_score m state)
                    mb_modify
                Save.write_state to_fn state

-- * upgrade

-- | Upgrade from %c=1 control namespace to unified c=1 style.
-- I probably won't use this again, but it's an example for when I need to do
-- a mass upgrade again.
upgrade_controls :: Text -> Text
upgrade_controls = Regex.substituteGroups controls_re (\_ gs -> mconcat gs)
    where controls_re = Regex.compileUnsafe "%([a-z0-9.-]+)\\b"

{-
    I could parse but maybe regex is enough?
    '%([0-9a-z._-] *=)' -> '\1'
    . tscore already uses leading %ky=''...'', and 'score = %default',
      don't match those.  I want track titles and event text and inside
      ky.
-}

upgrade_score :: (Text -> Text) -> Ui.State -> IO Ui.State
upgrade_score modify state = do
    (state, changes) <- return $ Transform.map_code modify state
    mapM_ Text.IO.putStrLn changes
    (state, diff) <- return $ map_ky modify state
    when (diff /= "") $
        Text.IO.putStrLn $ "ky:\n" <> diff
    (state, diff) <- return $ map_ky modify state
    when (diff /= "") $
        Text.IO.putStrLn $ "tscore:\n" <> diff
    return state

map_ky :: (Text -> Text) -> Ui.State -> (Ui.State, Text)
map_ky modify state =
    ( Ui.config#UiConfig.ky #= new $ state
    , if new == old then "" else Diffs.colored1 old new
    )
    where
    old = UiConfig.config_ky (Ui.state_config state)
    new = modify old

map_tscore :: (Text -> Text) -> Ui.State -> (Ui.State, Text)
map_tscore modify state =
    ( Ui.config#UiConfig.tscore #= new $ state
    , if new == old then "" else Diffs.colored1 old new
    )
    where
    old = UiConfig.config_tscore (Ui.state_config state)
    new = modify old

-- * util

err_msg :: Text -> IO ()
err_msg = Text.IO.hPutStrLn IO.stdout

fail_with :: Text -> IO ()
fail_with msg = do
    err_msg msg
    Exit.exitWith (Exit.ExitFailure 1)
