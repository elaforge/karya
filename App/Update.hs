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
        [fromFn, toFn] -> update Nothing fromFn toFn
        ["unify-env", fromFn, toFn] ->
            update (Just upgradeControls) fromFn toFn
        _ -> failWith "usage: update from_fn to_fn"

update :: Maybe (Text -> Text) -> FilePath -> FilePath -> IO ()
update mbModify fromFn toFn
    | ".ky" `List.isSuffixOf` fromFn = do
        old <- Text.IO.readFile fromFn
        new <- case mbModify of
            Nothing -> pure old
            Just modify -> do
                let new = modify old
                when (old /= new) $ do
                    putStrLn $ "ky file: " <> fromFn
                    Text.IO.putStrLn $ Diffs.colored1 old new
                pure new
        Text.IO.writeFile toFn new
    | otherwise = do
        result <- Save.read_ fromFn
        case result of
            Left err -> failWith $ "Reading " <> showt fromFn <> ": " <> err
            Right state -> do
                state <- maybe (pure state) (\m -> upgradeScore m state)
                    mbModify
                Save.writeState toFn state

-- * upgrade

-- | Upgrade from %c=1 control namespace to unified c=1 style.
-- I probably won't use this again, but it's an example for when I need to do
-- a mass upgrade again.
upgradeControls :: Text -> Text
upgradeControls = Regex.substituteGroups controlsRe (\_ gs -> mconcat gs)
    where controlsRe = Regex.compileUnsafe "%([a-z0-9.-]+)\\b"

{-
    I could parse but maybe regex is enough?
    '%([0-9a-z._-] *=)' -> '\1'
    . tscore already uses leading %ky=''...'', and 'score = %default',
      don't match those.  I want track titles and event text and inside
      ky.
-}

upgradeScore :: (Text -> Text) -> Ui.State -> IO Ui.State
upgradeScore modify state = do
    (state, changes) <- pure $ Transform.mapCode modify state
    mapM_ Text.IO.putStrLn changes
    (state, diff) <- pure $ mapKy modify state
    when (diff /= "") $
        Text.IO.putStrLn $ "ky:\n" <> diff
    (state, diff) <- pure $ mapKy modify state
    when (diff /= "") $
        Text.IO.putStrLn $ "tscore:\n" <> diff
    pure state

mapKy :: (Text -> Text) -> Ui.State -> (Ui.State, Text)
mapKy modify state =
    ( Ui.config#UiConfig.ky #= new $ state
    , if new == old then "" else Diffs.colored1 old new
    )
    where
    old = UiConfig.configKy (Ui.stateConfig state)
    new = modify old

mapTscore :: (Text -> Text) -> Ui.State -> (Ui.State, Text)
mapTscore modify state =
    ( Ui.config#UiConfig.tscore #= new $ state
    , if new == old then "" else Diffs.colored1 old new
    )
    where
    old = UiConfig.configTscore (Ui.stateConfig state)
    new = modify old

-- * util

errMsg :: Text -> IO ()
errMsg = Text.IO.hPutStrLn IO.stdout

failWith :: Text -> IO ()
failWith msg = do
    errMsg msg
    Exit.exitWith (Exit.ExitFailure 1)
