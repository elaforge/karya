-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Parse the command line and load files.
module App.ParseArgs (parse_args, open_keycaps) where
import qualified Control.Monad.Trans as Trans
import qualified Data.Text as Text

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Load.Midi as Load.Midi
import qualified Cmd.Save as Save
import qualified Cmd.SaveGit as SaveGit
import qualified Cmd.SyncKeycaps as SyncKeycaps

import           Global


-- | By default, this winds up in 'App.StaticConfig.setup_cmd'.
parse_args :: [String] -> Either Text (Cmd.CmdT IO Cmd.Status)
parse_args = run_after SyncKeycaps.open $ \case
    [] -> Right $ Save.load_template "save/default" >> return Cmd.Done
    -- Load a template.
    ["-t", fn] -> Right $ Save.load_template fn >> return Cmd.Done
    ["midi", fn] -> Right $ load_midi fn
    [fn] -> Right $ Save.load fn >> return Cmd.Done
    [fn, ref_or_commit] -> Right $ do
        commit <- Cmd.require ("not a ref or commit: " <> txt ref_or_commit)
            =<< Trans.liftIO (SaveGit.infer_commit fn ref_or_commit)
        Save.load_git fn (Just commit)
        return Cmd.Done
    args -> Left $ Text.unlines
        [ "Expected:"
        , "<filename>       - load score"
        , "<repo.git>       - load git repo"
        , "<repo.git> <hash> - load git repo at a particular commit\
            \\n    (GIT_DIR=score.git git log to see them)"
        , "-t <template>    - load score but don't set default save path"
        , "midi <score.mid> - load MIDI file"
        , ""
        , "But got: " <> pretty args
        ]

load_midi :: FilePath -> Cmd.CmdT IO Cmd.Status
load_midi fn = do
    block_id <- Load.Midi.load fn
    Create.unfitted_view block_id
    return Cmd.Done

-- | Open the keycaps window after doing whatever setup.
open_keycaps :: ([String] -> Either Text (Cmd.CmdT IO Cmd.Status))
    -> [String] -> Either Text (Cmd.CmdT IO Cmd.Status)
open_keycaps = run_after SyncKeycaps.open

run_after :: Cmd.CmdT IO ()
    -> ([String] -> Either Text (Cmd.CmdT IO Cmd.Status)) -> [String]
    -> Either Text (Cmd.CmdT IO Cmd.Status)
run_after post setup = \args -> case setup args of
    Right cmd -> Right $ cmd <* post
    Left err -> Left err
