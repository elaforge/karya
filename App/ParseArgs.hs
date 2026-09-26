-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Parse the command line and load files.
module App.ParseArgs (parseArgs) where
import qualified Control.Monad.Trans as Trans
import qualified Data.Text as Text

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Load.Midi as Load.Midi
import qualified Cmd.Save as Save
import qualified Cmd.SaveGit as SaveGit

import           Global


-- | By default, this winds up in 'App.StaticConfig.setup_cmd'.
parseArgs :: [String] -> Either Text (Cmd.CmdT IO Cmd.Status)
parseArgs = \case
    [] -> Right $ Save.loadTemplate "save/default" >> pure Cmd.Done
    -- Load a template.
    ["-t", fn] -> Right $ Save.loadTemplate fn >> pure Cmd.Done
    ["midi", fn] -> Right $ loadMidi fn
    [fn] -> Right $ Save.load fn >> pure Cmd.Done
    [fn, refOrCommit] -> Right $ do
        commit <- Cmd.require ("not a ref or commit: " <> txt refOrCommit)
            =<< Trans.liftIO (SaveGit.inferCommit fn refOrCommit)
        Save.loadGit fn (Just commit)
        pure Cmd.Done
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

loadMidi :: FilePath -> Cmd.CmdT IO Cmd.Status
loadMidi fn = do
    blockId <- Load.Midi.load fn
    Create.unfittedView blockId
    pure Cmd.Done
