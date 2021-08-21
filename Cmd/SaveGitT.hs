-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Types used by "Cmd.SaveGit".  This is so Cmd.Cmd doesn't have to incur
-- a dependency on the libgit2 bindings.  This doesn't matter so much now that
-- it's an external package, but it still seems like isolating dependencies
-- is a good thing.
module Cmd.SaveGitT where
import qualified Util.GitT as GitT
import qualified Util.Pretty as Pretty
import qualified Ui.Ui as Ui
import qualified Ui.Update as Update
import Global


-- | History to be saved to disk.  The updates are post-diff to know which bits
-- of state to write, and the commit is what commit the updates are relative
-- to, if any.  If they're Nothing, then save everything.
--
-- It's very important to bundle the commit and updates together, because
-- without the commit to know what they are relative to, the updates don't
-- mean anything, and if they're applied on top of the wrong commit the result
-- will be a corrupted state.
data SaveHistory =
    SaveHistory !Ui.State !(Maybe GitT.Commit) [Update.UiUpdate] ![Text]
    deriving (Show)

instance Pretty SaveHistory where
    format (SaveHistory _state commit updates cmds) =
        Pretty.record "SaveHistory"
            [ ("commit", Pretty.format commit)
            , ("updates", Pretty.format updates)
            , ("cmds", Pretty.format cmds)
            ]

-- | Git wants these fields for commits.  It probably doesn't matter much
-- what they are, but they might as well be accurate.
data User = User { name :: !Text, email :: !Text } deriving (Show)
