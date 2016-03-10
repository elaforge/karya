-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | A @.ghci@ file makes sure this module is in scope when debugging
-- interactively.
module D where
import qualified Data.Text as Text

import qualified Util.PPrint as PPrint
import qualified Util.Pretty as Pretty
import qualified Ui.Id as Id
import Types


-- | Rename 'Pretty.pprint' so it doesn't conflict with "Util.Test".
ppr :: Pretty.Pretty a => a -> IO ()
ppr = Pretty.pprint

pp :: Show a => a -> IO ()
pp = PPrint.pprint

mkid :: Text.Text -> Id.Id
mkid name = Id.read_short _default_ns name

bid :: Text.Text -> BlockId
bid = Id.BlockId . mkid

vid :: Text.Text -> ViewId
vid = Id.ViewId . mkid

tid :: Text.Text -> TrackId
tid = Id.TrackId . mkid

rid :: Text.Text -> RulerId
rid = Id.RulerId . mkid

-- | Change this to whatever namespace you're debugging.
_default_ns :: Id.Namespace
_default_ns = Id.namespace ""
