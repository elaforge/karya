-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | A @.ghci@ file makes sure this module is in scope when debugging
-- interactively.
module D where
import qualified Data.Maybe as Maybe

import qualified Ui.Id as Id
import qualified Ui.Types as Types
import Types


mkid :: String -> Id.Id
mkid name = Maybe.fromMaybe
    (error $ "D.mkid: invalid characters in " ++ show name)
    (Id.read_short _default_ns name)

bid :: String -> BlockId
bid = Types.BlockId . mkid

vid :: String -> ViewId
vid = Types.ViewId . mkid

tid :: String -> TrackId
tid = Types.TrackId . mkid

rid :: String -> RulerId
rid = Types.RulerId . mkid

-- | Change this to whatever namespace you're debugging.
_default_ns :: Id.Namespace
_default_ns = Id.unsafe_namespace "mridangam"
