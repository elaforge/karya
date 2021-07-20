-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Sc.Patch (Patch(..), c_pitch) where
import qualified GHC.Generics as Generics

import qualified Util.Pretty as Pretty
import qualified Derive.ScoreT as ScoreT
import qualified Perform.Sc.Note as Note

import           Global


data Patch = Patch {
    name :: !Note.PatchName
    , duration_control :: !Note.ControlId
    , controls :: Map ScoreT.Control Note.ControlId
    } deriving (Eq, Show, Generics.Generic)

instance Pretty.Pretty Patch where format = Pretty.formatG

c_pitch :: ScoreT.Control
c_pitch = "pitch"
