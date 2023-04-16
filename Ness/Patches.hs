-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ness.Patches where
import qualified Data.Map as Map
import qualified Util.Lists as Lists
import Global
import qualified Ness.Guitar as Guitar
import qualified Ness.Guitar.Patch as Guitar.Patch
import qualified Ness.Multiplate as Multiplate
import qualified Ness.Multiplate.Patch as Multiplate.Patch


patches :: Map Text Patch
patches = Map.fromList $ Lists.keyOn patchName $
    map PGuitar Guitar.Patch.instruments
    ++ map PMultiplate Multiplate.Patch.instruments

data Performance =
    Guitar Guitar.Instrument Guitar.Score
    | Multiplate Multiplate.Instrument Multiplate.Score
    deriving (Show)

data Patch = PGuitar Guitar.Instrument | PMultiplate Multiplate.Instrument
    deriving (Eq, Ord, Show)

performanceName :: Performance -> Text
performanceName p = case p of
    Guitar i _ -> "guitar-" <> Guitar.iName i
    Multiplate i _ -> "multiplate-" <> Multiplate.iName i

patchName :: Patch -> Text
patchName i = case i of
    PGuitar i -> "guitar-" <> Guitar.iName i
    PMultiplate i -> "multiplate-" <> Multiplate.iName i
