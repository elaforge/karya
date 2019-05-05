-- Copyright 2019 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Miscellaneous general purpose functions.
module Solkattu.Dsl.Misc where
import qualified Util.Seq as Seq


replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x = Seq.modify_at i (const x)
