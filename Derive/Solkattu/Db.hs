-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Collect korvais into a searchable form.
module Derive.Solkattu.Db where
import qualified Derive.Solkattu.Korvai as Korvai
import qualified Derive.Solkattu.All as All -- generated


korvais :: [Korvai.Korvai]
korvais = All.korvais
