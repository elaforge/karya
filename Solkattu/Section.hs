-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions to deal with per-section tags.  This is the section version
-- of "Solkattu.Korvai.Metadata".
module Solkattu.Section where
import qualified Solkattu.Korvai as Korvai
import Solkattu.Korvai (Section)
import Global


-- * types

devel = withType "development"
ending = withType "ending"

withType :: Text -> Section sollu -> Section sollu
withType = withTag "type"

withTag :: Text -> Text -> Section sollu -> Section sollu
withTag k v = Korvai.withSectionTags (Korvai.tag k v)
