-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | DSL functions to add tags to Sections.
module Solkattu.SectionGlobal (
    startOn, endOn
    -- * tags
    , devel, ending, localVar
) where
import qualified Solkattu.Korvai as Korvai
import Solkattu.Korvai (Section)
import qualified Solkattu.Sequence as Sequence
import qualified Solkattu.Tags as Tags

import Global


startOn, endOn :: Sequence.Duration -> Section stroke -> Section stroke
startOn dur section = section { Korvai.sectionStart = dur }
endOn dur section = section { Korvai.sectionEnd = dur }

-- * tags

devel, ending, localVar :: Section sollu -> Section sollu
devel = withType Tags.development
ending = withType Tags.ending
localVar = withType "local_variation"

withType :: Text -> Section sollu -> Section sollu
withType = withTag Tags.type_

withTag :: Text -> Text -> Section sollu -> Section sollu
withTag k v = Korvai.withSectionTags (Tags.tag k v)
