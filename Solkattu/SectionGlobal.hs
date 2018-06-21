-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | DSL functions to add tags to Sections.
module Solkattu.SectionGlobal (
    startOn, endOn, eddupu
    -- * tags
    , scomment, sdate
    , devel, ending, var, local
    , variations
) where
import qualified Util.CallStack as CallStack
import qualified Util.Num as Num
import qualified Solkattu.Korvai as Korvai
import Solkattu.Korvai (Section)
import qualified Solkattu.Metadata as Metadata
import qualified Solkattu.Sequence as Sequence
import qualified Solkattu.Solkattu as Solkattu
import qualified Solkattu.Tags as Tags

import Global


-- | Set expected starting and ending time.  Useful for eddupu, or sections
-- which are split in the middle of an avartanam.
startOn, endOn :: Sequence.Duration -> Section stroke -> Section stroke
startOn dur section = section { Korvai.sectionStart = dur }
endOn dur section = section { Korvai.sectionEnd = dur }

-- | Like 'endOn', but also mark this section with eddupu.
eddupu :: Sequence.Duration -> Section sollu -> Section sollu
eddupu dur = withTag Tags.eddupu (pretty dur) . endOn dur

-- * tags

-- | Separate date for a section, mostly just so I can find its recording.
-- Unlike the korvai date it's just text.
sdate :: CallStack.Stack => Int -> Int -> Int -> Section sollu -> Section sollu
sdate y m d = either Solkattu.throw (const $ withTag Tags.date date) $
    Metadata.checkDate y m d
    where date = showt y <> "-" <> Num.zeroPad 2 m <> "-" <> Num.zeroPad 2 d

scomment :: Text -> Section sollu -> Section sollu
scomment = withTag Tags.comment

devel, ending, var :: Section sollu -> Section sollu
devel = withType Tags.development
ending = withType Tags.ending
var = withType Tags.variation

-- | On a transcription, this section is a local variation, not in the original
-- transcription.
local :: Section sollu -> Section sollu
local = withTag "local" ""

withType :: Text -> Section sollu -> Section sollu
withType = withTag Tags.type_

withTag :: Text -> Text -> Section sollu -> Section sollu
withTag k v = Korvai.addSectionTags (Tags.tag k v)

-- * util

variations :: [Korvai.SequenceT sollu] -> [Section sollu]
variations = map (var . Korvai.section)
