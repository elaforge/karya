-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | DSL functions to add tags to Sections.
module Solkattu.SectionGlobal (
    startOn, endOn, eddupu
    -- * tags
    , commentS, dateS
    , devel, ending, var, local
    , times, x2, x3, x4
    , variations
    , withTypeS
) where
import qualified Util.CallStack as CallStack
import qualified Util.Num as Num
import qualified Solkattu.Korvai as Korvai
import Solkattu.Korvai (Section)
import qualified Solkattu.Metadata as Metadata
import qualified Solkattu.S as S
import qualified Solkattu.Solkattu as Solkattu
import qualified Solkattu.Tags as Tags

import Global


-- | Set expected starting and ending time.  Useful for eddupu, or sections
-- which are split in the middle of an avartanam.
startOn, endOn :: S.Duration -> Section stroke -> Section stroke
startOn dur section = section { Korvai.sectionStart = dur }
endOn dur section = section { Korvai.sectionEnd = dur }

-- | Like 'endOn', but also mark this section with eddupu.
eddupu :: S.Duration -> Section sollu -> Section sollu
eddupu dur = withTag Tags.eddupu (pretty dur) . endOn dur

-- * tags

-- | Separate date for a section, mostly just so I can find its recording.
-- Unlike the korvai date it's just text.
dateS :: CallStack.Stack => Int -> Int -> Int -> Section sollu -> Section sollu
dateS y m d = either Solkattu.throw (const $ withTag Tags.date date) $
    Metadata.checkDate y m d
    where date = showt y <> "-" <> Num.zeroPad 2 m <> "-" <> Num.zeroPad 2 d

commentS :: Text -> Section sollu -> Section sollu
commentS = withTag Tags.comment

devel, ending :: Korvai.SequenceT sollu -> Section sollu
devel = withTypeS Tags.development . Korvai.section
ending = withTypeS Tags.ending . Korvai.section

var :: Section sollu -> Section sollu
var = withTypeS Tags.variation

-- | On a transcription, this section is a local variation, not in the original
-- transcription.
local :: Section sollu -> Section sollu
local = withTag "local" ""

-- | Performance instruction to repeat this section.
times :: Int -> Section sollu -> Section sollu
times n = withTag Tags.times (showt n)

x2, x3, x4 :: Section sollu -> Section sollu
x2 = times 2
x3 = times 3
x4 = times 4

withTypeS :: Text -> Section sollu -> Section sollu
withTypeS = withTag Tags.type_

withTag :: Text -> Text -> Section sollu -> Section sollu
withTag k v = Korvai.addSectionTags (Tags.tag k v)

-- * util

variations :: [Korvai.SequenceT sollu] -> [Section sollu]
variations = map (var . Korvai.section)
