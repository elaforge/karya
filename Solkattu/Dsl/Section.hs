-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | DSL functions to add tags to Sections.
module Solkattu.Dsl.Section (
    section
    , startOn, endOn, eddupu
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
import Solkattu.Korvai (Section, section)
import qualified Solkattu.Metadata as Metadata
import qualified Solkattu.S as S
import qualified Solkattu.Solkattu as Solkattu
import qualified Solkattu.Tags as Tags

import Global


-- | Set expected starting and ending time.  Useful for eddupu, or sections
-- which are split in the middle of an avartanam.
startOn, endOn :: S.Duration -> Section sollu -> Section sollu
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

devel, ending :: sollu -> Section sollu
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

variations :: [sollu] -> [Section sollu]
variations = map (var . Korvai.section)

{- NOTE [solkattu-sections]
    sections are syntactically noisy
    . I have to put 'section' on everything, just so I can have one with
      e.g. 'x2'.
    . Also they break maps, like 'map (nadai 6)' has to become
      'map (fmap (nadai 6))'
    . The simplest thing is to put the section data inside Sequence, and
      then pull it out, like Alignment.
    . It means I could embed section metadata in sollus and have it show up
      in multiple sections, or multiple times.  That seems not right,
      because they really are per-section, and in fact the point of sections
      is to have that stuff.
    . Or a ToSection class, but that just means I could use 'x2' and omit
      'section', but that's not worth it.
    . The ideal is an implicit 'section' on each element of the list,
      unless there is one.  Like automatic coercion, or dynamic types.  But
      haskell doesn't have either.
    . Maybe I just shorten 'section' to 's' and live with it.
-}
