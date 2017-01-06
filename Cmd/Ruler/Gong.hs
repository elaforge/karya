-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Rulers for Balinese and Javanese music.
module Cmd.Ruler.Gong where
import qualified Data.Set as Set

import qualified Ui.Ruler as Ruler
import qualified Cmd.Ruler.Meter as Meter
import Global


{- | Create a number of gongs, each divided into a number of jegogan strokes.

    Labels start from 0, where 0 represents the last note.  So 0, 1, 2, 3, 4,
    5, 6, 7 can be read 8, 1, 2, 3, 4, 5, 6, 7, and in a 16 count cycle the the
    8 will be on the middle count as expected.

    + lines have labels, and 4 jegogan per gong:

    @
          01234567012345670123456701234567012345670123456701234567012345670
    + s 2 kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk 1/8t
    . e 2 k k k k k k k k k k k k k k k k k k k k k k k k k k k k k k k k k
    . q 2 k   k   k   k   k   k   k   k   k   k   k   k   k   k   k   k   k 1/2t
    + h 2 c       c       c       c       c       c       c       c       c 1t
    . w 4 J               J               J               J               J 2t
    .     o                               p                               o
    + Sec O                                                               O
          0       1       2       3       4       5       6       7       8
          0                                                               1
    @
-}
gongs :: Int -- ^ number of gongs
    -> Int -- ^ number of jegogan in one gong
    -> Ruler.Ruler
gongs sections jegog =
    Meter.make_measures config measure_dur meter sections jegog
    where
    measure_dur = 2 -- This gives a reasonable kotekan speed at tempo=1.

meter :: Meter.AbstractMeter
meter = Meter.regular_subdivision [2, 2, 2, 2, 2, 2]

labeled_ranks :: Set Meter.RankName
labeled_ranks = Set.fromList [Meter.Section, Meter.H, Meter.S, Meter.T128]

-- | Gong config starts counting from 0.  This is more appropriate for Balinese
-- and Javenese music.
config :: Meter.MeterConfig
config = Meter.default_config
    { Meter.config_labeled_ranks = labeled_ranks
    , Meter.config_label_components = Meter.big_number_components 0 0
    , Meter.config_meter_type = mtype
    }

-- | Balinese \"meters\" are just standard numbered meters, but each section
-- starts from 0, instead of 1.
mtype :: Ruler.MeterType
mtype = "gong"
