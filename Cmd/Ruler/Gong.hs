-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Rulers for Balinese and Javanese music.
module Cmd.Ruler.Gong where
import qualified Data.Set as Set

import qualified Ui.Meter.Meter as Meter

import           Global
import           Types


type Gongs = Int
type Jegogans = Int

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
regular :: Gongs -> Jegogans -> Meter.Meter
regular gongs jegogans = Meter.meter config (replicate gongs section)
    where
    section = Meter.MSection jegogans measure_dur meter
    meter = Meter.regular_subdivision [2, 2, 2, 2, 2, 2]

until :: TrackTime -> Meter.Meter
until end =
    Meter.modify_sections (Meter.sections_take end) $ regular gongs jegogans
    where
    jegogans = 4
    gongs = ceiling $ end / (fromIntegral jegogans * measure_dur)

measure_dur :: TrackTime
measure_dur = 2 -- This gives a reasonable kotekan speed at tempo=1.

-- | Gong config starts counting from 0.  This is more appropriate for Balinese
-- and Javenese music which are counted n 1 2 3 .. n
config :: Meter.Config
config = Meter.Config
    { config_labeled_ranks = labeled_ranks
    , config_label = Meter.BigNumber 0
    , config_start_measure = 1
    , config_min_depth = 1
    , config_strip_depth = 2
    }

labeled_ranks :: Set Meter.RankName
labeled_ranks = Set.fromList [Meter.Section, Meter.H, Meter.S, Meter.T128]
    -- Section: gong, W: gong stroke, H: jegog, Q: calung, E: kotekan*2,
    -- S: kotekan*4, ...
