-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Rulers for Balinese and Javanese music.
module Cmd.Ruler.Gong where
import qualified Data.Set as Set

import qualified Ui.Ruler as Ruler
import qualified Cmd.Ruler.Meter as Meter


{- | Create a number of gongs.

    Labels start from 0, where 0 represents the last note, as is usual.  So
    0, 1, 2, 3, 4, 5, 6, 7 can be read 8, 1, 2, 3, 4, 5, 6, 7, and the 8 will
    line up as expected.

    There is one section per gong, and each gong is numbered.  Then it's
    divided into w = variable number of gong strokes, h = 2 jegog,
    q = 2 calung, e = 2 kotekan groups, s = 8 (4*2) kotekan notes.
-}
gongs :: Int -- ^ number of gongs
    -> Int -- ^ number of strokes in one gong
    -> Ruler.Ruler
gongs sections strokes = Meter.make_measures config dur meter sections strokes
    where
    dur = 4 -- This gives a reasonable kotekan speed at tempo=1.

meter :: Meter.AbstractMeter
meter = Meter.regular_subdivision [2, 2, 2, 4, 2, 2]

labeled_ranks :: Set.Set Meter.RankName
labeled_ranks = Set.fromList [Meter.Section, Meter.H, Meter.S, Meter.T128]
    -- Section: gong, W: gong stroke, H: jegog, Q: calung, E: kotekan*2,
    -- S: kotekan*4, ...

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
