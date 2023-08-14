-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Rulers for Balinese and Javanese music.
module Cmd.Ruler.Gong (
    Gongs, Jegogans
    , regular, until
    , measure_dur
    , meter
    , config
    -- * java
    , java
) where
import           Prelude hiding (until)
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
        t 0                               1                               2
    @

    So it needs 4 "measures" in one section to make a full 8 count.
-}
regular :: Gongs -> Jegogans -> Meter.Meter
regular gongs jegogans = Meter.meter config (replicate gongs section)
    where section = Meter.MSection jegogans measure_dur meter

until :: TrackTime -> Meter.Meter
until end =
    Meter.modify_sections (Meter.sections_take end) $ regular gongs jegogans
    where
    jegogans = 4
    gongs = ceiling $ end / (fromIntegral jegogans * measure_dur)

-- | This gives a reasonable kotekan speed at tempo=1.  It makes kotekan into
-- "s" with one "cycle" of 8 as a "h".  A "w" is 2 cycles and 8 per section.
measure_dur :: TrackTime
measure_dur = 2

meter :: Meter.AbstractMeter
meter = Meter.regular_subdivision [2, 2, 2, 2, 2, 2]

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

labeled_ranks :: Set Meter.Rank
labeled_ranks = Set.fromList [Meter.Section, Meter.H, Meter.S, Meter.T128]
    -- Section: gong, W: gong stroke, H: jegog, Q: calung, E: kotekan*2,
    -- S: kotekan*4, ...

-- * java

{-
        t 0               1               2               3               1 4t
    + Sec 1                                                               2
    + w 4 gatra           1               2               3               4 1t
    . h 2 ir1     .       .       .       .       .       .       .       . 1/2t
    + q 2 ir2 .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   . 1/4t
    . e 2 ir3 . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 1/8t
    + s 2 ir4 .............................................................
          01234567012345670123456701234567012345670123456701234567012345670
    gong    8x gatra
    kenong  2x gatra, or 4x gatra
            S   2t      gatra
    gatra   w   1t
            h   1/2t    note
    note    q   1/4t    ir1
    ir1     e   1/8     ir2
    ir2     s   1/16    ir3
    ir3     t32 1/32    ir4
    ir4     t64 1/64

        cmd ts  subdiv T    function    peking panerus
    +   1   S   n      n    kenong/pul
    +   2   w   2      1    gatra
        3   h   2      1/2
    +   4   q   2      1/4  note        lancar
        5   e   2      1/8              ir1
    +   6   s   2      1/16             ir2     ir1
        7   t32 2      1/32             ir3     ir2
        8   t64 2      1/64             ir4     ir3
            t128 2     1/128                    ir4

        cmd ts  subdiv T    function    peking panerus
    +   1   S   4      4    gatra
    +   2   w   2      1    note        lancar
        3   h   2      1/2              ir1
    +   4   q   2      1/4              ir2
        5   e   2      1/8              ir3
    +   6   s   2      1/16             ir4
        7   t32 2      1/32
        8   t64        1/64

        cmd ts  subdiv T    function    panerus
    +   1   S   n      n    kenong/pul
    +   2   w   4      1    gatra
        3   h   2      1/4  ir1 peking
    +   4   q   2      1/8              ir1
        5   e   2      1/16             ir2
    +   6   s   2      1/32             ir3
        7   t32 2      1/64             ir4
        8   t64        1/128

    This way I can get the range down to 128, but is it awkward to have a 1/4
    right at the top?  I will surely have notes moving at that speed.

    It's difficult if I expect to keep a constant meter across such a wide
    tempo range.  Alternately, I could expect we 2x the tempo at some point.
    What if I 2x tempo at every irama change, and instead keep the meter
    constant per irama?  Gatra length will then change.

    This won't track if I use the same balungan, but that doesn't seem like a
    big deal, except where the rulers won't line up.

    Tempo jumps for irama, timestep relative to irama would be say peking is
    always 1/8.  Panerus / gambang at 1/16.

                tempo   peking  note        gatra
        lancar  .5      e       1/8t e      1/2t h
        ir1     .5      e       1/4t q        1t w
        ir2     .5      e       1/2t h        2  w2
        ir3     .5      e         1t w        4  w4
        ir4     .5      e         2t w2       8  w8

    Tempo continuous, timestep absolute would say 1 gatra = 1t, and peking
    changes per irama:

                tempo   peking  gp  note    gatra
        lancar  1       q        e  1/4t q  1t w
        ir1     .5      e        s  1/4t q  1t w
        ir2     .25     s      t32  1/4t q  1t w
        ir3     .125    t32    t64  1/4t q  1t w
        ir4     .0625   t64   t128  1/4t q  1t w

    This is unworkable, I don't have t128 and I could see going to t256.  Also
    tempos wind up very small if I stick to 1 = 1t = 1s.  If I say one note is
    1t then tempos are x4 and more reasonable.  Rulers get bigger if I need
    to go down to 1/256... though it would mean I only need one kind which is
    shared.  Also this is implying I may have a block of say 8t being one whole
    section.  But in ir4 that's a lot of music, and w timestep is basically
    sections.  It could make a block always represent a section, but that's not
    necessarily a convenient unit to work with.  A block doesn't have to be a
    whole section, but if it's not then the lower end of timesteps are not
    useful.  This is the whole tradeoff with using a wide range of timesteps.

    Another approach would be to make a timestep shift, e.g. shift cmd-1 to be
    w, then h, etc.  This would adjust the UI but it would be one more bit of
    state to remember.  The only other option would be to reduce the resolution
    of timesteps, e.g. by putting some 4 subdivisions in there, but there's no
    convenient place.

    Or, abandon setting absolute timestep, and instead have +/- relative, like
    with octaves.

    Or, base timestep on zoom, which is already relative.  Adjust timestep to
    be the visible step on the current zoom level.  What about note duration?
    I don't seem to use that much, but I can have a relative adjustment for it.

    But if I do discontinuous tempo, what are the ruler divisions?  I
    definitely want gatras to be counted, but they are varying!  I would have
    to change ruler by irama, which is possible actually.  But that means
    for the score track, I'd want to do the promote child ruler thing, since
    ruler is changing.

        cmd ts  subdiv T    function    panerus
    +   1   S          n    kenong/pul
    +   2   w   4      1    gatra
        3   h   4      1/4  ir1 peking
    +   4   q   2      1/8              ir1
        5   e   2      1/16             ir2
    +   6   s   2      1/32             ir3
        7   t32 2      1/64             ir4
        8   t64 2      1/128

    . irama:            1/2    1        2    3     4
                        lancar tanggung dadi wilet rangkep
      peking / note     1       2        4    8     16
      panerus / note    2       4        8   16     32
      peking / gatra    4       8       16   32     64
      panerus / gatra   8      16       32   64    128

      But, this is just ScoreTime, which is fungible.  The main thing is the
      range of timesteps, which is 1-8 aka S to 1/64.  If they are mostly
      binary, then
      1=S, 2=g, 3=1/4, 4 q 1/4t ir1
      1
      2
      3
      4 1/4 q
      5 1/8 e
      6 1/16 s
      7 1/32
      8 1/64

    One nice thing about constant ScoreTime is that the tempo track will
    reflect the actual overall tempo, with no discontinuities, due to
    notational convenience.  Since I don't have note flags, shouldn't I take
    advantage of that?
-}

-- | (number, name, peking/gatra)
irama :: [(Double, Text, Int)]
irama =
    [ (1/2, "lancar",   4)
    , (1,   "tanggung", 8)
    , (2,   "dadi",     16)
    , (3,   "wilet",    32)
    , (4,   "rangkep",  64)
    ]

{-
        cmd ts  subdiv   T4    T  function    peking panerus
        1   S   n              n  kenong/pul
    +   2   w   2         4    1  gatra
        3   h   2         2  1/2
    +   4   q   2         1  1/4  note        lancar
        5   e   2       1/2  1/8              ir1
        6   s   2       1/4 1/16              ir2     ir1
        7   t32 2       1/8 1/32              ir3     ir2
    +   8   t64 2      1/16 1/64              ir4     ir3
            t128 2     1/32 1/128                     ir4
-}

java :: Int -> Meter.Meter
java lines = Meter.meter java_config (replicate lines section)
    where
    section = Meter.MSection
        { section_measures = 4
        , section_measure_duration = 4
        , section_measure = meter
        }
    meter = Meter.regular_subdivision [2, 2, 2, 2, 2, 2]

java_config :: Meter.Config
java_config = Meter.Config
    { config_labeled_ranks = Set.fromList [Meter.W, Meter.Q, Meter.T64]
    , config_label = Meter.BigNumber 0
    , config_start_measure = 1
    , config_min_depth = 1
    , config_strip_depth = 2
    }
