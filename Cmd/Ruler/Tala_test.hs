-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Ruler.Tala_test where
import qualified Data.List as List
import qualified Data.Text as Text

import qualified Cmd.Ruler.Tala as Tala
import qualified Ui.Meter.Make as Make
import qualified Ui.Meter.Mark as Mark

import           Global
import           Types
import           Util.Test


test_make_ruler :: Test
test_make_ruler = do
    let make zoom = extract_marklist zoom . Make.make_measures
    -- TODO used to be at zoom 20, now is at 200
    equal (map snd $ make 200 (Tala.adi 1)) $
        Text.words "1 .1 .2 .3 .X .O .X .O 2"
    equal (map snd $ extract_marklist 400 $ Make.make_measures (Tala.adi3 1)) $
        List.intercalate ["..2", "..3"] $ map (:[]) $
        Text.words "1 .1 .2 .3 .X .O .X .O 2"
    let ranks = map (Mark.mark_rank . snd) $ extract_zoom 8000 $
            Make.make_measures (Tala.adi3 1)
    equal (filter (<=4) ranks)
        [ 0, 4, 4, 3, 4, 4, 3, 4, 4, 3, 4, 4
        , 2, 4, 4, 3, 4, 4, 2, 4, 4, 3, 4, 4
        , 0
        ]

{-
    1.0.1   1       0 Section, avartanam, anga
    1.0.2   ..2     4 nadai
    1.0.3   ..3     4 nadai
    1.1.1   .1      3 Quarter akshara
    1.1.2   ..2     4
    1.1.3   ..3     4
    1.2     .2      3 akshara
    1.3     .3      3 akshara
    1.X     .X      2 anga
    1.O     .O      3 akshara
    1.X     .X      2 anga
    1.O     .O      3 akshara
    2       2       1 Avartanam

-- * r_section is several avartanam -- sections
-- * r_1 avartanam -- avartanams
-- * r_2 anga -- based on tala
-- * r_4 akshara -- basad on akshara type and jati
-- * r_8 nadai / gati -- variable
-- . r_16 -- 2
-- * r_32 -- 2
-- . r_64 -- 2
-- * r_128 -- 2
-- . r_256 -- 2
-}

-- test_make_meter :: Test
-- test_make_meter = do
--     let f = Meter.labeled_marklist . Tala.make_meter
--         extract = extract_marklist 20
--         chatusra = Tala.Ruler Tala.adi_tala 1 1 4 1
--         tisra = Tala.Ruler Tala.adi_tala 1 1 3 1
--         round_trip = Meter.labeled_marklist . Meter.marklist_labeled
--     let labels = map Meter.join_label $ Meter.strip_prefixes "" 2
--             [ Meter.biggest_label (showt n) : if Text.null o then [] else [o]
--             | n <- [1, 2, 3], o <- adi
--             ]
--         adi = "" : map Meter.big_label ["1", "2", "3", "X", "O", "X", "O"]
--     -- Ensure that concatenated marklists get the right labelling, and that
--     -- rulers with 1/3 divisions still wind up at integral points.  Previously,
--     -- Meter.Meter used ScoreTime, which got inaccurate after consecutive
--     -- additions.
--     equal (extract $ f [chatusra, tisra]) (zip (Seq.range 0 16 1) labels)
--     equal (extract $ round_trip $ f [chatusra, tisra])
--         (zip (Seq.range 0 16 1) labels)

extract_zoom :: Double -> [(a, Mark.Mark)] -> [(a, Mark.Mark)]
extract_zoom zoom = filter ((<= zoom) . Mark.mark_name_zoom_level . snd)

extract_marklist :: Double -> Make.Marklist -> [(ScoreTime, Text)]
extract_marklist zoom =
    map (second (Make.strip_markup . Mark.mark_name)) . extract_zoom zoom
