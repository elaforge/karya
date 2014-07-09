-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Tala_test where
import qualified Data.Map as Map

import Util.Control
import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.Ruler as Ruler
import qualified Cmd.Meter as Meter
import qualified Cmd.Tala as Tala
import Types


test_make_ruler = do
    let adi = Tala.ruler $ Tala.make_meter [Tala.Ruler Tala.adi_tala 1 1 4 1]
        extract zoom = map snd . extract_marklist zoom . snd . snd . head
            . Map.toList . Ruler.ruler_marklists
    equal (extract 20 adi)
        ["1.0", "1.1", "1.2", "1.3", "1.X", "1.O", "1.X", "1.O", "2.0"]

test_make_meter = do
    let f = Meter.labeled_marklist . Tala.make_meter
        extract = extract_marklist 20
        chatusra = Tala.Ruler Tala.adi_tala 1 1 4 1
        tisra = Tala.Ruler Tala.adi_tala 1 1 3 1
        round_trip = Meter.labeled_marklist . Meter.marklist_labeled
    let labels = [showt n <> "." <> o | n <- [1, 2, 3],
            o <- ["0", "1", "2", "3", "X", "O", "X", "O"]]
    -- Ensure that concatenated marklists get the right labelling, and that
    -- rulers with 1/3 divisions still wind up at integral points.  Previously,
    -- Meter.Meter used ScoreTime, which got inaccurate after consecutive
    -- additions.
    equal (extract $ f [chatusra, tisra]) (zip (Seq.range 0 16 1) labels)
    equal (extract $ round_trip $ f [chatusra, tisra])
        (zip (Seq.range 0 16 1) labels)

extract_marklist :: Double -> Ruler.Marklist -> [(ScoreTime, Text)]
extract_marklist zoom = mapMaybe name_of . Ruler.ascending 0
    where
    name_of (t, m)
        | Ruler.mark_name_zoom_level m <= zoom = Just (t, Ruler.mark_name m)
        | otherwise = Nothing
