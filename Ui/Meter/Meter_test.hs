-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ui.Meter.Meter_test where
import qualified Util.Control as Control
import qualified Ui.Meter.Meter as Meter
import           Ui.Meter.Meter (AbstractMeter(..))
import qualified Ui.Meter.Meters as Meters

import           Util.Test


test_sections_drop :: Test
test_sections_drop = do
    let f s = map unsection . Meter.sections_drop s . map section
    equal (f 0 [(1, 1, Meters.m44)]) [(1, 1, 32)]
    equal (f 0 [(2, 1, Meters.m44)]) [(2, 1, 32)]
    equal (f 1 [(2, 1, Meters.m44)]) [(1, 1, 32)]
    equal (f 1 [(3, 1, Meters.m44)]) [(2, 1, 32)]
    -- round down if it's a fractional division
    equal (f 1.01 [(3, 1, Meters.m44)]) [(2, 1, 32)]
    -- round up
    equal (f 1.99 [(3, 1, Meters.m44)]) [(1, 1, 32)]
    equal (f (1+1/32) [(3, 1, Meters.m44)]) [(1, 31/32, 31), (1, 1, 32)]
    equal (f 1 [(1, 1, Meters.m44), (1, 1, Meters.m44)]) [(1, 1, 32)]
    equal (f 2 [(2, 1, Meters.m44)]) []
    equal (f 0.5 [(2, 1, Meters.m44)]) [(1, 0.5, 16), (1, 1, 32)]
    equal (f 1.5 [(2, 1, Meters.m44)]) [(1, 0.5, 16)]
    let m44_34 = [(2, 1, Meters.m44), (2, 3/4, Meters.m34)]
    equal (f 2 m44_34) [(2, 3/4, 24)]
    -- quarter is 1/4
    equal (f (2+3/4) m44_34) [(1, 3/4, 24)]
    equal (f 3 m44_34) [(1, 2/4, 16)]

test_sections_take :: Test
test_sections_take = do
    let f s = map unsection . Meter.sections_take s . map section
    let m442 = [(2, 1, Meters.m44)]
    equal (f 2 m442) [(2, 1, 32)]
    equal (f 1.5 m442) [(1, 1, 32), (1, 0.5, 16)]
    equal (f (1+2/32) m442) [(1, 1, 32), (1, 2/32, 2)]
    equal (f (1+1/32) m442) [(1, 1, 32), (1, 1/32, 1)]
    equal (f 1.99 m442) [(2, 1, 32)]
    equal (f 1.01 m442) [(1, 1, 32)]
    equal (f 1 m442) [(1, 1, 32)]
    equal (f 0 m442) []
    let m44_34 = [(2, 1, Meters.m44), (2, 3/4, Meters.m34)]
    equal (f (2+6/4) m44_34) [(2, 1, 32), (2, 3/4, 24)]
    equal (f (2+5/4) m44_34) [(2, 1, 32), (1, 3/4, 24), (1, 2/4, 16)]
    equal (f (2+4/4) m44_34) [(2, 1, 32), (1, 3/4, 24), (1, 1/4, 8)]
    equal (f (2+3/4) m44_34) [(2, 1, 32), (1, 3/4, 24)]

section :: (Meter.Measures, Meter.Duration, AbstractMeter) -> Meter.MSection
section = Control.uncurry3 Meter.MSection

unsection :: Meter.MSection -> (Meter.Measures, Meter.Duration, Int)
unsection (Meter.MSection c d m) = (c, d, Meter.meter_length m)

make_meter :: [(Meter.Measures, Meter.Duration, AbstractMeter)] -> Meter.Meter
make_meter = Meter.meter Meter.default_config . map section

test_meter_drop :: Test
test_meter_drop = do
    let f = Meter.meter_drop
    equal (f 0 T) $ Just T
    equal (f 0 (D [T, T])) $ Just $ D [T, T]
    equal (f 1 (D [T, T])) $ Just $ D [T]
    equal (f 2 (D [T, T])) Nothing
    equal (f 1 (D [D [T, T], T])) $ Just $ D [D [T], T]
    equal (f 2 (D [D [T, T], T])) $ Just $ D [T]
    equal (f 3 (D [D [T, T], T])) Nothing

test_meter_take :: Test
test_meter_take = do
    let f = Meter.meter_take
    equal [f n (D [D [T, T], D [T, T, T], T]) | n <- [0..7]]
        [ Nothing
        , Just $ D [D [T]]
        , Just $ D [D [T, T]]
        , Just $ D [D [T, T], D [T]]
        , Just $ D [D [T, T], D [T, T]]
        , Just $ D [D [T, T], D [T, T, T]]
        , Just $ D [D [T, T], D [T, T, T], T]
        , Just $ D [D [T, T], D [T, T, T], T]
        ]

test_meter_drop_end :: Test
test_meter_drop_end = do
    let f = Meter.meter_drop_end
    equal (f 0 T) $ Just T
    equal (f 0 (D [T, T])) $ Just $ D [T, T]
    equal (f 1 (D [T, T])) $ Just $ D [T]
    equal [f n (D [D [T, T], D [T, T, T], T]) | n <- [1..6]]
        [ Just $ D [D [T, T], D [T, T, T]]
        , Just $ D [D [T, T], D [T, T]]
        , Just $ D [D [T, T], D [T]]
        , Just $ D [D [T, T]]
        , Just $ D [D [T]]
        , Nothing
        ]
