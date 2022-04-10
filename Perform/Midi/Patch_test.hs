-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Midi.Patch_test where
import Util.Test
import qualified Derive.Attrs as Attrs
import qualified Perform.Midi.Patch as Patch
import qualified Instrument.Common as Common


test_convert_scale :: Test
test_convert_scale = do
    let f scale = Patch.convert_scale (Patch.make_scale "name" scale)
    equal (map (f [(2, 2)]) [1, 2, 3]) [Nothing, Just 2, Nothing]
    equal (map (f [(2, 2), (4, 4)]) [1, 2, 3, 4, 5])
        [Nothing, Just 2, Just 3, Just 4, Nothing]

test_scale_tuning :: Test
test_scale_tuning = do
    let f attr_map = Patch.scale_tuning attr_map
        scale = Patch.make_scale "scale" [(1, 2), (2, 3.5)]
    equal (take 4 $ f Nothing scale)
        [Nothing, Just (1, 2), Just (2, 3.5), Nothing]
    let attr_map = Common.attribute_map
            [ (Attrs.mute, ([Patch.Keyswitch 0],
                Just (Patch.PitchedKeymap 2 6 1)))
            , (mempty, ([Patch.Keyswitch 1],
                Just (Patch.PitchedKeymap 8 12 1)))
            ]
    equal (take 12 $ f (Just attr_map) scale)
        [ Nothing, Nothing
        , Just (1, 2), Just (2, 3.5), Nothing, Nothing
        , Nothing, Nothing
        , Just (1, 2), Just (2, 3.5), Nothing, Nothing
        ]
