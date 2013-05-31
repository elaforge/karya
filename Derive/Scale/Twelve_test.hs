-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.Twelve_test where
import qualified Data.Map as Map

import Util.Test
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Scale.TwelveScales as TwelveScales
import qualified Perform.Pitch as Pitch


test_note_to_nn = do
    let f = fmap snd
            . flip Map.lookup
                (TwelveScales.smap_note_to_degree Twelve.scale_map)
            . Pitch.Note
    equal (f "4c") (Just 60)
    equal (f "-1c") Nothing
    equal (f "-1c#") (Just 1)
    equal (f "-2b") Nothing
    equal (f "0c") (Just 12)
    equal (f "9g") (Just 127)
    equal (f "9g#") Nothing
    equal (f "10g") Nothing
