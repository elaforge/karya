-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Midi.Instrument_test where
import qualified Data.Text as Text

import Util.Test
import qualified Derive.Score as Score
import qualified Perform.Midi.Instrument as Instrument


attr_map = Instrument.simple_keyswitches
    [ (mkattrs "pizz", 0)
    , (mkattrs "sfz trem", 1)
    , (mkattrs "sfz", 2)
    , (mkattrs "trem", 3)
    ]
mkattrs = Score.attrs . Text.words
unattrs = Text.unwords . Score.attrs_list

test_overlapping_attributes = do
    let overlapping = Instrument.AttributeMap
            [ (mkattrs "", [Instrument.Keyswitch 0], Nothing)
            , (mkattrs "a b", [Instrument.Keyswitch 1], Nothing)
            ]
    let f = Instrument.overlapping_attributes
    equal (f attr_map) []
    equal (f overlapping) ["attrs +a+b shadowed by +"]
