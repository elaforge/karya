-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Instrument.Common_test where
import qualified Data.Text as Text

import Util.Test
import qualified Derive.ScoreTypes as ScoreTypes
import qualified Instrument.Common as Common
import Global


test_overlapping_attributes = do
    let overlapping = Common.AttributeMap
            [ (mkattrs "", 0)
            , (mkattrs "a b", 1)
            ]
    let f = Common.overlapping_attributes
    equal (f attr_map) []
    equal (f overlapping) ["attrs +a+b shadowed by +"]

attr_map = Common.attribute_map
    [ (mkattrs "pizz", 0)
    , (mkattrs "sfz trem", 1)
    , (mkattrs "sfz", 2)
    , (mkattrs "trem", 3)
    ]

mkattrs :: Text -> ScoreTypes.Attributes
mkattrs = ScoreTypes.attrs . Text.words
