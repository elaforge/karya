-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Sampler.Patch.Break_test where
import qualified Data.Map as Map

import qualified Synth.Sampler.Patch.Break as Break

import           Global
import           Util.Test


test_lookupStroke = do
    let f octave char = pretty <$>
            Break.lookupStroke (Break._increment break)
                (Break._perMeasure break) strokeMap octave char
        strokeMap = Map.fromList
            [(beat, stroke) | (beat, stroke, _) <- Break._beats break]
        break = Break.medeski
        c1 = ';'
        c2 = '\''
    equal (f 2 c1) Nothing
    equal (f 2 c2) (Just "[bd1]")
    equal (f 2 '2') (Just "[n 1.15]")
    equal (f 3 c1) (Just "[bd1]")
    equal (f 3 c2) (Just "[bd2]")
    equal (f 4 c1) (Just "[bd2]")
    equal (f 4 c2) (Just "[bd3]")
    equal (f 5 c1) (Just "[bd3]")
    equal (f 5 c2) (Just "[bd4]")
    equal (f 6 c1) (Just "[bd4]")
    equal (f 6 c2) (Just "[bd5]")
    equal (f 6 ',') Nothing
    equal (f 7 c1) (Just "[bd5]")
    equal (f 7 'q') Nothing
    equal (f 7 c2) Nothing
