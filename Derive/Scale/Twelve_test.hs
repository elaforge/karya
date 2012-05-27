module Derive.Scale.Twelve_test where
import qualified Data.Map as Map

import Util.Test
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Scale.TwelveUtil as TwelveUtil
import qualified Perform.Pitch as Pitch


test_note_to_nn = do
    let f = fmap snd
            . flip Map.lookup (TwelveUtil.sys_note_to_degree Twelve.system)
            . Pitch.Note
    equal (f "4c") (Just 60)
    equal (f "-1c") Nothing
    equal (f "-1c#") (Just 1)
    equal (f "-2b") Nothing
    equal (f "0c") (Just 12)
    equal (f "9g") (Just 127)
    equal (f "9g#") Nothing
    equal (f "10g") Nothing
