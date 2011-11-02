module Derive.Scale.Twelve_test where
import qualified Data.Map as Map

import Util.Test

import qualified Perform.Pitch as Pitch
import qualified Derive.Scale.Twelve as Twelve


test_note_to_nn = do
    let f = flip Map.lookup Twelve.note_to_degree . Pitch.Note
    equal (f "4c") (Just 60)
    equal (f "-1c") Nothing
    equal (f "-1c#") (Just 1)
    equal (f "-2b") Nothing
    equal (f "0c") (Just 12)
    equal (f "9g") (Just 127)
    equal (f "9g#") Nothing
