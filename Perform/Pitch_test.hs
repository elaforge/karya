module Perform.Pitch_test where
import Util.Test
import qualified Perform.Pitch as Pitch


relatives = map (uncurry Pitch.Generic)
    [ (0, 0)
    , (0, 1.1)
    , (1, 0)
    , (0, 0.25)
    , (0, -1)
    , (-1, -1)
    ]

test_from_relative = do
    equal (map Pitch.from_relative relatives) $ map Pitch.Note
        [ "+0"
        , "+1.1"
        , "+1/"
        , "+.25"
        , "-1"
        , "-1/-1"
        ]

test_to_relative = do
    equal (map (Pitch.to_relative . Pitch.from_relative) relatives)
        (map Just relatives)
