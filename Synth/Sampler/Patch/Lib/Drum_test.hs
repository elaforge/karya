module Synth.Sampler.Patch.Lib.Drum_test where
import qualified Synth.Sampler.Patch.Lib.Drum as Drum

import           Global
import           Util.Test.Global


test_pickWeighted :: Test
test_pickWeighted = do
    let f v = fromMaybe ' ' $
            Drum.pickWeighted v (zip "abcd" (repeat 0.25))
    equal (map f [0, 0.15, 0.25, 0.3]) "aabb"
    equal (map f [0.75, 1]) "dd"

test_pickDynWeighted :: Test
test_pickDynWeighted = do
    let samples = zip ['a'..'z'] [0, 0.125 .. 1]
    let f dyn var = fromMaybe (' ', 0) $
            Drum.pickDynWeighted 0.25 samples dyn var
    putStrLn $ unwords [c : ' ' : show n | (c, n) <- samples]
    equal (map (f 0.5) [0, 0.125 .. 1]) $ concat
        [ replicate 2 ('d', 1.125)
        , replicate 4 ('e', 1)
        , replicate 3 ('f', 1-0.125)
        ]
    equal (map (fst . f 0.24) [0, 0.125 .. 1]) "abbccccdd"
    equal (map (fst . f 0.0) [0, 0.125 .. 1]) "aaaaaabbb"
    equal (map (fst . f 1.0) [0, 0.125 .. 1]) "hhhiiiiii"
