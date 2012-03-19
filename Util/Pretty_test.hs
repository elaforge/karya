module Util.Pretty_test where
import Util.Pretty


-- Some hand tests for the pretty printer.  Not sure how to automate this...
test_pretty = do
    let max = 30 :: Int
        pp :: Pretty a => a -> IO ()
        pp = putStrLn . render . format
    -- putStrLn $ pretty [0..max]
    -- pp [0..max]
    -- pp [0..max]
    -- pp [0..max]
    -- pp "some string"
    -- pp ("aaaaaaaaaaaaaaaaaaaa", "bbbbbbbbbbbbbbbbbbbbbbb")
    -- pp $ Map.fromList $ zip [0..max] ['a'..'z']
    -- -- ugly because it should preferentially wrap on the pair divisions
    -- -- Not sure how to do that though.
    -- pp $ Map.fromList $ zip [0..max] (replicate max "some big long line stuff")
    pp $ record $ replicate 5 ("some_big_long_label", format max)
    pp $ record $ replicate 2 ("ssnatoheusantohusnaotheuome_big_long_labelatohusaotnehusaotneuhsaoetuh", format max)
    -- As with Map, I want to prefer to not split on the 'x = y'
    pp $ record $ replicate 30 ("a", format max)
    pp $ record $ replicate 5 ("medium_label", format [0..max])
