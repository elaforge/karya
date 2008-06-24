module Instrument.Parse_test where
import qualified Data.Either as Either
import qualified Text.ParserCombinators.Parsec as Parsec

import Util.Test
import qualified Util.PPrint as PPrint

import qualified Instrument.Parse as Parse


parse parser s = case Parsec.parse parser "" s of
    Left err -> do
        putStrLn $ "error: " ++ show err
        return Nothing
    Right val -> return (Just val)

test_patch_line = do
    let parse_p = parse Parse.p_patch_line
    io_equal (parse_p "") Nothing -- no \n
    io_equal (parse_p "inst\n") (Just ("inst", Nothing, Nothing, Nothing))
    io_equal (parse_p "inst, cat, -10,12, 1.23\n")
        (Just ("inst", Just "cat", Just (-10, 12), Just (Just 1.23)))
    io_equal (parse_p "inst, cat, -10,12, _\n")
        (Just ("inst", Just "cat", Just (-10, 12), Just Nothing))

test_patch_file = do
    let parse_p = parse Parse.p_patch_file
    io_equal (parse_p "") (Just [])
    PPrint.pprint =<< parse_p "inst1, cat1\ninst2\n"
