module Instrument.MakeDb where
import qualified System.Environment as Environment

import qualified Local.Instrument
import qualified App.Config as Config


main :: IO ()
main = do
    args <- Environment.getArgs
    case args of
        [] -> Local.Instrument.make_dbs =<< Config.get_app_dir
        _ -> Local.Instrument.make_named_dbs args =<< Config.get_app_dir
