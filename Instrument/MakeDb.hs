import qualified Local.Instrument
import qualified App.Config as Config


main :: IO ()
main = Local.Instrument.make_dbs =<< Config.get_app_dir
