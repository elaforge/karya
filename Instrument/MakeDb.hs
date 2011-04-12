import System.FilePath ((</>))
import Control.Monad
import qualified Util.Log as Log

import qualified Instrument.MidiDb as MidiDb
import qualified Instrument.Serialize as Serialize
import qualified Instrument.Search as Search

import qualified Local.Instrument
import qualified App.Config as Config


main :: IO ()
main = Local.Instrument.make_dbs =<< Config.get_app_dir
