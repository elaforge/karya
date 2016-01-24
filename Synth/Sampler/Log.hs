-- | Placeholder logging until I can reuse the one from karya.
module Log where
import Prelude hiding (error)
import qualified Data.Text.IO as Text.IO

import Global


debug, notice, warn, error :: Text -> IO ()
debug   = Text.IO.putStrLn . ("*    "<>)
notice  = Text.IO.putStrLn . ("**   "<>)
warn    = Text.IO.putStrLn . ("***  "<>)
error   = Text.IO.putStrLn . ("**** "<>)
