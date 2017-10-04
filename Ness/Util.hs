module Ness.Util where
import qualified Data.Text.IO as Text.IO
import System.FilePath ((</>))
import qualified System.Process as Process

import Global


scratchDir :: FilePath
scratchDir = "ness-data"

run :: String -> Text -> Text -> IO FilePath
run model instrument score = do
    let ifn = scratchDir </> model ++ ".inst"
    let sfn = scratchDir </> model ++ ".score"
    let out = scratchDir </> model ++ "-out.wav"
    Text.IO.writeFile ifn instrument
    Text.IO.writeFile sfn score
    Process.callProcess "build/opt/ness-submit" [ifn, sfn, out]
    Process.callProcess "afplay" [out]
    return out

play :: String -> IO ()
play model =
    Process.callProcess "afplay" [scratchDir </> model ++ "-out.wav"]
