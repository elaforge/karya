module Ness.Util where
import qualified Data.Text.IO as Text.IO
import System.FilePath ((</>))
import qualified System.Process as Process

import qualified Util.File as File
import Global
import qualified Ness.Submit as Submit


scratchDir :: FilePath
scratchDir = "ness-data"

submit :: String -> Text -> Text -> Bool -> IO ()
submit model instrument score demo = do
    let ifn = scratchDir </> model ++ ".inst"
    let sfn = scratchDir </> model ++ ".score"
    let out = scratchDir </> model ++ "-out.wav"
    oldInst <- File.ignoreEnoent $ Text.IO.readFile ifn
    oldScore <- File.ignoreEnoent $ Text.IO.readFile sfn
    ok <- if Just instrument == oldInst && Just score == oldScore
        then return True
        else do
            Text.IO.writeFile ifn instrument
            Text.IO.writeFile sfn score
            Submit.submitDownload demo ifn sfn out
    when ok $ Process.callProcess "afplay" [out]

replayModel :: String -> IO ()
replayModel model =
    Process.callProcess "afplay" [scratchDir </> model ++ "-out.wav"]

data Interactive = Interactive {
    replay :: IO ()
    , r :: IO ()
    , demo :: IO ()
    }

interactive :: String -> (i -> s -> (Text, Text)) -> i -> s -> Interactive
interactive model renderAll instrument score = Interactive
    { replay = replayModel model
    , r = let (i, s) = renderAll instrument score
        in submit model i s False
    , demo = let (i, s) = renderAll instrument score
        in submit model i s True
    }
