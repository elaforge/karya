module Ness.Util where
import qualified Codec.Binary.Base64Url as Base64Url
import qualified Data.Bits as Bits
import Data.Bits ((.&.))
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.Digest.CRC32 as CRC32
import qualified Data.Text.IO as Text.IO

import qualified System.Directory as Directory
import System.FilePath ((</>))
import qualified System.Process as Process

import Util.Crc32Instances ()
import Global
import qualified Ness.Submit as Submit


scratchDir :: FilePath
scratchDir = "ness-data"

submit :: String -> Text -> Text -> Bool -> IO ()
submit model instrument score demo = do
    let dir = scratchDir </> model </> dirFor demo instrument score
    let exists = putStrLn ("exists: " <> dir) >> return True
    let out = dir </> "out.wav"
    ok <- ifM (Directory.doesDirectoryExist dir) exists $ do
        let ifn = dir </> "inst"
        let sfn = dir </> "score"
        Directory.createDirectory dir
        Text.IO.writeFile ifn instrument
        Text.IO.writeFile sfn score
        Submit.submitDownload demo ifn sfn out
    when ok $ Process.callProcess "afplay" [out]

-- TODO separate out SR and put differing SR in the same dir?
dirFor :: Bool -> Text -> Text -> FilePath
dirFor demo instrument score =
    ByteString.Char8.unpack $ Base64Url.encode $ ByteString.pack bytes
    where
    n = CRC32.crc32 (demo, instrument, score)
    chop = fromIntegral . (.&. 0xff)
    bytes = map (chop . Bits.shiftR n) [0, 8, 16, 24]

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
