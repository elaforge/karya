module Ness.Util where
import qualified Codec.Binary.Base64Url as Base64Url
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.QSem as QSem
import qualified Control.Exception as Exception

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
import qualified Util.Seq as Seq
import qualified Util.Thread as Thread

import Global
import qualified Ness.Submit as Submit


scratchDir :: FilePath
scratchDir = "ness-data"

concurrentSubmits :: Int
concurrentSubmits = 10

submitMultiple :: (score -> (Text, Text)) -> String
    -> FilePath -> [(FilePath, [score])] -> IO ()
submitMultiple render model baseDir variations = do
    let base = scratchDir </> model </> baseDir
    Directory.createDirectoryIfMissing True base
    let jobs =
            [ (base </> name </> zeroPad 3 idx, render var)
            | (name, vars) <- variations, (idx, var) <- zip [0..] vars
            ]
    okUrls <- forDelay concurrentSubmits jobs $
        \(d, (i, s)) -> submit False i s d
    let (oks, urls) = unzip okUrls
    mapM_ putStrLn ["failed: " <> name | (False, (name, _)) <- zip oks jobs]
    forM_ (findDups fst (zip urls (map fst jobs))) $ \(count, (url, name)) ->
        putStrLn $ "duplicate: " <> show count <> ": " <> url
            <> " -> " <> name

findDups :: Ord k => (a -> k) -> [a] -> [(Int, a)]
findDups key = map (second head) . filter ((>1) . fst) . Seq.key_on length
    . Seq.group_sort key

submitOne :: String -> (Text, Text) -> Bool -> IO ()
submitOne model (instrument, score) demo = do
    let dir = scratchDir </> model </> dirFor demo instrument score
    let out = dir </> "out.wav"
    ok <- ifM (Directory.doesDirectoryExist dir)
        (putStrLn ("exists: " <> dir) >> return True)
        (fst <$> submit demo instrument score dir)
    putStrLn out
    when ok $ Process.callProcess "afplay" [out]

submit :: Bool -> Text -> Text -> FilePath -> IO (Bool, Submit.Url)
submit demo instrument score dir = do
    let ifn = dir </> "inst"
    let sfn = dir </> "score"
    let out = dir </> "out.wav"
    Directory.createDirectoryIfMissing True dir
    Text.IO.writeFile ifn instrument
    Text.IO.writeFile sfn score
    Submit.submitDownload demo ifn sfn out

-- TODO separate out SR and put differing SR in the same dir?
dirFor :: Bool -> Text -> Text -> FilePath
dirFor demo instrument score =
    dropR (=='=') $ ByteString.Char8.unpack $ Base64Url.encode $
        ByteString.pack bytes
    where
    n = CRC32.crc32 (demo, instrument, score)
    chop = fromIntegral . (.&. 0xff)
    bytes = map (chop . Bits.shiftR n) [0, 8, 16, 24]
    dropR f = reverse . dropWhile f . reverse

data Interactive score = Interactive {
    r :: IO ()
    , demo :: IO ()
    , multiple :: FilePath -> [(FilePath, [score])] -> IO ()
    }

interactive :: String -> (score -> (Text, Text)) -> score -> Interactive score
interactive model render score = Interactive
    { r = submitOne model (render score) False
    , demo = submitOne model (render score) True
    , multiple = submitMultiple render model
    }

forConcurrent :: Int -> [a] -> (a -> IO b) -> IO [b]
forConcurrent threads as f = mapConcurrent threads f as

forDelay :: Int -> [a] -> (a -> IO b) -> IO [b]
forDelay threads as f = mapDelay threads f as

mapDelay :: Int -> (a -> IO b) -> [a] -> IO [b]
mapDelay threads f =
    fmap concat . mapM (Async.mapConcurrently go) . map (zip [0..])
        . Seq.chunked threads
    where go (i, a) = Thread.delay (fromIntegral i * 2) >> f a

mapConcurrent :: Int -> (a -> IO b) -> [a] -> IO [b]
mapConcurrent threads f as = do
    avail <- QSem.newQSem threads
    -- seconds <- MVar.newMVar IntSet.empty
    Async.forConcurrently as $ \a ->
        Exception.bracket_ (QSem.waitQSem avail) (QSem.signalQSem avail) $ do
            -- waitForSecond seconds
            f a
    -- where
    -- waitForSecond seconds = do
    --     now <- nowSeconds
    --     ok <- MVar.modifyMVar seconds (return . check now)
    --     if ok
    --         then return ()
    --         else Thread.delay 1 >> waitForSecond seconds
    --
    -- check now seconds
    --     | IntSet.member now seconds = (IntSet.insert now seconds, True)
    --     | otherwise = (seconds, False)

-- nowSeconds :: IO Int
-- nowSeconds = floor <$> Clock.POSIX.getPOSIXTime
--
-- t0 xs = do
--     IO.hSetBuffering IO.stdout IO.LineBuffering
--     mapDelay 3 (\x -> do
--         putStrLn $ "start " ++ show x
--         Thread.delay $ fromIntegral x / 10
--         putStrLn $ "end " ++ show x
--         return (x + 1)) xs

zeroPad :: Show a => Int -> a -> String
zeroPad chars n = replicate (chars - length s) '0' ++ s
    where s = show n
