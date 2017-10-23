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

submitVariations :: (score -> (Text, Text)) -> String
    -> FilePath -> [(FilePath, [score])] -> IO ()
submitVariations render model baseDir variations =
    submitMany render (model </> baseDir)
        [ (name </> zeroPad 3 idx, score)
        | (name, scores) <- variations
        , (idx, score) <- zip [0..] scores
        ]

submitMany :: (score -> (Text, Text)) -> FilePath -> [(FilePath, score)]
    -> IO ()
submitMany render dir scores = do
    okUrls <- forDelay concurrentSubmits scores $ \(name, score) -> do
        let (i, s) = render score
        submit i s (scratchDir </> dir </> name)
    let (oks, urls) = unzip okUrls
    mapM_ putStrLn ["failed: " <> name | (False, (name, _)) <- zip oks scores]
    forM_ (findDups fst (zip urls (map fst scores))) $ \(count, (url, name)) ->
        putStrLn $ "duplicate: " <> show count <> ": " <> url
            <> " -> " <> name

findDups :: Ord k => (a -> k) -> [a] -> [(Int, a)]
findDups key = map (second head) . filter ((>1) . fst) . Seq.key_on length
    . Seq.group_sort key

submitOne :: String -> (Text, Text) -> IO ()
submitOne model (instrument, score) = do
    let dir = scratchDir </> model </> dirFor instrument score
    let out = dir </> "out.wav"
    ok <- ifM (Directory.doesDirectoryExist dir)
        (putStrLn ("exists: " <> dir) >> return True)
        (fst <$> submit instrument score dir)
    putStrLn out
    when ok $ Process.callProcess "afplay" [out]

submit :: Text -> Text -> FilePath -> IO (Bool, Submit.Url)
submit instrument score dir = do
    let ifn = dir </> "inst"
    let sfn = dir </> "score"
    let out = dir </> "out.wav"
    Directory.createDirectoryIfMissing True dir
    Text.IO.writeFile ifn instrument
    Text.IO.writeFile sfn score
    Submit.submitDownload False ifn sfn out

-- TODO separate out SR and put differing SR in the same dir?
dirFor :: Text -> Text -> FilePath
dirFor instrument score = fingerprint [instrument, score]

fingerprint :: [Text] -> FilePath
fingerprint texts =
    dropR (=='=') $ ByteString.Char8.unpack $ Base64Url.encode $
        ByteString.pack bytes
    where
    n = CRC32.crc32 texts
    chop = fromIntegral . (.&. 0xff)
    bytes = map (chop . Bits.shiftR n) [0, 8, 16, 24]
    dropR f = reverse . dropWhile f . reverse

data Interactive score = Interactive {
    r :: IO ()
    , variations :: FilePath -> [(FilePath, [score])] -> IO ()
    }

interactive :: String -> (score -> (Text, Text)) -> score -> Interactive score
interactive model render score = Interactive
    { r = submitOne model (render score)
    , variations = submitVariations render model
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