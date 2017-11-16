-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ness.Util where
import qualified Prelude
import Prelude hiding (putStrLn)
import qualified Codec.Binary.Base64Url as Base64Url
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.QSem as QSem
import qualified Control.Exception as Exception

import qualified Data.Bits as Bits
import Data.Bits ((.&.))
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.Digest.CRC32 as CRC32
import qualified Data.Map as Map
import qualified Data.Text.IO as Text.IO

import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.Process as Process

import Util.Crc32Instances ()
import qualified Util.File as File
import qualified Util.Seq as Seq
import qualified Util.Thread as Thread

import qualified Synth.Shared.Config as Config
import Global
import Ness.Global (SamplingRate)
import qualified Ness.Sound as Sound
import qualified Ness.Submit as Submit


baseDir :: FilePath
baseDir = "ness-data"

concurrentSubmits :: Int
concurrentSubmits = 10

defaultSR :: SamplingRate
defaultSR = 11000

type Render score = SamplingRate -> score -> (Text, Text)

submitDir :: FilePath -> IO ()
submitDir dir = do
    (ok, url) <- Submit.submitDownload False
        (dir </> "inst") (dir </> "score") (dir </> "out.wav")
    if ok then play (dir </> "out.wav")
        else putStrLn $ "not ok: " <> show url

submitVariations :: SamplingRate -> Render score -> String -> FilePath
    -> [(FilePath, [score])] -> IO ()
submitVariations sr render model baseDir variations =
    submitMany sr render (model </> baseDir)
        [ (name </> zeroPad 3 idx, score)
        | (name, scores) <- variations
        , (idx, score) <- zip [0..] scores
        ]

submitMany :: SamplingRate -> Render score -> FilePath -> [(FilePath, score)]
    -> IO ()
submitMany sr render dir subdirScores = do
    let (subdirs, scores) = unzip subdirScores
    let rendered = map (render sr) scores
    submitAndCheck (baseDir </> dir) $
        zip3 (repeat ()) (map txt subdirs) rendered
    return ()

submitInstruments :: FilePath -> [(FilePath, Text, (Text, Text))] -> IO ()
submitInstruments dir outputNameScores = do
    let fprint = fingerprint $
            concat [[i, s] | (_, _, (i, s)) <- outputNameScores]
    let scratchDir = baseDir </> dir </> fprint
    let nameOutput = Map.fromList
            [(name, output) | (output, name, _) <- outputNameScores]
    outputFiles <- ifM (Directory.doesDirectoryExist scratchDir)
        (previousRender nameOutput scratchDir)
        (submitAndCheck scratchDir outputNameScores)
    Async.forConcurrently_ outputFiles $ \(url, output, scratch) -> do
        putStrLn $ if null url
            then scratch <> " -> " <> output
            else unwords ["download", show url, show scratch, show output]
        Directory.createDirectoryIfMissing True (FilePath.takeDirectory output)
        Sound.resample Config.samplingRate scratch output

-- | I think the web service gives no way to tell if the file is complete.
-- Redownload manually if it was partial.
download :: Submit.Url -> FilePath -> FilePath -> IO ()
download url scratch output = do
    ok <- Submit.download 2 scratch url
    unless ok $ errorIO "download failed"
    Sound.resample Config.samplingRate scratch output

previousRender :: Map Text FilePath -> FilePath
    -> IO [(Submit.Url, FilePath, FilePath)]
previousRender nameOutput dir = do
    subdirs <- File.list dir
    putStrLn $ "found previous submit: " <> show subdirs
    return $ mapMaybe get subdirs
    where
    get subdir = (\out -> ("", out, subdir </> "out.wav"))
        <$> Map.lookup (txt (FilePath.takeFileName subdir)) nameOutput

-- | Submit scores in their own subdirs, check the output for duplicate
-- responses, and pair them with their destination output.
submitAndCheck :: FilePath -> [(out, Text, (Text, Text))]
    -> IO [(Submit.Url, out, FilePath)]
submitAndCheck dir outputNameScores = do
    let (outputs, names, rendered) = unzip3 outputNameScores
    let subdirs = map untxt names
    okUrls <- forDelay concurrentSubmits (zip subdirs rendered) $
        \(subdir, (i, s)) -> submit i s (dir </> subdir)
    let (oks, urls) = unzip okUrls
    mapM_ putStrLn ["failed: " <> subdir | (Nothing, subdir) <- zip oks subdirs]
    forM_ (findDups fst (zip urls subdirs)) $ \(count, (url, subdir)) ->
        putStrLn $ "duplicate: " <> show count <> ": " <> url <> " -> "
            <> subdir
    return [(url, output, ok) | (output, (Just ok, url)) <- zip outputs okUrls]

findDups :: Ord k => (a -> k) -> [a] -> [(Int, a)]
findDups key = map (second head) . filter ((>1) . fst) . Seq.key_on length
    . Seq.group_sort key

submitOne :: String -> (Text, Text) -> IO ()
submitOne model (instrument, score) = do
    let dir = baseDir </> model </> dirFor instrument score
        out = dir </> "out.wav"
    out <- ifM (Directory.doesFileExist out)
        (putStrLn ("exists: " <> out) >> return (Just out))
        (fst <$> submit instrument score dir)
    print out
    whenJust out play

play :: FilePath -> IO ()
play fn = Process.callProcess "afplay" [fn]

submit :: Text -> Text -> FilePath -> IO (Maybe FilePath, Submit.Url)
submit instrument score dir = do
    let ifn = dir </> "inst"
    let sfn = dir </> "score"
    let out = dir </> "out.wav"
    Directory.createDirectoryIfMissing True dir
    Text.IO.writeFile ifn instrument
    Text.IO.writeFile sfn score
    (ok, url) <- Submit.submitDownload False ifn sfn out
    return $ if ok then (Just out, url) else (Nothing, url)

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
    , rsr :: SamplingRate -> IO ()
    }

interactive :: String -> Render score -> score -> Interactive score
interactive model render score = Interactive
    { r = submitOne model (render defaultSR score)
    , rsr = \sr -> submitOne model (render sr score)
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

putStrLn :: String -> IO ()
putStrLn s = do
    IO.hSetBuffering IO.stdout IO.LineBuffering
    Prelude.putStrLn s
