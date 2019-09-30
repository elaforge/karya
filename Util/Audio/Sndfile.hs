-- Copyright 2019 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Wrap "Sound.File.Sndfile" for better errors and to work around bugs.
module Util.Audio.Sndfile (
    Handle
    , openFile, hInfo, hClose, hSeek, hGetBuffer, hPutBuffer
    , ignoreEnoent
    , module Sndfile
) where
import qualified Control.Exception as Exception
import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Sound.File.Sndfile as Sndfile
import           Sound.File.Sndfile
       hiding (Handle, openFile, hInfo, hClose, hSeek, hGetBuffer, hPutBuffer)

import           Global


data Handle = Handle {
    _filename :: !FilePath
    -- | libsndfile has no protection against multiple closes on the same
    -- handle and happily double frees memory, and hsndfile provides no
    -- protection either.
    , _isOpen :: !(IORef.IORef Bool)
    , _handle :: !Sndfile.Handle
    }

instance Show Handle where
    show hdl = "(Handle " <> show (_filename hdl) <> ")"

openFile :: FilePath -> IOMode -> Info -> IO Handle
openFile fname mode info = withFilename "openFile" fname $ do
    hdl <- Sndfile.openFile fname mode info
    open <- IORef.newIORef True
    return $ Handle fname open hdl

hInfo :: Handle -> Info
hInfo = Sndfile.hInfo . _handle

hClose :: Handle -> IO ()
hClose hdl = whenM (IORef.readIORef (_isOpen hdl)) $ do
    IORef.atomicWriteIORef (_isOpen hdl) False
    -- hsndfile is buggy and calls sf_error(nullptr) after closing the handle,
    -- which gets any previous error that might still be in a static variable.
    Sndfile.hClose (_handle hdl)
        `Exception.catch` \(_exc :: Sndfile.Exception) -> return ()

hSeek :: Handle -> Count -> IO Count
hSeek hdl count
    | 0 <= count && count <= frames =
        Sndfile.hSeek (_handle hdl) AbsoluteSeek count
    -- Otherwise libsndfile will throw a much more confusing error:
    -- "Internal psf_fseek() failed."
    -- It's ok to seek to the end of the file though, and that happens when the
    -- resample consumed all samples, but they're in its internal buffer.
    | otherwise = Exception.throw $ Sndfile.Exception $
        "tried to seek to " <> show count <> " in " <> show (_filename hdl)
        <> ", but it only has " <> show frames
    where frames = Sndfile.frames $ Sndfile.hInfo $ _handle hdl

hGetBuffer :: (Sample e, Buffer a e) => Handle -> Count -> IO (Maybe (a e))
hGetBuffer hdl count = withFilename "hGetBuffer" (_filename hdl) $
    Sndfile.hGetBuffer (_handle hdl) count

hPutBuffer :: (Sample e, Buffer a e) => Handle -> a e -> IO Count
hPutBuffer hdl buf = Sndfile.hPutBuffer (_handle hdl) buf

-- | Sndfile's errors don't include the filename.
withFilename :: String -> FilePath -> IO a -> IO a
withFilename operation fname = Exception.handle $
    Exception.throw . annotate (operation <> " " <> show fname <> ": ")

annotate :: String -> Exception -> Exception
annotate prefix = \case
    Sndfile.Exception err -> Sndfile.Exception $ prefix <> err
    Sndfile.UnrecognisedFormat err -> Sndfile.UnrecognisedFormat $ prefix <> err
    Sndfile.SystemError err -> Sndfile.SystemError $ prefix <> err
    Sndfile.MalformedFile err -> Sndfile.MalformedFile $ prefix <> err
    Sndfile.UnsupportedEncoding err ->
        Sndfile.UnsupportedEncoding $ prefix <> err

ignoreEnoent :: IO a -> IO (Maybe a)
ignoreEnoent = ignoreError $ \case
    -- hsndfile doesn't preserve the underlying error code
    Sndfile.SystemError msg -> "No such file or directory" `List.isInfixOf` msg
    _ -> False

ignoreError :: Exception.Exception e => (e -> Bool) -> IO a -> IO (Maybe a)
ignoreError ignore action = Exception.handleJust (guard . ignore)
    (const (return Nothing)) (fmap Just action)
