{-# LANGUAGE DataKinds, KindSignatures #-}
module Util.Audio.File where
import Prelude hiding (read)
import qualified Control.Exception as Exception
import Control.Monad.Trans (liftIO, lift)
import qualified Control.Monad.Trans.Resource as Resource

import qualified System.IO as IO
import qualified Data.Vector.Storable as V
import qualified GHC.TypeLits as TypeLits
import qualified Sound.File.Sndfile as Sndfile
import qualified Sound.File.Sndfile.Buffer.Vector as Sndfile.Buffer.Vector
import qualified Streaming as S
import qualified Streaming.Prelude as S
import qualified System.IO.Error as IO.Error


{-
Stream (Of a) m r
open file and read chunks, close when done
how to bracket?

data ByteString m r =
  Empty r
  | Chunk {-#UNPACK #-} !S.ByteString (ByteString m r )
  | Go (m (ByteString m r ))

bracketByteString :: (MonadResource m) =>
       IO a -> (a -> IO ()) -> (a -> ByteString m b) -> ByteString m b
bracketByteString alloc free inside = do
        (key, seed) <- lift (Resource.allocate alloc free)
        clean key (inside seed)
  where
    clean key = loop where
      loop str = case str of
        Empty r        -> Go (release key >> return (Empty r))
        Go m           -> Go (liftM loop m)
        Chunk bs rest  -> Chunk bs (loop rest)

readFile :: MonadResource m => FilePath -> ByteString m ()
readFile f = bracketByteString (openBinaryFile f ReadMode) hClose hGetContents

hGetContentsN :: MonadIO m => Int -> Handle -> ByteString m ()
hGetContentsN chunkSize h = loop -- TODO close on exceptions
  where
--    lazyRead = unsafeInterleaveIO loop
    loop = do
        c <- liftIO (S.hGetSome h chunkSize)
        -- only blocks if there is no data available
        if S.null c
          then Empty ()
          else Chunk c loop
{-#INLINABLE hGetContentsN #-} -- very effective inline pragma


-}

newtype Audio (rate :: TypeLits.Nat) (channels :: TypeLits.Nat) = Audio {
    _stream :: S.Stream (S.Of Chunk) (Resource.ResourceT IO) ()
    }

newtype Chunk = Chunk { unChunk :: V.Vector Sample }
    deriving (Show)

type Sample = Float
-- | Should be >=0.
type Frames = Int


{-
Make silence from 0 to Frames.
Then get the earliest audio, and copy through until the next earliest.
Then get all overlappings, mix them, and emit.
Tricky because I have to deal with partial overlaps.  I can trust that chunk
size is always the same though.

or

Can I do merge with silence, but make silence cheap?  Instead of actual
chunks of 0s, have a distinguished Empty chunk, which trivially mixes.
I still have to pull elements, but they're cheap.

Since all chunks should be the same size, I should be able to use V.empty.

But then I lose unequal chunks, what is the use for those?
Will resample create them?  Yes.
-}
mix :: [(Frames, Audio rate channels)] -> Audio rate channels
mix audios = undefined


read44k :: FilePath -> Audio 44100 2
read44k = read

read :: forall rate channels.
    (TypeLits.KnownNat rate, TypeLits.KnownNat channels) =>
    FilePath -> Audio rate channels
read fname = Audio $ do
    (key, handle) <- lift $ Resource.allocate
        (Sndfile.openFile fname Sndfile.ReadMode Sndfile.defaultInfo)
        Sndfile.hClose
    liftIO $ check fname (Sndfile.hInfo handle)
        (TypeLits.natVal (Proxy :: Proxy rate))
        (TypeLits.natVal (Proxy :: Proxy channels))
    let loop = liftIO (Sndfile.hGetBuffer handle chunkSize) >>= \case
            Nothing -> lift (Resource.release key) >> return ()
            Just buf -> do
                S.yield $ Chunk $ Sndfile.Buffer.Vector.fromBuffer buf
                loop
    loop

check :: FilePath -> Sndfile.Info -> Integer -> Integer -> IO ()
check fname info rate channels
    | int (Sndfile.samplerate info) == rate
            && int (Sndfile.channels info) == channels =
        return ()
    | otherwise = Exception.throwIO $ IO.Error.mkIOError
        IO.Error.userErrorType
        ("(rate, channels) " ++ show (rate, channels) ++ " /= "
            ++ show (Sndfile.samplerate info, Sndfile.channels info))
        Nothing (Just fname)
    where
    int = fromIntegral

write :: forall rate channels.
    (TypeLits.KnownNat rate, TypeLits.KnownNat channels)
    => FilePath -> Sndfile.Format -> Audio rate channels
    -> Resource.ResourceT IO ()
write fname format (Audio audio) = do
    let info = Sndfile.defaultInfo
            { Sndfile.samplerate = fromIntegral $
                TypeLits.natVal (Proxy :: Proxy rate)
            , Sndfile.channels = fromIntegral $
                TypeLits.natVal (Proxy :: Proxy channels)
            , Sndfile.format = format
            }
    (key, handle) <- Resource.allocate
        (Sndfile.openFile fname Sndfile.WriteMode info)
        Sndfile.hClose
    S.mapM_ (liftIO . Sndfile.hPutBuffer handle
            . Sndfile.Buffer.Vector.toBuffer . unChunk)
        audio
    -- loop audio handle
    Resource.release key
    -- where
    -- loop audio handle = S.next audio >>= \case
    --     Left () -> return ()
    --     Right (chunk, audio) -> do
    --         liftIO $ Sndfile.hPutBuffer handle (Sndfile.Buffer.Vector.toBuffer chunk)
    --         loop audio handle

wavFormat :: Sndfile.Format
wavFormat = Sndfile.Format
    { headerFormat = Sndfile.HeaderFormatWav
    , sampleFormat = Sndfile.SampleFormatFloat -- Sndfile.SampleFormatPcm16
    , endianFormat = Sndfile.EndianFile
    }


chunkSize :: Frames
chunkSize = 5000

data Proxy (a :: TypeLits.Nat) = Proxy
