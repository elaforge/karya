-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators, TypeApplications #-}
{- | This is a basic library for audio streaming.  It uses the @streaming@
    package to interleave streaming and IO, and represents signals as a stream
    of Vectors of 'Sample's, which is hardcoded to Float.  There are separate
    types for interleaved samples 'Audio' and non-interleaved samples 'NAudio'.

    The sampling rate and channels are recorded as type naturals.  This removes
    the need for runtime error checking, but makes dealing with an unknown
    sampling rate or channels more awkward.
-}
module Util.Audio.Audio (
    -- * types
    Audio(..), AudioIO, AudioId
    , NAudio(..), NAudioIO, NAudioId
    , Sample, Frame(..), secondsToFrame, frameToSeconds
    , Duration(..)
    , Count, Channels, Rate, Seconds
    , framesCount, countFrames, chunkFrames
    -- * construct
    , fromSamples, toSamples
    -- * transform
    , take, mapSamples, gain, multiply
    -- * mix
    , mix
    -- * channels
    , mergeChannels
    , expandChannels, mixChannels
    , interleaveV, deinterleaveV
    -- ** non-interleaved
    , nonInterleaved, interleaved
    , synchronizeToSize
    , zeroPadN
    -- * generate
    , silence, sine
    , linear
    -- * error
    , Exception(..), throw
    -- * constants
    , chunkSize, silentChunk
    -- * conversions
    , dbToLinear, linearToDb
    -- * util
    , loop1
    , takeFrames
#ifdef TESTING
    , module Util.Audio.Audio
#endif
) where
import Prelude hiding (take)
import qualified Control.Exception as Exception
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.Maybe as Maybe
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import qualified GHC.TypeLits as TypeLits
import GHC.TypeLits (KnownNat)
import qualified Streaming as S
import qualified Streaming.Prelude as S

import qualified Util.Num as Num
import qualified Util.Seq as Seq
import Global


-- * types

{- | A stream of chunks of interleaved samples.

    There is an invariant that the length of the vector should always be
    a multiple of channels, in other words that a chunk is an integral number
    of frames.  The chunks may be of variable size, but should default to
    'chunkSize', and align to multiples of chunkSize, to minimize re-chunking
    when synchronizing streams.
-}
newtype Audio m (rate :: TypeLits.Nat) (channels :: TypeLits.Nat) =
    Audio { _stream :: S.Stream (S.Of (V.Vector Sample)) m () }

type AudioIO rate channels = Audio (Resource.ResourceT IO) rate channels
type AudioId rate channels = Audio Identity.Identity rate channels

{- | Non-interleaved audio stream.  Ok so it's still interleaved, just per
    chunk instead of per sample.

    Each of the lists will be channels length.  Each Sample vector will be the
    same length until the signal runs out, at which point it may be short, and
    then will be empty.  The stream ends when all signals are empty.

    The same chunk length guidelines as in 'Audio' also apply.

    Unlike 'Audio', the channel count is dynamic, not static.  This is because
    I use NAudio to stream a variably sized collection of control signals,
    while I use Audio to represent audio signals, which generally have a global
    number of channels determined by how many speakers you have.  But it does
    mean that you have to be careful that the length of each chunks list always
    matches '_nchannels'.  TODO it should be possible to use an existentially
    quantified type natural and length indexed list to ensure this, but not
    sure if it's worth it.
-}
data NAudio m (rate :: TypeLits.Nat) = NAudio
    { _nchannels :: !Channels
    , _nstream :: S.Stream (S.Of [(V.Vector Sample)]) m ()
    }

type NAudioIO rate = NAudio (Resource.ResourceT IO) rate
type NAudioId rate = NAudio Identity.Identity rate

-- | I hardcode the sample format to Float for now, since I have no need to
-- work with any other format.
type Sample = Float

-- | Should be >=0.
newtype Frame = Frame Int
    deriving (Show, Eq, Ord, Num, Real, Enum, Integral, Pretty)

data Duration = Frames !Frame | Seconds !Seconds
    deriving (Eq, Show)

-- | Sample count.  This is Frame * channels.
type Count = Int
type Channels = Int
type Rate = Int

type Seconds = Double

secondsToFrame :: Rate -> Seconds -> Frame
secondsToFrame rate seconds = Frame $ round $ fromIntegral rate * seconds

frameToSeconds :: Rate -> Frame -> Seconds
frameToSeconds rate (Frame frames) = fromIntegral frames / fromIntegral rate

framesCount :: KnownNat channels => Proxy channels -> Frame -> Count
framesCount channels (Frame frames) = frames * natVal channels

countFrames :: KnownNat channels => Proxy channels -> Count -> Frame
countFrames channels count = Frame $ count `div` natVal channels

chunkFrames :: KnownNat channels => Proxy channels -> V.Vector Sample -> Frame
chunkFrames channels = countFrames channels . V.length

-- * construct

-- | Construct audio manually for testing.  The length of each vector should be
-- a multiple of the channels, or this will crash.
fromSamples :: forall m rate chan. (Monad m, KnownNat chan)
    => [V.Vector Sample] -> Audio m rate chan
fromSamples = Audio . S.each . map check
    where
    check chunk
        | V.length chunk `mod` chan == 0 = chunk
        | otherwise = error $ "chunk length " <> show (V.length chunk)
            <> " not a multiple of channels " <> show chan
    chan = natVal (Proxy @chan)

toSamples :: Monad m => Audio m rate channels -> m [V.Vector Sample]
toSamples = S.toList_ . _stream

-- * transform

take :: forall m rate chan. (Monad m, KnownNat rate, KnownNat chan)
    => Duration -> Audio m rate chan -> Audio m rate chan
take (Seconds seconds) audio =
    take (Frames (secondsToFrame (natVal (Proxy :: Proxy rate)) seconds)) audio
take (Frames frames) (Audio audio) = Audio $ loop1 (0, audio) $
    \loop (start, audio) -> lift (S.uncons audio) >>= \case
        Nothing -> return ()
        Just (chunk, audio)
            | end <= frames -> S.yield chunk >> loop (end, audio)
            | start >= frames -> return ()
            | otherwise -> S.yield $
                V.take (framesCount chan (min frames end - start)) chunk
            where end = start + chunkFrames chan chunk
    where chan = Proxy :: Proxy chan

mapSamples :: Monad m => (Float -> Float) -> Audio m rate chan
    -> Audio m rate chan
mapSamples f (Audio audio) = Audio (S.map (V.map f) audio)

-- | Set linear gain.  Use 'dbToLinear' to scale by dB.
gain :: Monad m => Float -> Audio m rate channels -> Audio m rate channels
gain n audio
    | n == 1 = audio
    | otherwise = mapSamples (*n) audio

-- | Multiply two signals, and end with the shorter one.
multiply :: (Monad m, KnownNat chan) => Audio m rate chan -> Audio m rate chan
    -> Audio m rate chan
multiply audio1 audio2 =
    Audio $ S.unfoldr (fmap merge . S.next) $ synchronize audio1 audio2
    where
    merge = \case
        Left () -> Left ()
        Right ((Nothing, _), _) -> Left ()
        Right ((_, Nothing), _) -> Left ()
        -- Since they have the same channels, there's no need to deinterleave.
        Right ((Just a1, Just a2), audio) -> Right (V.zipWith (*) a1 a2, audio)

-- * mix

-- | Mix together the audio streams at the given start times.
--
-- TODO the input could also be a stream, in case it somehow comes from IO.
mix :: forall m rate channels. (Monad m, KnownNat channels, KnownNat rate)
    => [(Duration, Audio m rate channels)] -> Audio m rate channels
mix = Audio . S.map merge . synchronizeChunks . map pad
    where
    pad (Seconds secs, a) = pad (Frames (secondsToFrame rate secs), a)
    pad (Frames frame, Audio a)
        | frame > 0 =
            S.cons (Silence (framesCount channels frame)) (S.map Chunk a)
        | otherwise = S.map Chunk a
    merge chunks
        | null vs = case [c | Silence c <- chunks] of
            -- All Silences should be the same, thanks to 'synchronizeChunks'.
            count : _ -> V.replicate count 0
            -- 'synchronizeChunks' shouldn't emit empty chunks.
            [] -> V.empty
        | otherwise = zipWithN (+) vs
        where vs = [v | Chunk v <- chunks]
    channels = Proxy :: Proxy channels
    rate = natVal (Proxy :: Proxy rate)

-- | The strategy is to pad the beginning of each audio stream with silence,
-- but make mixing silence cheap with a special Silence constructor.
--
-- I used to have Chunk in 'Audio' so I could process Silence more
-- efficiently.  But it turns out it's annoying to do that and I only need it
-- for 'mix' anyway.
data Chunk = Chunk (V.Vector Sample) | Silence !Count
    deriving (Show)

chunkCount :: Chunk -> Count
chunkCount (Chunk v) = V.length v
chunkCount (Silence c) = c

zipWithN :: V.Storable a => (a -> a -> a) -> [V.Vector a] -> V.Vector a
zipWithN f vs = case vs of
    [] -> V.empty
    [v1] -> v1
    [v1, v2] -> V.zipWith f v1 v2
    v1 : v2 : vs -> V.zipWith f (V.zipWith f v1 v2) (zipWithN f vs)
    -- TODO is this actually efficient?  I'm not sure how nested zipWiths fuse.
    -- Otherwise, I could construct a new 'f' and zipWith4 etc. on that.
    -- Or split in halves and make a balanced tree.

-- | Synchronize chunk size for all streams: pull from each one, then split
-- each to the shortest one.
synchronizeChunks :: Monad m => [S.Stream (S.Of Chunk) m ()]
    -> S.Stream (S.Of [Chunk]) m ()
synchronizeChunks = S.unfoldr unfold
    where
    unfold audios = do
        pairs <- Maybe.catMaybes <$> mapM S.uncons audios
        return $ case Seq.minimum $ map (chunkCount . fst) pairs of
            Nothing -> Left ()
            Just shortest -> Right $ unzip $ map (recons shortest) pairs
    recons size (chunk, tail)
        | chunkCount chunk <= size = (chunk, tail)
        | otherwise = case chunk of
            Chunk v -> (Chunk pre, S.cons (Chunk post) tail)
                where (pre, post) = V.splitAt size v
            Silence count ->
                (Silence size, S.cons (Silence (count - size)) tail)


-- * channels

mergeChannels :: forall m rate chan1 chan2.
    (Monad m, KnownNat chan1, KnownNat chan2)
    => Audio m rate chan1 -> Audio m rate chan2
    -> Audio m rate (chan1 TypeLits.+ chan2)
mergeChannels audio1 audio2 =
    Audio $ S.map (merge . to0) $ synchronize audio1 audio2
    where
    to0 (a1, a2) =
        ( fromMaybe (V.replicate count1 0) a1
        , fromMaybe (V.replicate count2 0) a2
        )
        where
        count1 = framesCount chan1 $ maybe 0 (chunkFrames chan2) a2
        count2 = framesCount chan2 $ maybe 0 (chunkFrames chan1) a1
    -- These should now have the same number of frames.
    merge (a1, a2) = interleaveV $
        deinterleaveV (natVal chan1) a1 ++ deinterleaveV (natVal chan2) a2
    chan1 = Proxy @chan1
    chan2 = Proxy @chan2

-- | Take a single channel signal to multiple channels by copying samples.
--
-- This could be generalized to expand n to m where m is divisible by n, but
-- that's more complicated and I don't need it.
expandChannels :: forall m rate chan. (Monad m, KnownNat chan)
    => Audio m rate 1 -> Audio m rate chan
expandChannels (Audio audio) = Audio $ S.map (expandV chan) audio
    where chan = natVal (Proxy :: Proxy chan)

-- | Do the reverse of 'expandChannels', mixing all channels to a mono signal.
mixChannels :: forall m rate chan. (Monad m, KnownNat chan)
    => Audio m rate chan -> Audio m rate 1
mixChannels (Audio audio) = Audio $ S.map mix audio
    where
    mix = zipWithN (+) . deinterleaveV chan
    chan = natVal (Proxy :: Proxy chan)

expandV :: Channels -> V.Vector Sample -> V.Vector Sample
expandV chan chunk = V.generate (V.length chunk * chan) $
        \i -> chunk V.! (i `div` chan)

deinterleaveV :: V.Storable a => Channels -> V.Vector a -> [V.Vector a]
deinterleaveV channels v
    | channels == 1 = [v]
    | otherwise = map gen [0 .. channels - 1]
    where
    gen chan = V.generate frames (\i -> v V.! (channels * i + chan))
    frames = V.length v `div` channels

interleaveV :: V.Storable a => [V.Vector a] -> V.Vector a
interleaveV vs = V.create $ do
    out <- VM.new (sum (map V.length vs))
    forM_ (zip [0..] vs) $ \(vi, v) ->
        forM_ (Seq.range' 0 (V.length v) 1) $ \i ->
            VM.write out (i*stride + vi) (V.unsafeIndex v i)
    return out
    where stride = length vs

-- | Synchronize chunk size for two streams.  If one stream runs out ahead of
-- the other, it will emit Nothings.
synchronize :: forall m rate chan1 chan2.
    (Monad m, KnownNat chan1, KnownNat chan2)
    => Audio m rate chan1 -> Audio m rate chan2
    -> S.Stream (S.Of (Maybe (V.Vector Sample), Maybe (V.Vector Sample))) m ()
synchronize audio1 audio2 = S.unfoldr unfold (_stream audio1, _stream audio2)
    where
    unfold (a1, a2) = recons a1 a2 <$> ((,) <$> S.uncons a1 <*> S.uncons a2)
    recons a1 a2 = \case
        (Nothing, Nothing) -> Left ()
        (Just (c1, as1), Nothing) -> Right ((Just c1, Nothing), (as1, a2))
        (Nothing, Just (c2, as2)) -> Right ((Nothing, Just c2), (a1, as2))
        (Just (c1, as1), Just (c2, as2)) -> Right $
            case compare frames1 frames2 of
                LT -> ((Just c1, Just pre2), (as1, S.cons post2 as2))
                GT -> ((Just pre1, Just c2), (S.cons post1 as1, as2))
                EQ -> ((Just c1, Just c2), (as1, as2))
            where
            frames1 = chunkFrames chan1 c1
            frames2 = chunkFrames chan2 c2
            (pre1, post1) = V.splitAt (framesCount chan1 shortest) c1
            (pre2, post2) = V.splitAt (framesCount chan2 shortest) c2
            shortest = min frames1 frames2
    chan1 = Proxy :: Proxy chan1
    chan2 = Proxy :: Proxy chan2

-- ** non-interleaved

-- | Merge 'Audio's into a single 'NAudio' stream, synchronized with the given
-- chunk size.
nonInterleaved :: Monad m => Frame -> [Audio m rate 1] -> NAudio m rate
nonInterleaved size audios = NAudio (length audios) $
    S.unfoldr unfold (map (_stream . synchronizeToSize size) audios)
    where
    unfold streams = do
        pairs <- mapM S.uncons streams
        let heads = map (fmap fst) pairs
            tails = [tail | Just (_, tail) <- pairs]
        return $ if null tails then Left ()
            else Right (map (fromMaybe V.empty) heads, tails)

-- | Convert a non-interleaved NAudio with an unknown number of channels
-- to an interleaved one with a known number.  This is the inverse of
-- 'nonInterleaved'.
--
-- This is limited to only work when the channels are equal, or the NAudio has
-- 1 channel, in which case it acts like 'expandChannels'.  Similar to
-- 'expandChannels', I could generalize it, but I don't need that now.
interleaved :: forall m rate chan. (Monad m, KnownNat chan)
    => NAudio m rate -> Either Text (Audio m rate chan)
interleaved naudio
    | _nchannels naudio == 1 =
        Right $ Audio $ S.map (expandV chan . head) $ _nstream naudio
    | _nchannels naudio == chan =
        Right $ Audio $ S.map interleaveV $ _nstream naudio
    | otherwise = Left $ "can't convert " <> showt (_nchannels naudio)
        <> " channels to " <> showt chan
    where
    chan = natVal (Proxy @chan)

synchronizeToSize :: forall m rate chan. (Monad m, KnownNat chan)
    => Frame -> Audio m rate chan -> Audio m rate chan
synchronizeToSize size = Audio . S.unfoldr unfold
    where
    unfold audio = do
        (chunks, audio) <- takeFrames size audio
        let (pre, post) = V.splitAt (framesCount chan size) $ mconcat chunks
        let consed
                | V.null post = audio
                | otherwise = apply (S.cons post) audio
        return $ if V.null pre
            then Left ()
            else Right (pre, consed)
    chan = Proxy @chan
    apply f = Audio . f . _stream

-- | Extend chunks shorter than the given size with zeros, and pad the end with
-- zeros forever.  Composed with 'nonInterleaved', which may leave a short
-- final chunk, the output should be infinite and have uniform chunk size.
zeroPadN :: Monad m => Frame -> NAudio m rate -> NAudio m rate
zeroPadN size_ naudio = naudio { _nstream = S.unfoldr unfold (_nstream naudio) }
    where
    unfold audio = do
        result <- S.uncons audio
        return $ case result of
            Nothing -> Right (replicate (_nchannels naudio) silentChunk, audio)
            Just (chunks, audio) -> Right (map pad chunks, audio)
    pad chunk
        | V.length chunk >= size = chunk
        | V.null chunk = silentChunk
        | otherwise = chunk V.++ (V.replicate (size - V.length chunk) 0)
    size = framesCount (Proxy @1) size_


-- * generate

-- | Silence.  Forever.
silence :: Monad m => Audio m rate 1
silence = constant 0

-- | An infinite constant stream, which reuses the same buffer.
constant :: Monad m => Float -> Audio m rate 1
constant = Audio . constant_

constant_ :: Monad m => Float -> S.Stream (S.Of (V.Vector Sample)) m r
constant_ val = S.repeat chunk
    where
    chunk
        | val == 0 = silentChunk
        | otherwise = V.replicate (framesCount (Proxy @1) chunkSize) val

-- | Generate a test tone at the given frequency, forever.  This is not
-- efficient, but it's just for testing.
sine :: forall m rate. (Monad m, KnownNat rate) => Float -> Audio m rate 1
sine frequency =
    Audio $ loop1 0 $ \loop frame ->
        S.yield (gen frame) >> loop (frame + chunkSize)
    where
    gen start = V.generate (fromIntegral (end - start)) (val . (+start) . Frame)
        where end = start + chunkSize
    val frame = sin $ 2 * pi * frequency * (fromIntegral frame / rate)
    rate = fromIntegral $ TypeLits.natVal (Proxy :: Proxy rate)

-- | Generate a piecewise linear signal from breakpoints.  The signal will
-- continue forever with the last value.
linear :: forall m rate. (Monad m, KnownNat rate)
    => [(Seconds, Double)] -> Audio m rate 1
linear breakpoints = Audio $ loop (0, 0, 0, from0 breakpoints)
    where
    loop (start, prevX, prevY, breakpoints) = case breakpoints of
        (x, y) : xys
            | toFrame x <= start -> loop (start, x, y, xys)
            | otherwise -> do
                S.yield $ segment prevX prevY x y start (toCount generate)
                loop (start + generate, prevX, prevY, breakpoints)
            where generate = min chunkSize (toFrame x - start)
        -- Line the output up to chunkSize boundaries so subsequent
        -- synchronization doesn't have to reallocate.
        -- TODO I could avoid all reallocation by always using chunkSize, but
        -- then the interpolate stuff gets a bit more complicated.  Not sure
        -- if worth it.
        []  | leftover == 0 -> constant_ (Num.d2f prevY)
            | otherwise -> do
                S.yield $ V.replicate (toCount leftover) (Num.d2f prevY)
                loop (start + leftover, prevX, prevY, [])
                where leftover = start `mod` chunkSize
    -- Go through some hassle to create constant segments efficiently, since
    -- they should be pretty common.
    segment x1 y1 x2 y2 start count
        | y1 == y2 && y1 == 0 && count <= V.length silentChunk =
            V.take count silentChunk
        | y1 == y2 = V.replicate count (Num.d2f y1)
        | otherwise = V.generate count
            (interpolate x1 y1 x2 y2 . toSec . (+start) . Frame)
    interpolate x1 y1 x2 y2 x = Num.d2f $
        (y2 - y1) / (x2 - x1) * (x - x1) + y1
    toFrame = secondsToFrame rate
    toSec = frameToSeconds rate
    toCount = framesCount (Proxy @1)
    rate = natVal (Proxy :: Proxy rate)
    -- The signal is implicitly constant 0 before the first sample.
    from0 bps@((x, y) : _) | x > 0 && y /= 0 = (x, 0) : bps
    from0 bps = bps

-- * error

newtype Exception = Exception Text
    deriving (Show)

instance Exception.Exception Exception

throw :: Text -> AudioIO rate chan
throw = Audio . liftIO . Exception.throwIO . Exception

-- * constants

chunkSize :: Frame
chunkSize = 5000

silentChunk :: V.Vector Sample
silentChunk = V.replicate (framesCount (Proxy @1) chunkSize) 0

-- * conversions

linearToDb, dbToLinear :: Float -> Float
linearToDb x = logBase 10 x * 20
dbToLinear x = 10**(x / 20)

-- * util

natVal :: KnownNat n => Proxy n -> Int
natVal = fromIntegral . TypeLits.natVal

-- | A loop with state.  This is like fix, except it has a state argument.
-- The only reason it's here is that it's convenient for stream loops.
loop1 :: forall state a. state -> ((state -> a) -> state -> a) -> a
loop1 state f = f again state
    where
    again :: state -> a
    again = f again

-- | This is like 'S.breakWhen', except it breaks after the place where the
-- predicate becomes true, not before it.
breakAfter :: Monad m => (accum -> a -> accum) -> accum -> (accum -> Bool)
    -> S.Stream (S.Of a) m r -> S.Stream (S.Of a) m (S.Stream (S.Of a) m r)
breakAfter combine accum check = loop accum
    where
    loop accum as = lift (S.next as) >>= \case
        Left r -> return (return r)
        Right (a, as)
            | check next -> S.yield a >> return as
            | otherwise -> S.yield a >> loop next as
            where next = combine accum a

-- | Take at least the given number of frames.  It may take more if the size
-- doesn't line up on a chunk boundary.
takeFrames :: forall m rate chan. (Monad m, KnownNat chan)
    => Frame -> Audio m rate chan -> m ([V.Vector Sample], Audio m rate chan)
takeFrames frames (Audio audio) = do
    chunks S.:> rest <- S.toList $
        breakAfter (\n -> (+n) . chunkFrames chan) 0 (>=frames) audio
    return (chunks, Audio rest)
    where
    chan = Proxy :: Proxy chan
