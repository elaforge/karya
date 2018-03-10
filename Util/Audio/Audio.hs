-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators, TypeApplications #-}
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
    , take, gain, multiply
    -- * mix
    , mix
    -- * channels
    , mergeChannels
    , expandChannels, mixChannels
    , interleave, deinterleave
    -- ** non-interleaved
    , nonInterleaved, splitNonInterleaved
    , synchronizeToSize
    , zeroPadN
    -- * generate
    , silence, sine
    , linear
    -- * constants
    , chunkSize, silentChunk
    -- * util
    , loop1
#ifdef TESTING
    , module Util.Audio.Audio
#endif
) where
import Prelude hiding (take)
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

newtype Audio m (rate :: TypeLits.Nat) (channels :: TypeLits.Nat) =
    Audio { _stream :: S.Stream (S.Of (V.Vector Sample)) m () }
    -- There is an invariant that the length of the vector should always
    -- be a multiple of channels, in other words that a chunk is an integral
    -- number of frames.

type AudioIO rate channels = Audio (Resource.ResourceT IO) rate channels
type AudioId rate channels = Audio Identity.Identity rate channels

-- | Non-interleaved audio stream.  Ok so it's still interleaved, just per
-- chunk instead of per sample.
--
-- Each of the lists will be channels length.  Each Sample vector will be
-- the same length until the signal runs out, at which point it may be short,
-- and then will be empty.  The stream ends when all signals are empty.
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
    chan = natVal (Proxy :: Proxy chan)

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

gain :: Monad m => Float -> Audio m rate channels -> Audio m rate channels
gain n (Audio audio)
    | n == 1 = Audio audio
    | otherwise = Audio $ S.map (V.map (*n)) audio

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
    merge (a1, a2) = interleave $
        deinterleave (natVal chan1) a1 ++ deinterleave (natVal chan2) a2
    chan1 = Proxy @chan1
    chan2 = Proxy @chan2

-- | Take a single channel signal to multiple channels by copying samples.
expandChannels :: forall m rate chan. (Monad m, KnownNat chan)
    => Audio m rate 1 -> Audio m rate chan
expandChannels (Audio audio) = Audio $ S.map expand audio
    where
    expand chunk = V.generate (V.length chunk * chan) $
        \i -> chunk V.! (i `div` chan)
    chan = natVal (Proxy :: Proxy chan)

-- | Do the reverse of 'expandChannels', mixing all channels to a mono signal.
mixChannels :: forall m rate chan. (Monad m, KnownNat chan)
    => Audio m rate chan -> Audio m rate 1
mixChannels (Audio audio) = Audio $ S.map mix audio
    where
    mix = zipWithN (+) . deinterleave chan
    chan = natVal (Proxy :: Proxy chan)

deinterleave :: V.Storable a => Channels -> V.Vector a -> [V.Vector a]
deinterleave channels v
    | channels == 1 = [v]
    | otherwise = map gen [0 .. channels - 1]
    where
    gen chan = V.generate frames (\i -> v V.! (channels * i + chan))
    frames = V.length v `div` channels

interleave :: V.Storable a => [V.Vector a] -> V.Vector a
interleave vs = V.create $ do
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

nonInterleaved :: Monad m => [Audio m rate 1] -> NAudio m rate
nonInterleaved = nonInterleaved_ chunkSize

nonInterleaved_ :: Monad m => Frame -> [Audio m rate 1] -> NAudio m rate
nonInterleaved_ size audios = NAudio (length audios) $
    S.unfoldr unfold (map (_stream . synchronizeToSize size) audios)
    where
    unfold streams = do
        pairs <- mapM S.uncons streams
        let heads = map (fmap fst) pairs
            tails = [tail | Just (_, tail) <- pairs]
        return $ if null tails then Left ()
            else Right (map (fromMaybe V.empty) heads, tails)

-- | Undo 'nonInterleaved'.
--
-- TODO does this run the stream multiple times?
-- I think I have to go directly to interleaved after all.
splitNonInterleaved :: Monad m => NAudio m rate -> [Audio m rate 1]
splitNonInterleaved naudio =
    map (\i -> Audio $ S.map (!! i) (_nstream naudio))
        [0 .. _nchannels naudio - 1]

synchronizeToSize :: forall m rate chan. (Monad m, KnownNat chan)
    => Frame -> Audio m rate chan -> Audio m rate chan
synchronizeToSize size = Audio . S.unfoldr unfold . _stream
    where
    unfold audio = do
        chunks S.:> rest <- S.toList $ collect audio
        let (pre, post) = V.splitAt (framesCount chan size) $ mconcat chunks
        return $ if V.null pre
            then Left ()
            else Right (pre, (if V.null post then id else S.cons post) rest)
    collect audio = breakAfter (\n -> (+n) . chunkFrames chan) 0 (>=size) audio
    chan = Proxy @chan

-- | Extend chunks shorter than 'chunkSize' with zeros, and pad the end with
-- zeros forever.  Composed with 'nonInterleaved', which may leave a short
-- final chunk, the output should be infinite and have uniform chunk size.
zeroPadN :: Monad m => NAudio m rate -> NAudio m rate
zeroPadN naudio = naudio { _nstream = S.unfoldr unfold (_nstream naudio) }
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
    size = framesCount (Proxy @1) chunkSize


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

-- | Generate a test tone.
sine :: forall m rate. (Monad m, KnownNat rate)
    => Duration -> Float -> Audio m rate 1
sine (Seconds seconds) frequency =
    sine (Frames (secondsToFrame rate seconds)) frequency
    where rate = natVal (Proxy :: Proxy rate)
sine (Frames frame) frequency = Audio (gen 0)
    where
    gen start
        | start >= frame = return ()
        | otherwise = S.yield chunk >> gen end
        where
        chunk = V.generate (fromIntegral (end - start))
            (val . (+start) . Frame)
        end = min frame (start + chunkSize)
    rate = fromIntegral $ TypeLits.natVal (Proxy :: Proxy rate)
    val frame = sin $ 2 * pi * frequency * (fromIntegral frame / rate)

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
                S.yield $ V.generate (toCount generate)
                    (interpolate prevX prevY x y . toSec . (+start) . Frame)
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
    interpolate x1 y1 x2 y2 x = Num.d2f $
        (y2 - y1) / (x2 - x1) * (x - x1) + y1
    toFrame = secondsToFrame rate
    toSec = frameToSeconds rate
    toCount = framesCount (Proxy @1)
    rate = natVal (Proxy :: Proxy rate)
    -- The signal is implicitly constant 0 before the first sample.
    from0 bps@((x, y) : _) | x > 0 && y /= 0 = (x, 0) : bps
    from0 bps = bps

-- * constants

chunkSize :: Frame
chunkSize = 5000

silentChunk :: V.Vector Sample
silentChunk = V.replicate (framesCount (Proxy @1) chunkSize) 0

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
breakAfter combine accum check = loop0 accum
    where
    loop0 accum as = lift (S.next as) >>= \case
        Left r -> return (return r)
        Right (a, as)
            | check next -> S.yield a >> return as
            | otherwise -> S.yield a >> loop0 next as
            where next = combine accum a
