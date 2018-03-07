-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
module Util.Audio.Audio (
    -- * types
    AudioM(..), AudioIO, AudioId
    , Sample, Frame(..), secondsToFrame, framesToSeconds
    , Duration(..), Count, Channels, Rate
    , chunkSize, framesCount, countFrames, chunkFrames
    -- * construct
    , fromSamples, toSamples
    -- * transform
    , take, gain, multiply
    -- * mix
    , mix
    -- * channels
    , mergeChannels
    , interleave, deinterleave
    -- * generate
    , sine
    , linear
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

newtype AudioM m (rate :: TypeLits.Nat) (channels :: TypeLits.Nat) =
    Audio { _stream :: S.Stream (S.Of (V.Vector Sample)) m () }

type AudioIO rate channels = AudioM (Resource.ResourceT IO) rate channels
type AudioId rate channels = AudioM Identity.Identity rate channels

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

framesToSeconds :: Rate -> Frame -> Seconds
framesToSeconds rate (Frame frames) = fromIntegral frames / fromIntegral rate

chunkSize :: Frame
chunkSize = 5000

framesCount :: KnownNat channels => Proxy channels -> Frame -> Count
framesCount channels (Frame frames) = frames * natVal channels

countFrames :: KnownNat channels => Proxy channels -> Count -> Frame
countFrames channels count = Frame $ count `div` natVal channels

chunkFrames :: KnownNat channels => Proxy channels -> V.Vector Sample -> Frame
chunkFrames channels = countFrames channels . V.length

-- * construct

fromSamples :: Monad m => [V.Vector Sample] -> AudioM m rate channels
fromSamples = Audio . S.each

toSamples :: Monad m => AudioM m rate channels -> m [V.Vector Sample]
toSamples = S.toList_ . _stream

-- * transform

take :: forall m rate chan. (Monad m, KnownNat rate, KnownNat chan)
    => Duration -> AudioM m rate chan -> AudioM m rate chan
take (Seconds seconds) audio =
    take (Frames (secondsToFrame (natVal (Proxy :: Proxy rate)) seconds)) audio
take (Frames frames) (Audio audio) = Audio $ loop1 (0, audio) $
    \loop (start, audio) -> lift (S.next audio) >>= \case
        Left () -> return ()
        Right (chunk, audio)
            | end <= frames -> S.yield chunk >> loop (end, audio)
            | start >= frames -> return ()
            | otherwise -> S.yield $
                V.take (framesCount chan (min frames end - start)) chunk
            where end = start + chunkFrames chan chunk
    where chan = Proxy :: Proxy chan

gain :: Monad m => Float -> AudioM m rate channels -> AudioM m rate channels
gain n (Audio audio) = Audio $ S.map (V.map (*n)) audio

-- | Multiply two signals, and end with the shorter one.
multiply :: (Monad m, KnownNat chan) => AudioM m rate chan -> AudioM m rate chan
    -> AudioM m rate chan
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
    => [(Duration, AudioM m rate channels)] -> AudioM m rate channels
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
-- I used to have Chunk in 'AudioM' so I could process Silence more
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
    => AudioM m rate chan1 -> AudioM m rate chan2
    -> AudioM m rate (chan1 TypeLits.+ chan2)
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
    chan1 = Proxy :: Proxy chan1
    chan2 = Proxy :: Proxy chan2

deinterleave :: V.Storable a => Channels -> V.Vector a -> [V.Vector a]
deinterleave channels v
    | channels == 1 = [v]
    | otherwise = map gen [0 .. channels - 1]
    where
    gen chan = V.generate frames (\f -> v V.! (channels * f + chan))
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
    => AudioM m rate chan1 -> AudioM m rate chan2
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

-- * generate

-- | Generate a test tone.
sine :: forall m rate. (Monad m, KnownNat rate)
    => Duration -> Float -> AudioM m rate 1
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

-- | Generate a piecewise linear signal from breakpoints.  If the last
-- breakpoint is 0, the signal will end there.  Otherwise, it will continue
-- with the final value forever.
linear :: forall m rate. (Monad m, KnownNat rate)
    => [(Seconds, Double)] -> AudioM m rate 1
linear breakpoints =
    Audio $ S.unfoldr (pure . unfold) (0, 0, 0, from0 breakpoints)
    where
    unfold (start, prevX, prevY, breakpoints) = case breakpoints of
        []  | prevY == 0 -> Left ()
            | otherwise -> Right
                ( V.replicate (framesCount chan chunkSize) (Num.d2f prevY)
                , (start + chunkSize, prevX, prevY, [])
                )
        (x, y) : xys
            | toFrame x <= start -> unfold (start, x, y, xys)
            | otherwise -> Right
                ( V.generate (framesCount chan generate)
                    (interpolate prevX prevY x y . toSec . (+start) . Frame)
                , (start + generate, prevX, prevY, breakpoints)
                )
            where
            -- If null xys, this is the last segment, so add a sample to get
            -- the final value.
            generate = min chunkSize
                (toFrame x - start + if null xys then 1 else 0)
    interpolate x1 y1 x2 y2 x = Num.d2f $
        (y2 - y1) / (x2 - x1) * (x - x1) + y1
    toFrame = secondsToFrame rate
    toSec = framesToSeconds rate
    chan = Proxy :: Proxy 1
    rate = natVal (Proxy :: Proxy rate)
    -- The signal is implicitly constant 0 before the first sample.
    from0 bps@((x, y) : _) | x > 0 && y /= 0 = (x, 0) : bps
    from0 bps = bps

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
