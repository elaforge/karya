-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
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
    , UnknownAudio(..), UnknownAudioIO
    , Block(..)
    , Sample, Frames(..), secondsToFrames, secondsToFramesCeil, framesToSeconds
    , Count, Channels, Rate, Seconds
    , framesCount, countFrames, blockFrames, vectorFrames
    , blockCount, isEmptyBlock, blockSamples, blockVector
    -- * construct
    , fromBlocks, fromSamples, fromSampleLists
    , toBlocks, toSamples, toBlocksN, toSamplesN
    -- * transform
    , apply
    , castRate
    , take, takeS, mapSamples, gain, multiply
    , takeClose, takeCloseS
    , pan, panConstant
    -- * mix
    , mix
    , mixV
    -- * channels
    , mergeChannels, extractChannel, splitChannels
    , expandChannels, expandV
    , mixChannels, interleaveV, deinterleaveB, deinterleaveV
    -- ** non-interleaved
    , nonInterleaved, interleaved
    , synchronizeToSize
    ,takeN
    , zeroPadN
    -- * generate
    , silence, sine
    , linear
    -- * effects
    , effect
    -- * error
    , Exception(..), exceptionText, throw, throwIO, assert, assertIn
    -- * constants
    , blockSize, silentBlock
    -- * conversions
    , dbToLinear, linearToDb
    -- * util
    , takeFramesGE, splitAt
    , next, isEmpty
    , natVal, someNat
#ifdef TESTING
    , module Util.Audio.Audio
#endif
) where
import Prelude hiding (splitAt, take)
import qualified Control.Exception as Exception
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.Maybe as Maybe
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import qualified GHC.TypeLits as TypeLits
import           GHC.TypeLits (KnownNat)
import qualified GHC.Stack as Stack
import qualified Streaming as S
import qualified Streaming.Prelude as S

import           Util.Audio.AudioT (Frames(..))
import qualified Util.CallStack as CallStack
import qualified Util.Control as Control
import qualified Util.Num as Num
import qualified Util.Lists as Lists
import qualified Util.Test.ApproxEq as ApproxEq
import qualified Util.VectorC as VectorC

import qualified Util.Pretty as Pretty

import Global


-- * types

{- | A stream of blocks of interleaved samples.

    There is an invariant that the length of the vector should always be
    a multiple of channels, in other words that a block is an integral number
    of frames.  The blocks may be of variable size, but should default to
    'blockSize', and align to multiples of blockSize, to minimize re-chunking
    when synchronizing streams.
-}
newtype Audio m (rate :: TypeLits.Nat) (chan :: TypeLits.Nat) =
    Audio { _stream :: S.Stream (S.Of Block) m () }
    deriving (Semigroup, Monoid)

type AudioIO rate chan = Audio (Resource.ResourceT IO) rate chan
type AudioId rate chan = Audio Identity.Identity rate chan

{- | Non-interleaved audio stream.  Ok so it's still interleaved, just per
    block instead of per sample.

    Each of the lists will be channels length.  Each Sample vector will be the
    same length until the signal runs out, at which point it may be short, and
    then will be empty.  The stream ends when all signals are empty.

    The same block length guidelines as in 'Audio' also apply.

    Unlike 'Audio', the channel count is dynamic, not static.  This is because
    I use NAudio to stream a variably sized collection of control signals,
    while I use Audio to represent audio signals, which generally have a global
    number of channels determined by how many speakers you have.  But it does
    mean that you have to be careful that the length of each blocks list always
    matches '_nchannels'.  TODO it should be possible to use an existentially
    quantified type natural and length indexed list to ensure this, but not
    sure if it's worth it.
-}
data NAudio m (rate :: TypeLits.Nat) = NAudio
    { _nchannels :: !Channels
    , _nstream :: S.Stream (S.Of [Block]) m ()
    }

type NAudioIO rate = NAudio (Resource.ResourceT IO) rate
type NAudioId rate = NAudio Identity.Identity rate

-- | There is a special representation for a constant block.  0 blocks are
-- common when aligning sounds, and constant ones are common for control
-- signals.
data Block = Block !(V.Vector Sample) | Constant !Count !Sample
    deriving (Eq, Show)

instance Pretty Block where
    format (Block v) = Pretty.format v
    format (Constant count val) = Pretty.text $ "const:" <> pretty (count, val)

-- | This is an Audio with dynamic rate and channels.
data UnknownAudio m = forall rate chan. (KnownNat rate, KnownNat chan) =>
    UnknownAudio (Audio m rate chan)
type UnknownAudioIO = UnknownAudio (Resource.ResourceT IO)

-- | I hardcode the sample format to Float for now, since I have no need to
-- work with any other format.
type Sample = Float

-- | Sample count.  This is Frames * channels.
type Count = Int
type Channels = Int
type Rate = Int

type Seconds = Double

secondsToFrames :: Rate -> Seconds -> Frames
secondsToFrames rate seconds = Frames $ round $ fromIntegral rate * seconds

secondsToFramesCeil :: Rate -> Seconds -> Frames
secondsToFramesCeil rate seconds =
    Frames $ ceiling $ fromIntegral rate * seconds

framesToSeconds :: Rate -> Frames -> Seconds
framesToSeconds rate (Frames frames) = fromIntegral frames / fromIntegral rate

framesCount :: KnownNat chan => Proxy chan -> Frames -> Count
framesCount chan (Frames frames) = frames * natVal chan

framesCount_ :: Channels -> Frames -> Count
framesCount_ chan (Frames frames) = frames * chan

countFrames :: KnownNat chan => Proxy chan -> Count -> Frames
countFrames chan = countFrames_ (natVal chan)

countFrames_ :: Channels -> Count -> Frames
countFrames_ chan count = Frames $ count `div` chan

blockFrames :: KnownNat chan => Proxy chan -> Block -> Frames
blockFrames chan = countFrames chan . blockCount

blockFrames_ :: Channels -> Block -> Frames
blockFrames_ chan = countFrames_ chan . blockCount

vectorFrames :: KnownNat chan => Proxy chan -> V.Vector Sample -> Frames
vectorFrames chan = countFrames chan . V.length

blockCount :: Block -> Count
blockCount (Block v) = V.length v
blockCount (Constant c _) = c

isEmptyBlock :: Block -> Bool
isEmptyBlock = (==0) . blockCount

blockSamples :: Block -> [V.Vector Sample]
blockSamples (Block v) = [v]
blockSamples (Constant count _) | count <= 0 = []
blockSamples (Constant count 0)
    | count > V.length silentBlock =
        silentBlock : blockSamples (Constant (count - V.length silentBlock) 0)
    | otherwise = [V.take count silentBlock]
blockSamples (Constant count val) = [V.replicate count val]

blockVector :: Block -> V.Vector Sample
blockVector = mconcat . blockSamples

blockSplit :: Count -> Block -> (Block, Block)
blockSplit n (Block v) = (Block pre, Block post)
    where (pre, post) = V.splitAt n v
blockSplit n (Constant count val) =
    (Constant (min count n) val, Constant (max 0 (count - n)) val)

instance Semigroup Block where
    Constant c1 v1 <> Constant c2 v2 | v1 == v2 = Constant (c1+c2) v1
    b1 <> b2 = Block $ mconcat $ blockSamples b1 ++ blockSamples b2

instance Monoid Block where
    mempty = Constant 0 0
    -- This reduces allocation if there are multiple vectors, though that's
    -- pretty unlikely.
    mconcat (Constant c1 v1 : bs)
        | null vs && all ((==v1) . snd) cs =
            Constant (c1 + Num.sum (map fst cs)) v1
        where
        cs = [(c, v) | Constant c v <- bs]
        vs = [v | Block v <- bs]
    mconcat bs = Block $ mconcat $ concatMap blockSamples bs

-- * construct

-- | Construct audio manually for testing.  The length of each vector should be
-- a multiple of the channels, or this will crash.
fromSamples :: forall m rate chan. (Monad m, KnownNat chan)
    => [V.Vector Sample] -> Audio m rate chan
fromSamples = fromBlocks . map Block

fromBlocks :: forall m rate chan. (Monad m, KnownNat chan)
    => [Block] -> Audio m rate chan
fromBlocks = Audio . S.each . map check
    where
    check block
        | blockCount block `mod` chan == 0 = block
        | otherwise = error $ "block count " <> show (blockCount block)
            <> " not a multiple of channels " <> show chan
    chan = natVal (Proxy @chan)

fromSampleLists :: forall m rate chan. (Monad m, KnownNat chan)
    => [[Sample]] -> Audio m rate chan
fromSampleLists = fromSamples . map V.fromList

toBlocks :: Monad m => Audio m rate chan -> m [Block]
toBlocks = S.toList_ . _stream

toSamples :: Monad m => Audio m rate chan -> m [V.Vector Sample]
toSamples = fmap (concatMap blockSamples) . toBlocks

toBlocksN :: Monad m => NAudio m rate -> m [[Block]]
toBlocksN = S.toList_ . _nstream

toSamplesN :: Monad m => NAudio m rate -> m [[V.Vector Sample]]
toSamplesN = fmap (map (map blockVector)) . toBlocksN

-- * transform

apply :: (S.Stream (S.Of Block) m () -> S.Stream (S.Of Block) m ())
    -> Audio m rate chan -> Audio m rate chan
apply f (Audio stream) = Audio (f stream)

castRate :: Audio m rate1 chan -> Audio m rate2 chan
castRate (Audio stream) = Audio stream

take :: forall m rate chan. (Monad m, KnownNat chan)
    => Frames -> Audio m rate chan -> Audio m rate chan
take = takeClose (return ())

takeS :: forall m rate chan. (Monad m, KnownNat rate, KnownNat chan)
    => Seconds -> Audio m rate chan -> Audio m rate chan
takeS = takeCloseS (return ())

-- | This is like 'take', but it takes a close action to run when the audio
-- stream is terminated.  This is because streaming can't otherwise close
-- the input file in a timely way, because the take can't tell upstream that
-- it will never demand another chunk.  This appears to be a problem with all
-- pull-based streaming libraries, including pipes and conduit.
takeCloseS :: forall m rate chan. (Monad m, KnownNat rate, KnownNat chan)
    => m () -> Seconds -> Audio m rate chan -> Audio m rate chan
takeCloseS close seconds audio
    | seconds <= 0 = mempty
    | otherwise = takeClose close
        (secondsToFrames (natVal (Proxy :: Proxy rate)) seconds) audio

takeClose :: forall m rate chan. (Monad m, KnownNat chan)
    => m () -> Frames -> Audio m rate chan -> Audio m rate chan
takeClose close frames (Audio audio)
    | frames <= 0 = Audio $ lift close
    | otherwise = Audio $ Control.loop1 (0, audio) $
        \loop (now, audio) -> lift (S.uncons audio) >>= \case
            Nothing -> lift close
            Just (block, audio)
                | end <= frames -> S.yield block >> loop (end, audio)
                | now >= frames -> lift close
                | otherwise -> do
                    lift close
                    S.yield $ fst $ blockSplit left block
                where
                end = now + blockFrames chan block
                left = framesCount chan (min frames end - now)
    where chan = Proxy :: Proxy chan

mapSamples :: Monad m => (Float -> Float) -> Audio m rate chan
    -> Audio m rate chan
mapSamples f = apply (S.map block)
    where
    block (Block v) = Block (V.map f v)
    block (Constant count val) = Constant count (f val)

-- | Set linear gain.  Use 'dbToLinear' to scale by dB.
gain :: Monad m => Float -> Audio m rate chan -> Audio m rate chan
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
        Right ((Just b1, Just b2), audio) -> Right (blockMultiply b1 b2, audio)

blockMultiply :: Block -> Block -> Block
blockMultiply (Constant c1 v1) (Constant c2 v2) = Constant (min c1 c2) (v1*v2)
blockMultiply b1@(Block {}) b2@(Constant {}) = blockMultiply b2 b1
blockMultiply (Constant c1 v1) (Block b2)
    | v1 == 0 = Constant (min c1 (V.length b2)) 0
    | v1 == 1 = Block b2
    | otherwise = Block $ V.map (*v1) $ V.take c1 b2
blockMultiply (Block b1) (Block b2) = Block $ V.zipWith (*) b1 b2

-- blockZipWith :: (Sample -> Sample -> Sample) -> Block -> Block -> Block
-- blockZipWith f (Constant c1 v1) (Constant c2 v2) =
--     Constant (min c1 c2) (f v1 v2)
-- blockZipWith f b1 b2 = Block $
--     V.zipWith f (mconcat (blockSamples b1)) (mconcat (blockSamples b2))

-- | Pan a stereo signal with a mono one.  The pan signal goes from -1 to 1.
--
-- TODO This is linear panning, try constant power or -4.5dB:
-- http://www.cs.cmu.edu/~music/icm-online/readings/panlaws/
pan :: Monad m => Audio m rate 1 -> Audio m rate 2 -> Audio m rate 2
pan pos audio = Audio $ S.unfoldr (fmap merge . S.next) $ synchronize pos audio
    where
    merge = \case
        Left () -> Left ()
        Right ((Nothing, _), _) -> Left ()
        Right ((_, Nothing), _) -> Left ()
        Right ((Just _, Just (Constant c 0)), audio) ->
            Right (Constant c 0, audio)
        Right ((Just (Constant _ 0), Just stereo), audio) ->
            Right (stereo, audio)
        Right ((Just pos_, Just stereo), audio) -> Right
            ( Block $ interleaveV
                [ V.zipWith (*) (V.map ((2-) . (+1)) pos) left
                , V.zipWith (*) (V.map (+1) pos) right
                ]
            , audio
            )
            where
            pos = blockVector pos_
            [left, right] = deinterleaveV 2 $ blockVector stereo

-- | Like 'pan', but more efficient.  TODO also linear pan, as in 'pan'.
panConstant :: Monad m => Sample -> Audio m rate 2 -> Audio m rate 2
panConstant pos
    | ApproxEq.eq 0.01 pos 0 = id
    | otherwise = Audio . S.map pan . _stream
    where
    pan (Constant count val) | val == 0 = Constant count 0
    pan stereo = Block $ interleaveV
        [ V.map (* (2 - (pos+1))) left
        , V.map (* (pos+1)) right
        ]
        where [left, right] = deinterleaveV 2 $ blockVector stereo

-- * mix

-- | Mix together the audio streams at the given start times.
--
-- TODO the input could also be a stream, in case it somehow comes from IO.
mix :: Monad m => [Audio m rate chan] -> Audio m rate chan
mix = Audio . S.map merge . synchronizeList
    where
    merge [] = Block V.empty
    merge blocks@(block:_)
        | null vectors = Constant (blockCount block) constant
        | otherwise = Block $
            (if constant == 0 then id else V.map (+constant)) $ mixV 0 vectors
        where
        constant = Num.sum [val | Constant _ val <- blocks]
        vectors = [v | Block v <- blocks]

-- | Add vectors of samples.  The output will be the max of the longest vector
-- and the provided minimum length.
mixV :: Count -> [V.Vector Sample] -> V.Vector Sample
mixV len [] = V.replicate len 0
mixV len [v]
    | V.length v >= len = v
    | otherwise = v <> V.replicate (len - V.length v) 0
mixV len vectors = VectorC.mixFloats len vectors


-- * channels

mergeChannels :: forall m rate chan1 chan2.
    (Monad m, KnownNat chan1, KnownNat chan2)
    => Audio m rate chan1 -> Audio m rate chan2
    -> Audio m rate (chan1 TypeLits.+ chan2)
mergeChannels audio1 audio2 =
    Audio $ S.map (merge . to0) $ synchronize audio1 audio2
    where
    to0 (a1, a2) =
        ( fromMaybe (Constant count1 0) a1
        , fromMaybe (Constant count2 0) a2
        )
        where
        count1 = framesCount chan1 $ maybe 0 (blockFrames chan2) a2
        count2 = framesCount chan2 $ maybe 0 (blockFrames chan1) a1
    -- These should now have the same number of frames.
    merge = \case
        -- Due to synchronize, c1 == c2
        (Constant c1 v1, Constant c2 v2) | v1 == v2 -> Constant (c1+c2) v1
        (b1, b2) -> Block $ interleaveV $
            deinterleaveV (natVal chan1) (blockVector b1)
            ++ deinterleaveV (natVal chan2) (blockVector b2)
    chan1 = Proxy @chan1
    chan2 = Proxy @chan2

-- | Extract a single channel.  It will crash unless 0 <= idx < chan.
--
-- See 'splitChannels' to get them all.
extractChannel :: forall m rate chan. (Monad m, KnownNat chan)
    => Channels -> Audio m rate chan -> Audio m rate 1
extractChannel idx = Audio . S.map extract . _stream
    where
    extract (Constant count val) = Constant (count `div` chan) val
    extract (Block v) = Block $ deinterleaveV chan v !! idx
    chan = natVal (Proxy @chan)

-- | De-interleave the audio.  This is the inverse of 'splitChannels', but it
-- converts to NAudio, since [Audio] would amount to an unzip, which streams
-- don't support in a straightforward way.
splitChannels :: forall m rate chan. (Monad m, KnownNat chan)
    => Audio m rate chan -> NAudio m rate
splitChannels = NAudio chan . S.map (deinterleaveB chan) . _stream
    where chan = natVal (Proxy @chan)

-- | Take a single channel signal to multiple channels by copying samples.
--
-- This could be generalized to expand n to m where m is divisible by n, but
-- that's more complicated and I don't need it.
expandChannels :: forall m rate chan. (Monad m, KnownNat chan)
    => Audio m rate 1 -> Audio m rate chan
expandChannels (Audio audio) = Audio $ S.map (expandB chan) audio
    where chan = natVal (Proxy :: Proxy chan)

expandB :: Channels -> Block -> Block
expandB chan (Constant count val) = Constant (count * chan) val
expandB chan (Block v) = Block $ expandV chan v

expandV :: Channels -> V.Vector Sample -> V.Vector Sample
expandV chan block =
    V.generate (V.length block * chan) $ \i -> block V.! (i `div` chan)

-- | Do the reverse of 'expandChannels', mixing all channels to a mono signal.
mixChannels :: forall m rate chan. (Monad m, KnownNat chan)
    => Audio m rate chan -> Audio m rate 1
mixChannels (Audio audio) = Audio $ S.map mix audio
    where
    mix (Constant count val) = Constant (count `div` chan) val
    mix (Block v) = Block $ mixV 0 $ deinterleaveV chan v
    chan = natVal (Proxy @chan)

deinterleaveB :: Channels -> Block -> [Block]
deinterleaveB chan (Constant count val) =
    replicate chan $ Constant (count `div` chan) val
deinterleaveB chan (Block v) = map Block $ deinterleaveV chan v

deinterleaveV :: V.Storable a => Channels -> V.Vector a -> [V.Vector a]
deinterleaveV channels v
    | channels == 1 = [v]
    | otherwise = map gen [0 .. channels - 1]
    where
    gen chan = V.generate frames (\i -> v V.! (channels * i + chan))
    frames = V.length v `div` channels

interleaveB :: [Block] -> Block
interleaveB (b@(Constant count val) : bs) | all (==b) bs =
    Constant (count * (length bs + 1)) val
interleaveB bs = Block $ interleaveV $ map blockVector bs

interleaveV :: V.Storable a => [V.Vector a] -> V.Vector a
interleaveV vs = V.create $ do
    out <- VM.new $ Num.sum (map V.length vs)
    forM_ (zip [0..] vs) $ \(vi, v) ->
        forM_ (Lists.range' 0 (V.length v) 1) $ \i ->
            VM.write out (i*stride + vi) (V.unsafeIndex v i)
    return out
    where stride = length vs

-- | Synchronize block size for two streams.  If one stream runs out ahead of
-- the other, it will emit Nothings.
synchronize :: forall m rate chan1 chan2.
    (Monad m, KnownNat chan1, KnownNat chan2)
    => Audio m rate chan1 -> Audio m rate chan2
    -> S.Stream (S.Of (Maybe Block, Maybe Block)) m ()
synchronize audio1 audio2 = S.unfoldr unfold (_stream audio1, _stream audio2)
    where
    unfold (a1, a2) = recons a1 a2 <$> ((,) <$> S.uncons a1 <*> S.uncons a2)
    recons a1 a2 = \case
        (Nothing, Nothing) -> Left ()
        (Just (b1, as1), Nothing) -> Right ((Just b1, Nothing), (as1, a2))
        (Nothing, Just (b2, as2)) -> Right ((Nothing, Just b2), (a1, as2))
        (Just (b1, as1), Just (b2, as2)) -> Right $
            case compare frames1 frames2 of
                LT -> ((Just b1, Just pre2), (as1, S.cons post2 as2))
                GT -> ((Just pre1, Just b2), (S.cons post1 as1, as2))
                EQ -> ((Just b1, Just b2), (as1, as2))
            where
            frames1 = blockFrames chan1 b1
            frames2 = blockFrames chan2 b2
            (pre1, post1) = blockSplit (framesCount chan1 shortest) b1
            (pre2, post2) = blockSplit (framesCount chan2 shortest) b2
            shortest = min frames1 frames2
    chan1 = Proxy :: Proxy chan1
    chan2 = Proxy :: Proxy chan2

-- | Synchronize block size for all streams: pull from each one, then split
-- each to the shortest one.
synchronizeList :: Monad m => [Audio m rate chan]
    -> S.Stream (S.Of [Block]) m ()
synchronizeList = S.unfoldr unfold . map _stream
    where
    unfold audios = do
        pairs <- Maybe.catMaybes <$> mapM S.uncons audios
        return $ case Lists.minimum $ map (blockCount . fst) pairs of
            Nothing -> Left ()
            Just shortest -> Right $ unzip $ map (recons shortest) pairs
    recons size (block, tail)
        | blockCount block <= size = (block, tail)
        | otherwise = case block of
            Block v -> (Block pre, S.cons (Block post) tail)
                where (pre, post) = V.splitAt size v
            Constant count val ->
                (Constant size val, S.cons (Constant (count - size) val) tail)

-- ** non-interleaved

-- | Merge 'Audio's into a single 'NAudio' stream, synchronized with the given
-- block size.
nonInterleaved :: Monad m => Frames -> Frames -> [Audio m rate 1]
    -> NAudio m rate
nonInterleaved now size audios = NAudio (length audios) $
    S.unfoldr unfold (map (_stream . synchronizeToSize now size) audios)
    where
    unfold streams = do
        pairs <- mapM S.uncons streams
        let heads = map (fmap fst) pairs
            tails = [tail | Just (_, tail) <- pairs]
        return $ if null tails then Left ()
            else Right (map (fromMaybe mempty) heads, tails)

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
        Right $ Audio $ S.map (expandB chan . head) $ _nstream naudio
    | _nchannels naudio == chan =
        Right $ Audio $ S.map interleaveB $ _nstream naudio
    | otherwise = Left $ "can't convert " <> showt (_nchannels naudio)
        <> " channels to " <> showt chan
    where
    chan = natVal (Proxy @chan)

synchronizeToSize :: forall m rate chan. (Monad m, KnownNat chan)
    => Frames -> Frames -> Audio m rate chan -> Audio m rate chan
synchronizeToSize now size = Audio . S.unfoldr unfold . (True,)
    where
    unfold (initial, audio) = do
        (blocks, audio) <- splitAt (if initial then align else size) audio
        return $ if null blocks
            then Left ()
            else Right (mconcat blocks, (False, audio))
    align = if now `mod` size == 0 then size else size - now `mod` size

takeN :: Monad m => Frames -> NAudio m rate -> NAudio m rate
takeN frames naudio
    | frames <= 0 = naudio
    | otherwise = NAudio (_nchannels naudio) $
        Control.loop1 (0, _nstream naudio) $ \loop (now, audio) ->
            lift (S.uncons audio) >>= \case
                Nothing -> return ()
                Just ([], audio) -> loop (now, audio)
                Just (blocks@(block:_), audio)
                    | end <= frames -> S.yield blocks >> loop (end, audio)
                    | now >= frames -> return ()
                    | otherwise -> S.yield $ map (fst . blockSplit left) blocks
                    where
                    end = now + blockFrames_ chan block
                    left = framesCount_ chan (min frames end - now)
        where chan = _nchannels naudio

-- | Extend blocks shorter than the given size with zeros, and pad the end with
-- zeros forever.  Composed with 'nonInterleaved', which may leave a short
-- final block, the output should be infinite and have uniform block size.
zeroPadN :: Monad m => Frames -> NAudio m rate -> NAudio m rate
zeroPadN size_ naudio = naudio { _nstream = S.unfoldr unfold (_nstream naudio) }
    where
    unfold audio = S.uncons audio >>= return . \case
        Nothing ->
            Right (replicate (_nchannels naudio) (Constant size 0), audio)
        Just (blocks, audio) -> Right (map pad blocks, audio)
    pad (Constant count val) | val == 0 = Constant (max size count) 0
    pad block
        | blockCount block >= size = block
        | otherwise = Block $ v <> V.replicate (size - V.length v) 0
        where v = blockVector block
    size = framesCount (Proxy @1) size_

-- * generate

-- | Silence.  Forever.
silence :: (Monad m, KnownNat chan) => Audio m rate chan
silence = constant 0

-- | An infinite constant stream, which reuses the same buffer.
constant :: forall m rate chan. (Monad m, KnownNat chan)
    => Sample -> Audio m rate chan
constant val = Audio $ S.repeat $ Constant size val
    where size = framesCount (Proxy @chan) blockSize

-- | Generate a test tone at the given frequency, forever.  This is not
-- efficient, but it's just for testing.
sine :: forall m rate. (Monad m, KnownNat rate) => Float -> Audio m rate 1
sine frequency =
    Audio $ Control.loop1 0 $ \loop frame ->
        S.yield (Block $ gen frame) >> loop (frame + blockSize)
    where
    gen start =
        V.generate (fromIntegral (end - start)) (val . (+start) . Frames)
        where end = start + blockSize
    val frame = sin $ 2 * pi * frequency * (fromIntegral frame / rate)
    rate = fromIntegral $ TypeLits.natVal (Proxy :: Proxy rate)

-- | Generate a piecewise linear signal from breakpoints.  The signal will
-- continue forever with the last value.
--
-- TODO take blockSize and start args so a subsequent synchronization doesn't
-- have to reallocate.
linear :: forall m rate. (Monad m, KnownNat rate)
    => Bool -> [(Seconds, Double)] -> Audio m rate 1
linear forever breakpoints = Audio $ loop (0, 0, 0, from0 breakpoints)
    where
    loop (start, prevX, prevY, breakpoints) = case breakpoints of
        (x, y) : xys
            | toFrame x <= start -> loop (start, x, y, xys)
            | otherwise -> do
                S.yield $ segment prevX prevY x y start (toCount generate)
                loop (start + generate, prevX, prevY, breakpoints)
            where generate = min blockSize (toFrame x - start)
        []  | forever -> _stream $ constant @_ @_ @1 (Num.d2f prevY)
            | otherwise -> S.yield $ Constant 1 (Num.d2f prevY)
    -- Go through some hassle to create constant segments efficiently, since
    -- they should be pretty common.
    segment x1 y1 x2 y2 start count
        | y1 == y2 = Constant count (Num.d2f y1)
        | otherwise = Block $ V.generate count
            (interpolate x1 y1 x2 y2 . toSec . (+start) . Frames)
    interpolate x1 y1 x2 y2 x = Num.d2f $
        (y2 - y1) / (x2 - x1) * (x - x1) + y1
    toFrame = secondsToFrames rate
    toSec = framesToSeconds rate
    toCount = framesCount (Proxy @1)
    rate = natVal (Proxy :: Proxy rate)
    -- The signal is implicitly constant 0 before the first sample.
    from0 bps@((x, y) : _) | x > 0 && y /= 0 = (x, 0) : bps
    from0 bps = bps

-- * effects

-- | Prepend an effect to the stream, useful to log when a stream is started.
effect :: MonadIO m => IO () -> Audio m rate chan -> Audio m rate chan
effect m (Audio s) = Audio (liftIO m >> s)

-- * error

newtype Exception = Exception Text
    deriving (Show)

exceptionText :: Exception -> Text
exceptionText (Exception msg) = msg

instance Exception.Exception Exception where
    displayException (Exception msg) = untxt msg

throw :: Stack.HasCallStack => Text -> AudioIO rate chan
throw = Audio . throwIO

throwIO :: (Stack.HasCallStack, MonadIO m) => Text -> m a
throwIO = liftIO . Exception.throwIO . Exception
    . ((CallStack.getStack <> ": ") <>)

assert :: (Stack.HasCallStack, MonadIO m) => Bool -> Text -> m ()
assert True _ = return ()
assert False msg = throwIO $ "assertion: " <> msg

-- | Insert an assertion into the audio stream.
assertIn :: (Stack.HasCallStack, MonadIO m) => Bool -> Text
    -> Audio m rate chan -> Audio m rate chan
assertIn check msg = apply (assert check msg *>)

-- * constants

blockSize :: Frames
blockSize = 5000

silentBlock :: V.Vector Sample
silentBlock = V.replicate (framesCount (Proxy @1) blockSize) 0

-- * conversions

linearToDb, dbToLinear :: Float -> Float
linearToDb x = logBase 10 x * 20
dbToLinear x = 10**(x / 20)
    -- Here's another way.  Is it faster?  Does it matter?
    -- dbToLinear db = 2**(db * 0.16609640474)

-- * audio util

-- | Take >= the given number of frames.  It may take more if the size doesn't
-- line up on a block boundary.
--
-- TODO rename to splitAtGE
takeFramesGE :: forall m rate chan. (Monad m, KnownNat chan)
    => Frames -> Audio m rate chan -> m ([Block], Audio m rate chan)
takeFramesGE frames (Audio audio) = do
    blocks S.:> rest <- S.toList $
        breakAfter (\n -> (+n) . blockFrames chan) 0 (>=frames) audio
    return (blocks, Audio rest)
    where
    chan = Proxy :: Proxy chan

-- | Take exactly the given number of frames.
splitAt :: forall m rate chan. (Monad m, KnownNat chan)
    => Frames -> Audio m rate chan -> m ([Block], Audio m rate chan)
splitAt frames (Audio audio)
    | frames <= 0 = return ([], Audio audio)
    | otherwise = S.next audio >>= \case
        Left () -> return ([], mempty)
        Right (block, audio)
            | produced < frames ->
                first (block:) <$> splitAt (frames - produced) (Audio audio)
            | produced == frames -> return ([block], Audio audio)
            | otherwise -> return ([pre], Audio $ S.cons post audio)
            where
            produced = blockFrames chan block
            (pre, post) = blockSplit (framesCount chan frames) block
    where
    chan = Proxy :: Proxy chan

next :: Monad m => Audio m rate chan -> m (Maybe (Block, Audio m rate chan))
next (Audio audio) = S.next audio >>= \case
    Left () -> return Nothing
    Right (block, audio) -> return $ Just (block, Audio audio)

-- | Nothing if the Audio stream is completed.  Otherwise, it returns
-- the same stream but with the first chunk evaluated, to avoid duplicating
-- effects.
isEmpty :: Monad m => Audio m rate chan -> m (Maybe (Audio m rate chan))
isEmpty (Audio audio) = S.next audio >>= \case
    Left () -> return Nothing
    Right (block, audio) -> return $ Just $ Audio $ S.cons block audio

-- * util

natVal :: KnownNat n => Proxy n -> Int
natVal = fromIntegral . TypeLits.natVal

someNat :: Int -> TypeLits.SomeNat
someNat int = case TypeLits.someNatVal (fromIntegral int) of
    Nothing -> error $ "not a natural: " <> show int
    Just n -> n

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
