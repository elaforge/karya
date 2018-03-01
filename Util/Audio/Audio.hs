-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DataKinds, KindSignatures #-}
module Util.Audio.Audio (
    -- * types
    AudioM(..), AudioIO, AudioId
    , Chunk(..), chunkSamples, chunkCount, chunkSize
    , Sample, Frames, Count, Channels
    -- * construct
    , fromSamples, toSamples
    -- * transform
    , gain
    -- * mix
    , mix
) where
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.Maybe as Maybe
import qualified Data.Vector.Storable as V
import qualified GHC.TypeLits as TypeLits
import qualified Streaming as S
import qualified Streaming.Prelude as S

import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import Global


-- * types

newtype AudioM m (rate :: TypeLits.Nat) (channels :: TypeLits.Nat) =
    Audio { _stream :: S.Stream (S.Of Chunk) m () }

type AudioIO rate channels = AudioM (Resource.ResourceT IO) rate channels
type AudioId rate channels = AudioM Identity.Identity rate channels

data Chunk = Chunk (V.Vector Sample) | Silence !Count
    deriving (Show)

instance Pretty.Pretty Chunk where
    format (Chunk v) = Pretty.format v
    format (Silence c) = "silence " <> Pretty.format c

chunkSamples :: Chunk -> V.Vector Sample
chunkSamples (Chunk v) = v
chunkSamples (Silence c) = V.replicate c 0

chunkCount :: Chunk -> Count
chunkCount (Chunk v) = V.length v
chunkCount (Silence c) = c

-- | I hardcode the sample format to Float for now, since I have no need to
-- work with any other format.
type Sample = Float

-- | Should be >=0.
newtype Frames = Frames Int
    deriving (Show, Eq, Ord, Num)

-- | Sample count.  This is Frames * channels.
type Count = Int
type Channels = Int

chunkSize :: Count
chunkSize = 5000

frameCount :: TypeLits.KnownNat channels => Proxy channels -> Frames -> Count
frameCount channels (Frames frames) =
    frames * fromIntegral (TypeLits.natVal channels)

-- * construct

fromSamples :: Monad m => [V.Vector Sample] -> AudioM m rate channels
fromSamples = Audio . S.each . map Chunk

toSamples :: Monad m => AudioM m rate channels -> m [V.Vector Sample]
toSamples = S.toList_ . S.map chunkSamples . _stream

-- * transform

gain :: Monad m => Float -> AudioM m rate channels -> AudioM m rate channels
gain n (Audio audio) = Audio $ S.map f audio
    where
    f (Chunk v) = Chunk $ V.map (*n) v
    f (Silence c) = Silence c

-- * mix

-- | Mix together the audio streams at the given start times.
--
-- TODO the input could also be a stream, in case it somehow comes from IO.
mix :: forall m rate channels. (Monad m, TypeLits.KnownNat channels)
    => [(Frames, AudioM m rate channels)] -> AudioM m rate channels
mix = Audio . S.map merge . synchronize . map pad
    where
    pad (frames, Audio a)
        | frames > 0 = Audio $ S.cons (Silence (frameCount channels frames)) a
        | otherwise = Audio a
    merge chunks
        | null vs = case [c | Silence c <- chunks] of
            -- All Silences should be the same, thanks to 'synchronize'.
            count : _ -> Silence count
            -- 'synchronize' shouldn't emit empty chunks.
            [] -> Silence 0
        | otherwise = Chunk $ zipWithN (+) vs
        where vs = [v | Chunk v <- chunks]
    channels = Proxy :: Proxy channels
    -- The strategy is to pad the beginning of each audio stream with silence,
    -- but make mixing silence cheap with a special Silence constructor.

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
synchronize :: Monad m => [AudioM m rate channels]
    -> S.Stream (S.Of [Chunk]) m ()
synchronize = S.unfoldr unfold . map _stream
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
            Silence frames ->
                (Silence size, S.cons (Silence (frames - size)) tail)
