-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Audio.Resample (
    resample
    , ConverterType(..)
) where
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.Conduit.Audio.SampleRate.Binding as Binding
import Data.Conduit.Audio.SampleRate.Binding (ConverterType(..))
import qualified Data.Maybe as Maybe
import qualified Data.Vector.Storable as V

import qualified Foreign
import qualified GHC.TypeLits as TypeLits
import qualified Streaming.Prelude as S

import qualified Util.Audio.Audio as Audio
import Global


-- TODO I could maybe unsafePerformIO the resampling C.

-- | Resample the audio.
resample :: forall rate chan. (TypeLits.KnownNat chan)
    => ConverterType -> Double
    -> Audio.AudioIO rate chan -> Audio.AudioIO rate chan
resample ctype ratio audio = Audio.Audio $ do
    (key, state) <- lift $
        Resource.allocate (Binding.new ctype channels) Binding.delete
    let loop audio = lift (S.next audio) >>= \case
            Left () -> lift (Resource.release key) >> return ()
            Right (Audio.Silence count, audio) -> do
                S.yield $ Audio.Silence count -- TODO
                loop audio
            Right (Audio.Chunk chunk, audio) -> handle1 loop state chunk audio
    loop (Audio._stream audio)
    where
    channels :: Int
    channels = fromIntegral $ TypeLits.natVal (Proxy :: Proxy chan)

    handle1 loop state chunk audio = do
        (next, audio) <- either (const (Nothing, audio)) (first Just) <$>
            lift (S.next audio)
        let inputFrames = V.length chunk `div` channels
            outputFrames = round $ fromIntegral inputFrames * ratio * 1.1
        (used, generated, outFP) <- liftIO $ V.unsafeWith chunk $ \chunkp -> do
            outp <- Foreign.mallocArray $ outputFrames * channels
            result <- Binding.process state $ Binding.DataIn
                { data_in = Foreign.castPtr chunkp
                , data_out = Foreign.castPtr outp
                , input_frames = fromIntegral inputFrames
                , output_frames = fromIntegral outputFrames
                , src_ratio = ratio
                , end_of_input = Maybe.isNothing next
                }
            outFP <- Foreign.newForeignPtr Foreign.finalizerFree outp
            return
                ( fromIntegral $ Binding.input_frames_used result
                , Binding.output_frames_gen result
                , outFP
                )
        let out = V.unsafeFromForeignPtr0 outFP $
                fromIntegral generated * channels
        S.yield (Audio.Chunk out)
        let withNext = maybe audio (`S.cons` audio) next
            left = V.drop (used * channels) chunk
        if  | used >= inputFrames -> loop withNext
            -- Only consumed part of the input.
            | generated > 0 -> loop $ S.cons (Audio.Chunk left) withNext
            -- It wants more input, so combine with the next chunk.
            | otherwise -> case next of
                Nothing -> return ()
                Just (Audio.Silence _) -> return () -- TODO
                Just (Audio.Chunk next) ->
                    loop $ S.cons (Audio.Chunk $ left V.++ next) audio


{-

inspect :: Monad m => Stream f m r -> m (Either r (f (Stream f m r)))
next :: Monad m => Stream (Of a) m r -> m (Either r (a, Stream (Of a) m r))
yield :: Monad m => a -> Stream (Of a) m ()

    Audio { _stream :: S.Stream (S.Of Chunk) m () }

unfoldr :: Monad m => (s -> m (Either r (a, s))) -> s -> Stream (Of a) m r
unfold :: (Monad m, Functor f) => (s -> m (Either r (f s))) -> s -> Stream f m r

This should take a signal of breakpoints, not a plain number.


make a DataIn:

    get this chunk and next chunk

    Binding.process state $ Binding.DataIn
        { data_in       = castPtr inPtr
        , data_out      = castPtr outPtr
        , input_frames  = fromIntegral inLen
        , output_frames = fromIntegral outLen
        , src_ratio     = ratio
        , end_of_input  = isEnd
        }

-- resample ctype ratio = Audio.Audio . S.unfoldr unfold . Audio._stream
--     where
--     unfold audio = do
--         (key, state) <- Resource.allocate (Binding.new ctype channels)
--             Binding.delete
--         undefined
-}
