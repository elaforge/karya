-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Resample audio signals via libsamplerate.
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

-- | Resample the audio.  This doesn't actually change the sampling rate, since
-- I just use this to change the pitch.
resample :: forall rate chan. (TypeLits.KnownNat chan)
    => ConverterType -> Double
    -> Audio.AudioIO rate chan -> Audio.AudioIO rate chan
resample ctype ratio audio = Audio.Audio $ do
    (key, state) <- lift $
        Resource.allocate (Binding.new ctype channels) Binding.delete
    let loop audio = lift (S.next audio) >>= \case
            Left () -> lift (Resource.release key) >> return ()
            Right (chunk, audio) -> handle1 loop state chunk audio
    loop (Audio._stream audio)
    where
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
        S.yield out
        let withNext = maybe audio (`S.cons` audio) next
            left = V.drop (used * channels) chunk
        if  | used >= inputFrames -> loop withNext
            -- Only consumed part of the input.
            | generated > 0 -> loop $ S.cons left withNext
            -- It wants more input, so combine with the next chunk.
            | otherwise -> case next of
                Nothing -> return ()
                Just next -> loop $ S.cons (left V.++ next) audio
    channels :: Int
    channels = fromIntegral $ TypeLits.natVal (Proxy :: Proxy chan)
