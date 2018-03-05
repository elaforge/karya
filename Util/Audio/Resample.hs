-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Resample audio signals via libsamplerate.
module Util.Audio.Resample (
    resample, resampleBy, resampleRate
    , ConverterType(..)
) where
import qualified Control.Monad.Trans.Resource as Resource
import qualified Unsafe.Coerce as Coerce
-- import qualified Data.Conduit.Audio.SampleRate.Binding as Binding
-- import Data.Conduit.Audio.SampleRate.Binding (ConverterType(..))
import qualified Data.Maybe as Maybe
import qualified Data.Vector.Storable as V

import qualified Foreign
import qualified GHC.TypeLits as TypeLits
import GHC.TypeLits (KnownNat)
import qualified Streaming.Prelude as S

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.Binding as Binding
import Util.Audio.Binding (ConverterType(..))
import qualified Util.Segment as Segment
import qualified Util.Seq as Seq
import qualified Util.Debug as Debug

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Global


-- TODO I could maybe unsafePerformIO the resampling C.

-- | Resample the audio.  This doesn't actually change the sampling rate, since
-- I just use this to change the pitch.
resample :: forall rate chan. (KnownNat chan)
    => ConverterType -> Double
    -> Audio.AudioIO rate chan -> Audio.AudioIO rate chan
resample ctype ratio audio = Audio.Audio $ do
    (key, state) <- lift $
        Resource.allocate (Binding.new ctype channels) Binding.delete
    Audio.loop1 (Audio._stream audio) $ \loop audio ->
        lift (S.next audio) >>= \case
            Left () -> lift (Resource.release key) >> return ()
            Right (chunk, audio) -> handle loop state chunk audio
    where
    handle loop state chunk audio = do
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

-- TODO
resampleBy :: forall rate chan. (KnownNat rate, KnownNat chan)
    => ConverterType -> Signal.Control
    -> Audio.AudioIO rate chan -> Audio.AudioIO rate chan
resampleBy ctype ratio audio = undefined

-- | Resample the audio.  This doesn't actually change the sampling rate, since
-- I just use this to change the pitch.
--
-- TODO the timing is off, because I set outputFrames = inputFrames.
resampleBy_old :: forall rate chan. (KnownNat rate, KnownNat chan)
    => ConverterType -> Signal.Control
    -> Audio.AudioIO rate chan -> Audio.AudioIO rate chan
resampleBy_old ctype ratio audio = Audio.Audio $ do
    let breakpoints = map (Audio.secondsToFrames rate) $
            Seq.drop_dups id $ map (RealTime.to_seconds . Segment.sx) $
            Signal.to_samples ratio
    (key, state) <- lift $ Resource.allocate
        (Binding.new ctype (fromIntegral (TypeLits.natVal chan)))
        Binding.delete
    let stream = Audio._stream (Audio.synchronizeBy breakpoints audio)

    Debug.tracepM "start ratio" (Signal.at 0 ratio)
    liftIO $ Binding.setRatio state (Signal.at 0 ratio)
    liftIO $ Debug.traceM "1st ratio" =<< Foreign.peek (Coerce.unsafeCoerce state :: Foreign.Ptr Double)
    Audio.loop1 (0, stream) $
        \loop (frame, audio) -> lift (S.next audio) >>= \case
            Left () -> lift (Resource.release key) >> return ()
            Right (chunk, audio) -> handle loop state chunk frame audio
    where
    handle loop state chunk start audio = do
        (next, audio) <- either (const (Nothing, audio)) (first Just) <$>
            lift (S.next audio)
        let inputFrames = Audio.chunkFrames chan chunk
        --     outputFrames = fromIntegral inputFrames * 
        let endRatio = ratioAt (start + inputFrames)
        let outputFrames = inputFrames
        -- let outputFrames = round $
        --         fromIntegral inputFrames * max (ratioAt start) endRatio * 1.1
        (used, generated, outFP) <- liftIO $ V.unsafeWith chunk $ \chunkp -> do
            outp <- Foreign.mallocArray $ Audio.framesCount chan outputFrames
            result <- Binding.process state $ Binding.DataIn
                { data_in = Foreign.castPtr chunkp
                , data_out = Foreign.castPtr outp
                , input_frames = fromIntegral inputFrames
                , output_frames = fromIntegral outputFrames
                , src_ratio = endRatio
                , end_of_input = Maybe.isNothing next
                }
            Debug.tracepM "ratio" (start, start + inputFrames, next==Nothing, endRatio)
            Debug.traceM "last ratio" =<< Foreign.peek (Coerce.unsafeCoerce state :: Foreign.Ptr Double)
            -- when (startRatio /= endRatio) $
            Binding.setRatio state endRatio
            outFP <- Foreign.newForeignPtr Foreign.finalizerFree outp
            return
                ( Audio.Frames $ fromIntegral $ Binding.input_frames_used result
                , Audio.Frames $ fromIntegral $ Binding.output_frames_gen result
                , outFP
                )
        Debug.tracepM "input, used, generated, outFrames" (inputFrames, used, generated, outputFrames)
        let out = V.unsafeFromForeignPtr0 outFP $
                Audio.framesCount chan generated
        Debug.tracepM "len" (V.length out)
        S.yield out
        -- return ()
        let again s = loop (start + used, s)
        let withNext = maybe audio (`S.cons` audio) next
            left = V.drop (Audio.framesCount chan used) chunk
        if  | used >= inputFrames -> again withNext
            -- Only consumed part of the input.
            | generated > 0 -> again $ S.cons left withNext
            -- It wants more input, so combine with the next chunk.
            | otherwise -> case next of
                Nothing -> return ()
                Just next -> again $ S.cons (left V.++ next) audio

    ratioAt frame = Signal.at
        (RealTime.seconds (Audio.framesToSeconds rate frame))
        ratio
    chan = Proxy :: Proxy chan
    rate :: Audio.Rate
    rate = fromIntegral $ TypeLits.natVal (Proxy :: Proxy rate)

{-
    I think the intended mode is that you have a destination ratio, and
    a small constant out frames, and you juust make blocks until it
    reaches the destination.
    Ratios should be indexed by *output* time.  So set output frames
    to the next ratios breakpoint, then loop process until I reach it.
    The problem is that I want to stop exactly at the breakpoint.
    So if I'm coming up on a breakpoint, I need to intentionally supply
    fewer input frames, but how many?  I'd need to project where in the
    input the breakpoint will fall, based on the integral of the ratio change
    slope.

    For callback API:

    typedef long (*src_callback_t) (void *cb_data, float **data) ;
    getSamples(_cb_data, data) = *data = S.pull
    state = src_callback_new(getSamples, ctype, chan, error, nullPtr)

    for (x, y) in breakpoints:
        gen = src_callback_read(state, y, secondToFrames(x), &data)
        if gen == 0 then return ()
            else S.yield (gen, data)

    But this is inconvenient in haskell, especially because I need a mutable
    S.pull for the callback.
-}

-- dropUntil :: Ord a => a -> [(a, y)] -> [(a, y)]
-- dropUntil x = go
--     where
--     go [] = []
--     go xys@(_ : xy2s@((x2, _) : _))
--         | x2 <= x = go xy2s
--         | otherwise = xys
--     go xys = xys
--
-- toPairs :: Int -> Signal.Control -> [(Audio.Frames, Double)]
-- toPairs rate = map (first (Audio.secondsToFrames rate . RealTime.to_seconds))
--     . Signal.to_pairs

resampleRate :: forall rateIn rateOut chan.
    (KnownNat rateIn, KnownNat rateOut, KnownNat chan)
    => ConverterType
    -> Audio.AudioIO rateIn chan -> Audio.AudioIO rateOut chan
resampleRate ctype =
    Audio.Audio . Audio._stream . resample ctype (rateOut / rateIn)
    where
    rateIn = fromIntegral $ TypeLits.natVal (Proxy :: Proxy rateIn)
    rateOut = fromIntegral $ TypeLits.natVal (Proxy :: Proxy rateOut)
