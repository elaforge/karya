-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Resample audio signals via libsamplerate.
module Util.Audio.Resample (
    resample, resampleRate
    , Config(..), defaultConfig
    , resampleBy
    , Quality(..)
    , SavedState
) where
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.ByteString as ByteString
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Vector.Storable as V

import qualified Foreign
import qualified Foreign.C as C
import qualified GHC.TypeLits as TypeLits
import GHC.TypeLits (KnownNat)
import qualified Streaming.Prelude as S

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.SampleRateC as SampleRateC
import Util.Audio.SampleRateC (Quality(..), SavedState(..))
import qualified Util.Control as Control
import qualified Util.Segment as Segment
import qualified Util.Serialize as Serialize

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import Global
import Synth.Types


-- TODO resampling is theoretically pure, so I could maybe unsafePerformIO the
-- resampling

-- | Resample the audio by a constant ratio.  This doesn't actually change the
-- sampling rate, since I just use this to change the pitch.
resample :: forall rate chan. (KnownNat chan, KnownNat rate)
    => Quality -> Double
    -> Audio.AudioIO rate chan -> Audio.AudioIO rate chan
resample quality ratio =
    resampleBy (defaultConfig quality) (Signal.constant ratio)

instance Pretty SavedState where
    pretty (SavedState bs1 bs2) = Text.unwords
        [ "((SavedState"
        , showt (ByteString.length bs1)
        , showt (ByteString.length bs2) <> "))"
        ]

-- | Configure the resampler.
data Config = Config {
    _quality :: Quality
    , _state :: Maybe SavedState
    -- | Called before yielding a chunk.  The final call is with Nothing,
    -- before yielding the final chunk.  At that point the state should be
    -- used up.
    , _notifyState :: Maybe (Audio.Frame, SavedState) -> IO ()
    , _chunkSize :: Audio.Frame
    -- | This affects the first chunk size.  This is so that chunk boundaries
    -- fall on multiples of chunkSize.
    , _now :: Audio.Frame
    -- | This is unused, but useful for debugging.
    , _name :: Text
    }

defaultConfig :: Quality -> Config
defaultConfig quality = Config
    { _quality = quality
    , _state = Nothing
    , _notifyState = const $ return ()
    , _now = 0
    , _chunkSize = Audio.chunkSize
    , _name = "default"
    }

-- | Resample the audio by the given curve.  This doesn't actually change the
-- sampling rate, since I just use this to change the pitch.
resampleBy :: forall rate chan. (KnownNat rate, KnownNat chan)
    => Config -> Signal.Control
    -> Audio.AudioIO rate chan -> Audio.AudioIO rate chan
resampleBy config ratio audio = Audio.Audio $ do
    (key, state) <- lift $ Resource.allocate
        (SampleRateC.new (_quality config)
            (fromIntegral (TypeLits.natVal chan)))
        SampleRateC.delete
    liftIO $ case (_state config) of
        Nothing -> SampleRateC.setRatio state $ Signal.at 0 ratio
        Just saved -> do
            ok <- SampleRateC.putState (_quality config) state saved
            unless ok $
                Audio.throwIO $ "state is the wrong size: " <> pretty saved
    -- I have to collect chunks until I fill up the output chunk size.  The key
    -- thing is to not let the resample state get ahead of the chunk boundary.
    let align = _chunkSize config - (_now config `mod` _chunkSize config)
    let resampleC = resampleChunk chan rate state
    -- I keep track of the number of samples used from upstream.  This gets
    -- reported to '_notifyState' so I can restart the sample at the right
    -- place.
    -- I also keep track of the previous segment to detect discontinuities.
    Control.loop2 (0, segmentAt 0 ratio) (0, Audio._stream audio, [], align) $
        \loop (used, prevSegment) (now, audio, collect, chunkLeft) -> do
            let segment = segmentAt (toSeconds now) ratio
            resampleC now chunkLeft prevSegment segment audio >>= \case
                Nothing -> done key collect
                Just (framesUsed, chunk, audio) ->
                    loop (used + framesUsed, segment)
                        =<< yield state now (used + framesUsed) collect
                            chunkLeft chunk audio
    where
    done key collect = do
        liftIO $ _notifyState config Nothing
        unless (null collect) $
            S.yield $ mconcat (reverse collect)
        lift $ Resource.release key
    yield state now used collect chunkLeft chunk audio
        | chunkLeft - generated > 0 = return
            ( now + generated
            , audio
            , chunk : collect
            , chunkLeft - generated
            )
        | chunkLeft - generated == 0 = do
            let sizes = map (Audio.chunkFrames chan) (chunk:collect)
            Audio.assert (sum sizes <= _chunkSize config) $ Text.unwords
                [ "sum", pretty (sum sizes), "> chunkSize"
                , pretty (_chunkSize config) <> ":", pretty sizes
                ]
            liftIO $ do
                rState <- SampleRateC.getState state
                _notifyState config $ Just (used, rState)
            S.yield $ mconcat (reverse (chunk : collect))
            return
                ( now + generated
                , audio
                , []
                , _chunkSize config
                )
        | otherwise = Audio.throwIO $ "resampleChunk generated too much: "
            <> pretty (chunkLeft, generated)
        where generated = Audio.chunkFrames chan chunk

    chan = Proxy :: Proxy chan
    rate :: Audio.Rate
    rate = fromIntegral $ TypeLits.natVal (Proxy :: Proxy rate)
    toSeconds = RealTime.seconds . Audio.frameToSeconds rate

type Segment = Segment.Segment Signal.Y

segmentAt :: RealTime -> Signal.Control -> Segment
segmentAt x0 ratio = case Signal.segment_at x0 ratio of
    Just segment -> segment
    Nothing
        | Just (x, y) <- Signal.last ratio, x0 >= x ->
            Segment.Segment 0 y RealTime.large y
        | otherwise -> Segment.Segment 0 1 RealTime.large 1

type Stream a =
    S.Stream (S.Of (V.Vector Audio.Sample)) (Resource.ResourceT IO) a

-- | Generate no more than the given number of frames.
resampleChunk :: KnownNat chan => Proxy chan -> Audio.Rate
    -> SampleRateC.State -> Audio.Frame -> Audio.Frame
    -> Segment -> Segment -> Stream ()
    -> Stream (Maybe (Audio.Frame, V.Vector Audio.Sample, Stream ()))
resampleChunk chan rate state start maxFrames prevSegment segment audio = do
    (inputChunk, audio) <- next audio
    (atEnd, audio) <- lift $ checkEnd audio
    let inputFrames = maybe 0 (Audio.chunkFrames chan) inputChunk
    -- Never go past the next breapoint.
    let outputFrames = min maxFrames (toFrames (Segment._x2 segment) - start)
    let destRatio = Segment.num_interpolate_s segment $
            toSeconds $ start + outputFrames
        -- Progress through the ratio signal proceeds in real time, which is
        -- to say, outputFrames.
    let with = V.unsafeWith (fromMaybe V.empty inputChunk)
    (used, generated, outFP) <- liftIO $ with $ \chunkp -> do
        when (segment /= prevSegment
                && Segment._y2 prevSegment /= Segment._y1 segment) $ do
            -- Debug.tracepM "setRatio" (prevSegment, segment)
            SampleRateC.setRatio state (Segment._y1 segment)
        outp <- Foreign.mallocArray $ Audio.framesCount chan outputFrames
        result <- SampleRateC.process state $ SampleRateC.Input
            { data_in =
                (Foreign.castPtr :: Foreign.Ptr Float -> Foreign.Ptr C.CFloat)
                chunkp
            , data_out = Foreign.castPtr outp
            , input_frames = fromIntegral inputFrames
            , output_frames = fromIntegral outputFrames
            , src_ratio = destRatio
            , end_of_input = atEnd
            }
        -- Debug.tracepM "segment" (start, start+outputFrames, segment)
        -- Debug.tracepM "in, out, ->rat" (inputFrames, outputFrames, destRatio)
        outFP <- Foreign.newForeignPtr Foreign.finalizerFree outp
        -- Debug.tracepM "used, gen"
        --     ( SampleRateC.input_frames_used result
        --     , SampleRateC.output_frames_generated result
        --     )
        return
            ( Audio.Frame $ fromIntegral $
                SampleRateC.input_frames_used result
            , Audio.Frame $ fromIntegral $
                SampleRateC.output_frames_generated result
            , outFP
            )

    -- Stick unconsumed input back on the stream.
    let left = maybe V.empty (V.drop (Audio.framesCount chan used)) inputChunk
        recons = if V.null left then id else S.cons left
    let outputChunk = V.unsafeFromForeignPtr0 outFP $
            Audio.framesCount chan generated
    return $ if Maybe.isNothing inputChunk && generated == 0
        then Nothing
        else Just (used, outputChunk, recons audio)
    where
    next audio = either (const (Nothing, audio)) (first Just) <$>
        lift (S.next audio)
    toFrames = Audio.secondsToFrame rate . RealTime.to_seconds
    toSeconds = RealTime.seconds . Audio.frameToSeconds rate

-- | True if this stream is empty.  It also returns the stream since it has to
-- peek an element to check, and if it's not empty, it conses the element back
-- on to avoid repeating the effect.
checkEnd :: Monad m => S.Stream (S.Of a) m r -> m (Bool, S.Stream (S.Of a) m r)
checkEnd stream = (S.next stream) >>= \case
    Left a -> pure (True, pure a)
    Right (x, xs) -> pure (False, S.cons x xs)

resampleRate :: forall rateIn rateOut chan.
    (KnownNat rateIn, KnownNat rateOut, KnownNat chan)
    => Quality
    -> Audio.AudioIO rateIn chan -> Audio.AudioIO rateOut chan
resampleRate ctype =
    Audio.Audio . Audio._stream . resample ctype (rateOut / rateIn)
    where
    rateIn = fromIntegral $ TypeLits.natVal (Proxy :: Proxy rateIn)
    rateOut = fromIntegral $ TypeLits.natVal (Proxy :: Proxy rateOut)

instance Serialize.Serialize SavedState where
    put (SavedState a b) = Serialize.put a >> Serialize.put b
    get = SavedState <$> Serialize.get <*> Serialize.get
