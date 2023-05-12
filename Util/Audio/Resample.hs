-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
-- | Resample audio signals via libsamplerate.
module Util.Audio.Resample (
    resample, resampleRate
    , Config(..), defaultConfig
    , resampleBy
    , Quality(..)
    , SavedState
#ifdef TESTING
    , module Util.Audio.Resample
#endif
) where
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.ByteString as ByteString
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Vector.Storable as V

import qualified Foreign
import qualified Foreign.C as C
import qualified GHC.TypeLits as TypeLits
import           GHC.TypeLits (KnownNat)
import qualified Streaming.Prelude as S

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.SampleRateC as SampleRateC
import           Util.Audio.SampleRateC (Quality(..), SavedState(..))
import qualified Util.Control as Control
import qualified Util.Num as Num
import qualified Util.Segment as Segment
import qualified Util.Serialize as Serialize

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import           Global
import           Synth.Types


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
    -- | Called before yielding a block.  The final call is with Nothing,
    -- before yielding the final block.  At that point the state should be
    -- used up.
    , _notifyState :: Maybe (Audio.Frames, SavedState) -> IO ()
    , _blockSize :: Audio.Frames
    -- | This affects the first block size.  This is so that block boundaries
    -- fall on multiples of blockSize.
    , _now :: Audio.Frames
    -- | This is unused, but useful for debugging.
    , _name :: String
    }

defaultConfig :: Quality -> Config
defaultConfig quality = Config
    { _quality = quality
    , _state = Nothing
    , _notifyState = const $ return ()
    , _now = 0
    , _blockSize = Audio.blockSize
    , _name = "default"
    }

-- | Resample the audio by the given curve.  This doesn't actually change the
-- sampling rate, since I just use this to change the pitch.
resampleBy :: forall rate chan. (KnownNat rate, KnownNat chan)
    => Config -> Signal.Control
    -> Audio.AudioIO rate chan -> Audio.AudioIO rate chan
resampleBy config ratios audio = Audio.Audio $ do
    (key, state) <- lift $ Resource.allocate
        (SampleRateC.new (_quality config)
            (fromIntegral (TypeLits.natVal chan)))
        SampleRateC.delete
    -- Debug.tracepM "RESAMPLE: now, ratios" (_now config, ratios)
    liftIO $ case (_state config) of
        Nothing -> SampleRateC.setRatio state $ Signal.at ratios 0
        Just saved -> do
            ok <- SampleRateC.putState (_quality config) state saved
            unless ok $
                Audio.throwIO $ "state is the wrong size: " <> pretty saved
    -- I have to collect blocks until I fill up the output block size.  The key
    -- thing is to not let the resample state get ahead of the block boundary.
    let align = _blockSize config - (_now config `mod` _blockSize config)
    -- I keep track of the number of samples read from upstream.  This gets
    -- reported to '_notifyState' so I can restart the sample at the right
    -- place.
    -- I also keep track of the previous segment to detect discontinuities.
    -- Use frame -1 for the initial prevSegment.  If I happen to be starting on
    -- a discontinuity, I need to do a setRatio.
    let initialState = (0, segmentAt rate ratios (toSeconds (-1)))
    Control.loop2 initialState (0, Audio._stream audio, [], align) $
        \loop (framesRead, prevSegment) (now, audio, collect, blockLeft) -> do
            let segment = segmentAt rate ratios (toSeconds now)
            result <- resampleBlock chan rate state now blockLeft prevSegment
                segment audio
            case result of
                Nothing -> done key collect
                Just (blockRead, block, audio) -> do
                    nextState <- yield state now (framesRead + blockRead)
                        collect blockLeft block audio
                    loop (framesRead + blockRead, segment) nextState
    where
    done key collect = do
        liftIO $ _notifyState config Nothing
        unless (null collect) $
            S.yield $ Audio.Block $ mconcat (reverse collect)
        -- Resource.release will delete the state, at which point a pointer
        -- from unsafeGetState will be dangling.  I think Sampler.Render will
        -- omit it from the playing notes and not ask for the state anyway, but
        -- let's not leave a dangling pointer around anyway.
        liftIO $ _notifyState config Nothing
        Resource.release key
    yield state now used collect blockLeft block audio
        | blockLeft - generated > 0 = return
            ( now + generated
            , audio
            , block : collect
            , blockLeft - generated
            )
        | blockLeft - generated == 0 = do
            let sizes = map (Audio.vectorFrames chan) (block:collect)
            Audio.assert (Num.sum sizes <= _blockSize config) $ Text.unwords
                [ "sum", pretty (Num.sum sizes), "> blockSize"
                , pretty (_blockSize config) <> ":", pretty sizes
                ]
            liftIO $ do
                rState <- SampleRateC.unsafeGetState state
                _notifyState config $ Just (used, rState)
            S.yield $ Audio.Block $ mconcat (reverse (block : collect))
            return
                ( now + generated
                , audio
                , []
                , _blockSize config
                )
        | otherwise = Audio.throwIO $ "resampleBlock generated too much: "
            <> pretty (blockLeft, generated)
        where generated = Audio.vectorFrames chan block

    chan = Proxy :: Proxy chan
    rate :: Audio.Rate
    rate = fromIntegral $ TypeLits.natVal (Proxy :: Proxy rate)
    toSeconds = RealTime.seconds . Audio.framesToSeconds rate

type Segment = Segment.Segment Signal.Y

segmentAt :: Audio.Rate -> Signal.Control -> RealTime -> Segment
segmentAt rate ratios x0 = case Signal.segment_at ratios x0 of
    Just segment
        -- If the difference is less than a sample, I don't want it, because
        -- resampleBlock will then give 0 input samples, which will loop
        -- forever.
        | Segment._x2 segment - eta < x0 -> segmentAt rate ratios (x0+eta)
        | otherwise -> segment
    Nothing
        | Just (x, y) <- Signal.last ratios, x0 >= x ->
            Segment.Segment 0 y RealTime.large y
        | otherwise -> Segment.Segment 0 1 RealTime.large 1
    where
    eta = 1 / fromIntegral rate

type Stream a = S.Stream (S.Of Audio.Block) (Resource.ResourceT IO) a

-- | Generate no more than the given number of frames.
resampleBlock :: KnownNat chan => Proxy chan -> Audio.Rate
    -> SampleRateC.State -> Audio.Frames -> Audio.Frames
    -> Segment -> Segment -> Stream ()
    -> Stream (Maybe (Audio.Frames, V.Vector Audio.Sample, Stream ()))
resampleBlock chan rate state start maxFrames prevSegment segment audio = do
    -- 'start' is the *output* frame.  It indexes ratios, because that's
    -- RealTime.
    (inputBlock, audio) <- next audio
    (atEnd, audio) <- lift $ checkEnd audio
    let inputFrames = maybe 0 (Audio.vectorFrames chan) inputBlock
    -- Never go past the next breakpoint.
    let outputFrames = min maxFrames (toFrames (Segment._x2 segment) - start)

    -- Debug.tracepM "(start, end), (prev, segment)"
    --     ((start, start+outputFrames), (showSeg prevSegment, showSeg segment))

    let destRatio = Segment.num_interpolate_s segment $
            toSeconds $ start + outputFrames
        -- Progress through the ratios signal proceeds in real time, which is
        -- to say, outputFrames.
    let with = V.unsafeWith (fromMaybe V.empty inputBlock)
    (used, generated, outFP) <- liftIO $ with $ \blockp -> do
        -- Discontinuity in the ratios signal.
        when (segment /= prevSegment
                && Segment._y2 prevSegment /= Segment._y1 segment) $ do
            -- Debug.tracepM "setRatio" (Segment._y1 segment)
            SampleRateC.setRatio state (Segment._y1 segment)
        outp <- Foreign.mallocArray $ Audio.framesCount chan outputFrames
        result <- SampleRateC.process state $ SampleRateC.Input
            { data_in =
                (Foreign.castPtr :: Foreign.Ptr Float -> Foreign.Ptr C.CFloat)
                blockp
            , data_out = Foreign.castPtr outp
            , input_frames = fromIntegral inputFrames
            , output_frames = fromIntegral outputFrames
            , src_ratio = destRatio
            , end_of_input = atEnd
            }
        outFP <- Foreign.newForeignPtr Foreign.finalizerFree outp
        return
            ( Audio.Frames $ fromIntegral $
                SampleRateC.input_frames_used result
            , Audio.Frames $ fromIntegral $
                SampleRateC.output_frames_generated result
            , outFP
            )

    -- Stick unconsumed input back on the stream.
    let left = maybe V.empty (V.drop (Audio.framesCount chan used)) inputBlock
        recons = if V.null left then id else S.cons (Audio.Block left)
    let outputBlock = V.unsafeFromForeignPtr0 outFP $
            Audio.framesCount chan generated

    -- srcRatio <- liftIO $ SampleRateC.getRatio state
    -- Debug.tracepM "OUTPUT"
    --     ((srcRatio, destRatio), (used, generated),
    --         (oneChan <$> inputBlock, oneChan outputBlock))

    let framesRead = Audio.countFrames chan $
            maybe 0 V.length inputBlock - V.length left
    return $ if Maybe.isNothing inputBlock && generated == 0
        then Nothing
        else Just (framesRead, outputBlock, recons audio)
    where
    -- TODO I convert Audio.Constant to vectors.  I could possibly resample
    -- Constant efficiently by just changing the length, but the resampler has
    -- lots of state inside so it would only work if preceding audio was all
    -- 0, and in that case I probably will be prepend the silence before
    -- resampling.
    next audio =
        either (const (Nothing, audio)) (first (Just . Audio.blockVector)) <$>
        lift (S.next audio)
    toFrames = Audio.secondsToFrames rate . RealTime.to_seconds
    toSeconds = RealTime.seconds . Audio.framesToSeconds rate

    -- showSeg (Segment.Segment x1 y1 x2 y2) =
    --     pretty (toFrames x1, y1) <> "--" <> pretty (toFrames x2, y2)
    -- oneChan :: V.Vector Audio.Sample -> V.Vector Audio.Sample
    -- oneChan = head . Audio.deinterleaveV 2

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
