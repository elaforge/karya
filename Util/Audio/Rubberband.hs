-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeApplications #-}
-- | Bind to the rubberband library.
module Util.Audio.Rubberband (
    Config(..)
    , config
    , Option -- ProcessOffline and ProcessRealTime omitted
        ( StretchElastic, StretchPrecise, TransientsCrisp
        , TransientsMixed, TransientsSmooth
        , DetectorCompound, DetectorPercussive, DetectorSoft
        , PhaseLaminar, PhaseIndependent
        -- threading omitted
        , WindowStandard, WindowShort, WindowLong
        , SmoothingOff, SmoothingOn
        , FormantShifted, FormantPreserved
        , PitchHighSpeed, PitchHighQuality, PitchHighConsistency
        , ChannelsApart, ChannelsTogether
        )
    , percussiveOptions
    , offline
) where
import qualified Control.Exception as Exception
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.Vector.Storable as V
import qualified Foreign
import           Foreign (Ptr)
import           GHC.TypeLits (KnownNat)
import qualified Streaming.Prelude as S

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.RubberbandC as RubberbandC
import           Util.Audio.RubberbandC (percussiveOptions, Option(..))
import qualified Util.Control as Control
import qualified Util.Seq as Seq

import           Global


data Config = Config {
    _options :: ![Option]
    , _timeRatio :: !Double
    , _pitchRatio :: !Double
    } deriving (Show)

type Samples = V.Vector Audio.Sample

config :: Config
config = Config [] 1 1

{- Options:

    StretchElastic - offline only, variable stretch to preserve transients
    StretchPrecise - online mode, try to get linear stretch rate throughout

    TransientsCrisp - good for percussive
    TransientsMixed
    TransientsSmooth - good for melodic

    DetectorCompound - general transient detector, default
    DetectorPercussive
    DetectorSoft - for melodic

    ThreadingAuto - one thread per channel in offline
    ThreadingNever
    ThreadingAlways

    WindowStandard
    WindowShort - better for timing
    WindowLong

    FormantShifted - default, no formant processing
    FormantPreserved - maybe better for pitch shifting instruments?

    PitchHigh{Speed,Quality,Consistency} - streaming only
-}


-- TODO
-- take key frame map as a signal?

-- | Stretch or pitch shift audio in a non-streaming way.  This collects
-- the whole stream, does the transformation, and then streams the output.
offline :: forall rate chan. (KnownNat rate, KnownNat chan)
    => Config -> Audio.AudioIO rate chan -> Audio.AudioIO rate chan
offline config audio = Audio.Audio $ do
    blocks <- lift $ Audio.toBlocks audio
    interleave $ offlineStream rate chan config (deinterleave chan blocks)
    where
    rate = Audio.natVal (Proxy @rate)
    chan = Audio.natVal (Proxy @chan)

interleave :: Monad m => S.Stream (S.Of [Samples]) m ()
    -> S.Stream (S.Of Audio.Block) m ()
interleave = S.map (Audio.Block . Audio.interleaveV)

-- | This reallocates the whole audio stream as one big vector for each
-- channel.  I could avoid this by passing it into rubberband_study and
-- rubberband_process as chunks, but since they all have to be kept in memory
-- anyway, I don't think it would help, and it's more complicated to implement.
deinterleave :: Audio.Channels -> [Audio.Block] -> [Samples]
deinterleave chan = map (mconcat . map Audio.blockVector) . Seq.rotate
    . map (Audio.deinterleaveB chan)

offlineStream :: Audio.Rate -> Audio.Channels -> Config -> [Samples]
    -> S.Stream (S.Of [Samples]) (Resource.ResourceT IO) ()
offlineStream _ _ _ [] = return ()
offlineStream srate chan config inputs@(input0:_) = do
    (key, state) <- lift $ Resource.allocate new RubberbandC.rubberband_delete
    -- Debug.traceM "set_expected" totalFrames
    liftIO $ RubberbandC.rubberband_set_expected_input_duration state
        totalFrames
    -- Debug.traceM "study" ()
    liftIO $ incrementPtrs inputs 0 $ \inputpp ->
        RubberbandC.rubberband_study state inputpp totalFrames True
    Control.loop1 0 (process key state inputs)
    where
    process key state inputs loop now = do
        required <- liftIO $ RubberbandC.rubberband_get_samples_required state
        let left = totalFrames - now
        -- Debug.traceM "required" (now, required, left)
        when (required > 0 && left > 0) $ liftIO $ do
            incrementPtrs inputs (fromIntegral now) $ \inputpp ->
                RubberbandC.rubberband_process state inputpp
                    (min required left)
                    (now + required >= totalFrames)
            -- Debug.traceM "process"
            --     (now, min required left, now + required >= totalFrames)
        available <- liftIO $ RubberbandC.rubberband_available state
        -- Debug.traceM "available" available
        if available == -1
            then lift (Resource.release key)
            else do
                S.yield =<< liftIO (retrieve state available)
                loop $ now + min required left
    retrieve state available = do
        outputsps <- replicateM chan (Foreign.mallocArray available)
        Foreign.withArray outputsps $ \outputspp -> do
            retrieved <- RubberbandC.rubberband_retrieve state outputspp
                (fromIntegral available)
            outFPs <- mapM (Foreign.newForeignPtr Foreign.finalizerFree)
                outputsps
            return $ map
                (flip V.unsafeFromForeignPtr0 (fromIntegral retrieved))
                outFPs

    totalFrames = fromIntegral (V.length input0)
    new = RubberbandC.rubberband_new (fromIntegral srate) chan
        (ProcessOffline : _options config) (_timeRatio config)
        (_pitchRatio config)

-- | Convert vectors to ptrs, incremented by some index.
incrementPtrs :: Foreign.Storable val => [V.Vector val] -> Int
    -> (Ptr (Ptr val) -> IO a) -> IO a
incrementPtrs vs increment action = Foreign.withMany V.unsafeWith vs $ \vps ->
    Foreign.withArray (map (flip Foreign.advancePtr increment) vps) action


-- * old

-- I have to collect the whole input, but then I can stream the output.
-- So I could output as a callback, or I could just return a stream.
offlineList :: Audio.Rate -> Config -> [Samples] -> IO [[Samples]]
offlineList _ _ [] = return []
offlineList srate config inputs =
    Exception.bracket new RubberbandC.rubberband_delete $ \state -> do
        RubberbandC.rubberband_set_expected_input_duration state totalFrames
        Foreign.withMany V.unsafeWith inputs $ \inputps -> do
            incrementPtrs2 inputps 0 $ \inputpp ->
                RubberbandC.rubberband_study state inputpp totalFrames True
            Control.loop2 [] 0 (process state inputps)
    where
    process state inputps loop accum now = do
        wanted <- RubberbandC.rubberband_get_samples_required state
        unless (wanted == 0) $
            incrementPtrs2 inputps (fromIntegral now) $ \inputpp ->
                RubberbandC.rubberband_process state inputpp wanted
                    (now + wanted >= totalFrames)
        available <- RubberbandC.rubberband_available state
        if available == -1 then return (reverse accum) else do
            outputsps <- replicateM chan (Foreign.mallocArray available)
            (outputs, retrieved)<-Foreign.withArray outputsps $ \outputspp -> do
                retrieved <- RubberbandC.rubberband_retrieve state outputspp
                    (fromIntegral available)
                outFPs <- mapM (Foreign.newForeignPtr Foreign.finalizerFree)
                    outputsps
                let outputs = map
                        (flip V.unsafeFromForeignPtr0 (fromIntegral retrieved))
                        outFPs
                return (outputs, retrieved)
            loop (outputs:accum) (now + retrieved)

    totalFrames = fromIntegral (V.length (head inputs))
    new = RubberbandC.rubberband_new (fromIntegral srate) chan
        (ProcessOffline : _options config) (_timeRatio config)
        (_pitchRatio config)
    chan = length inputs

incrementPtrs2 :: Foreign.Storable val => [Ptr val] -> Int
    -> (Ptr (Ptr val) -> IO a) -> IO a
incrementPtrs2 vps increment action = do
    Foreign.withArray (map (flip Foreign.advancePtr increment) vps) action
