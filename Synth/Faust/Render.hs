-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DataKinds #-}
-- | Render FAUST instruments.
module Synth.Faust.Render where
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.Map as Map
import qualified Data.Vector.Storable as V
import qualified Streaming.Prelude as S

import qualified Util.Audio.Audio as Audio
import qualified Util.CallStack as CallStack
import qualified Util.Seq as Seq

import qualified Synth.Faust.Convert as Convert
import qualified Synth.Faust.DriverC as DriverC
import qualified Synth.Lib.AUtil as AUtil
import Synth.Lib.Global
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import Global


-- | Render a FAUST instrument incrementally.
--
-- Chunk size is determined by the size of the 'NAudio' chunks, or
-- Audio.chunkSize if they're empty or run out.  The inputs will go to zero
-- if they end before the given time.
render :: DriverC.Patch -> NAudio -> RealTime -- ^ logical end time
    -> RealTime
    -- ^ max decay, force an end if the signal hasn't gone to zero before this
    -> NAudio
render patch inputs end decay = Audio.NAudio (DriverC.patchOutputs patch) $ do
    (key, inst) <- lift $
        Resource.allocate (DriverC.initialize patch) DriverC.destroy
    let nstream = Audio._nstream (Audio.zeroPadN inputs)
    Audio.loop1 (0, nstream) $ \loop (start, inputs) -> do
        -- Audio.zeroPadN should have made this infinite.
        (controls, inputs) <-
            maybe (CallStack.errorIO "end of endless stream") return
                =<< lift (S.uncons inputs)
        result <- render1 inst controls start inputs
        case result of
            Nothing -> Resource.release key
            Just (start, inputs) -> loop (start, inputs)
    where
    render1 inst controls start inputs = do
        outputs <- liftIO $ DriverC.render2 inst controls
        S.yield outputs
        return $ case outputs of
            [] -> Nothing
            output : _
                | frames == 0 || blockEnd >= final + maxDecay
                        || blockEnd >= final && isBasicallySilent output ->
                    Nothing
                | otherwise -> Just (blockEnd, inputs)
                where
                blockEnd = start + frames
                frames = Audio.Frame $ V.length output
    final = AUtil.toFrames end
    maxDecay = AUtil.toFrames decay

isBasicallySilent :: V.Vector Audio.Sample -> Bool
isBasicallySilent _samples = False -- TODO RMS < -n dB

-- TODO old render, remove when 'render' is done
renderInstrument :: DriverC.Patch -> [Note.Note] -> AUtil.Audio
renderInstrument patch notes = Audio.Audio $ do
    (key, inst) <- lift $
        Resource.allocate (DriverC.initialize patch) DriverC.destroy
    supported <- liftIO $ DriverC.getControls patch
    let gate = if Control.gate `elem` supported
            then Map.insert Control.gate (makeGate notes) else id
    let controls = gate $ mergeControls supported notes
    Convert.controls supported controls $ \controlLengths ->
        Audio.loop1 0 $ \loop start -> do
            let end = min final (start + Audio.chunkSize)
            buffers <- liftIO $ DriverC.render inst start end controlLengths
            case buffers of
                [left, right] -> S.yield $ Audio.interleave [left, right]
                [center] -> S.yield $ Audio.interleave [center, center]
                -- This should have been verified by DriverC.getParsedMetadata.
                _ -> errorIO $ "expected 1 or 2 outputs, but got "
                    <> showt (length buffers)
            if end >= final
                then Resource.release key >> return ()
                else loop end
    where
    final = maybe 0 (AUtil.toFrames . (+decay) . Note.end) (Seq.last notes)
    decay = 2
    -- TODO how can I figure out how long decay actually is?
    -- I probably have to keep rendering until the samples stay below
    -- a threshold.

-- | This makes a sawtooth that goes to 1 on every note start.  This is
-- suitable for percussion, but maybe continuious instruments expect a constant
-- 1 for as long as the note is sustained?
makeGate :: [Note.Note] -> Signal.Signal
makeGate notes = Signal.from_pairs $
    concat [[(Note.start n, 0), (Note.start n, 1)]  | n <- notes]

mergeControls :: [Control.Control] -> [Note.Note]
    -> Map Control.Control Signal.Signal
mergeControls supported notes =
    Map.fromList $ filter (not . Signal.null . snd) $ map merge supported
    where
    merge control = (control,) $ mconcat
        [ Signal.clip_before (Note.start n) signal
        | n <- notes
        , signal <- maybe [] (:[]) $ Map.lookup control (Note.controls n)
        ]
