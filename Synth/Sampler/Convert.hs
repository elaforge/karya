-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE OverloadedStrings, DisambiguateRecordFields #-}
-- | Convert 'Note.Note's to 'Sample.Sample's.
module Synth.Sampler.Convert where
import qualified Data.List as List
import qualified Data.Map as Map
import System.FilePath ((</>))

import qualified Util.Seq as Seq
import qualified Perform.Pitch as Pitch
import Global
import qualified Synth.Sampler.Instrument as Instrument
import qualified Synth.Sampler.InstrumentDb as InstrumentDb
import qualified Synth.Sampler.Note as Note
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Sampler.Signal as Signal


noteToSample :: Note.Note -> Either Text Sample.Sample
noteToSample note@(Note.Note instName start controls attr) = do
    inst <- maybe (Left $ "instrument not found: " <> instName) Right $
        Map.lookup instName InstrumentDb.db
    let msg = "sample not found for " <> showt (instName, attr)
            <> " with pitch " <> showt (Note.initialPitch note)
    (samplePath, instSample) <- maybe (Left msg) Right $
        lookupSample inst attr (Note.initialPitch note)
    let get k = Map.lookup k controls
    return $ Sample.Sample
        { start = start
        , filename = Instrument.sampleDirectory inst </> samplePath
        , offset = 0
        , envelope = fromMaybe (Signal.constant 1) $ get Note.envelope
        , ratio = case (Instrument.pitch instSample, get Note.pitch) of
            (Just sampleNn, Just noteNns) ->
                Signal.mapY (pitchToRatio (Pitch.nn_to_hz sampleNn) . Pitch.nn)
                    noteNns
            _ -> Signal.constant 1
        }

-- | Find the sample with the closest pitch, or if there is no pitch, the first
-- unpitched sample.
lookupSample :: Instrument.Instrument -> Instrument.Attribute
    -> Maybe Pitch.NoteNumber -> Maybe (FilePath, Instrument.Sample)
lookupSample inst attr maybePitch = case maybePitch of
    Nothing -> List.find ((==Nothing) . Instrument.pitch . snd) samples
    Just pitch -> fmap snd $ Seq.minimum_on (abs . subtract pitch . fst) $
        keyOnMaybe (Instrument.pitch . snd) samples
    where
    samples = filter ((==attr) . Instrument.attribute . snd) $ Map.toList $
        Instrument.samples inst

keyOnMaybe :: (a -> Maybe k) -> [a] -> [(k, a)]
keyOnMaybe f xs = [(k, a) | (Just k, a) <- zip (map f xs) xs]

pitchToRatio :: Pitch.Hz -> Pitch.NoteNumber -> Signal.Y
pitchToRatio sampleHz nn = sampleHz / Pitch.nn_to_hz nn -- / sampleHz

-- When I go up *2, I should be skipping every other sample.  So srate should
-- be *2.  Number of frames is /2.
