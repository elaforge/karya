-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Convert 'Note.Note's to 'Sample.Sample's.
module Synth.Sampler.Convert where
import qualified Data.List as List
import qualified Data.Map as Map
import System.FilePath ((</>))

import qualified Util.Seq as Seq
import qualified Derive.Attrs as Attrs
import qualified Perform.Pitch as Pitch
import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import Synth.Types
import Global


-- TODO use dur for an envelope
-- TODO old
noteToSample :: Patch.Db -> Note.Note -> Either Text (RealTime, Sample.Sample)
noteToSample db note = do
    let patch = Note.patch note
    -- TODO I think the sampler doesn't care about individual instruments?
    inst <- justErr ("patch not found: " <> patch) $
        Map.lookup patch (Patch._patches db)
    let msg = "sample not found for " <> showt (patch, Note.attributes note)
            <> " with pitch " <> showt (Note.initialPitch note)
    (samplePath, instSample) <- justErr msg $
        lookupSample inst (Note.attributes note) (Note.initialPitch note)
    return $ (Note.start note,) $ makeSample
        (Patch._rootDir db </> Patch.sampleDirectory inst </> samplePath)
        instSample note

-- | Find the sample with the closest pitch, or if there is no pitch, the first
-- unpitched sample.
lookupSample :: Patch.Patch -> Attrs.Attributes
    -> Maybe Pitch.NoteNumber -> Maybe (FilePath, Patch.Sample)
lookupSample inst attrs maybePitch = case maybePitch of
    Nothing -> List.find ((==Nothing) . Patch.pitch . snd) samples
    Just pitch -> fmap snd $ Seq.minimum_on (abs . subtract pitch . fst) $
        Seq.key_on_just (Patch.pitch . snd) samples
    where
    samples = filter ((==attrs) . Patch.attributes . snd) $ Map.toList $
        Patch.samples inst

makeSample :: Sample.SamplePath -> Patch.Sample -> Note.Note -> Sample.Sample
makeSample filename instSample note = Sample.Sample
    { filename = filename
    , offset = 0
    , envelope = fromMaybe (Signal.constant 1) $ get Control.dynamic
    , ratio = case (Patch.pitch instSample, get Control.pitch) of
        (Just sampleNn, Just noteNns) ->
            -- Converting to a ratio is nonlinear, so I have to resample.
            -- To avoid that I think libsamplerate would have to take
            -- source and destination hz and convert to ratio internally
            -- per-sample.
            Signal.map_y 0.05
                (pitchToRatio (Pitch.nn_to_hz sampleNn) . Pitch.nn)
                noteNns
        _ -> Signal.constant 1
    }
    where
    get k = Map.lookup k (Note.controls note)

pitchToRatio :: Pitch.Hz -> Pitch.NoteNumber -> Signal.Y
pitchToRatio sampleHz nn = sampleHz / Pitch.nn_to_hz nn

-- When I go up *2, I should be skipping every other sample.  So srate should
-- be *2.  Number of frames is /2.  Ratio is 0.5.
