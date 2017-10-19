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
import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.PatchDb as PatchDb
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Types as Types
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import Global


-- TODO use dur for an envelope
noteToSample :: Note.Note -> Either Text Sample.Sample
noteToSample note@(Note.Note _instrument patch start _dur controls attrs) = do
    -- TODO I think the sampler doesn't care about individual instruments?
    inst <- justErr ("patch not found: " <> patch) $ Map.lookup patch PatchDb.db
    let msg = "sample not found for " <> showt (patch, attrs)
            <> " with pitch " <> showt (Note.initialPitch note)
    (samplePath, instSample) <- justErr msg $
        lookupSample inst attrs (Note.initialPitch note)
    let get k = Map.lookup k controls
    return $ Sample.Sample
        { start = start
        , filename = Patch.sampleDirectory inst </> samplePath
        , offset = 0
        , envelope = fromMaybe (Signal.constant 1) $ get Control.amp
        , ratio = case (Patch.pitch instSample, get Control.pitch) of
            (Just sampleNn, Just noteNns) ->
                Signal.map_y (pitchToRatio (Pitch.nn_to_hz sampleNn) . Pitch.nn)
                    noteNns
            _ -> Signal.constant 1
        }

-- | Find the sample with the closest pitch, or if there is no pitch, the first
-- unpitched sample.
lookupSample :: Patch.Patch -> Types.Attributes
    -> Maybe Pitch.NoteNumber -> Maybe (FilePath, Patch.Sample)
lookupSample inst attrs maybePitch = case maybePitch of
    Nothing -> List.find ((==Nothing) . Patch.pitch . snd) samples
    Just pitch -> fmap snd $ Seq.minimum_on (abs . subtract pitch . fst) $
        keyOnMaybe (Patch.pitch . snd) samples
    where
    samples = filter ((==attrs) . Patch.attributes . snd) $ Map.toList $
        Patch.samples inst

keyOnMaybe :: (a -> Maybe k) -> [a] -> [(k, a)]
keyOnMaybe f xs = [(k, a) | (Just k, a) <- zip (map f xs) xs]

pitchToRatio :: Pitch.Hz -> Pitch.NoteNumber -> Signal.Y
pitchToRatio sampleHz nn = sampleHz / Pitch.nn_to_hz nn -- / sampleHz

-- When I go up *2, I should be skipping every other sample.  So srate should
-- be *2.  Number of frames is /2.
