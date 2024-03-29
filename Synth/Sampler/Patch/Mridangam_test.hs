-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Sampler.Patch.Mridangam_test where
import qualified Data.Maybe as Maybe

import qualified Util.Log as Log
import qualified Util.Lists as Lists
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Derive.Stream as Stream

import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.Patch.Mridangam as Mridangam
import qualified Synth.Sampler.PatchDb as PatchDb
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Note as Note

import qualified Ui.UiTest as UiTest

import Global
import Util.Test


test_variations :: Test
test_variations = do
    let run title = runNotes title
        -- extract = Sample.filename . snd
    let histo = map (second length) . Lists.keyedGroupSort id
    -- prettyp (run "dyn=.75" [(0, 0, "k")])
    -- A normal-ish curve, centering around 95.25.
    equal (first (histo . map Sample.filename) $
            run "dyn=.75" [(t, 0, "k") | t <- Lists.range 0 20 1])
        ( [ ("Ki/36-42-47-89-91-ki.flac", 2)
          , ("Ki/36-42-47-92-94-ki.flac", 8)
          , ("Ki/36-42-47-95-97-ki.flac", 6)
          , ("Ki/36-42-47-98-100-ki.flac", 5)
          ]
        , []
        )

runNotes :: Text -> [UiTest.EventSpec] -> ([Sample.Sample], [Text])
runNotes title events = (samples, logs ++ convert_logs)
    where
    (notes, logs) = derive title [(">m", events)]
    (samples, convert_logs) = convert patch notes
    Just patch = Patch.lookupPatch "mridangam-d" Mridangam.patches

convert :: Patch.Patch -> [Note.Note] -> ([Sample.Sample], [Text])
convert patch notes = (Maybe.catMaybes samples, concat logs)
    where
    (samples, logs) = unzip (map convert1 (Patch._preprocess patch notes))
    convert1 note = case Patch.runConvert (Patch._convert patch note) of
        Left err -> (Nothing, [err])
        Right (sample, logs) -> (Just sample, map Log.msg_text logs)

derive :: Text -> [UiTest.TrackSpec] -> ([Note.Note], [Text])
derive title = perform allocs . Derive.r_events
    . DeriveTest.derive_tracks_setup (withSynth allocs) title
    where
    allocs = [("m", "sampler/mridangam-d")]

withSynth :: DeriveTest.SimpleAllocations -> DeriveTest.Setup
withSynth allocs = DeriveTest.with_synths_im allocs [PatchDb.synth]

perform :: DeriveTest.SimpleAllocations -> Stream.Stream Score.Event
    -> ([Note.Note], [Text])
perform allocs = DeriveTest.perform_im_synths allocs [PatchDb.synth]
