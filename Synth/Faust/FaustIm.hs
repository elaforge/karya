-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Offline synthesizer that uses FAUST.
module Synth.Faust.FaustIm (main) where
import qualified Data.Map as Map
import qualified System.Environment as Environment

import qualified Synth.Shared.Note as Note

import qualified Synth.Faust.DriverC as DriverC

import Global


main :: IO ()
main = do
    patches <- DriverC.getPatches
    forM_ (Map.toList patches) $ \(name, patch) -> do
        print name
        print =<< DriverC.getControls patch
        print =<< DriverC.getUiControls patch
    -- args <- Environment.getArgs
    -- case args of
    --     [notesJson] -> process Config.cache =<< loadJson notesJson
    --     [] -> process Config.cache
    --         =<< loadBinary (Config.notes Config.defaultConfig)
    --     _ -> errorIO $ "usage: faust_driver notes.json"

{-

. Faust has a bunch of *.dsp files.
. shakefile runs faust to generate .cc from the result, and then collect into
  a map of (name, *dsp)
. Export a Load function that introspects all of those for the inst db.
  If there is karya code, it gets attached by name just like Sampler.
. faust_driver loads Notes, and maps PatchName to dsp pointers.
. Based on patch config (polyphonic, strings, etc.), allocate a certain number
  of dsps by cloning them, and allocate Notes among them.
. Call compute dsp startFrame endFrame controlsp outputp for each time range,
  and mix the output into a wav.

How does FAUST handle global things like a reverb bus?  I guess I have to pipe
around the samples myself.  But what if you have multiple strings with
resonance?  I guess it has to have inputs for each one, as one giant list of
input channels.  But since they're all interleaved samples, I can't just leave
some empty, or not easily at least.

Where does the state go?  I.e. how do I save and restore energy in strings,
etc?

-}

-- process :: FilePath -> [Note.Note] -> IO ()
-- process outputDir notes = do
--     samples <- either errorIO return $ mapM Convert.noteToSample notes
--     mapM_ print samples
--     realizeSamples outputDir samples


