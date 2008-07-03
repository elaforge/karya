{- | The MidiDb type.  Split apart to avoid circular imports.
-}
module Instrument.MidiDb where
import qualified Data.Map as Map

import qualified Perform.Midi.Instrument as Instrument


data MidiDb = MidiDb
    (Map.Map Instrument.SynthName (Instrument.Synth, SynthPatches))
    deriving (Show)

midi_db :: [SynthDesc] -> MidiDb
midi_db synth_map = MidiDb $ Map.fromList
    [ (Instrument.synth_name synth, (synth, patches))
    | (synth, patches) <- synth_map]

type SynthDesc = (Instrument.Synth, SynthPatches)

data SynthPatches =
    -- | This synth has whatever patch you name, generated from the template.
    -- This usually means it can't be queried automatically and can be "".
    -- The name will be replaced with the queried name.
    PatchTemplate Instrument.Patch
    | PatchMap (Map.Map Instrument.InstrumentName Instrument.Patch)
    deriving (Show)

patch_map :: [Instrument.Patch] -> SynthPatches
patch_map patches = PatchMap $ Map.fromList
    [(Instrument.inst_name (Instrument.patch_instrument p), p) | p <- patches]
