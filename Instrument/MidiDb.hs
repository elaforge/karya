{- | The MidiDb type.  Split from Instrument.Db to avoid circular imports.
-}
module Instrument.MidiDb where
import qualified Data.Char as Char
import qualified Data.Map as Map

import qualified Perform.Midi.Instrument as Instrument


data MidiDb = MidiDb
    (Map.Map Instrument.SynthName (Instrument.Synth, SynthPatches))
    deriving (Show)

-- | Merge the MidiDbs, favoring instruments in the leftmost one.
merge :: MidiDb -> MidiDb -> MidiDb
merge (MidiDb db0) (MidiDb db1) = MidiDb $ Map.unionWith merge_synth db0 db1
    where
    merge_synth (synth, pmap0) (_, pmap1) = (synth, merge_patches pmap0 pmap1)
    merge_patches (PatchMap ps0) (PatchMap ps1) = PatchMap (Map.union ps0 ps1)
    merge_patches a _ = a

midi_db :: [SynthDesc] -> MidiDb
midi_db synth_map = MidiDb $ Map.fromList
    [ (lc (Instrument.synth_name synth), (synth, patches))
    | (synth, patches) <- synth_map]

size :: MidiDb -> Int
size (MidiDb synths) = sum $ map ssize (Map.elems synths)
    where
    ssize (_, PatchTemplate _) = 1
    ssize (_, PatchMap patches) = Map.size patches

empty = MidiDb Map.empty

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
    [(lc (Instrument.inst_name (Instrument.patch_instrument p)), p)
    | p <- patches]

-- | Since instruments are stored in the index as lower case for case
-- insensitive lookup, they should be stored as lower case here too.
--
-- TODO: having two different structures for lookup seems messy, it would be
-- nicer to put it in the index flat, but not clear how to best to that with
-- PatchTemplates.
lc :: String -> String
lc = map Char.toLower
