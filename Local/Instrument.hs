-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Load the instrument db.  This collects together all the local instrument
    definitions.

    The convention is that each synthesizer has a module in Local\/Instrument\/,
    and each one exports:

    > synth :: MidiInst.Synth

    If loading is expensive, then it also exports:

    > synth_name :: InstTypes.SynthName
    > make_db :: MidiInst.MakeDb
    > load :: MidiInst.Load
-}
module Local.Instrument (midi_synths, all_loads) where
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Instrument.InstTypes as InstTypes
import qualified Local.Instrument.Derailer as Derailer
import qualified Local.Instrument.Drumaxx as Drumaxx
import qualified Local.Instrument.Fm8 as Fm8
import qualified Local.Instrument.Kontakt as Kontakt
import qualified Local.Instrument.Massive as Massive
import qualified Local.Instrument.Morpheus as Morpheus
import qualified Local.Instrument.Morphine as Morphine
import qualified Local.Instrument.Pianoteq as Pianoteq
import qualified Local.Instrument.Reaktor as Reaktor
import qualified Local.Instrument.Spicy as Spicy
import qualified Local.Instrument.Tassman as Tassman
import qualified Local.Instrument.Vl1 as Vl1
import qualified Local.Instrument.Vsl as Vsl
import qualified Local.Instrument.Z1 as Z1


-- | Synth declarations for each synth that is declared purely.
midi_synths :: [MidiInst.Synth]
midi_synths =
    [ Derailer.synth, Drumaxx.synth, Fm8.synth, Kontakt.synth, Massive.synth
    , Morphine.synth, Pianoteq.synth, Reaktor.synth, Spicy.synth, Tassman.synth
    , Vsl.synth
    ]

-- | Each synth that caches to disk has a function to make the cache, and one
-- to load it.
all_loads :: [(InstTypes.SynthName, (MidiInst.MakeDb, MidiInst.Load))]
all_loads =
    [ (Morpheus.synth_name, (Morpheus.make_db, Morpheus.load))
    , (Vl1.synth_name, (Vl1.make_db, Vl1.load))
    , (Z1.synth_name, (Z1.make_db, Z1.load))
    ]
