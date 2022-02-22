-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Load the instrument db.  This collects together all the local instrument
    definitions.

    The convention is that each synthesizer has a module in Local\/Instrument\/,
    and each one exports:

    > synth :: MidiInst.Synth

    If loading is expensive, then it also exports:

    > synth_name :: InstT.SynthName
    > make_db :: MidiInst.MakeDb
    > load :: MidiInst.Load
-}
module User.Elaforge.Instrument (midi_synths, all_loads) where
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Instrument.InstT as InstT
import qualified User.Elaforge.Instrument.Derailer as Derailer
import qualified User.Elaforge.Instrument.Drumaxx as Drumaxx
import qualified User.Elaforge.Instrument.Fm8 as Fm8
import qualified User.Elaforge.Instrument.Kontakt as Kontakt
import qualified User.Elaforge.Instrument.Massive as Massive
import qualified User.Elaforge.Instrument.Morpheus as Morpheus
import qualified User.Elaforge.Instrument.Morphine as Morphine
import qualified User.Elaforge.Instrument.Pianoteq as Pianoteq
import qualified User.Elaforge.Instrument.Reaktor as Reaktor
import qualified User.Elaforge.Instrument.Spicy as Spicy
import qualified User.Elaforge.Instrument.Swam as Swam
import qualified User.Elaforge.Instrument.Tassman as Tassman
import qualified User.Elaforge.Instrument.Vl1 as Vl1
import qualified User.Elaforge.Instrument.Vsl as Vsl
import qualified User.Elaforge.Instrument.Z1 as Z1
import qualified User.Empty.Instrument.GeneralMidi as GeneralMidi
import qualified User.Empty.Instrument.Surge as Surge


-- | Synth declarations for each synth that is declared purely.
midi_synths :: [MidiInst.Synth]
midi_synths =
    [ Derailer.synth
    , Drumaxx.synth
    , Fm8.synth
    , GeneralMidi.synth
    , Kontakt.synth
    , Massive.synth
    , Morphine.synth
    , Pianoteq.synth
    , Reaktor.synth
    , Spicy.synth
    , Surge.synth
    , Swam.synth
    , Tassman.synth
    , Vsl.synth
    ]

-- | Each synth that caches to disk has a function to make the cache, and one
-- to load it.
all_loads :: [(InstT.SynthName, (MidiInst.MakeDb, MidiInst.Load))]
all_loads =
    [ (Morpheus.synth_name, (Morpheus.make_db, Morpheus.load))
    , (Vl1.synth_name, (Vl1.make_db, Vl1.load))
    , (Z1.synth_name, (Z1.make_db, Z1.load))
    ]
