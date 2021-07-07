-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
-- | This is the actual midi implementation.  This is the only module that
-- should depend on the implementation, so switching backends is relatively
-- easy.
module Midi.MidiDriver (module MidiDriver) where

#include "hsconfig.h"

#if defined(CORE_MIDI)
import Midi.CoreMidi as MidiDriver
#elif defined(JACK_MIDI)
import Midi.JackMidi as MidiDriver
#else
import Midi.StubMidi as MidiDriver
#endif
