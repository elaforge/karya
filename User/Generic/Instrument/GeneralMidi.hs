-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module User.Generic.Instrument.GeneralMidi where
import qualified Data.Text as Text

import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Cmd.Instrument.MidiInst as MidiInst

import qualified Derive.Attrs as Attrs
import qualified Derive.Expr as Expr
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Patch as Patch

import           Global


synth :: MidiInst.Synth
synth =
    MidiInst.synth "gm" "Yes it's General MIDI." $
        MidiInst.synth_controls controls $
        MidiInst.default_patch pb_range [] : patches
    where
    controls = []

-- Who knows really, but fluidsynth seems to hardcode this.
pb_range :: (Int, Int)
pb_range = (-2, 2)

patches :: [MidiInst.Patch]
patches = percussion : map (uncurry make_patch) (zip [0..] patch_names)

percussion :: MidiInst.Patch
percussion =
    MidiInst.doc #= "This must be allocated on channel 10, GM says so." $
    CUtil.simple_drum CUtil.MidiThru Nothing stroke_keys $
    MidiInst.named_patch pb_range "percussion" []

make_patch :: Midi.Program -> Text -> MidiInst.Patch
make_patch pgm name = set_pgm pgm $ MidiInst.named_patch pb_range name []

set_pgm :: Midi.Program -> MidiInst.Patch -> MidiInst.Patch
set_pgm pgm = MidiInst.patch#Patch.initialize
    #= Patch.initialize_midi [Midi.ProgramChange pgm]

clean :: Text -> Text
clean = Text.replace ")" "" . Text.replace "(" "" . Text.replace " " "-"
    . Text.toLower

-- Copied and pasted from wikipedia.

patch_names :: [Text]
patch_names = map clean
    -- Piano
    [ "Acoustic Grand Piano"
    , "Bright Acoustic Piano"
    , "Electric Grand Piano"
    , "Honky-tonk Piano"
    , "Electric Piano 1"
    , "Electric Piano 2"
    , "Harpsichord"
    , "Clavi"
    -- Chromatic Percussion
    , "Celesta"
    , "Glockenspiel"
    , "Music Box"
    , "Vibraphone"
    , "Marimba"
    , "Xylophone"
    , "Tubular Bells"
    , "Dulcimer"
    -- Organ
    , "Drawbar Organ"
    , "Percussive Organ"
    , "Rock Organ"
    , "Church Organ"
    , "Reed Organ"
    , "Accordion"
    , "Harmonica"
    , "Tango Accordion"
    -- Guitar
    , "Acoustic Guitar (nylon)"
    , "Acoustic Guitar (steel)"
    , "Electric Guitar (jazz)"
    , "Electric Guitar (clean)"
    , "Electric Guitar (muted)"
    , "Overdriven Guitar"
    , "Distortion Guitar"
    , "Guitar Harmonics"
    -- Bass
    , "Acoustic Bass"
    , "Electric Bass (finger)"
    , "Electric Bass (pick)"
    , "Fretless Bass"
    , "Slap Bass 1"
    , "Slap Bass 2"
    , "Synth Bass 1"
    , "Synth Bass 2"
    -- Strings
    , "Violin"
    , "Viola"
    , "Cello"
    , "Contrabass"
    , "Tremolo Strings"
    , "Pizzicato Strings"
    , "Orchestral Harp"
    , "Timpani"
    -- Ensemble
    , "String Ensemble 1"
    , "String Ensemble 2"
    , "Synth Strings 1"
    , "Synth Strings 2"
    , "Choir Aahs"
    , "Voice Oohs"
    , "Synth Voice"
    , "Orchestra Hit"
    -- Brass
    , "Trumpet"
    , "Trombone"
    , "Tuba"
    , "Muted Trumpet"
    , "French Horn"
    , "Brass Section"
    , "Synth Brass 1"
    , "Synth Brass 2"
    -- Reed
    , "Soprano Sax"
    , "Alto Sax"
    , "Tenor Sax"
    , "Baritone Sax"
    , "Oboe"
    , "English Horn"
    , "Bassoon"
    , "Clarinet"
    -- Pipe
    , "Piccolo"
    , "Flute"
    , "Recorder"
    , "Pan Flute"
    , "Blown bottle"
    , "Shakuhachi"
    , "Whistle"
    , "Ocarina"
    -- Synth Lead
    , "Lead 1 (square)"
    , "Lead 2 (sawtooth)"
    , "Lead 3 (calliope)"
    , "Lead 4 (chiff)"
    , "Lead 5 (charang)"
    , "Lead 6 (voice)"
    , "Lead 7 (fifths)"
    , "Lead 8 (bass + lead)"
    -- Synth Pad
    , "Pad 1 (new age)"
    , "Pad 2 (warm)"
    , "Pad 3 (polysynth)"
    , "Pad 4 (choir)"
    , "Pad 5 (bowed)"
    , "Pad 6 (metallic)"
    , "Pad 7 (halo)"
    , "Pad 8 (sweep)"
    -- Synth Effects
    , "FX 1 (rain)"
    , "FX 2 (soundtrack)"
    , "FX 3 (crystal)"
    , "FX 4 (atmosphere)"
    , "FX 5 (brightness)"
    , "FX 6 (goblins)"
    , "FX 7 (echoes)"
    , "FX 8 (sci-fi)"
    -- Ethnic
    , "Sitar"
    , "Banjo"
    , "Shamisen"
    , "Koto"
    , "Kalimba"
    , "Bag pipe"
    , "Fiddle"
    , "Shanai"
    -- Percussive
    , "Tinkle Bell"
    , "Agogo"
    , "Steel Drums"
    , "Woodblock"
    , "Taiko Drum"
    , "Melodic Tom"
    , "Synth Drum"
    , "Reverse Cymbal"
    -- Sound Effects
    , "Guitar Fret Noise"
    , "Breath Noise"
    , "Seashore"
    , "Bird Tweet"
    , "Telephone Ring"
    , "Helicopter"
    , "Applause"
    , "Gunshot"
    ]

stroke_keys :: [(Drums.Stroke, Midi.Key)]
stroke_keys = map make
    -- Clearly more of these could be mapped.  I'll do it someday if I need it.
    [ (27, "High Q",            Nothing)
    , (28, "Slap",              Nothing)
    , (29, "Scratch Push",      Nothing)
    , (30, "Scratch Pull",      Nothing)
    , (31, "Sticks",            Nothing)
    , (32, "Square Click",      Nothing)
    , (33, "Metronome Click",   Nothing)
    , (34, "Metronome Bell",    Nothing)
    , (35, "Acoustic Bass Drum", Just Drums.c_bd)
    , (36, "Electric Bass Drum", Just Drums.c_bd2)
    , (37, "Side Stick",        Nothing)
    , (38, "Acoustic Snare",    Just Drums.c_sn)
    , (39, "Hand Clap",         Nothing)
    , (40, "Electric Snare",    Just Drums.c_sn2)
    , (41, "Low Floor Tom",     Nothing)
    , (42, "Closed Hi-hat",     Just Drums.c_hh)
    , (43, "High Floor Tom",    Nothing)
    , (44, "Pedal Hi-hat",      Just Drums.c_phh)
    , (45, "Low Tom",           Just Drums.c_ltom)
    , (46, "Open Hi-hat",       Just Drums.c_ohh)
    , (47, "Low-Mid Tom",       Just Drums.c_mtom)
    , (48, "Hi-Mid Tom",        Just Drums.c_hmtom)
    , (49, "Crash Cymbal 1",    Just Drums.c_crash)
    , (50, "High Tom",          Just Drums.c_htom)
    , (51, "Ride Cymbal 1",     Just Drums.c_ride)
    , (52, "Chinese Cymbal",    Nothing)
    , (53, "Ride Bell",         Just Drums.c_bell)
    , (54, "Tambourine",        Nothing)
    , (55, "Splash Cymbal",     Nothing)
    , (56, "Cowbell",           Nothing)
    , (57, "Crash Cymbal 2",    Nothing)
    , (58, "Vibra Slap",        Nothing)
    , (59, "Ride Cymbal 2",     Nothing)
    , (60, "High Bongo",        Nothing)
    , (61, "Low Bongo",         Nothing)
    , (62, "Mute High Conga",   Nothing)
    , (63, "Open High Conga",   Nothing)
    , (64, "Low Conga",         Nothing)
    , (65, "High Timbale",      Nothing)
    , (66, "Low Timbale",       Nothing)
    , (67, "High Agogo",        Nothing)
    , (68, "Low Agogo",         Nothing)
    , (69, "Cabasa",            Nothing)
    , (70, "Maracas",           Nothing)
    , (71, "Short Whistle",     Nothing)
    , (72, "Long Whistle",      Nothing)
    , (73, "Short Guiro",       Nothing)
    , (74, "Long Guiro",        Nothing)
    , (75, "Claves",            Nothing)
    , (76, "High Woodblock",    Nothing)
    , (77, "Low Woodblock",     Nothing)
    , (78, "Mute Cuica",        Nothing)
    , (79, "Open Cuica",        Nothing)
    , (80, "Mute Triangle",     Nothing)
    , (81, "Open Triangle",     Nothing)
    , (82, "Shaker",            Nothing)
    , (83, "Jingle Bell",       Nothing)
    , (84, "Belltree",          Nothing)
    , (85, "Castanets",         Nothing)
    , (86, "Mute Surdo",        Nothing)
    , (87, "Open Surdo",        Nothing)
    ]
    where
    make (key, name, mb_stroke) = (fromMaybe (generic name) mb_stroke, key)
    -- Make a generic unbound stroke.
    generic name = Drums.stroke ' ' (Expr.Symbol name) (Attrs.attr name)
