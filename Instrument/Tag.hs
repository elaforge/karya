-- | Tags are used to index patches.  They're just strings and their meaning
-- is convention, but there are some with standardized meanings.
--
-- Any attribute of a patch that should be searchable should be given a tag.
module Instrument.Tag where
import Data.Text (Text)
import Perform.Midi.Instrument (TagKey)


category :: TagKey
category = "category"

-- | One control tag is given for each control signal the instrument
-- supports.  Automatically added.
control :: TagKey
control = "control"

-- | Patch was loaded from this file.
file :: TagKey
file = "file"

-- | Instrument name.  Automatically added.
name :: TagKey
name = "name"

-- | Synth name.  Automatically added.
synth :: TagKey
synth = "synth"

-- | Indicates that the instrument has a sysex message as its midi
-- initialization, which probably means it's not built in to the synth.
sysex :: TagKey
sysex = "sysex"


-- * categories

c_strings, c_woodwinds, c_brass, c_percussion, c_synth :: Text
c_strings = "strings"
c_woodwinds = "woodwinds"
c_brass = "brass"
c_percussion = "percussion"
c_synth = "synth"
