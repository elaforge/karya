-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Tags are used to index patches.  They're just strings and their meaning
-- is convention, but there are some with standardized meanings.
--
-- Any attribute of a patch that should be searchable should be given a tag.
module Instrument.Tag where
import Data.Text (Text)


type Tag = (Key, Value)
type Key = Text
type Value = Text

category :: Key
category = "category"

-- | One control tag is given for each control signal the instrument
-- supports.  Automatically added.
control :: Key
control = "control"

-- | Patch was loaded from this file.
file :: Key
file = "file"

-- | Instrument name.  Automatically added.
name :: Key
name = "name"

-- | Synth name.  Automatically added.
synth :: Key
synth = "synth"

-- | Indicates that the instrument has a sysex message as its midi
-- initialization, which probably means it's not built in to the synth.
sysex :: Key
sysex = "sysex"


-- * categories

c_strings, c_woodwinds, c_brass, c_percussion, c_synth :: Text
c_strings = "strings"
c_woodwinds = "woodwinds"
c_brass = "brass"
c_percussion = "percussion"
c_synth = "synth"
