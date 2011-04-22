module Cmd.Instrument.Drums where
import qualified Midi.Midi as Midi

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Keymap as Keymap
import qualified Cmd.Msg as Msg
import qualified Cmd.Instrument.Util as CUtil

import Derive.Attrs
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.Instrument.Util as DUtil


-- | Create a LookupCall for the given Notes.
make_calls :: [Note] -> Derive.LookupCall Derive.NoteCall
make_calls notes = Derive.make_lookup $ Derive.make_calls
    [(note_name n, DUtil.with_attrs (note_attrs n)) | n <- notes]

-- | Create keymap Cmd for the given Notes.  This should be paired with
-- 'make_calls' so the Cmd will create calls that the deriver understands.
make_cmd :: (Cmd.M m) => [(Note, Midi.Key)] -> Msg.Msg -> m Cmd.Status
make_cmd note_keys = CUtil.keymaps
    [(Keymap.physical_key (note_char n), note_name n, key)
        | (n, key) <- note_keys]


-- | Description of a generic drum set.  There are many drum set instruments,
-- each of which probably use different MIDI keys, but at least I can
-- standardize call names, attributes, and keymap key.  Of course there will be
-- drum sets that don't fit in (e.g. have two or three snares), but at least
-- this provides a standard base.
data Note = Note {
    note_name :: String
    , note_attrs :: Score.Attributes
    , note_char :: Char
    } deriving (Show)


c_bd    = Note "bd"     bd              'z'
c_sn    = Note "sn"     snare           'x'
c_ltom  = Note "ltom"   (tom @+ low)    'c'
c_mtom  = Note "mtom"   (tom @+ middle) 'v'
c_htom  = Note "htom"   (tom @+ high)   'b'

c_hh    = Note "hh"     hh              'q'
c_ohh   = Note "ohh"    (open @+ hh)    'q'
c_chh   = Note "chh"    (closed @+ hh)  'w'
c_phh   = Note "phh"    (pedal @+ hh)   'e'

c_ride  = Note "ride"   ride            't'
c_crash = Note "crash"  crash           'y'

-- TODO other drum style ornaments like double strikes, rolls, etc.
