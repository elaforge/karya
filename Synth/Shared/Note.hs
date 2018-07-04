-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | The 'Note' type and support.
module Synth.Shared.Note where
import qualified Data.Digest.CRC32 as CRC32
import qualified Data.Map.Strict as Map
import qualified Data.Word as Word

import qualified Util.Pretty as Pretty
import qualified Util.Serialize as Serialize
import Util.Serialize (get, put)

import qualified Derive.Attrs as Attrs
import qualified Perform.Pitch as Pitch
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Signal as Signal
import Synth.Types

import Global


-- | High level representation of one note.  This will be converted into
-- one or more 'Sample.Sample's.
data Note = Note {
    -- | Map this note to one of the synthesizer's patches.
    patch :: !PatchName
    , instrument :: !InstrumentName
    -- | Address this note to a particular element within the patch.  What it
    -- is depends on the instrument.  For instance, it might the a particular
    -- string on a pipa.  The difference from 'attributes' is that each element
    -- is mutually exclusive.
    , element :: !Text
    , start :: !RealTime
    , duration :: !RealTime
    -- | E.g. envelope, pitch, lpf.
    , controls :: !(Map Control.Control Signal.Signal)
    , attributes :: !Attrs.Attributes
    -- | The hash of the note itself, minus the hash field.  This is in the
    -- Note as a lazy field so it only ever gets calculated once.
    --
    -- It turns out to be more convenient to group with the note since then I
    -- can cheaply sort Notes by hash.  The downside is that now I can't
    -- modify a Note without having to update the hash, but I shouldn't be
    -- modifying Notes except in a few tests.
    , hash :: Hash
    } deriving (Show)

-- | Unique identifier for a patch.
type PatchName = Text

-- | This is a specific instantiation of a 'PatchName'.  This is the same as
-- 'Derive.ScoreTypes.Instrument'.
type InstrumentName = Text

end :: Note -> RealTime
end n = start n + duration n

instance Serialize.Serialize Note where
    put (Note a b c d e f g _hash) =
        put a *> put b *> put c *> put d *> put e *> put f *> put g
    get = fmap setHash $
        Note <$> get <*> get <*> get <*> get <*> get <*> get <*> get
            <*> pure mempty

instance Pretty Note where
    format (Note patch inst element start dur controls attrs hash) =
        Pretty.record "Note"
            [ ("patch", Pretty.format patch)
            , ("instrument", Pretty.format inst)
            , ("element", Pretty.format element)
            , ("start", Pretty.format start)
            , ("duration", Pretty.format dur)
            , ("controls", Pretty.format controls)
            , ("attributes", Pretty.format attrs)
            , ("hash", Pretty.format hash)
            ]

-- | Make a Note for testing.
note :: PatchName -> InstrumentName -> RealTime -> RealTime -> Note
note patch instrument start duration = setHash $ Note
    { patch = patch
    , instrument = instrument
    , element = ""
    , start = start
    , duration = duration
    , controls = mempty
    , attributes = mempty
    , hash = mempty
    }

initialPitch :: Note -> Maybe Pitch.NoteNumber
initialPitch note =
    Pitch.nn . Signal.at (start note) <$>
        Map.lookup Control.pitch (controls note)

withControl :: Control.Control -> Signal.Signal -> Note -> Note
withControl control signal note =
    note { controls = Map.insert control signal (controls note) }

withPitch :: Pitch.NoteNumber -> Note -> Note
withPitch = withControl Control.pitch . Signal.constant . Pitch.nn_to_double

-- * serialize

serialize :: FilePath -> [Note] -> IO Bool
serialize = Serialize.serialize notesMagic

unserialize :: FilePath -> IO (Either Serialize.UnserializeError [Note])
unserialize  = Serialize.unserialize notesMagic

notesMagic :: Serialize.Magic [Note]
notesMagic = Serialize.Magic 'n' 'o' 't' 'e'


-- * hash

newtype Hash = Hash Word.Word32
    deriving (Show, Eq, Ord, Pretty, Serialize.Serialize)

setHash :: Note -> Note
setHash note = note { hash = Hash (CRC32.crc32 note) }

instance Semigroup Hash where
    Hash h1 <> Hash h2 = Hash $ CRC32.crc32Update h1 h2

instance Monoid Hash where
    mempty = Hash 0
    mappend = (<>)
    -- Otherwise, the extra <>0 means that mconcat [x] /= x.  It's not wrong,
    -- but it's less confusing when mconcat [x] == x.
    mconcat (x:xs) = foldl' (<>) x xs
    mconcat [] = mempty

instance CRC32.CRC32 Note where
    crc32Update n (Note name inst element start dur controls attrs _hash) =
        n & name & inst & element & start & dur & controls & Attrs.to_set attrs
        where
        (&) :: CRC32.CRC32 a => Word.Word32 -> a -> Word.Word32
        (&) = CRC32.crc32Update
