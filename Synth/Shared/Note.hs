-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
-- | The 'Note' type and support.
module Synth.Shared.Note where
import qualified Data.Map.Strict as Map

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
    } deriving (Show)

type InstrumentName = Text
-- | Unique identifier for a patch.
type PatchName = Text

end :: Note -> RealTime
end n = start n + duration n

instance Serialize.Serialize Note where
    put (Note a b c d e f) =
        put a *> put b *> put c *> put d *> put e *> put f
    get = Note <$> get <*> get <*> get <*> get <*> get <*> get

instance Pretty Note where
    format (Note patch element start dur controls attrs) =
        Pretty.record "Note"
            [ ("patch", Pretty.format patch)
            , ("element", Pretty.format element)
            , ("start", Pretty.format start)
            , ("duration", Pretty.format dur)
            , ("controls", Pretty.format controls)
            , ("attributes", Pretty.format attrs)
            ]

note :: PatchName -> RealTime -> RealTime -> Note
note patch start duration = Note
    { patch = patch
    , element = ""
    , start = start
    , duration = duration
    , controls = mempty
    , attributes = mempty
    }

initialControl :: Control.Control -> Note -> Maybe Signal.Y
initialControl control note =
    fromMaybe 0 . Signal.at (start note) <$> Map.lookup control (controls note)

initialPitch :: Note -> Maybe Pitch.NoteNumber
initialPitch = fmap Pitch.nn . initialControl Control.pitch


-- * serialize

serialize :: FilePath -> [Note] -> IO ()
serialize = Serialize.serialize notesMagic

unserialize :: FilePath -> IO (Either Serialize.UnserializeError [Note])
unserialize  = Serialize.unserialize notesMagic

notesMagic :: Serialize.Magic [Note]
notesMagic = Serialize.Magic 'n' 'o' 't' 'e'
