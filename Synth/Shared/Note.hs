-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
-- | The 'Note' type and support.
module Synth.Shared.Note where
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.Map.Strict as Map

import qualified GHC.Generics as Generics

import qualified Util.Pretty as Pretty
import qualified Util.Serialize as Serialize
import Util.Serialize (get, put)

import qualified Perform.Pitch as Pitch
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Signal as Signal
import qualified Synth.Shared.Types as Types
import Synth.Types

import Global


-- | High level representation of one note.  This will be converted into
-- one or more 'Sample.Sample's.
data Note = Note {
    instrument :: !Types.PatchName
    , start :: !RealTime
    , duration :: !RealTime
    -- | E.g. envelope, pitch, lpf.
    , controls :: !(Map Control.Control Signal.Signal)
    , attributes :: !Types.Attributes
    } deriving (Show, Generics.Generic, Aeson.ToJSON, Aeson.FromJSON)

end :: Note -> RealTime
end n = start n + duration n

instance Serialize.Serialize Note where
    put (Note a b c d e) = put a *> put b *> put c *> put d *> put e
    get = Note <$> get <*> get <*> get <*> get <*> get

instance Pretty Note where
    format (Note inst start dur controls attrs) = Pretty.record "Note"
        [ ("instrument", Pretty.format inst)
        , ("start", Pretty.format start)
        , ("duration", Pretty.format dur)
        , ("controls", Pretty.format controls)
        , ("attributes", Pretty.format attrs)
        ]

note :: Types.PatchName -> RealTime -> RealTime -> Note
note inst start duration = Note
    { instrument = inst
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
serialize = Serialize.serialize notes_magic

unserialize :: FilePath -> IO (Either Serialize.UnserializeError [Note])
unserialize  = Serialize.unserialize notes_magic

notes_magic :: Serialize.Magic [Note]
notes_magic = Serialize.Magic 's' 'a' 'm' 'p'

unserializeJson :: FilePath -> IO (Either Text [Note])
unserializeJson filename =
    first txt . Aeson.eitherDecode <$> ByteString.Lazy.readFile filename
