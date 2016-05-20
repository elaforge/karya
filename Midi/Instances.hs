-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | These instances would be better in "Midi.Midi", but that would be
-- a circular import with "Midi.Encode".  And putting them in "Midi.Encode"
-- would make them a bit too hidden.
module Midi.Instances where
import qualified Util.Serialize as Serialize
import qualified Midi.Encode as Encode
import qualified Midi.Midi as Midi


instance Serialize.Serialize Midi.ReadMessage where
    put (Midi.ReadMessage a b c) =
        Serialize.put a >> Serialize.put b >> Serialize.put c
    get = Midi.ReadMessage <$>
        Serialize.get <*> Serialize.get <*> Serialize.get

instance Serialize.Serialize Midi.WriteMessage where
    put (Midi.WriteMessage a b c) =
        Serialize.put a >> Serialize.put b >> Serialize.put c
    get = Midi.WriteMessage <$>
        Serialize.get <*> Serialize.get <*> Serialize.get

instance Serialize.Serialize Midi.Message where
    put = Serialize.put . Encode.encode
    get = Encode.decode <$> Serialize.get
