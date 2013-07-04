-- | These instances would be better in "Midi.Midi", but that would be
-- a circular import with "Midi.Parse".  And putting them in "Midi.Parse" would
-- make them a bit too hidden.
module Midi.Instances where
import Util.Control
import qualified Midi.Midi as Midi
import qualified Midi.Parse as Parse
import qualified Util.Serialize as Serialize


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
    put = Serialize.put . Parse.encode
    get = Parse.decode <$> Serialize.get

instance Serialize.Serialize Midi.Key where
    put (Midi.Key a) = Serialize.put a
    get = Midi.Key <$> Serialize.get
