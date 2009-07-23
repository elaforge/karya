module Perform.Midi.Instrument_test where
import qualified Data.Set as Set

import Util.Test
import qualified Perform.Midi.Instrument as Instrument


attr_map = Instrument.make_keyswitches
    [("pizz", 0), ("sfz+trem", 1), ("sfz", 2), ("trem", 3)]

test_get_keyswitch = do
    let f attrs = fmap (\(Instrument.Keyswitch name _) -> name) $
            Instrument.get_keyswitch attr_map (Set.fromList (words attrs))
    equal (f "bazzle") Nothing
    equal (f "pizz sfz") (Just "pizz")
    equal (f "trem sfz") (Just "sfz+trem")
    equal (f "sfz bazzle trem") (Just "sfz+trem")
    equal (f "pizz sfz trem") (Just "pizz")
    -- equal (f ("cresc.fast")) (Just "cresc")

test_make_keyswitches = do
    let f = Instrument.make_keyswitches
    equal (f [("a+b", 1), ("", 2)]) $
        Instrument.KeyswitchMap
            [ (Set.fromList ["a", "b"], Instrument.Keyswitch "a+b" 1)
            , (Set.empty, Instrument.Keyswitch "" 2)
            ]
    throws "attr [\"pizz\"] is shadowed by []" (f [("", 1), ("pizz", 2)])
