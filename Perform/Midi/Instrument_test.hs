module Perform.Midi.Instrument_test where
import qualified Data.Text as Text

import Util.Test
import qualified Derive.Score as Score
import qualified Perform.Midi.Instrument as Instrument


ksmap = Instrument.simple_keyswitches
    [ (mkattrs "pizz", 0)
    , (mkattrs "sfz trem", 1)
    , (mkattrs "sfz", 2)
    , (mkattrs "trem", 3)
    ]
mkattrs = Score.attrs . Text.words
unattrs = Text.unwords . Score.attrs_list

test_get_keyswitch = do
    let f attrs = fmap (unattrs . snd) $
            Instrument.get_keyswitch ksmap (mkattrs attrs)
    equal (f "bazzle") Nothing
    equal (f "pizz sfz") (Just "pizz")
    equal (f "trem sfz") (Just "sfz trem")
    equal (f "sfz bazzle trem") (Just "sfz trem")
    equal (f "pizz sfz trem") (Just "pizz")
    -- equal (f ("cresc.fast")) (Just "cresc")

test_overlapping_keyswitches = do
    let overlapping = Instrument.simple_keyswitches
            [(mkattrs "", 0), (mkattrs "a b", 1)]
    let f = Instrument.overlapping_keyswitches
    equal (f ksmap) []
    equal (f overlapping) ["keyswitch attrs +a+b shadowed by -"]
