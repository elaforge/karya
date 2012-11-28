module Local.Instrument.Vsl_test where
import Util.Control
import Util.Test
import qualified Derive.Score as Score
import qualified Local.Instrument.Vsl as Vsl


test_strip_attributes = do
    let f = map Score.attrs_list . Vsl.strip_attributes
            . map (mconcat . map Score.attr)
    -- Strip them.
    equal (f [["sustain"], ["marcato", "vibrato"]]) [[], ["marcato"]]
    -- Can't strip if it would be non-unique.
    equal (f [["a", "sustain"], ["a"]]) [["a", "sustain"], ["a"]]
    -- Or if it's non-unique with an already stripped one.
    equal (f [["sustain"], ["vibrato"]]) [[], ["vibrato"]]
