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

test_simplify_secs = do
    let f = map Score.attrs_list . Vsl.simplify_secs
            . map (mconcat . map Score.attr)
    equal (f [["sec1", "a"]]) [["a"]]
    equal (f [["sec1", "a"], ["sec2", "b"]]) [["a", "fast"], ["b", "slow"]]
    equal (f [["sec2", "a"], ["sec1", "b"]]) [["a", "slow"], ["b", "fast"]]
    equal (f [["sec1", "a"], ["sec2", "b"], ["sec3"]])
        [["a", "fast"], ["b", "medium"], ["slow"]]
    equal (f [["sec1", "a"], ["sec2", "b"], ["sec3"], ["sec4"]])
        [["a", "sec1"], ["b", "sec2"], ["sec3"], ["sec4"]]
