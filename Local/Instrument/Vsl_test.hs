module Local.Instrument.Vsl_test where
import Util.Test
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Local.Instrument.Vsl as Vsl


test_strip_attrs = do
    let f = map ShowVal.show_val . Vsl.strip_attrs . map Score.attrs
    -- Strip them.
    equal (f [["sus"], ["vib", "marcato"]]) ["-", "+marcato"]
    -- Can't strip if it would be non-unique.
    equal (f [["a", "sus"], ["a"]]) ["+a+sus", "+a"]
    -- Or if it's non-unique with an already stripped one.
    equal (f [["sus"], ["vib"]]) ["-", "+vib"]
