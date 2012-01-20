module Derive.TrackInfo_test where
import Util.Test
import qualified Derive.TrackInfo as TrackInfo


test_parse_control = do
    let f = fmap TrackInfo.unparse_control . TrackInfo.parse_control
    equal (f "*") (Right "*")
    equal (f "*scale") (Right "*scale")
    equal (f "*scale #") (Right "*scale")
    equal (f "*scale #name") (Right "*scale #name")
    equal (f "*scale #name") (Right "*scale #name")
    equal (f "tempo") (Right "tempo")
    equal (f "c") (Right "c")
    equal (f "%") (Right "%")
    equal (f "add %") (Right "add %")
    left_like (f "$ bad") "parse error"
    left_like (f "a b c") "control track must be one of"
