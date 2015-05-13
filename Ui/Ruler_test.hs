module Ui.Ruler_test where
import Util.Test
import qualified Ui.Ruler as Ruler


test_get_set_bounds = do
    let f s e = Ruler.get_bounds $ Ruler.set_bounds s e Ruler.no_ruler
    equal (f Nothing Nothing) (Nothing, Nothing)
    equal (f (Just 1) Nothing) (Just 1, Nothing)
    equal (f Nothing (Just 1)) (Nothing, Just 1)
    equal (f (Just 1) (Just 2)) (Just 1, Just 2)
    -- Start is always <= end.
    equal (f (Just 2) (Just 1)) (Just 1, Just 2)
