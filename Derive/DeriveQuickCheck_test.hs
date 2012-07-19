module Derive.DeriveQuickCheck_test where
import Util.Test
import qualified Derive.DeriveQuickCheck as DeriveQuickCheck


-- TODO placeholder for now, just to force DeriveQuickCheck to compile.
test_derive = do
    equal (length (DeriveQuickCheck.simple_derive [])) 0
