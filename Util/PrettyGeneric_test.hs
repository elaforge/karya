{-# LANGUAGE DeriveGeneric #-}
module Util.PrettyGeneric_test where
import qualified Data.List as List
import qualified GHC.Generics as G

import qualified Util.PrettyGeneric as PrettyGeneric
import Util.Test


test_valuePairs = do
    let f = map (\(a, b) -> (List.intercalate "." a, b))
            . PrettyGeneric.valuePairs . PrettyGeneric.extract
    pprint (f r2)

instance PrettyGeneric.Extract R1
data R1 = R1 {
    r1_f1 :: Bool
    , r1_f2 :: Int
    } deriving (Show, G.Generic)

r1 = R1 True 42

instance PrettyGeneric.Extract R2
data R2 = R2 { r2_f1 :: R1 } deriving (Show, G.Generic)

r2 = R2 r1
