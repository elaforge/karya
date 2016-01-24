{-# LANGUAGE MagicHash #-}
module Num where
import qualified GHC.Types as Types
import qualified GHC.Prim as Prim


-- | realToFrac doesn't preserve the special float values and is inefficient.
--
-- There are some RULEs for this, but they aren't reliable.
d2f :: Double -> Float
d2f (Types.D# d) = Types.F# (Prim.double2Float# d)

f2d :: Float -> Double
f2d (Types.F# f) = Types.D# (Prim.float2Double# f)
