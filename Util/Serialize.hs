module Util.Serialize where
import Data.Serialize (Serialize, get, put)
import qualified Data.Word as Word

-- | Data.Serialize doesn't support -0.0, so this works around that.
newtype NDouble = NDouble Double

instance Serialize NDouble where
    put (NDouble d) = put (decode_double d)
    get = get >>= return . NDouble . encode_double

decode_double :: Double -> (Word.Word8, Integer, Int)
decode_double d = (if isNegativeZero d then 1 else 0, mant, exp)
    where (mant, exp) = decodeFloat d

encode_double :: (Word.Word8, Integer, Int) -> Double
encode_double (negative_zero, mant, exp)
    | negative_zero == 0 = encodeFloat mant exp
    | otherwise = -0.0
