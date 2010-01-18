module Util.Binary where
import Data.Binary (Binary, get, put)
import qualified Data.Word as Word

-- | Data.Binary doesn't support -0.0, so this works around that.
newtype NDouble = NDouble Double

instance Binary NDouble where
    put (NDouble d) = put (decode_double d)
    get = get >>= return . NDouble . encode_double

decode_double :: Double -> (Word.Word8, Integer, Int)
decode_double d = (if isNegativeZero d then 1 else 0, mant, exp)
    where (mant, exp) = decodeFloat d

encode_double :: (Word.Word8, Integer, Int) -> Double
encode_double (negative_zero, mant, exp)
    | negative_zero == 0 = encodeFloat mant exp
    | otherwise = -0.0
