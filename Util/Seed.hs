-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Seed class to hash 'Derive.Stack.Frame's into the random seed.  This is
-- like Hashable, except that it must be stable (so hashable isn't suitable),
-- and since it's for a random seed, doesn't care about being well-distributed.
-- So I use the addition algorithm, which has been stable for a while.
module Util.Seed (
    Seed(to_seed), (&)
) where
import qualified Data.Primitive.PrimArray as PrimArray
import qualified Data.Text as Text
import qualified Data.Text.Array as Text.Array
import qualified Data.Text.Internal as Text.Internal
import qualified Data.Word as Word

import qualified Foreign
import qualified System.IO.Unsafe as Unsafe


class Seed a where
    to_seed :: Int -> a -> Int

(&) :: Seed a => Int -> a -> Int
(&) = to_seed
infixl 1 &

combine :: Int -> Int -> Int
combine = (+)
-- This is fnv, which hashable uses (for now):
-- combine h1 h2 = (h1 * 16777619) `Bit.xor` h2

instance Seed Int where
    to_seed salt n = combine salt n

instance Seed Word.Word64 where
    to_seed salt n = salt `combine` fromIntegral n
instance Seed Word.Word32 where
    to_seed salt n = salt `combine` fromIntegral n

instance Seed Double where
    to_seed salt n = to_seed salt
        ((Unsafe.unsafeDupablePerformIO $ Foreign.with n $
            Foreign.peek . Foreign.castPtr) :: Word.Word64)

instance Seed Text.Text where
    to_seed = simple_text

simple_text :: Int -> Text.Text -> Int
simple_text = Text.foldl' (\n -> (n+) . fromEnum)

_prim_text :: Int -> Text.Text -> Int
_prim_text salt t = salt `combine` fromIntegral (prim_text_sum t)

-- | Sum the Word16 components of the Text directly.  In theory this should be
-- faster than Text.foldl', since it skips decoding UTF16 to a Char, but in
-- practice it seems to be just the same.
--
-- This is only 16 bits so it'll wrap before an Int, but since seed winds up
-- getting trimmed to 0-999, I don't really care.
prim_text_sum :: Text.Text -> Word.Word16
prim_text_sum (Text.Internal.Text tarray offset len) = go 0 offset
    where
    go !accum !offset
        | offset < end =
            let !v = PrimArray.indexPrimArray array offset
            in go (accum + v) (offset+1)
        | otherwise = accum
    end = offset + len
    array = toPrim tarray

toPrim :: Text.Array.Array -> PrimArray.PrimArray Word.Word16
toPrim array = PrimArray.PrimArray (Text.Array.aBA array)
