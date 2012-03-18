{- | Like Show, but designed to be easy to read rather than unambiguous and
    complete.
-}
module Util.Pretty (Pretty(..), lines, show_float) where
import Prelude hiding (lines)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import qualified Data.Vector.Unboxed as Unboxed
import qualified Data.Word as Word

import qualified Numeric

import qualified Util.Seq as Seq
import qualified Util.Then as Then


-- | Format values in an eye-pleasing way.  Unlike Show, this isn't intended
-- to produce any kind of valid syntax, or even preserve information.
class Show a => Pretty a where
    pretty :: a -> String
    pretty = show

instance Pretty Int
instance Pretty Integer
instance Pretty Word.Word8
instance Pretty Word.Word16
instance Pretty Word.Word32
instance Pretty Word.Word64
instance Pretty Double where pretty = show_float 3
instance Pretty Float where pretty = show_float 3

-- | Pretty up a list with a line for each element.
lines :: (Pretty a) => [a] -> String
lines = List.unlines . map pretty


-- | Display a float with the given precision, dropping trailing
-- zeros.
show_float :: (RealFloat a) => Int -> a -> String
show_float precision f = clean $ Numeric.showFFloat Nothing f ""
    where
    clean = drop0 . Seq.rdrop_while (=='.')
        . (Then.takeWhile (/='.') $ Then.take 1 $
            \rest -> Seq.rdrop_while (=='0') (take precision rest))
    drop0 "0" = "0"
    drop0 ('-':'0':'.':s) = '-':'.':s
    drop0 ('0':'.':s) = '.':s
    drop0 s = s

instance (Pretty a) => Pretty [a] where
    pretty xs = "[" ++ Seq.join ", " (map pretty xs) ++ "]"

instance (Unboxed.Unbox a, Pretty a) => Pretty (Unboxed.Vector a) where
    pretty v = "<" ++ Seq.join ", " (map pretty (Unboxed.toList v)) ++ ">"
instance (Pretty a) => Pretty (Vector.Vector a) where
    pretty v = "<" ++ Seq.join ", " (map pretty (Vector.toList v)) ++ ">"

instance (Pretty a) => Pretty (Maybe a) where
    pretty Nothing = "<nothing>"
    pretty (Just a) = pretty a

instance (Pretty a) => Pretty (Set.Set a) where
    pretty set = "{" ++ Seq.join ", " (map pretty (Set.toList set)) ++ "}"

instance (Pretty a, Pretty b) => Pretty (a, b) where
    pretty (a, b) = "(" ++ pretty a ++ ", " ++ pretty b ++ ")"

instance (Pretty k, Pretty v) => Pretty (Map.Map k v) where
    pretty m
        | Map.size m < 4 = "{" ++ Seq.join ", " (map ent (Map.assocs m)) ++ "}"
        | otherwise = "{\n"
            ++ (Seq.join "\n" . map ("  "++) . map ent . Map.assocs) m
            ++ "\n}"
        where ent (k, v) = pretty k ++ ": " ++ pretty v
