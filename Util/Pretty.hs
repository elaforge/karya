{- | Like Show, but designed to be easy to read rather than unambiguous and
    complete.
-}
module Util.Pretty (Pretty(..), show_float) where
import qualified Numeric
import qualified Data.Map as Map

import qualified Util.Seq as Seq


-- | Format values in an eye-pleasing way.  Unlike Show, this isn't intended
-- to produce any kind of valid syntax, or even preserve information.
class Show a => Pretty a where
    pretty :: a -> String
    pretty = show

instance Pretty Int
instance Pretty Integer
instance Pretty Double where pretty = show_float (Just 3)
instance Pretty Float where pretty = show_float (Just 3)

-- | Display a float with the given precision, dropping leading and trailing
-- zeros.  So this can produce ".2" which is not a valid haskell float.
show_float :: (RealFloat a) => Maybe Int -> a -> String
show_float precision float
    | f == 0 = show i
    | stripped == "." = "0"
    | otherwise = stripped
    where
    (i, f) = properFraction float
    s = Numeric.showFFloat precision float ""
    stripped = Seq.rdrop_while (=='0') (dropWhile (=='0') s)

instance Pretty a => Pretty [a] where
    pretty xs = "[" ++ Seq.join ", " (map pretty xs) ++ "]"
instance Pretty a => Pretty (Maybe a) where
    pretty Nothing = "<nothing>"
    pretty (Just a) = pretty a

instance (Pretty a, Pretty b) => Pretty (a, b) where
    pretty (a, b) = "(" ++ pretty a ++ ", " ++ pretty b ++ ")"

-- instance (Pretty k, Pretty v) => Pretty (Map.Map k v) where
instance (Show k, Show v) => Pretty (Map.Map k v) where
    pretty m = "{\n"
        -- ++ (indent_lines . Seq.join "\n" . map ent . Map.assocs) m
        ++ (indent_lines . Seq.join "\n" . map ent . Map.assocs) m
        ++ "\n}"
        where ent (k, v) = show k ++ ": " ++ show v

indent_lines :: String -> String
indent_lines = unlines . map (indent++) . lines

indent = "  "
