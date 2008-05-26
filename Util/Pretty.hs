{- | Like Show, but designed to be easy to read rather than unambiguous and
complete.
-}
module Util.Pretty (Pretty, pretty) where
import qualified Data.Map as Map

import qualified Util.Seq as Seq


class Show a => Pretty a where
    pretty :: a -> String
    pretty = show

instance Pretty Int
instance Pretty Integer

instance Pretty a => Pretty [a] where
    pretty xs = "[" ++ Seq.join ", " (map pretty xs) ++ "]"

instance (Pretty a, Pretty b) => Pretty (a, b) where
    pretty x = show x

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
