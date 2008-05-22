{- | Like Show, but designed to be easy to read rather than unambiguous and
complete.
-}
module Util.Pretty where
import qualified Util.Seq as Seq

class Show a => Pretty a where
    pretty :: a -> String
    pretty = show

instance Pretty Int
instance Pretty Integer

instance Pretty a => Pretty [a] where
    pretty xs = "[" ++ Seq.join ", " (map pretty xs) ++ "]"
