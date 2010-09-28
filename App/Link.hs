{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Types pertaining to dynamic linking of code at runtime.
module App.Link where
import qualified Data.Binary as Binary


-- | A globally unique name for a bit of code to link at runtime.  It should
-- be the name of a particular symbol, fully qualified by its module, e.g.
-- @A.B.sym@.
--
-- Since I can't serialize code (or perhaps, the code is separately serialized
-- as a shared library), I save these and link the code on startup.
newtype ModuleId = ModuleId String
    deriving (Binary.Binary, Eq, Ord, Show)
