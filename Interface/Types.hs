{-# OPTIONS_GHC -XGeneralizedNewtypeDeriving #-}
{-
The basic types that the interface modules use.
-}
module Interface.Types where
import Foreign

-- TODO:
-- implement Ui such that you can only run Ui actions in the monad
-- returned by initialize.
type UI = IO

-- | Used to associate auxiliary values with UI objects.
type Attrs = [(String, String)]

-- | The position of an Event on a track.  One of these is normally a second.
newtype TrackPos = TrackPos Double deriving (Eq, Ord, Show, Storable)
