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
-- The type of the value here should be kept in sync with the type of the c++
-- TrackPos value.
newtype TrackPos = TrackPos Double deriving (Eq, Ord, Show)

#include "c_interface.h"
instance Storable TrackPos where
    sizeOf _ = #size TrackPos
    alignment _ = undefined
    peek posp = (#peek TrackPos, _val) posp >>= return . TrackPos
    poke posp (TrackPos pos) = (#poke TrackPos, _val) posp pos
