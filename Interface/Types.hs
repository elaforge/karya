{-
The basic types that the interface modules use.
-}
module Interface.Types where

type UI = IO -- probably just IO, but maybe add errors?
-- r, g, b, alpha, from 0--1
newtype Color = Color (Double, Double, Double, Double) deriving (Show)

-- | Used to associate auxiliary values with UI objects.
type Attrs = [(String, String)]


-- | The position of an Event on a track.  One of these is normally a second.
type TrackPos = Double
