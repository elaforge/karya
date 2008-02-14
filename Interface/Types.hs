{-
The basic types that the interface modules use.
-}
module Interface.Types where
import Foreign.C

type UI = IO -- probably just IO, but maybe add errors?

-- | Used to associate auxiliary values with UI objects.
type Attrs = [(String, String)]

-- | The position of an Event on a track.  One of these is normally a second.
type TrackPos = Double
type CTrackPos = CDouble

c_trackpos :: TrackPos -> CTrackPos
c_trackpos = realToFrac
