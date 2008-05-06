{-# OPTIONS_GHC -XGeneralizedNewtypeDeriving #-}
{- |
The basic types that the interface modules use.

Unlike the other modules, this is designed to be used with a non-qualified
import.  It puts various commonly used types into scope, and may re-export
just the type part from other modules, for easier type signatures.
-}
module Ui.Types (
    Color
    , TrackPos(..)

) where
import Foreign
import Foreign.C

import qualified Ui.Util as Util
import Ui.Color (Color)

#include "c_interface.h"

-- * trackpos

-- | The position of an Event on a track.  The units are arbitrary, so how
-- many units are in one second depends on the tempo.  TrackPos units
-- can be negative, but once they get to the UI they will be clamped to be
-- within 0--ULONG_MAX.
newtype TrackPos = TrackPos Integer deriving (Num, Eq, Ord, Show, Read)
zero_trackpos = TrackPos 0

instance Storable TrackPos where
    sizeOf _ = #size TrackPos
    alignment _ = undefined
    peek posp = do
        v <- (#peek TrackPos, _val) posp :: IO CLLong
        return (TrackPos (fromIntegral v))
    poke posp (TrackPos pos) = (#poke TrackPos, _val) posp cpos
        where
        cpos :: CLLong
        cpos = fromIntegral
            (Util.bounded 0 (fromIntegral (maxBound::CLLong)) pos)
