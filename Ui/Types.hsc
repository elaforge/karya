{-# OPTIONS_GHC -XGeneralizedNewtypeDeriving #-}
{- |
The basic types that the interface modules use.

Unlike the other modules, this is designed to be used with a non-qualified
import.  It puts various commonly used types into scope, and may re-export
just the type part from other modules, for easier type signatures.
-}
module Ui.Types (
    Color, TrackPos(..)
) where
import Foreign
import Foreign.C

import qualified Ui.Util as Util
import Ui.Color (Color)

#include "c_interface.h"

-- * trackpos

-- | The position of an Event on a track.  The units are arbitrary, so how
-- many units are in one second depends on the tempo.  TrackPos units
-- can be negative, but blocks only display events at positive TrackPos.
newtype TrackPos = TrackPos Integer
    deriving (Num, Enum, Real, Integral, Eq, Ord, Show, Read)

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
            (Util.bounded (fromIntegral (minBound::CLLong))
                (fromIntegral (maxBound::CLLong)) pos)
