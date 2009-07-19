{-# OPTIONS_GHC -XGeneralizedNewtypeDeriving #-}
{- | The basic types that the interface modules use.

    Unlike the other modules, this is designed to be used with a non-qualified
    import.  It puts various commonly used types into scope, and may re-export
    just the type part from other modules, for easier type signatures.
-}
module Ui.Types (
    Color, TrackPos(..), track_pos, pretty_pos
) where
import Foreign
import Foreign.C
import Text.Printf

import Ui.Color (Color)

#include "c_interface.h"
-- See comment in BlockC.hsc.
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-- * trackpos

-- | The position of an Event on a track.  The units are arbitrary, so how
-- many units are in one second depends on the tempo.  TrackPos units
-- can be negative, but blocks only display events at positive TrackPos.
newtype TrackPos = TrackPos Double
    deriving (Num, Enum, Real, Fractional, RealFrac, Eq, Ord, Show, Read)

track_pos :: (Real a) => a -> TrackPos
track_pos n = TrackPos (realToFrac n)

instance Storable TrackPos where
    sizeOf _ = #size TrackPos
    alignment _ = #{alignment TrackPos}
    peek posp = do
        v <- (#peek TrackPos, _val) posp :: IO CDouble
        return (TrackPos (realToFrac v))
    poke posp (TrackPos pos) =
        (#poke TrackPos, _val) posp (realToFrac pos :: CDouble)

pretty_pos :: TrackPos -> String
pretty_pos pos = printf "%.3f" (realToFrac pos :: Double)
