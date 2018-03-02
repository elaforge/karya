-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Convert types to their low-level form.
module Synth.Faust.Convert (controls, SignalP) where
import qualified Control.Monad.Trans as Trans
import qualified Data.Map as Map
import qualified Data.Vector.Storable as Vector.Storable
import qualified Foreign
import qualified Foreign.ForeignPtr.Unsafe as ForeignPtr.Unsafe

import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Signal as Signal
import Global


-- | Get signal pointers for 'DriverC.render'.
controls :: Trans.MonadIO m => [Control.Control]
    -> Map Control.Control Signal.Signal
    -> ([(SignalP, Int)] -> m a) -> m a
controls patchControls controls process = go [] patchControls
    where
    go accum (c:cs) = case Map.lookup c controls of
        Nothing -> go ((Foreign.nullPtr, 0) : accum) cs
        Just sig -> signal sig $ \sigp len -> go ((sigp, len) : accum) cs
    go accum [] = process (reverse accum)

type SignalP = Foreign.Ptr (Signal.Sample Double)

signal :: Trans.MonadIO m => Signal.Signal -> (SignalP -> Int -> m a) -> m a
signal sig process = withForeignPtr fptr $ \ptr -> process ptr len
    where
    (fptr, len) = Vector.Storable.unsafeToForeignPtr0 $ Signal.to_vector sig

-- | This is the same as 'Foreign.withForeignPtr', except in MonadIO.
withForeignPtr :: Trans.MonadIO m => Foreign.ForeignPtr a
    -> (Foreign.Ptr a -> m b) -> m b
withForeignPtr fptr io = do
    r <- io (ForeignPtr.Unsafe.unsafeForeignPtrToPtr fptr)
    liftIO (Foreign.touchForeignPtr fptr)
    return r
