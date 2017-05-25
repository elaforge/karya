-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Faust.DriverC (
    getPatches
) where
import qualified Data.Map as Map
import qualified Util.CUtil as CUtil
import Util.ForeignC
import Global

#include "Synth/Faust/driver.h"



-- | A patch can be used to create 'Instrument's.
type Patch = Ptr DspP
data DspP

-- | An allocated patch.
type Instrument = Ptr DspI
data DspI

-- | Get all patches and their names.
getPatches :: IO (Map Text Patch)
getPatches = do
    alloca $ \namespp -> alloca $ \patchpp -> do
        count <- fromIntegral <$> c_get_patches namespp patchpp
        namesp <- peek namespp
        names <- peekArray count namesp
        names <- mapM CUtil.peekCString names
        patchp <- peek patchpp
        patches <- peekArray count patchp
        return $ Map.fromList $ zip names patches

-- int get_patches(const char ***names, Patch **patches);
foreign import ccall "get_patches"
    c_get_patches :: Ptr (Ptr CString) -> Ptr (Ptr Patch) -> IO CInt

-- int get_controls(Patch inst, const char ***controls);
foreign import ccall "get_controls"
    c_get_controls :: Patch -> Ptr (Ptr CString) -> IO CInt

-- dsp *initialize(Patch dsp, int sample_rate);
foreign import ccall "initialize"
    c_initialize :: Patch -> CInt -> IO Instrument

-- void destroy(dsp *instrument);
foreign import ccall "destroy"
    c_destroy :: Instrument -> IO ()

-- void compute(dsp *inst, int start_frame, int end_frame, Point **controls,
--     float **output);
