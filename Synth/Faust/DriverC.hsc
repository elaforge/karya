-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Faust.DriverC (
    Patch, Instrument
    , getPatches
    , getControls
    , getUiControls
) where
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Util.CUtil as CUtil
import Util.ForeignC

import qualified Synth.Shared.Control as Control

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
getPatches = alloca $ \namespp -> alloca $ \patchpp -> do
    count <- fromIntegral <$> c_faust_patches namespp patchpp
    names <- peekTexts count =<< peek namespp
    patches <- peekArray count =<< peek patchpp
    return $ Map.fromList $ zip names patches

-- int faust_patches(const char ***names, Patch **patches);
foreign import ccall "faust_patches"
    c_faust_patches :: Ptr (Ptr CString) -> Ptr (Ptr Patch) -> IO CInt

-- | Get control names and docs from the faust metadata.
--
-- The convention is that controls are called declare control#_name "Doc.".
-- The # is so they sort in the same order as the input signals to which they
-- correspond.
getControls :: Patch -> IO (Text, [(Control.Control, Text)])
    -- ^ patch description, control names in their input order
getControls patch = do
    meta <- getMetadata patch
    return
        ( Map.findWithDefault "" "description" meta
        , [ (Control.Control (Text.drop 1 (Text.dropWhile (/='_') k)), v)
          | (k, v) <- Map.toAscList meta
          , "control" `Text.isPrefixOf` k
          ]
        )

getMetadata :: Patch -> IO (Map Text Text)
getMetadata patch = alloca $ \keyspp -> alloca $ \valuespp -> do
    count <- fromIntegral <$> c_faust_metadata patch keyspp valuespp
    kvs <- zip <$> (peekTexts count =<< peek keyspp)
        <*> (peekTexts count =<< peek valuespp)
    free =<< peek keyspp
    free =<< peek valuespp
    return $ Map.fromList kvs

-- int faust_metadata(Patch inst, const char ***keys, const char ***values);
foreign import ccall "faust_metadata"
    c_faust_metadata :: Patch -> Ptr (Ptr CString) -> Ptr (Ptr CString)
        -> IO CInt

getUiControls :: Patch -> IO [(Control.Control, Ptr CFloat, Text)]
getUiControls patch = do
    (count, controlsp, docsp, valsp) <-
        alloca $ \controlspp -> alloca $ \docspp -> alloca $ \valspp -> do
            count <- c_faust_controls patch controlspp docspp valspp
            (,,,) (fromIntegral count) <$>
                peek controlspp <*> peek docspp <*> peek valspp
    controls <- map Control.Control <$> peekTexts count controlsp
    free controlsp
    docs <- peekTexts count docsp
    mapM_ free =<< peekArray count docsp
    free docsp
    vals <- peekArray count valsp
    free valsp
    return $ zip3 controls vals docs

-- int faust_controls(Patch patch, const char ***out_controls, char ***out_docs,
--     FAUSTFLOAT ***out_vals)
foreign import ccall "faust_controls"
    c_faust_controls :: Patch -> Ptr (Ptr CString) -> Ptr (Ptr CString)
        -> Ptr (Ptr (Ptr CFloat)) -> IO CInt

-- dsp *faust_initialize(Patch dsp, int sample_rate);
foreign import ccall "faust_initialize"
    c_faust_initialize :: Patch -> CInt -> IO Instrument

-- void faust_destroy(dsp *instrument);
foreign import ccall "faust_destroy"
    c_faust_destroy :: Instrument -> IO ()

-- void faust_render(Instrument inst, int start_frame, int end_frame,
--     Point **controls, float **output);

-- * util

peekTexts :: Int -> Ptr CString -> IO [Text]
peekTexts count textp = do
    texts <- peekArray count textp
    mapM CUtil.peekCString texts
