-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
-- | Low level binding to the Patch c++ object, which represents any faust
-- dsp, both instruments and effects processors.
module Synth.Faust.PatchC where
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Unsafe as ByteString.Unsafe
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector.Storable as V

import qualified Foreign

import qualified Util.Audio.Audio as Audio
import qualified Util.CUtil as CUtil
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Control as Control

import           ForeignC
import           Global


type PatchP = Ptr CConstPatchP
data CConstPatchP

-- | An allocated patch.
type InstrumentP = Ptr CPatchP
data CPatchP


patches :: IO [(Text, PatchP)]
patches = alloca $ \patchpp -> do
    count <- fromIntegral <$> c_faust_patches patchpp
    patches <- peekArray count =<< peek patchpp
    names <- mapM (CUtil.peekCString . c_faust_name) patches
    -- I use hyphens, but C doesn't allow them in names.
    return $ zip (map (Text.replace "_" "-") names) patches

-- int faust_patches(const Patch ***patches);
foreign import ccall "faust_patches"
    c_faust_patches :: Ptr (Ptr PatchP) -> IO CInt

foreign import ccall "faust_name" c_faust_name :: PatchP -> CString

patchInputs, patchOutputs :: PatchP -> Int
patchInputs = fromIntegral . c_faust_num_inputs
patchOutputs = fromIntegral . c_faust_num_outputs

instInputs, instOutputs :: InstrumentP -> Int
instInputs = patchInputs . castPtr
instOutputs = patchOutputs . castPtr

foreign import ccall unsafe "faust_num_inputs"
    c_faust_num_inputs :: PatchP -> CInt
foreign import ccall unsafe "faust_num_outputs"
    c_faust_num_outputs :: PatchP -> CInt

getMetadata :: PatchP -> IO (Map Text Text)
getMetadata patch = alloca $ \keyspp -> alloca $ \valuespp -> do
    count <- fromIntegral <$> c_faust_metadata patch keyspp valuespp
    kvs <- zip <$> (peekTexts count =<< peek keyspp)
        <*> (peekTexts count =<< peek valuespp)
    free =<< peek keyspp
    free =<< peek valuespp
    return $ Map.fromList kvs

parseDescription :: Map Text Text -> Text
parseDescription meta = Text.intercalate "\n" $ filter (/="") $
    Map.findWithDefault "" "description" meta
    : map (\k -> maybe "" ((k <> ": ")<>) $ Map.lookup k meta)
        ["author", "copyright", "version", "license"]

-- int faust_metadata(
--     const Patch *patch, const char ***keys, const char ***values);
foreign import ccall "faust_metadata"
    c_faust_metadata :: PatchP -> Ptr (Ptr CString) -> Ptr (Ptr CString)
        -> IO CInt

getUiControls :: PatchP -> IO [(([Text], Control.Control), Text)]
getUiControls patch = do
    (count, pathspp, controlsp, docsp) <-
        alloca $ \pathsppp -> alloca $ \controlspp -> alloca $ \docspp -> do
            count <- c_faust_controls patch pathsppp controlspp docspp
            (,,,) (fromIntegral count)
                <$> peek pathsppp <*> peek controlspp <*> peek docspp
    -- pathsp is a null-terminated list of char*
    pathsp <- peekArray count pathspp
    paths <- mapM peekTexts0 pathsp
    mapM_ free pathsp
    free pathspp

    -- Faust likes underscores, but I use hyphens for control names.
    controls <- map (Control.Control . Text.replace "_" "-") <$>
        peekTexts count controlsp
    free controlsp
    docs <- peekTexts count docsp
    mapM_ free =<< peekArray count docsp
    free docsp

    -- Lead with "_" to suppress the whole control.  But "_" was turned to "-".
    return $ filter (not . ("-" `Text.isPrefixOf`) . pretty . snd . fst) $
        zip (zip paths controls) docs


-- int faust_controls(Patch patch, const char ****out_paths,
--     const char ***out_controls, char ***out_docs)
foreign import ccall "faust_controls"
    c_faust_controls :: PatchP -> Ptr (Ptr (Ptr CString)) -> Ptr (Ptr CString)
        -> Ptr (Ptr CString) -> IO CInt

-- * Instrument

allocate :: PatchP -> IO (InstrumentP, [(([Text], Control.Control), Ptr Float)])
allocate patch = do
    ptr <- c_faust_allocate patch (CUtil.c_int Config.samplingRate)
    cptrs <- alloca $ \cptrspp -> do
        count <- c_faust_control_ptrs ptr cptrspp
        cptrsp <- peek cptrspp
        cptrs <- peekArray (fromIntegral count) cptrsp
        free cptrsp
        return cptrs
    -- I need an allocated instrument to get valid control pointers.  But since
    -- I already changed their order to put them in a Map, I have to get the
    -- control names again, which is inefficient but easy to do.
    uis <- getUiControls patch
    return (ptr, zip (map fst uis) cptrs)

-- Patch *faust_allocate(const Patch *patch, int srate);
foreign import ccall "faust_allocate"
    c_faust_allocate :: PatchP -> CInt -> IO InstrumentP

-- int faust_control_ptrs(Patch *inst, FAUSTFLOAT ***out_vals)
foreign import ccall "faust_control_ptrs"
    c_faust_control_ptrs :: InstrumentP -> Ptr (Ptr (Ptr Float)) -> IO CInt

-- void faust_destroy(Patch *patch) { delete patch; }
foreign import ccall "faust_destroy" c_faust_destroy :: InstrumentP -> IO ()

-- | Render chunk of time and return samples.
render :: Audio.Frames -> Audio.Frames -> InstrumentP
    -> [(Ptr Float, Audio.Block)]
    -> [V.Vector Float] -- ^ Input signals.  The length must be equal to the
    -- the patchInputs, and each vector must have the same length.
    -> IO [V.Vector Float] -- ^ one block of samples for each output channel
render controlSize controlsPerBlock inst controls inputs = do
    unless (length inputs == instInputs inst) $
        errorIO $ "instrument expects " <> showt (instInputs inst)
            <> " inputs, but was given " <> showt (length inputs)
    let inputSizes = map (Audio.vectorFrames (Proxy @1)) inputs
    unless (all (==blockSize) inputSizes) $
        errorIO $ "all inputs should be block size " <> pretty blockSize
            <> ": " <> pretty inputSizes
    let controlSizes = map (Audio.blockFrames (Proxy @1) . snd) controls
    unless (all (==controlsPerBlock) controlSizes) $
        errorIO $ "all controls should have size " <> pretty controlsPerBlock
            <> ": " <> pretty controlSizes

    -- Debug.tracepM "controls" controls
    -- Debug.tracepM "inputs" inputs
    -- Debug.tracepM "blockSize" blockSize
    -- Use ForeignPtr to keep the output arrays alive until I can stuff them
    -- into a V.Vector.
    outputFptrs <- mapM Foreign.mallocForeignPtrArray
        (replicate (instOutputs inst) (unframe blockSize))
    -- Holy manual memory management, Batman.
    CUtil.withForeignPtrs outputFptrs $ \outputPtrs ->
        withVectors inputs $ \inputsP ->
        withArray outputPtrs $ \outputsP ->
        withControls controls $ \controlCount controlPtrs controlValsP ->
            c_faust_render inst
                (c_frames controlSize) (c_frames controlsPerBlock)
                controlCount controlPtrs controlValsP
                inputsP outputsP
    return $ map (\fptr -> V.unsafeFromForeignPtr0 fptr (unframe blockSize))
        outputFptrs
    where
    blockSize = controlSize * controlsPerBlock
    c_frames = CUtil.c_int . unframe
    unframe (Audio.Frames f) = f

withControls :: [(Ptr Float, Audio.Block)]
    -> (CInt -> Ptr (Ptr Float) -> Ptr (Ptr Float) -> IO a) -> IO a
withControls controls action = do
    -- Avoid allocating an array if it's constant.
    sequence_ [Foreign.poke ptr val | (ptr, Audio.Constant _ val) <- controls]
    withArray ptrs $ \controlPtrs ->
        withVectors vecs $ \controlValsP ->
        action (CUtil.c_int (length ptrs)) controlPtrs controlValsP
    where (ptrs, vecs) = unzip [(ptr, vec) | (ptr, Audio.Block vec) <- controls]

-- void faust_render(
--     Patch *patch,
--     int control_size, int controls_per_block,
--     int control_count, float **control_ptrs, const float **controls,
--     const float **inputs, float **outputs);
foreign import ccall "faust_render"
    c_faust_render :: InstrumentP
        -- control_size -> controls_per_block
        -> CInt -> CInt
        -- control_count -> control_ptrs -> controls
        -> CInt -> Ptr (Ptr Float) -> Ptr (Ptr Float)
        -- inputs -> outputs
        -> Ptr (Ptr Float) -> Ptr (Ptr Float)
        -> IO ()

-- ** state

getState :: InstrumentP -> IO ByteString.ByteString
getState inst = alloca $ \statepp -> do
    c_faust_get_state inst statepp
    statep <- peek statepp
    ByteString.packCStringLen
        (statep, fromIntegral $ c_faust_get_state_size inst)

-- | 'getState', but without copying, if you promise to finish with the State
-- before you call 'render', which will change it.
unsafeGetState :: InstrumentP -> IO ByteString.ByteString
unsafeGetState inst = alloca $ \statepp -> do
    c_faust_get_state inst statepp
    statep <- peek statepp
    ByteString.Unsafe.unsafePackCStringLen
        (statep, fromIntegral $ c_faust_get_state_size inst)

putState :: ByteString.ByteString -> Text -> InstrumentP -> IO ()
putState state name inst =
    ByteString.Unsafe.unsafeUseAsCStringLen state $ \(statep, size) -> do
        let psize = c_faust_get_state_size inst
        unless (fromIntegral size == psize) $
            errorIO $ "inst " <> showt name <> " expects state size "
                <> showt psize <> " but got " <> showt size
        c_faust_put_state inst statep

-- size_t faust_get_state_size(const Patch *patch) { return patch->size; }
foreign import ccall "faust_get_state_size"
    c_faust_get_state_size :: InstrumentP -> CSize

-- size_t faust_get_state(const Patch *patch, const char **state);
foreign import ccall "faust_get_state"
    c_faust_get_state :: InstrumentP -> Ptr CString -> IO ()

-- void faust_put_state(Patch *patch, const char *state);
foreign import ccall "faust_put_state"
    c_faust_put_state :: InstrumentP -> CString -> IO ()

-- * util

peekTexts :: Int -> Ptr CString -> IO [Text]
peekTexts count textp = do
    texts <- peekArray count textp
    mapM CUtil.peekCString texts

peekTexts0 :: Ptr CString -> IO [Text]
peekTexts0 textp = do
    texts <- Foreign.peekArray0 nullPtr textp
    mapM CUtil.peekCString texts

-- | Allocate a list of vectors as **float.  Pass nullptr for [], to avoid
-- allocation.
withVectors :: [V.Vector Float] -> (Ptr (Ptr Float) -> IO a) -> IO a
withVectors [] f = f nullPtr
withVectors vs f = withPtrs vs $ \ps -> withArray ps f

-- | Convert a list of vectors to a list of pointers, with no copying.
withPtrs :: [V.Vector Float] -> ([Ptr Float] -> IO a) -> IO a
withPtrs = Foreign.withMany V.unsafeWith
