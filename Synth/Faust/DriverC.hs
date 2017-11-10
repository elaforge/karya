-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Low level binding to driver.cc.
module Synth.Faust.DriverC (
    Patch, Instrument, asPatch
    , getPatches
    , getParsedMetadata
    , getControls
    , getUiControls
    -- * Instrument
    , withInstrument, patchInputs, patchOutputs
    , render
) where
import qualified Control.Exception as Exception
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector.Storable as Vector.Storable

import qualified Foreign

import qualified Util.CUtil as CUtil
import qualified Util.Doc as Doc
import Util.ForeignC

import qualified Ui.Id as Id
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Signal as Signal

import Global


-- | A patch can be used to create 'Instrument's.
type Patch = Ptr DspP
data DspP

-- | An allocated patch.
type Instrument = Ptr DspI
data DspI

-- | A Patch is just a const Instrument.
asPatch :: Instrument -> Patch
asPatch = Foreign.castPtr

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

getParsedMetadata :: Patch
    -> IO (Either Text (Doc.Doc, [(Control.Control, Text)]))
getParsedMetadata patch = do
    inputs <- patchInputs patch
    outputs <- patchOutputs patch
    meta <- getMetadata patch
    return $ do
        (doc, controls) <- parseMetadata meta
        unless (length controls == inputs) $
            Left $ "input count " <> showt inputs
                <> " doesn't match controls: "
                <> pretty (map fst controls)
        unless (outputs `elem` [1, 2]) $
            Left $ "expected 1 or 2 outputs, got " <> showt outputs
        -- Control.gate is used internally, so don't expose that.
        return (Doc.Doc doc, filter ((/=Control.gate) . fst) controls)

parseMetadata :: Map Text Text -> Either Text (Text, [(Control.Control, Text)])
parseMetadata meta =
    (Map.findWithDefault "" "description" meta ,) <$> metadataControls meta

-- | Get control names from the faust metadata.
--
-- The convention is that controls are declared via:
--
-- > declare control#_name "Doc."
--
-- The # is so they sort in the same order as the input signals to which they
-- correspond.
metadataControls :: Map Text Text -> Either Text [(Control.Control, Text)]
metadataControls = mapMaybeM parse . Map.toAscList
    where
    parse (c, doc)
        | c == "description" = Right Nothing
        | Id.valid_symbol stripped =
            Right $ Just (Control.Control stripped, doc)
        | otherwise = Left $ "invalid control name: " <> c
        where stripped = Text.drop 1 $ Text.dropWhile (/='_') c

-- | Get supported controls.  The order is important, since it's the same order
-- 'render' expects to see them.
getControls :: Patch -> IO [Control.Control]
getControls =
    fmap (either (const []) (map fst) . metadataControls) . getMetadata

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

withInstrument :: Patch -> (Instrument -> IO a) -> IO a
withInstrument patch = Exception.bracket (initialize patch) destroy

initialize :: Patch -> IO Instrument
initialize patch = c_faust_initialize patch (CUtil.c_int Config.samplingRate)

-- Instrument faust_initialize(Patch dsp, int sample_rate);
foreign import ccall "faust_initialize"
    c_faust_initialize :: Patch -> CInt -> IO Instrument

destroy :: Instrument -> IO ()
destroy = c_faust_destroy
-- void faust_destroy(Instrument instrument);
foreign import ccall "faust_destroy" c_faust_destroy :: Instrument -> IO ()

patchInputs, patchOutputs :: Patch -> IO Int
patchInputs = fmap fromIntegral . c_faust_num_inputs
patchOutputs = fmap fromIntegral . c_faust_num_outputs

foreign import ccall "faust_num_inputs" c_faust_num_inputs :: Patch -> IO CInt
foreign import ccall "faust_num_outputs" c_faust_num_outputs :: Patch -> IO CInt

type Frames = Int
type Sample = Signal.Sample Double

-- | Render a note on the instrument, and return samples.
render :: Instrument  -> Frames -> Frames -> [(Ptr Sample, Int)]
    -- ^ (control signal breakpoints, number of Samples)
    -> IO [Vector.Storable.Vector Float]
render inst start end controlLengths = do
    inputs <- patchInputs (asPatch inst)
    unless (length controlLengths == inputs) $
        errorIO $ "instrument has " <> showt inputs
            <> " controls, but was given " <> showt (length controlLengths)
    let (controls, lens) = unzip controlLengths
    let frames = end - start
    outputs <- patchOutputs (asPatch inst)
    buffer_fptrs <- mapM Foreign.mallocForeignPtrArray
        (replicate outputs frames)
    -- Holy manual memory management, Batman.
    CUtil.withForeignPtrs buffer_fptrs $ \buffer_ptrs ->
        withArray buffer_ptrs $ \bufferp ->
        withArray controls $ \controlsp ->
        withArray (map CUtil.c_int lens) $ \lensp ->
            c_faust_render inst (CUtil.c_int start) (CUtil.c_int end)
                controlsp lensp bufferp
    return $ map (\fptr -> Vector.Storable.unsafeFromForeignPtr0 fptr frames)
        buffer_fptrs

-- void faust_render(Instrument inst, int start_frame, int end_frame,
--     const ControlSample **controls, const int *control_lengths,
--     float **output)
foreign import ccall "faust_render"
    c_faust_render :: Instrument -> CInt -> CInt
        -> Ptr (Ptr Sample) -> Ptr CInt -> Ptr (Ptr Float) -> IO ()

-- * util

peekTexts :: Int -> Ptr CString -> IO [Text]
peekTexts count textp = do
    texts <- peekArray count textp
    mapM CUtil.peekCString texts
