-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Low level binding to driver.cc.
module Synth.Faust.DriverC (
    Patch, Instrument, asPatch
    , getPatches
    , ControlConfig(..), getParsedMetadata
    , getControls
    , getUiControls
    -- * Instrument
    , withInstrument, initialize, destroy
    , patchInputs, patchOutputs
    , render
    -- ** state
    , getState, putState
) where
import qualified Control.Exception as Exception
import qualified Data.ByteString as ByteString
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector.Storable as V

import qualified Foreign
import ForeignC
import qualified System.IO.Unsafe as Unsafe

import qualified Util.Audio.Audio as Audio
import qualified Util.CUtil as CUtil
import qualified Util.Doc as Doc
import qualified Util.Seq as Seq

import qualified Ui.Id as Id
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Control as Control
import Global


-- | A patch can be used to create 'Instrument's.
type Patch = Ptr ConstPatchP
data ConstPatchP
-- TODO Show instance that shows the name

-- | An allocated patch.
type Instrument = Ptr PatchP
data PatchP

-- | A Patch is just a const Instrument.
asPatch :: Instrument -> Patch
asPatch = Foreign.castPtr

-- | Get all patches and their names.
getPatches :: IO (Map Text Patch)
getPatches = alloca $ \patchpp -> do
    count <- fromIntegral <$> c_faust_patches patchpp
    patches <- peekArray count =<< peek patchpp
    names <- mapM patchName patches
    return $ Map.fromList $ zip names patches

-- int faust_patches(const Patch ***patches);
foreign import ccall "faust_patches"
    c_faust_patches :: Ptr (Ptr Patch) -> IO CInt

data ControlConfig = ControlConfig {
    _constant :: !Bool
    , _description :: !Text
    } deriving (Eq, Show)

instance Pretty ControlConfig where
    pretty (ControlConfig constant desc) =
        (if constant then "(per-note constant): " else "") <> desc

getParsedMetadata :: Patch
    -> IO (Either Text (Doc.Doc, [(Control.Control, ControlConfig)]))
getParsedMetadata patch = do
    let inputs = patchInputs patch
    let outputs = patchOutputs patch
    meta <- getMetadata patch
    return $ do
        (doc, controls) <- parseMetadata meta
        unless (length controls == inputs) $
            Left $ "input count " <> showt inputs
                <> " doesn't match controls: " <> pretty (map fst controls)
        unless (outputs `elem` [1, 2]) $
            Left $ "expected 1 or 2 outputs, got " <> showt outputs
        -- Control.gate is used internally, so don't expose that.
        return (Doc.Doc doc, amp : filter ((/=Control.gate) . fst) controls)
    where
    amp =
        ( Control.volume
        , ControlConfig False "Instrument volume, handled by faust-im."
        )

parseMetadata :: Map Text Text
    -> Either Text (Text, [(Control.Control, ControlConfig)])
parseMetadata meta =
    (Map.findWithDefault "" "description" meta ,) <$> metadataControls meta

-- | Get control names from the faust metadata.
--
-- The convention is that controls are declared via:
--
-- > declare control#_name "Doc."
--
-- The # is so they sort in the same order as the input signals to which they
-- correspond.  If the Doc starts with constant:, this is a constant control,
-- sampled at the note attack time.
metadataControls :: Map Text Text
    -> Either Text [(Control.Control, ControlConfig)]
    -- ^ this is in the declaration order, which should be the same as the dsp
    -- input order
metadataControls = check <=< mapMaybeM parse . Map.toAscList
    where
    check controls
        | Control.volume `elem` map fst controls =
            Left $ pretty Control.volume <> " shadowed by internal use"
        | null dups = Right controls
        | otherwise = Left $ "duplicate controls: "
            <> Text.intercalate ", " (map (pretty . fst) dups)
        where (_, dups) = Seq.partition_dups fst controls
    parse (c, desc)
        | c == "description" = Right Nothing
        | Id.valid_symbol stripped =
            Right $ Just (Control.Control stripped, parseControlText desc)
        | otherwise = Left $ "invalid control name: " <> c
        where stripped = Text.drop 1 $ Text.dropWhile (/='_') c

parseControlText :: Text -> ControlConfig
parseControlText desc
    | Just desc <- Text.stripPrefix "constant:" desc = ControlConfig True desc
    | otherwise = ControlConfig False desc


-- | Get supported controls.  The order is important, since it's the same order
-- 'render' expects to see them.
getControls :: Patch -> [Control.Control]
getControls = either (const []) (map fst) . metadataControls
    . Unsafe.unsafePerformIO . getMetadata

getMetadata :: Patch -> IO (Map Text Text)
getMetadata patch = alloca $ \keyspp -> alloca $ \valuespp -> do
    count <- fromIntegral <$> c_faust_metadata patch keyspp valuespp
    kvs <- zip <$> (peekTexts count =<< peek keyspp)
        <*> (peekTexts count =<< peek valuespp)
    free =<< peek keyspp
    free =<< peek valuespp
    return $ Map.fromList kvs

-- int faust_metadata(
--     const Patch *patch, const char ***keys, const char ***values);
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

-- Patch *faust_initialize(const Patch *patch, int srate);
foreign import ccall "faust_initialize"
    c_faust_initialize :: Patch -> CInt -> IO Instrument

destroy :: Instrument -> IO ()
destroy = c_faust_destroy
-- void faust_destroy(Patch *patch) { delete patch; }
foreign import ccall "faust_destroy" c_faust_destroy :: Instrument -> IO ()

patchName :: Patch -> IO Text
patchName = CUtil.peekCString . c_faust_name

foreign import ccall "faust_name" c_faust_name :: Patch -> CString

patchInputs, patchOutputs :: Patch -> Int
patchInputs = fromIntegral . c_faust_num_inputs
patchOutputs = fromIntegral . c_faust_num_outputs

foreign import ccall "faust_num_inputs" c_faust_num_inputs :: Patch -> CInt
foreign import ccall "faust_num_outputs" c_faust_num_outputs :: Patch -> CInt

-- | Render chunk of time and return samples.  The chunk size is determined by
-- the input controls, or 'Audio.chunkSize' if there are none.
render :: Instrument -> [V.Vector Float] -- ^ the length must be equal to the
    -- the patchInputs, and each vector must have the same length
    -> IO [V.Vector Float] -- ^ one chunk of samples for each output channel
render inst controls = do
    let inputs = patchInputs (asPatch inst)
    unless (length controls == inputs) $
        errorIO $ "instrument has " <> showt inputs
            <> " controls, but was given " <> showt (length controls)
    unless (all ((== V.length (head controls)) . V.length) controls) $
        errorIO $ "all controls don't have the same length: "
            <> pretty (map V.length controls)
    let Audio.Frame frames = maybe Audio.chunkSize (Audio.Frame . V.length)
            (Seq.head controls)
    let outputs = patchOutputs (asPatch inst)
    outFptrs <- mapM Foreign.mallocForeignPtrArray (replicate outputs frames)
    -- Holy manual memory management, Batman.
    CUtil.withForeignPtrs outFptrs $ \outPtrs ->
        withPtrs controls $ \controlPs _lens ->
        withArray outPtrs $ \outsP ->
        withArray controlPs $ \controlsP ->
            c_faust_render inst (CUtil.c_int frames) controlsP outsP
    return $ map (\fptr -> V.unsafeFromForeignPtr0 fptr frames) outFptrs

-- void faust_render(
--     Patch *patch, int frames, const float **controls, float **outputs);
foreign import ccall "faust_render"
    c_faust_render :: Instrument -> CInt -> Ptr (Ptr Float) -> Ptr (Ptr Float)
        -> IO ()

withPtrs :: [V.Vector Float] -> ([Ptr Float] -> [Int] -> IO a) -> IO a
withPtrs vs f = go [] vs
    where
    go accum [] = f ptrs lens
        where (ptrs, lens) = unzip (reverse accum)
    go accum (v:vs) = Foreign.withForeignPtr fptr $ \ptr ->
        go ((ptr, len) : accum) vs
        where (fptr, len) = V.unsafeToForeignPtr0 v

-- ** state

getState :: Instrument -> IO ByteString.ByteString
getState inst = alloca $ \statepp -> do
    c_faust_get_state inst statepp
    statep <- peek statepp
    ByteString.packCStringLen
        (statep, fromIntegral $ c_faust_get_state_size inst)

putState :: ByteString.ByteString -> Instrument -> IO ()
putState state inst = ByteString.useAsCStringLen state $ \(statep, size) -> do
    let psize = c_faust_get_state_size inst
    unless (fromIntegral size == psize) $
        errorIO $ "inst " <> showt inst <> " expects state size "
            <> showt psize <> " but got " <> showt size
    c_faust_put_state inst statep

-- size_t faust_get_state_size(const Patch *patch) { return patch->size; }
foreign import ccall "faust_get_state_size"
    c_faust_get_state_size :: Instrument -> CSize

-- size_t faust_get_state(const Patch *patch, const char **state);
foreign import ccall "faust_get_state"
    c_faust_get_state :: Instrument -> Ptr CString -> IO ()

-- void faust_put_state(Patch *patch, const char *state);
foreign import ccall "faust_put_state"
    c_faust_put_state :: Instrument -> CString -> IO ()

-- * util

peekTexts :: Int -> Ptr CString -> IO [Text]
peekTexts count textp = do
    texts <- peekArray count textp
    mapM CUtil.peekCString texts
