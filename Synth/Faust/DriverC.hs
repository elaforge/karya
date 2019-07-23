-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Low level binding to driver.cc.
module Synth.Faust.DriverC (
    PatchT(..), Patch, Instrument
    , getPatches
    , ControlConfig(..)
    -- * Instrument
    , withInstrument, initialize, destroy
    , render
    -- ** state
    , getState, unsafeGetState, putState
) where
import qualified Control.Exception as Exception
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Unsafe as ByteString.Unsafe
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector.Storable as V

import qualified Foreign

import qualified Util.Audio.Audio as Audio
import qualified Util.CUtil as CUtil
import qualified Util.Map
import qualified Util.Num as Num
import qualified Util.Seq as Seq

import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Lib.Checkpoint as Checkpoint
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Signal as Signal

import qualified Ui.Id as Id

import           ForeignC
import           Global


data PatchT ptr = Patch {
    _name :: !Text
    , _doc :: !Text
    , _controls :: !(Map Control.Control (Ptr CFloat, ControlConfig))
    , _inputControls :: ![(Control.Control, ControlConfig)]
    , _inputs :: !Int
    , _outputs :: !Int
    , _ptr :: !ptr
    } deriving (Show)

-- | A patch can be used to create 'Instrument's.
type Patch = PatchT PatchP
type PatchP = Ptr CConstPatchP
data CConstPatchP

-- | An allocated patch.
type Instrument = PatchT InstrumentP
type InstrumentP = Ptr CPatchP
data CPatchP

-- | Get all patches and their names.
getPatches :: IO (Map Text (Either Text Patch))
getPatches = alloca $ \patchpp -> do
    count <- fromIntegral <$> c_faust_patches patchpp
    patches <- peekArray count =<< peek patchpp
    names <- mapM (CUtil.peekCString . c_faust_name) patches
    metas <- mapM getMetadata patches
    uis <- mapM getUiControls patches
    let inputs = map (fromIntegral . c_faust_num_inputs) patches
        outputs = map (fromIntegral . c_faust_num_outputs) patches
    return $ Map.fromList $ zip names $
        List.zipWith6 makePatch names metas uis inputs outputs patches

-- int faust_patches(const Patch ***patches);
foreign import ccall "faust_patches"
    c_faust_patches :: Ptr (Ptr PatchP) -> IO CInt

makePatch :: Text -> Map Text Text -> [(Control.Control, (Ptr CFloat, Text))]
    -> Int -> Int -> PatchP -> Either Text Patch
makePatch name meta uis inputs outputs ptr = first ((name <> ": ")<>) $ do
    (doc, inputControls) <- parseMetadata meta
    unless (length inputControls == inputs) $
        Left $ "input count " <> showt inputs
            <> " doesn't match inputControls: "
            <> pretty (map fst inputControls)
    unless (outputs `elem` [1, 2]) $
        Left $ "expected 1 or 2 outputs, got " <> showt outputs
    let dups = Seq.find_dups id $ map fst inputControls ++ map fst uis
    unless (null dups) $
        Left $ "duplicate control names: " <> pretty (map fst dups)
            <> " in " <> pretty inputControls <> " and " <> pretty (map fst uis)
    let controls = Map.fromList
            -- I don't support constant for ui controls but I could.  Would it
            -- be useful?
            [ (control, (cptr, ControlConfig False cdoc))
            | (control, (cptr, cdoc)) <- uis
            ]
    return $ Patch
        { _name = name
        , _doc = doc
        , _controls = controls
        -- Control.gate is used internally, so don't expose that.
        -- And everyone gets amp, since faust-im handles it.
        , _inputControls = amp : filter ((/=Control.gate) . fst) inputControls
        , _inputs = inputs
        , _outputs = outputs
        , _ptr = ptr
        }
    where
    amp =
        ( Control.volume
        , ControlConfig False "Instrument volume, handled by faust-im."
        )

data ControlConfig = ControlConfig {
    _constant :: !Bool
    , _description :: !Text
    } deriving (Eq, Show)

instance Pretty ControlConfig where
    pretty (ControlConfig constant desc) =
        (if constant then "(per-note constant): " else "") <> desc

parseMetadata :: Map Text Text
    -> Either Text (Text, [(Control.Control, ControlConfig)])
parseMetadata meta = (parseDescription meta,) <$> metadataControls meta

parseDescription :: Map Text Text -> Text
parseDescription meta = Text.intercalate "\n" $ filter (/="") $
    Map.findWithDefault "" "description" meta
    : map (\k -> maybe "" ((k <> ": ")<>) $ Map.lookup k meta)
        ["author", "copyright", "version", "license"]

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
        | Just rest <- Text.stripPrefix "control" c =
            let stripped = Text.drop 1 $ Text.dropWhile (/='_') rest
            in if Id.valid_symbol stripped
                then Right $
                    Just (Control.Control stripped, parseControlText desc)
                else Left $ "invalid control name: " <> c
        | otherwise = Right Nothing

parseControlText :: Text -> ControlConfig
parseControlText desc
    | Just desc <- Text.stripPrefix "constant:" desc = ControlConfig True desc
    | otherwise = ControlConfig False desc

getMetadata :: PatchP -> IO (Map Text Text)
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
    c_faust_metadata :: PatchP -> Ptr (Ptr CString) -> Ptr (Ptr CString)
        -> IO CInt

getUiControls :: PatchP -> IO [(Control.Control, (Ptr CFloat, Text))]
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
    return $ zip controls (zip vals docs)

-- int faust_controls(Patch patch, const char ***out_controls, char ***out_docs,
--     FAUSTFLOAT ***out_vals)
foreign import ccall "faust_controls"
    c_faust_controls :: PatchP -> Ptr (Ptr CString) -> Ptr (Ptr CString)
        -> Ptr (Ptr (Ptr CFloat)) -> IO CInt

withInstrument :: Patch -> (Instrument -> IO a) -> IO a
withInstrument patch = Exception.bracket (initialize patch) destroy

initialize :: Patch -> IO Instrument
initialize patch = do
    ptr <- c_faust_initialize (_ptr patch) (CUtil.c_int Config.samplingRate)
    return $ patch { _ptr = ptr }

-- Patch *faust_initialize(const Patch *patch, int srate);
foreign import ccall "faust_initialize"
    c_faust_initialize :: PatchP -> CInt -> IO InstrumentP

destroy :: Instrument -> IO ()
destroy = c_faust_destroy . _ptr
-- void faust_destroy(Patch *patch) { delete patch; }
foreign import ccall "faust_destroy" c_faust_destroy :: InstrumentP -> IO ()

foreign import ccall "faust_name" c_faust_name :: PatchP -> CString

foreign import ccall "faust_num_inputs" c_faust_num_inputs :: PatchP -> CInt
foreign import ccall "faust_num_outputs" c_faust_num_outputs :: PatchP -> CInt

-- | Render chunk of time and return samples.  The block size is determined by
-- the inputs, or 'Audio.blockSize' if there are none.
render :: Instrument
    -> Audio.Frame
    -> Map Control.Control Signal.Signal
    -> [V.Vector Float] -- ^ Input signals.  The length must be equal to the
    -- the patchInputs, and each vector must have the same length.
    -> IO [V.Vector Float] -- ^ one block of samples for each output channel
render inst start controls inputs = do
    unless (length inputs == _inputs inst) $
        errorIO $ "instrument expects " <> showt (_inputs inst)
            <> " inputs, but was given " <> showt (length inputs)
    unless (all ((== V.length (head inputs)) . V.length) inputs) $
        errorIO $ "all inputs don't have the same length: "
            <> pretty (map V.length inputs)
    let Audio.Frame frames = maybe Audio.blockSize (Audio.Frame . V.length)
            (Seq.head inputs)
    outputFptrs <- mapM Foreign.mallocForeignPtrArray
        (replicate (_outputs inst) frames)
    setControls start controls (_controls inst)
    -- Holy manual memory management, Batman.
    CUtil.withForeignPtrs outputFptrs $ \outputPtrs ->
        withPtrs inputs $ \inputPs _lens ->
        withArray outputPtrs $ \outputsP ->
        withArray inputPs $ \inputP ->
            c_faust_render (_ptr inst) (CUtil.c_int frames) inputP outputsP
    return $ map (\fptr -> V.unsafeFromForeignPtr0 fptr frames) outputFptrs

-- void faust_render(
--     Patch *patch, int frames, const float **inputs, float **outputs);
foreign import ccall "faust_render"
    c_faust_render :: InstrumentP -> CInt -> Ptr (Ptr Float) -> Ptr (Ptr Float)
        -> IO ()

withPtrs :: [V.Vector Float] -> ([Ptr Float] -> [Int] -> IO a) -> IO a
withPtrs vs f = go [] vs
    where
    go accum [] = f ptrs lens
        where (ptrs, lens) = unzip (reverse accum)
    go accum (v:vs) = Foreign.withForeignPtr fptr $ \ptr ->
        go ((ptr, len) : accum) vs
        where (fptr, len) = V.unsafeToForeignPtr0 v

setControls :: Audio.Frame -> Map Control.Control Signal.Signal
    -> Map Control.Control (Ptr CFloat, a) -> IO ()
setControls start signals controls =
    forM_ (Util.Map.zip_intersection signals controls) $
        \(_, signal, (ptr, _)) -> poke ptr (get signal)
    where get = CUtil.c_float . Num.d2f . Signal.at (AUtil.toSeconds start)

-- ** state

getState :: Instrument -> IO Checkpoint.State
getState inst = alloca $ \statepp -> do
    c_faust_get_state (_ptr inst) statepp
    statep <- peek statepp
    Checkpoint.State <$> ByteString.packCStringLen
        (statep, fromIntegral $ c_faust_get_state_size (_ptr inst))

-- | 'getState', but without copying, if you promise to finish with the State
-- before you call 'render', which will change it.
unsafeGetState :: Instrument -> IO Checkpoint.State
unsafeGetState inst = alloca $ \statepp -> do
    c_faust_get_state (_ptr inst) statepp
    statep <- peek statepp
    Checkpoint.State <$> ByteString.Unsafe.unsafePackCStringLen
        (statep, fromIntegral $ c_faust_get_state_size (_ptr inst))

putState :: Checkpoint.State -> Instrument -> IO ()
putState (Checkpoint.State state) inst =
    ByteString.Unsafe.unsafeUseAsCStringLen state $ \(statep, size) -> do
        let psize = c_faust_get_state_size (_ptr inst)
        unless (fromIntegral size == psize) $
            errorIO $ "inst " <> showt inst <> " expects state size "
                <> showt psize <> " but got " <> showt size
        c_faust_put_state (_ptr inst) statep

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
