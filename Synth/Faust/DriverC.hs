-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
-- | Low level binding to driver.cc.
module Synth.Faust.DriverC (
    PatchT(..), Patch, Instrument
    , Control
    , getPatches
    , imControls
    , ControlConfig(..)
    -- * Instrument
    , withInstrument, allocate, destroy
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
import qualified Util.Seq as Seq

import qualified Synth.Lib.Checkpoint as Checkpoint
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Control as Control

import qualified Ui.Id as Id

import           ForeignC
import           Global


data PatchT ptr cptr = Patch {
    _name :: !Text
    , _doc :: !Text
    -- | Corresponds to 'Instrument.Common.Triggered' flag.
    , _impulseGate :: !Bool
    , _elementFrom :: !(Maybe Text)
    -- | An allocated Instrument has pointers to set control values, but a
    -- Patch doesn't.
    , _controls :: !(Map Control (cptr, ControlConfig))
    -- | Inputs are positional, so it's important to preserve their order.
    -- TODO: Inputs should also have an Element.
    , _inputControls :: ![(Control.Control, ControlConfig)]
    , _outputs :: !Int
    , _ptr :: !ptr
    } deriving (Show)

-- | A patch can be used to create 'Instrument's.
type Patch = PatchT PatchP ()
type PatchP = Ptr CConstPatchP
data CConstPatchP

-- | An allocated patch.
type Instrument = PatchT InstrumentP (Ptr Float)
type InstrumentP = Ptr CPatchP
data CPatchP

type Control = (Element, Control.Control)
type Element = Text

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
    -- I use hyphens, but C doesn't allow them in names.
    names <- return $ map (Text.replace "_" "-") names
    return $ Map.fromList $ zip names $
        List.zipWith6 makePatch names metas uis inputs outputs patches

-- int faust_patches(const Patch ***patches);
foreign import ccall "faust_patches"
    c_faust_patches :: Ptr (Ptr PatchP) -> IO CInt

makePatch :: Text -> Map Text Text -> [(Control, Text)]
    -> Int -> Int -> PatchP -> Either Text Patch
makePatch name meta uis inputs outputs ptr = first ((name <> ": ")<>) $ do
    (doc, inputControls) <- parseMetadata meta
    unless (length inputControls == inputs) $
        Left $ "input count " <> showt inputs
            <> " doesn't match inputControls: "
            <> pretty (map fst inputControls)
    unless (outputs `elem` [1, 2]) $
        Left $ "expected 1 or 2 outputs, got " <> showt outputs
    let dups = Seq.find_dups id (map fst inputControls) in unless (null dups) $
        Left $ "duplicate input names: " <> pretty (map fst dups)
    whenJust (verifyControls (map fst uis)) $ \err ->
        Left $ "controls " <> pretty (map fst uis) <> ": " <> err
    impulse <- case Map.findWithDefault "" "flags" meta of
        "" -> Right False
        "impulse-gate" -> Right True
        val -> Left $ "unknown flags, only 'impulse-gate' is supported: " <> val
    return $ Patch
        { _name = name
        , _doc = doc
        , _impulseGate = impulse
        , _elementFrom = Map.lookup "element_from" meta
        , _controls = Map.fromList
            [ ((elt, control), ((), ControlConfig False cdoc))
            | ((elt, control), cdoc) <- uis
            ]
        , _inputControls = inputControls
        , _outputs = outputs
        , _ptr = ptr
        }

-- | Map supported controls to ControlConfig.  This is for the Im.Patch.
imControls :: PatchT ptr cptr -> Map Control.Control ControlConfig
imControls patch =
    -- Control.gate is generated internally, and everyone gets Control.vol.
    Map.delete Control.gate $ Map.insert Control.volume vol $
        Map.fromList (_inputControls patch) <> ui_controls
    where
    ui_controls = Map.fromList $ do
        (control, controls@((_, (_, config)) : _)) <- by_control
        let elts = filter (/="") $ map (fst . fst) controls
        return $ (control,) $ config
            { _description =
                (if null elts then ""
                    else "elements: [" <> Text.intercalate ", " elts <> "], ")
                <> _description config
            }
    by_control = Seq.keyed_group_sort (snd . fst) $
        Map.toList $ _controls patch
    vol = ControlConfig False "Instrument volume, handled by faust-im."

verifyControls :: [Control] -> Maybe Text
verifyControls controls
    | not (null dups) = Just $ "duplicate (element, control): " <> pretty dups
    | otherwise = case Seq.group_fst $ filter ((/="") . fst) controls of
        [] -> Nothing
        (_, cs1) : rest -> case List.find ((/=cs1) . snd) rest of
            Nothing -> Nothing
            Just (elt2, cs2) -> Just $ "every element should have "
                <> pretty cs1 <> " but " <> elt2 <> " has " <> pretty cs2
    where dups = Seq.find_dups id controls

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

getUiControls :: PatchP -> IO [(Control, Text)]
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

    -- The first path component is always the patch name, so I don't need it.
    -- underscore prefixed controls are used for UI organization, and are not
    -- the element.
    let element = Text.intercalate "." . filter (not . ("_" `Text.isPrefixOf`))
            . drop 1
    -- Lead with "_" to suppress the whole control.  But "_" was turned to "-".
    return $ filter (not . ("-" `Text.isPrefixOf`) . pretty . snd . fst) $
        zip (zip (map element paths) controls) docs

-- int faust_controls(Patch patch, const char ****out_paths,
--     const char ***out_controls, char ***out_docs)
foreign import ccall "faust_controls"
    c_faust_controls :: PatchP -> Ptr (Ptr (Ptr CString)) -> Ptr (Ptr CString)
        -> Ptr (Ptr CString) -> IO CInt

-- int faust_control_ptrs(Patch *inst, FAUSTFLOAT ***out_vals)
foreign import ccall "faust_control_ptrs"
    c_faust_control_ptrs :: InstrumentP -> Ptr (Ptr (Ptr Float)) -> IO CInt

withInstrument :: Patch -> (Instrument -> IO a) -> IO a
withInstrument patch = Exception.bracket (allocate patch) destroy

allocate :: Patch -> IO Instrument
allocate patch = do
    ptr <- c_faust_allocate (_ptr patch) (CUtil.c_int Config.samplingRate)
    cptrs <- alloca $ \cptrspp -> do
        count <- c_faust_control_ptrs ptr cptrspp
        cptrsp <- peek cptrspp
        cptrs <- peekArray (fromIntegral count) cptrsp
        free cptrsp
        return cptrs
    -- I need an allocated instrument to get valid control pointers.  But since
    -- I already changed their order to put them in a Map, I have to get the
    -- control names again, which is inefficient but easy to do.
    uis <- getUiControls (_ptr patch)
    return $ patch
        { _ptr = ptr
        , _controls = Map.fromList
            [ ((elt, control), (cptr, ControlConfig False cdoc))
            | (cptr, ((elt, control), cdoc)) <- zip cptrs uis
            ]
        }

-- Patch *faust_allocate(const Patch *patch, int srate);
foreign import ccall "faust_allocate"
    c_faust_allocate :: PatchP -> CInt -> IO InstrumentP

destroy :: Instrument -> IO ()
destroy = c_faust_destroy . _ptr
-- void faust_destroy(Patch *patch) { delete patch; }
foreign import ccall "faust_destroy" c_faust_destroy :: InstrumentP -> IO ()

foreign import ccall "faust_name" c_faust_name :: PatchP -> CString

foreign import ccall "faust_num_inputs" c_faust_num_inputs :: PatchP -> CInt
foreign import ccall "faust_num_outputs" c_faust_num_outputs :: PatchP -> CInt

-- | Render chunk of time and return samples.
render :: Audio.Frames -> Audio.Frames -> Instrument
    -> [(Ptr Float, Audio.Block)]
    -> [V.Vector Float] -- ^ Input signals.  The length must be equal to the
    -- the patchInputs, and each vector must have the same length.
    -> IO [V.Vector Float] -- ^ one block of samples for each output channel
render controlSize controlsPerBlock inst controls inputs = do
    unless (length inputs == length (_inputControls inst)) $
        errorIO $ "instrument expects " <> showt (length (_inputControls inst))
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
        (replicate (_outputs inst) (unframe blockSize))
    -- Holy manual memory management, Batman.
    CUtil.withForeignPtrs outputFptrs $ \outputPtrs ->
        withVectors inputs $ \inputsP ->
        withArray outputPtrs $ \outputsP ->
        withControls controls $ \controlCount controlPtrs controlValsP ->
            c_faust_render (_ptr inst)
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
            errorIO $ "inst " <> showt (_name inst) <> " expects state size "
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
withPtrs vs f = go [] vs
    where
    go accum [] = f (reverse accum)
    go accum (v:vs) = Foreign.withForeignPtr fptr $ \ptr ->
        go (ptr : accum) vs
        where (fptr, _len) = V.unsafeToForeignPtr0 v
