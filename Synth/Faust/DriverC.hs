-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Low level binding to driver.cc.
module Synth.Faust.DriverC (
    PatchT(..), Patch, Instrument
    , Control
    -- * Patch
    , getPatches
    , imControls
    , ControlConfig(..)
    -- * Instrument
    , allocate, destroy
    , render
    -- ** state
    , getState, unsafeGetState, putState
) where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector.Storable as V

import qualified Util.Audio.Audio as Audio
import qualified Util.Seq as Seq
import qualified Synth.Faust.PatchC as PatchC
import qualified Synth.Lib.Checkpoint as Checkpoint
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
type Patch = PatchT PatchC.PatchP ()

-- | An allocated patch.
type Instrument = PatchT PatchC.InstrumentP (Ptr Float)

data ControlConfig = ControlConfig {
    _constant :: !Bool
    , _description :: !Text
    } deriving (Eq, Show)

instance Pretty ControlConfig where
    pretty (ControlConfig constant desc) =
        (if constant then "(per-note constant): " else "") <> desc

type Control = (Element, Control.Control)
type Element = Text


-- * Patch

-- | Get all patches and their names.
getPatches :: IO (Map Text (Either Text Patch))
getPatches = do
    (names, patches) <- unzip <$> PatchC.patches
    metas <- mapM PatchC.getMetadata patches
    uis <- mapM getUiControls patches
    let inputs = map PatchC.patchInputs patches
        outputs = map PatchC.patchOutputs patches
    return $ Map.fromList $ filter (not . ("effect-" `Text.isPrefixOf`) . fst) $
        zip names $
        List.zipWith6 makePatch names metas uis inputs outputs patches

makePatch :: Text -> Map Text Text -> [(Control, Text)]
    -> Int -> Int -> PatchC.PatchP -> Either Text Patch
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

parseMetadata :: Map Text Text
    -> Either Text (Text, [(Control.Control, ControlConfig)])
parseMetadata meta = (PatchC.parseDescription meta,) <$> metadataControls meta

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

getUiControls :: PatchC.PatchP -> IO [(Control, Text)]
getUiControls patch = map (first makeControl) <$> PatchC.getUiControls patch

makeControl :: ([Text], Control.Control) -> Control
makeControl = first $ Text.intercalate "."
    . filter (not . ("_" `Text.isPrefixOf`)) . drop 1
    -- The first path component is always the patch name, so I don't need it.
    -- underscore prefixed controls are used for UI organization, and are not
    -- the element.

-- * Instrument

allocate :: Patch -> IO Instrument
allocate patch = do
    (ptr, controls) <- PatchC.allocate (_ptr patch)
    return $ patch
        { _ptr = ptr
        , _controls = Map.fromList
            [ (control, (cptr, getConfig control))
            | (control, cptr) <- map (first makeControl) controls
            ]
        }
    where
    -- A Nothing means getUiControls returned different controls than it did
    -- when the 'Patch' was constructed, which should not happen.
    getConfig c = maybe (ControlConfig False "") snd $
        Map.lookup c (_controls patch)

destroy :: Instrument -> IO ()
destroy = PatchC.c_faust_destroy . _ptr

-- | Render chunk of time and return samples.
render :: Audio.Frames -> Audio.Frames -> Instrument
    -> [(Ptr Float, Audio.Block)]
    -> [V.Vector Float] -- ^ Input signals.  The length must be equal to the
    -- the patchInputs, and each vector must have the same length.
    -> IO [V.Vector Float] -- ^ one block of samples for each output channel
render controlSize controlsPerBlock inst =
    PatchC.render controlSize controlsPerBlock (_ptr inst)

-- * state

getState :: Instrument -> IO Checkpoint.State
getState = fmap Checkpoint.State . PatchC.getState . _ptr

-- | 'getState', but without copying, if you promise to finish with the State
-- before you call 'render', which will change it.
unsafeGetState :: Instrument -> IO Checkpoint.State
unsafeGetState = fmap Checkpoint.State . PatchC.unsafeGetState . _ptr

putState :: Instrument -> Checkpoint.State -> IO ()
putState inst (Checkpoint.State state) =
    PatchC.putState state (_name inst) (_ptr inst)
