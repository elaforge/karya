-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Low level binding to faust dsps, specialized for ones that process
-- audio, rather than generating it.
module Synth.Faust.EffectC (
    EffectT(_name, _doc, _controls)
    , Patch, Effect
    -- * Patch
    , getPatches, getPatch
    -- * Effect
    , allocate, destroy
    , render
    -- ** state
    , getState, unsafeGetState, putState
) where
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector.Storable as V

import qualified Util.Audio.Audio as Audio
import qualified Util.Seq as Seq
import qualified Synth.Faust.PatchC as PatchC
import qualified Synth.Lib.Checkpoint as Checkpoint
import qualified Synth.Shared.Control as Control

import           ForeignC
import           Global


data EffectT ptr cptr = EffectT {
    _name :: !Text
    , _doc :: !Text
    , _controls :: !(Map Control.Control (cptr, Text))
    , _ptr :: !ptr
    } deriving (Show)

-- | A patch can be used to create 'Effect's.
type Patch = EffectT PatchC.PatchP ()

-- | An allocated patch.
type Effect = EffectT PatchC.InstrumentP (Ptr Float)


-- * Patch

-- | Get all patches and their names.
getPatches :: IO (Map Text (Either Text Patch))
getPatches = do
    names <- filter ("effect-" `Text.isPrefixOf`) . map fst <$>
        PatchC.patches
    patches <- mapM getPatch names
    return $ Map.fromList $ Seq.map_maybe_snd id $ zip names patches

getPatch :: Text -> IO (Maybe (Either Text Patch))
getPatch name = do
    namePatches <- PatchC.patches
    case lookup name namePatches of
        Nothing -> return Nothing
        Just patch -> Just <$> getPatchP name patch

getPatchP :: Text -> PatchC.PatchP -> IO (Either Text Patch)
getPatchP name patch = do
    meta <- PatchC.getMetadata patch
    ui <- PatchC.getUiControls patch
    let inputs = PatchC.patchInputs patch
        outputs = PatchC.patchOutputs patch
    return $ makePatch name meta (map (first snd) ui) inputs outputs patch

makePatch :: Text -> Map Text Text -> [(Control.Control, Text)]
    -> Int -> Int -> PatchC.PatchP -> Either Text Patch
makePatch name meta uis inputs outputs ptr = first ((name <> ": ")<>) $ do
    unless (inputs == 2) $
        Left $ "expected 2 inputs, got " <> showt inputs
    unless (outputs == 2) $
        Left $ "expected 2 outputs, got " <> showt outputs
    return $ EffectT
        { _name = name
        , _doc = PatchC.parseDescription meta
        , _controls =
            Map.fromList [(control, ((), doc)) | (control, doc) <- uis]
        , _ptr = ptr
        }

-- * Effect

allocate :: Patch -> IO Effect
allocate patch = do
    (ptr, controls) <- PatchC.allocate (_ptr patch)
    return $ patch
        { _ptr = ptr
        , _controls = Map.fromList
            [ (control, (cptr, getDoc control))
            | ((_, control), cptr) <- controls
            ]
        }
    where
    -- A Nothing means getUiControls returned different controls than it did
    -- when the 'Patch' was constructed, which should not happen.
    getDoc c = maybe "" snd $ Map.lookup c (_controls patch)

destroy :: Effect -> IO ()
destroy = PatchC.c_faust_destroy . _ptr

-- | Render chunk of time and return samples.
render :: Audio.Frames -> Audio.Frames -> Effect
    -> [(Ptr Float, Audio.Block)]
    -> [V.Vector Float] -- ^ Input signals.  The length must be equal to the
    -- the patchInputs, and each vector must have the same length.
    -> IO [V.Vector Float] -- ^ one block of samples for each output channel
render controlSize controlsPerBlock effect =
    PatchC.render controlSize controlsPerBlock (_ptr effect)

-- ** state

getState :: Effect -> IO Checkpoint.State
getState = PatchC.getState . _ptr

-- | 'getState', but without copying, if you promise to finish with the State
-- before you call 'render', which will change it.
unsafeGetState :: Effect -> IO Checkpoint.State
unsafeGetState = PatchC.unsafeGetState . _ptr

putState :: Effect -> Checkpoint.State -> IO ()
putState effect state = PatchC.putState state (_name effect) (_ptr effect)
