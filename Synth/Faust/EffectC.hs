-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Low level binding to faust dsps, specialized for ones that process
-- audio, rather than generating it.
module Synth.Faust.EffectC (
    EffectT(_name, _doc, _controls)
    , Patch, Effect
    , State
    -- * Patch
    , patches
    -- * Effect
    , allocate, destroy
    , render
    -- ** state
    , getState, unsafeGetState, putState
) where
import qualified Data.ByteString as ByteString
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector.Storable as V

import qualified System.IO.Unsafe as Unsafe

import qualified Util.Audio.Audio as Audio
import qualified Util.Serialize as Serialize
import qualified Synth.Faust.PatchC as PatchC
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note

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

-- | All configured effects.
patches :: Map Text (Either Text Patch)
patches = Unsafe.unsafePerformIO $ do
    namePatches <- filter (("effect-" `Text.isPrefixOf`) . fst) <$>
        PatchC.patches
    return $ Map.fromList $
        zip (map (Text.drop (Text.length "effect-") . fst) namePatches)
            (map (uncurry getPatchP) namePatches)
    -- unsafePerformIO is ok for these since they are just looking up static
    -- data from C.

getPatchP :: Text -> PatchC.PatchP -> Either Text Patch
getPatchP name patch = Unsafe.unsafePerformIO $ do
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

newtype State = State ByteString.ByteString
    deriving (Serialize.Serialize)

instance Pretty State where
    pretty (State bytes) = txt $ Note.fingerprintBytes bytes

getState :: Effect -> IO State
getState = fmap State . PatchC.getState . _ptr

-- | 'getState', but without copying, if you promise to finish with the State
-- before you call 'render', which will change it.
unsafeGetState :: Effect -> IO State
unsafeGetState = fmap State . PatchC.unsafeGetState . _ptr

putState :: Effect -> State -> IO ()
putState effect (State state) =
    PatchC.putState state (_name effect) (_ptr effect)
