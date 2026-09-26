-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Functions to deal with tuning instruments.

    E.g.:

    > LTuning.realtime "pno" =<< LTuning.selection True
    > LTuning.write_ksp (Just "wayang") "charu.ksp"
    >       =<< LTuning.scale True "raga" "key=charukesi"

    nrpn tuning is a lot less hassle than copy pasting KSP everywhere:

    > LTuning.nrpn "pemade" =<< LTuning.selection True

    Don't forget to set the score to the same scale or things will sound
    confusing.  Also, reaper won't receive sysex on a track unless you set it
    to receive all channels.

    To retune all instruments that do tuning via either 'nrpn' or 'realtime':

    > LTuning.retune =<< LTuning.selection True
-}
module Cmd.Repl.LTuning where
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Vector.Unboxed as Unboxed

import qualified Util.Lists as Lists
import qualified Util.Num as Num
import qualified Util.Texts as Texts

import qualified Cmd.Cmd as Cmd
import qualified Cmd.InputNote as InputNote
import qualified Cmd.Perf as Perf
import qualified Cmd.PlayUtil as PlayUtil
import qualified Cmd.Repl.LInst as LInst
import qualified Cmd.Repl.Util as Util
import qualified Cmd.Selection as Selection

import qualified Derive.C.Prelude.Equal as Equal
import qualified Derive.Call as Call
import qualified Derive.Derive as Derive
import qualified Derive.Parse.Instruments as Instruments
import qualified Derive.Scale as Scale
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.Legong as Legong
import qualified Derive.Scale.Wayang as Wayang

import qualified Instrument.Common as Common
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Patch as Patch
import qualified Perform.Pitch as Pitch
import qualified User.Elaforge.Instrument.Kontakt.Util as Kontakt.Util

import           Global
import           Types


-- * Patch.Scale

-- | Format a Patch.Scale as a table.
table :: Patch.Scale -> Text
table scale =
    Text.unlines $ Texts.columns 1 $
        ["", "c", "", "d", "", "e", "f", "", "g", "", "a", "", "b"]
        : [oct : map (Num.showFloat 2) nns | (oct, nns) <- zip octaves groups]
    where
    octaves = map (("c"<>) . showt) [-1..]
    groups = Lists.chunked 12 $ Unboxed.toList $ Patch.scaleKeyToNn scale

-- | Get a patch scale for the scale at the selection.
selection :: Cmd.M m => Bool
    -- ^ False to throw if there are warnings or errors, True to ignore them.
    -> m Patch.Scale
selection ignoreErrors = do
    (blockId, _, trackId, _) <- Selection.getInsert
    (scale, errs) <- scaleAt blockId trackId
    unless (ignoreErrors || null errs) $
        Cmd.throw $ Text.unlines errs
    pure scale

-- | Figure out a 'Patch.Scale' by enumerating all inputs to the scale in
-- scope.
scaleAt :: Cmd.M m => BlockId -> TrackId -> m (Patch.Scale, [Text])
scaleAt blockId trackId = do
    scale <- Perf.deriveAtThrow blockId trackId Call.getScale
    (keyNns, errs) <- fmap unzip $ forM allInputs $ \(key, input) -> do
        let atTime = 0
        (val, logs) <- Perf.deriveAt blockId trackId $
            Scale.scaleInputToNn scale atTime input
        let prefix = (("key " <> pretty key <> ": ") <>)
        pure $ second (map prefix . (++ map pretty logs)) $ case val of
            Left err -> (Nothing, [err])
            Right (Left err) -> (Nothing, [pretty err])
            Right (Right nn) -> (Just (key, nn), [])
    let name = pretty (Scale.scaleId scale)
    pure (Patch.makeScale name (Maybe.catMaybes keyNns), concat errs)

-- | Create a Patch.Scale for the named scale.
named :: Cmd.M m => Bool
    -- ^ False to check for warnings and errors, True to ignore them.
    -> Text -> Text -> m Patch.Scale
named ignoreErrors name transform = do
    scale <- getScale name
    (scale, errs) <- makePatchScale scale transform
    unless (ignoreErrors || null errs) $
        Cmd.throw $ Text.unlines errs
    pure scale

getScale :: Cmd.M m => Text -> m Scale.Scale
getScale name =
    Cmd.require ("scale not found: " <> name)
        =<< Perf.lookupScaleEnv mempty (Pitch.ScaleId name)

makePatchScale :: Cmd.M m => Scale.Scale -> Text -> m (Patch.Scale, [Text])
makePatchScale scale transform = do
    (keyNns, errs) <- fmap unzip $ forM allInputs $ \(key, input) -> do
        let atTime = 0
        (val, logs) <- derive $ Equal.transformExpr transform $
            Scale.scaleInputToNn scale atTime input
        let prefix = (("key " <> pretty key <> ": ") <>)
        pure $ second (map prefix . (++ logs)) $ case val of
            Nothing -> (Nothing, [])
            Just (Left err) -> (Nothing, [pretty err])
            Just (Right nn) -> (Just (key, nn), [])
    let name = pretty (Scale.scaleId scale)
    pure (Patch.makeScale name (Maybe.catMaybes keyNns), concat errs)

derive :: Cmd.M m => Derive.Deriver a -> m (Maybe a, [Text])
derive deriver = do
    (val, _, logs) <- PlayUtil.run mempty mempty deriver
    pure $ case val of
        Left err -> (Nothing, pretty err : map pretty logs)
        Right val -> (Just val, map pretty logs)

allInputs :: [(Midi.Key, Pitch.Input)]
allInputs =
    [(key, InputNote.nnToInput (Midi.fromKey key)) | key <- [0..127]]

-- * retune

-- | All instruments with initialization get the new scale.
retune :: Cmd.M m => Patch.Scale -> m [Util.Instrument]
retune scale = do
    insts <- LInst.listMidi
    inits <- mapM (LInst.instInitialization . Util.instrument) insts
    fmap concat $ forM (zip insts inits) $ \(inst, init) -> case init of
        Nothing -> pure []
        Just Patch.Tuning -> realtime inst scale >> pure [inst]
        Just Patch.NrpnTuning -> nrpn inst scale >> pure [inst]

-- | Show tuning map for debugging.
getTuning :: Cmd.M m => Util.Instrument -> Patch.Scale -> m Text
getTuning inst scale = do
    attrMap@(Common.AttributeMap amap) <- Patch.patchAttributeMap . fst <$>
        Cmd.getMidiInstrument (Util.instrument inst)
    let tuning = Patch.scaleTuning (Just attrMap) scale
    pure $ Text.unlines $ concat
        [ map pretty amap
        , [""]
        , map (Text.unwords . map pretty) (Lists.chunked 6 tuning)
        ]

-- | Set the instrument's Scale to the given scale and send a MIDI tuning
-- message with 'LInst.initialize_realtime_tuning'.
--
-- Since instruments are now defined in ky, this can only return the
-- code to paste in.
realtime :: Cmd.M m => Util.Instrument -> Patch.Scale -> m Text
realtime inst scale = do
    LInst.initializeRealtimeTuning (Util.instrument inst)
    pure $ Text.unlines
        [ ">" <> inst <> " ..."
        , "  { initialization: Tuning }"
        , Instruments.unScale scale
        ]

-- | Just like 'realtime', but send tuning via 'LInst.initialize_nrpn_tuning'.
nrpn :: Cmd.M m => Util.Instrument -> Patch.Scale -> m Text
nrpn inst scale = do
    LInst.initializeNrpnTuning (Util.instrument inst)
    pure $ Text.unlines
        [ ">" <> inst <> " ..."
        , "  { initialization: NrpnTuning }"
        , Instruments.unScale scale
        ]

-- | Write KSP to retune a 12TET patch.  Don't forget to do 'LInst.set_scale'
-- to configure the instrument.
writeKsp :: Maybe Util.Instrument -> FilePath -> Patch.Scale -> Cmd.CmdT IO ()
writeKsp maybeInst filename scale = do
    attrMap <- case maybeInst of
        Nothing -> pure Nothing
        Just inst -> Just . Patch.patchAttributeMap . fst <$>
            Cmd.getMidiInstrument (Util.instrument inst)
    ksp <- Cmd.requireRight id $ Kontakt.Util.tuningKsp attrMap scale
    liftIO $ Text.IO.writeFile filename ksp
    pure ()

writeBaliScalesKsp :: Cmd.CmdT IO ()
writeBaliScalesKsp = do
    writeLarasKsp "wayang" (Wayang.instrumentScale True Wayang.larasSawan)
    writeLarasKsp "legong"
        (Legong.completeInstrumentScale Legong.larasRambat)

writeLarasKsp :: FilePath -> (BaliScales.Tuning -> Patch.Scale)
    -> Cmd.CmdT IO ()
writeLarasKsp name make = mapM_ (uncurry (writeKsp Nothing))
    [ (name <> "-umbang.ksp", make BaliScales.Umbang)
    , (name <> "-isep.ksp", make BaliScales.Isep)
    ]
