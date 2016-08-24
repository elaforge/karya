-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Functions to deal with tuning instruments.

    E.g.:

    > LTuning.realtime "pno" =<< LTuning.selection True
    > LTuning.write_ksp (Just "wayang") "charu.ksp"
    >       =<< LTuning.scale True "raga" "key=charukesi"

    Don't forget to set the score to the same scale or things will sound
    confusing.  Also, reaper won't receive sysex on a track unless you set it
    to receive all channels.
-}
module Cmd.Repl.LTuning where
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Vector.Unboxed as Unboxed

import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil

import qualified Midi.Midi as Midi
import qualified Cmd.Cmd as Cmd
import qualified Cmd.InputNote as InputNote
import qualified Cmd.Perf as Perf
import qualified Cmd.PlayUtil as PlayUtil
import qualified Cmd.Repl.LInst as LInst
import qualified Cmd.Repl.Util as Util
import qualified Cmd.Selection as Selection

import qualified Derive.Call as Call
import qualified Derive.Call.Prelude.Equal as Equal
import qualified Derive.Derive as Derive
import qualified Derive.Scale as Scale
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.Legong as Legong
import qualified Derive.Scale.Wayang as Wayang

import qualified Perform.Midi.Patch as Patch
import qualified Perform.Pitch as Pitch
import qualified Instrument.Common as Common
import qualified Local.Instrument.Kontakt.Util as Kontakt.Util
import Global
import Types


-- * Patch.Scale

-- | Format a Patch.Scale as a table.
table :: Patch.Scale -> Text
table scale =
    Text.unlines $ TextUtil.formatColumns 1 $
        ["", "c", "", "d", "", "e", "f", "", "g", "", "a", "", "b"]
        : [oct : map (Num.showFloat 2) nns | (oct, nns) <- zip octaves groups]
    where
    octaves = map (("c"<>) . showt) [-1..]
    groups = Seq.chunked 12 $ Unboxed.toList $ Patch.scale_key_to_nn scale

-- | Get a patch scale for the scale at the selection.
selection :: Cmd.M m => Bool
    -- ^ False to check for warnings and errors, True to ignore them.
    -> m Patch.Scale
selection ignore_errors = do
    (block_id, _, track_id, _) <- Selection.get_insert
    (scale, errs) <- scale_at block_id track_id
    unless (ignore_errors || null errs) $
        Cmd.throw $ Text.unlines errs
    return scale

scale_at :: Cmd.M m => BlockId -> TrackId -> m (Patch.Scale, [Text])
scale_at block_id track_id = do
    scale <- Perf.get_derive_at block_id track_id Call.get_scale
    (key_nns, errs) <- fmap unzip $ forM all_inputs $ \(key, input) -> do
        let at_time = 0
        (val, logs) <- Perf.derive_at block_id track_id $
            Scale.scale_input_to_nn scale at_time input
        let prefix = (("key " <> pretty key <> ": ") <>)
        return $ second (map prefix . (++ map pretty logs)) $ case val of
            Left err -> (Nothing, [err])
            Right (Left err) -> (Nothing, [pretty err])
            Right (Right nn) -> (Just (key, nn), [])
    let name = pretty (Scale.scale_id scale)
    return (Patch.make_scale name (Maybe.catMaybes key_nns), concat errs)

-- | Create a Patch.Scale for the named scale.
named :: Cmd.M m => Bool
    -- ^ False to check for warnings and errors, True to ignore them.
    -> Text -> Text -> m Patch.Scale
named ignore_errors name transform = do
    scale <- get_scale name
    (scale, errs) <- make_patch_scale scale transform
    unless (ignore_errors || null errs) $
        Cmd.throw $ Text.unlines errs
    return scale

get_scale :: Cmd.M m => Text -> m Scale.Scale
get_scale name =
    Cmd.require ("scale not found: " <> name)
        =<< Perf.lookup_scale_env mempty (Pitch.ScaleId name)

make_patch_scale :: Cmd.M m => Scale.Scale -> Text -> m (Patch.Scale, [Text])
make_patch_scale scale transform = do
    (key_nns, errs) <- fmap unzip $ forM all_inputs $ \(key, input) -> do
        let at_time = 0
        (val, logs) <- derive $ Equal.transform_expr transform $
            Scale.scale_input_to_nn scale at_time input
        let prefix = (("key " <> pretty key <> ": ") <>)
        return $ second (map prefix . (++ logs)) $ case val of
            Nothing -> (Nothing, [])
            Just (Left err) -> (Nothing, [pretty err])
            Just (Right nn) -> (Just (key, nn), [])
    let name = pretty (Scale.scale_id scale)
    return (Patch.make_scale name (Maybe.catMaybes key_nns), concat errs)

derive :: Cmd.M m => Derive.Deriver a -> m (Maybe a, [Text])
derive deriver = do
    (val, _, logs) <- PlayUtil.run mempty mempty deriver
    return $ case val of
        Left err -> (Nothing, pretty err : map pretty logs)
        Right val -> (Just val, map pretty logs)

all_inputs :: [(Midi.Key, Pitch.Input)]
all_inputs =
    [(key, InputNote.nn_to_input (Midi.from_key key)) | key <- [0..127]]

-- * retune

-- | Show tuning map for debugging.
get_tuning :: Cmd.M m => Util.Instrument -> Patch.Scale -> m Text
get_tuning inst scale = do
    attr_map@(Common.AttributeMap amap) <- Patch.patch_attribute_map <$>
        Cmd.get_midi_patch (Util.instrument inst)
    let tuning = Patch.scale_tuning (Just attr_map) scale
    return $ Text.unlines $ concat
        [ map pretty amap
        , [""]
        , map (Text.unwords . map pretty) (Seq.chunked 6 tuning)
        ]

-- | Set the instrument's Scale to the given scale and send a MIDI tuning
-- message with 'LInst.initialize_tuning'.
realtime :: Cmd.M m => Util.Instrument -> Patch.Scale -> m ()
realtime inst scale = do
    LInst.set_scale inst scale
    LInst.modify_midi_config inst $
        Patch.initialization %= Set.insert Patch.Tuning
    LInst.initialize_tuning (Util.instrument inst)

-- | Write KSP to retune a 12TET patch.  Don't forget to do 'LInst.set_scale'
-- to configure the instrument.
write_ksp :: Maybe Util.Instrument -> FilePath -> Patch.Scale -> Cmd.CmdT IO ()
write_ksp maybe_inst filename scale = do
    attr_map <- case maybe_inst of
        Nothing -> return Nothing
        Just inst -> Just . Patch.patch_attribute_map <$> Cmd.get_midi_patch
            (Util.instrument inst)
    ksp <- Cmd.require_right id $ Kontakt.Util.tuning_ksp attr_map scale
    liftIO $ Text.IO.writeFile filename ksp
    return ()

write_saih_ksp :: FilePath -> (BaliScales.Tuning -> Patch.Scale)
    -> Cmd.CmdT IO ()
write_saih_ksp name make = mapM_ (uncurry (write_ksp Nothing))
    [ (name <> "-umbang.ksp", make BaliScales.Umbang)
    , (name <> "-isep.ksp", make BaliScales.Isep)
    ]

write_bali_scales_ksp :: Cmd.CmdT IO ()
write_bali_scales_ksp = do
    write_saih_ksp "wayang" (Wayang.instrument_scale True Wayang.saih_sawan)
    write_saih_ksp "legong"
        (Legong.complete_instrument_scale Legong.saih_rambat)
