-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ScopedTypeVariables #-}
-- | Interpolate between two different scales.
module Derive.Scale.Interpolate where
import qualified Derive.Args as Args
import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.Environ as Environ
import qualified Derive.Eval as Eval
import qualified Derive.Pitches as Pitches
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Scales as Scales
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import Global


scales :: [Scale.Make]
scales = scale_make $ \env (Scale.LookupScale lookup) -> do
    (from_id, to_id) <- environ_from_to env
    let find msg scale_id = fromMaybe (Left $ Scale.ScaleError msg) $
            lookup env2 (Scale.LookupScale lookup) scale_id
        -- This should avoid an infinite loop if from_id is itself
        -- interpolate.
        env2 = TrackLang.delete_val scale_from $
            TrackLang.delete_val scale_to env
    from <- find ("from scale " <> prettyt from_id) from_id
    to <- find ("to scale " <> prettyt to_id) to_id
    return $ make from to
    where
    scale_make = (:[]) . Scale.Make scale_id ("same as from scale", doc)

make :: Scale.Scale -> Scale.Scale -> Scale.Scale
make from to = Scale.Scale
    { Scale.scale_id = scale_id
    , Scale.scale_pattern = "same as `from` scale"
    , Scale.scale_symbols = []
    , Scale.scale_transposers =
        Scale.scale_transposers from <> Scale.scale_transposers to
    , Scale.scale_read = Scale.scale_read from
    , Scale.scale_show = Scale.scale_show from
    , Scale.scale_layout = Scale.scale_layout from
    , Scale.scale_transpose = Scale.scale_transpose from
    , Scale.scale_enharmonics = Scale.scale_enharmonics from
    , Scale.scale_note_to_call = to_call
    , Scale.scale_input_to_note = to_note
    , Scale.scale_input_to_nn = Scales.computed_input_to_nn to_note to_call
    , Scale.scale_call_doc = doc
    }
    where
    to_call = note_to_call from to
    to_note = Scale.scale_input_to_note from

doc :: Derive.DocumentedCall
doc = Derive.extract_val_doc $ interpolated_degree dummy dummy
    where
    dummy = Derive.val_call Module.scale "dummy" mempty "" $
        Sig.call0 $ \_args -> return (0 :: Double)

scale_id :: Pitch.ScaleId
scale_id = "interpolate"

scale_at :: Score.Control
scale_at = "scale-at"

note_to_call :: Scale.Scale -> Scale.Scale -> Pitch.Note -> Maybe Derive.ValCall
note_to_call from to note =
    interpolated_degree <$> Scale.scale_note_to_call from note
        <*> Scale.scale_note_to_call to note

interpolated_degree :: Derive.ValCall -> Derive.ValCall -> Derive.ValCall
interpolated_degree from to = Derive.val_call Module.scale "pitch" mempty
    ("Emit a pitch between two scales. The scales are in the "
    <> doc scale_from <> " and " <> doc scale_to
    <> " environ values, and keys are from " <> doc key_from <> " and "
    <> doc key_to <> ". The " <> doc scale_at <> " control ranges from 0 to 1\
    \ and controls the intpolation between the scales. For this to work, the\
    \ scales must have the same degree names, since there's no way to manually\
    \ specify a correspondence between scale degrees."
    ) $ Sig.parsed_manually "passed to `from` and `to` scales" $ \args -> do
        start <- Args.real_start args
        n <- fromMaybe 0 <$> Derive.untyped_control_at scale_at start
        let apply key = rename_environ key Environ.key
                . Eval.apply_pitch (Args.start args)
        if n <= 0 then apply key_from from
            else if n >= 1 then apply key_to to
            else do
                p1 <- require . Sig.typecheck =<< apply key_from from
                p2 <- require . Sig.typecheck =<< apply key_to to
                return $ TrackLang.to_val $ Pitches.interpolated p1 p2 n
    where
    require = Derive.require_right (("interpolated_degree: "<>) . untxt)
    doc :: ShowVal.ShowVal a => a -> Text
    doc = ShowVal.doc_val

rename_environ :: TrackLang.ValName -> TrackLang.ValName -> Derive.Deriver a
    -> Derive.Deriver a
rename_environ from to deriver = do
    maybe_val :: Maybe TrackLang.Val <- Derive.lookup_val from
    maybe id (Derive.with_val to) maybe_val deriver


-- * util

environ_from_to :: TrackLang.Environ
    -> Either Scale.ScaleError (Pitch.ScaleId, Pitch.ScaleId)
environ_from_to env = do
    let get key = maybe (Left $ Scale.EnvironMissing key) Right $
            TrackLang.maybe_val key env
    from <- get scale_from
    to <- get scale_to
    return (TrackLang.sym_to_scale_id from, TrackLang.sym_to_scale_id to)

key_from, key_to :: TrackLang.ValName
key_from = "key-from"
key_to = "key-to"

scale_from, scale_to :: TrackLang.ValName
scale_from = "scale-from"
scale_to = "scale-to"
