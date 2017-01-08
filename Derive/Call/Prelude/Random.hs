-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls for randomized scores.
module Derive.Call.Prelude.Random where
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty

import qualified Util.Seq as Seq
import qualified Cmd.Ruler.Meter as Meter
import qualified Derive.Args as Args
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call as Call
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Eval as Eval
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream

import Global


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.call_maps
    [ ("alt", c_alternate)
    , ("alt-w", c_alternate_weighted)
    , ("alt-t", c_alternate_tracks)
    , ("t-alt", c_tempo_alternate)
    ]
    [("omit", c_omit)]

control_calls :: Derive.CallMaps Derive.Control
control_calls = Derive.call_maps
    [("alt", c_alternate), ("alt-w", c_alternate_weighted)]
    [("omit", c_omit)]

pitch_calls :: Derive.CallMaps Derive.Pitch
pitch_calls = Derive.call_maps
    [("alt", c_alternate), ("alt-w", c_alternate_weighted)]
    [("omit", c_omit)]

c_omit :: Derive.Callable d => Derive.Transformer d
c_omit = Derive.transformer Module.prelude "omit" Tags.random
    "Omit the derived call a certain percentage of the time."
    $ Sig.callt
    (Sig.defaulted "chance" (Sig.control "omit" 0.5)
        "Chance, from 0 to 1, that the transformed note will be omitted."
    ) $ \omit args deriver -> do
        omit <- Call.control_at omit =<< Args.real_start args
        ifM (Call.chance omit) (return Stream.empty) deriver

c_alternate :: Derive.Callable d => Derive.Generator d
c_alternate = Derive.generator Module.prelude "alternate" Tags.random
    "Pick one of several expressions and evaluate it."
    $ Sig.call (Sig.many1 "expr" "Expression to evaluate.") $
    \exprs args -> do
        quoted <- Call.pick exprs <$> Call.random
        Eval.eval_quoted (Args.context args) quoted

c_alternate_weighted :: Derive.Callable d => Derive.Generator d
c_alternate_weighted =
    Derive.generator Module.prelude "alternate-weighted" Tags.random
    "Pick one of several expressions and evaluate it."
    $ Sig.call (Sig.many1_pairs "expr" "(weight, expr) pairs.") $
    \pairs args ->
        Eval.eval_quoted (Args.context args) . Call.pick_weighted pairs
            =<< Call.random

c_alternate_tracks :: Derive.Generator Derive.Note
c_alternate_tracks = Derive.generator Module.prelude "alternate-tracks"
    (Tags.random <> Tags.subs) "Evaluate notes from one of the sub-tracks."
    $ Sig.call
    (Sig.many "weight" "Likelihood to choose each child track. Each number is\
        \ a relative weight, and tracks without a number default to 1. It's\
        \ an error to have more numbers than tracks.") $
    \weights args -> do
        subs <- Sub.sub_events args
        let err =  "more weights than tracks: " <> showt (length weights)
                <> " > " <> showt (length subs) <> " tracks"
        sub_weights <- mapM (pair err) $ Seq.zip_padded weights subs
        case NonEmpty.nonEmpty sub_weights of
            Nothing -> return mempty
            Just sub_weights ->
                Sub.derive . Call.pick_weighted sub_weights =<< Call.random
    where
    pair _ (Seq.Both weight sub) = return (weight, sub)
    pair err (Seq.First _) = Derive.throw err
    pair _ (Seq.Second sub) = return (1, sub)

-- TODO This doesn't really belong in here since it's not random.  Also not in
-- NoteTransformer since it's not a transformer.  Maybe all this stuff should
-- move to an Alternate module?
c_tempo_alternate :: Derive.Generator Derive.Note
c_tempo_alternate = Derive.generator Module.prelude "tempo-alternate"
    mempty "Alternate derivation depending on tempo."
    $ Sig.call ((,,)
    <$> Sig.required_env "bottom" Sig.None "Default alternate." -- TODO
    <*> Sig.many_pairs "threshold,expr"
        "Evaluate the expr if the tempo is above the threshold.\
        \ The thresholds should be in ascending order, so the fastest alternate\
        \ is at the left."
    <*> Sig.environ "timestep" Sig.Prefixed Meter.E
        "Use the duration of this timestep, in seconds."
    ) $ \(bottom, pairs, timestep) args -> do
        unless (map fst pairs == List.sort (map fst pairs)) $
            Derive.throw $ "thresholds should be in ascending order: "
                <> pretty (map fst pairs)
        dur <- Derive.real =<< Call.meter_duration (Args.start args) timestep 1
        Eval.eval_quoted (Args.context args) $ under_threshold dur bottom pairs

under_threshold :: Ord key => key -> val -> [(key, val)] -> val
under_threshold dur bottom ((threshold, expr) : rest)
    | dur < threshold = bottom
    | otherwise = under_threshold dur expr rest
under_threshold _ bottom [] = bottom

-- * val calls

val_calls :: [Derive.LookupCall Derive.ValCall]
val_calls = Derive.call_map
    [ ("alt", c_val_alternate) -- or ?
    , ("alt-w", c_val_alternate_weighted)
    , ("range", c_range) -- or -?
    ]

c_val_alternate :: Derive.ValCall
c_val_alternate = Derive.val_call Module.prelude "alternate" Tags.random
    "Pick one of the arguments randomly."
    $ Sig.call (Sig.many1 "val" "Value of any type.") $ \vals _ ->
        Call.pick (vals :: NonEmpty BaseTypes.Val) <$> Call.random

c_val_alternate_weighted :: Derive.ValCall
c_val_alternate_weighted = Derive.val_call Module.prelude "alternate-weighted"
    Tags.random "Pick one of the arguments randomly."
    $ Sig.call (Sig.many1_pairs "val" "(weight, val) pairs.") $
    \pairs _args -> do
        let vals :: NonEmpty BaseTypes.Val
            (weights, vals) = NonEmpty.unzip pairs
        Call.pick_weighted (NonEmpty.zip weights vals) <$> Call.random

c_range :: Derive.ValCall
c_range = Derive.val_call Module.prelude "range" Tags.random
    "Pick a random number within a range." $ Sig.call ((,)
    <$> Sig.defaulted "low" 0 "Bottom of range, inclusive."
    <*> Sig.defaulted "high" 1 "Top of range, inclusive."
    ) $ \(low, high) _args -> Call.random_in low high :: Derive.Deriver Double
