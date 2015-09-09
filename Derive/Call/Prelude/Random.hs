-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls for randomized scores.
module Derive.Call.Prelude.Random where
import qualified Data.Foldable as Foldable
import qualified Data.List.NonEmpty as NonEmpty

import qualified Util.Seq as Seq
import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Eval as Eval
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream
import qualified Derive.TrackLang as TrackLang
import qualified Derive.Typecheck as Typecheck

import Global


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.call_maps
    [ ("alt", c_alternate)
    , ("alt-w", c_alternate_weighted)
    , ("alt-t", c_alternate_tracks)
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
        let pairs = fmap ((,) 1) exprs
        val <- pick_weighted pairs <$> Call.random
        Call.eval (Args.context args) val

eval :: Derive.Callable d => Derive.Context d -> TrackLang.Val
    -> Derive.Deriver (Stream.Stream d)
eval info val = do
    quoted <- Derive.require_right id $ Call.val_to_quoted val
    Eval.eval_quoted info quoted

-- | Calls themselves are not first class, so this has to either take a string
-- and evaluate it, or turn a Val back into a string to evaluate.  That works
-- for most types, but not for Pitch.
c_alternate_weighted :: Derive.Callable d => Derive.Generator d
c_alternate_weighted =
    Derive.generator Module.prelude "alternate-weighted" Tags.random
    "Pick one of several expressions and evaluate it."
    $ Sig.call (Sig.many1 "weight,expr"
        "An even number of args in (Num, Val) pairs.") $
    \pairs args -> do
        pairs <- mapM (typecheck args)
            =<< Sig.paired_args (NonEmpty.toList pairs)
        case NonEmpty.nonEmpty pairs of
            Nothing -> Derive.throw "empty list"
            Just pairs -> pick_weighted pairs =<< Call.random
    where
    typecheck args (weight, expr) = do
        weight <- Typecheck.typecheck "" (Args.start args) weight
        quoted <- Derive.require_right id $ Call.val_to_quoted expr
        return (weight, Eval.eval_quoted (Args.context args) quoted)

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
                Sub.derive . pick_weighted sub_weights =<< Call.random
    where
    pair _ (Seq.Both weight sub) = return (weight, sub)
    pair err (Seq.First _) = Derive.throw err
    pair _ (Seq.Second sub) = return (1, sub)

pick_weighted :: NonEmpty (Double, a) -> Double -> a
pick_weighted weights rnd_ = go 0 weights
    where
    rnd = rnd_ * Foldable.sum (fmap fst weights)
    go collect ((weight, a) :| weights) = case weights of
        [] -> a
        w : ws
            | collect + weight > rnd -> a
            | otherwise -> go (collect + weight) (w :| ws)


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
    $ Sig.call (Sig.many1 "val" "Value of any type.") $ \vals _ -> do
        let pairs = fmap ((,) 1) (vals :: NonEmpty TrackLang.Val)
        pick_weighted pairs <$> Call.random

c_val_alternate_weighted :: Derive.ValCall
c_val_alternate_weighted = Derive.val_call Module.prelude "alternate-weighted"
    Tags.random "Pick one of the arguments randomly."
    $ Sig.call (Sig.many1
        "weight,val" "An even number of args in (Num, Val) pairs.") $
    \pairs args -> do
        (weights, vals) <- unzip <$> Sig.paired_args (NonEmpty.toList pairs)
        weights <- mapM (Typecheck.typecheck "" (Args.start args)) weights
        case NonEmpty.nonEmpty (zip weights vals) of
            Nothing -> Derive.throw "not reached"
            Just pairs -> pick_weighted pairs <$> Call.random

c_range :: Derive.ValCall
c_range = Derive.val_call Module.prelude "range" Tags.random
    "Pick a random number within a range." $ Sig.call ((,)
    <$> Sig.defaulted "low" 0 "Bottom of range, inclusive."
    <*> Sig.defaulted "high" 1 "Top of range, inclusive."
    ) $ \(low, high) _args -> Call.random_in low high :: Derive.Deriver Double
