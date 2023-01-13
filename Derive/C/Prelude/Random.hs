-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls for randomized scores.
module Derive.C.Prelude.Random where
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty

import qualified Util.Doc as Doc
import qualified Util.Log as Log
import qualified Util.Seq as Seq

import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Eval as Eval
import qualified Derive.Library as Library
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream

import qualified Ui.Meter.Meter as Meter

import           Global
import           Types


library :: Library.Library
library = mconcat
    [ Library.poly_generators
        [ ("alt", c_alternate)
        , ("alt-w", c_alternate_weighted)
        ]
    , Library.poly_transformers
        [ ("omit", c_omit)
        , ("log-seed", c_log_seed)
        ]
    , Library.generators
        [ ("alt-t", c_alternate_tracks)
        , ("t-alt", c_tempo_alternate)
        , ("t-alt-c", c_tempo_alternate_continuous)
        ]
    , Library.vals
        [ ("alt", c_val_alternate) -- or ?
        , ("alt-w", c_val_alternate_weighted)
        , ("range", c_range) -- or -?
        ]
    ]

c_omit :: Derive.CallableExpr d => Derive.Transformer d
c_omit = Derive.transformer Module.prelude "omit" Tags.random
    "Omit the derived call a certain percentage of the time."
    $ Sig.callt
    (Sig.defaulted "chance" (Sig.control "omit" 0.5)
        "Chance, from 0 to 1, that the transformed note will be omitted."
    ) $ \omit args deriver -> do
        omit <- Call.control_at omit =<< Args.real_start args
        ifM (Call.chance omit) (return Stream.empty) deriver

c_log_seed :: Derive.CallableExpr d => Derive.Transformer d
c_log_seed = Derive.transformer Module.prelude "log-seed" mempty
    "Emit a log message with the seed at this point. If you like how a\
    \ generator realized, and want to freeze it, then you can use this to\
    \ get the seed, and then hardcode it with `seed=xyz`."
    $ Sig.call0t $ \_args deriver -> do
        seed <- fromMaybe 0 <$> Derive.lookup_val EnvKey.seed
        Log.warn $ "log-seed: " <> showt (seed :: Int)
        deriver

c_alternate :: Derive.CallableExpr d => Derive.Generator d
c_alternate = Derive.generator Module.prelude "alternate" Tags.random
    "Pick one of several expressions and evaluate it."
    $ Sig.call (Sig.many1 "expr" "Expression to evaluate.") $
    \exprs args -> do
        quoted <- Call.pick exprs <$> Call.random
        Eval.eval_quoted (Args.context args) quoted

c_alternate_weighted :: Derive.CallableExpr d => Derive.Generator d
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
    mempty tempo_alternate_doc $ Sig.call ((,)
    <$> breakpoints_arg
    <*> Sig.environ "timestep" Sig.Prefixed Meter.E
        "Use the duration of this timestep, in seconds."
    ) $ \((bottom, pairs), timestep) args -> do
        dur <- Derive.real =<< Call.meter_duration (Args.start args) timestep 1
        Eval.eval_quoted (Args.context args) $ under_threshold bottom pairs dur

under_threshold :: Ord key => val -> [(key, val)] -> key -> val
under_threshold bottom ((threshold, expr) : rest) dur
    | dur < threshold = bottom
    | otherwise = under_threshold expr rest dur
under_threshold bottom [] _ = bottom

tempo_alternate_doc :: Doc.Doc
tempo_alternate_doc =
    "Derive alternate calls depending on the tempo, for Javanese-style wirama\
    \ transitions. For instance, `a 1/8 b 1/4 c` will play `a` when an 8th\
    \ note is between 0 and 1/8s, `b` when it's between 1/8s and 1/4s, and\
    \ `c` when it's above 1/4s."

c_tempo_alternate_continuous :: Derive.Generator Derive.Note
c_tempo_alternate_continuous =
    Derive.generator Module.prelude "tempo-alternate-continuous" mempty
        (tempo_alternate_doc <> "\nThis variant will\
        \ switch between the alternates even in the middle of the call.\
        \ Long notes will be clipped at the transition point.")
    $ Sig.call ((,,)
    <$> breakpoints_arg
    <*> Sig.environ "timestep" Sig.Prefixed Meter.E
        "Use the duration of this timestep, in seconds."
    <*> Sig.environ "interval" Sig.Prefixed Meter.Q
        "Switch between alternates at this time interval."
    ) $ \((bottom, pairs), timestep, interval) args ->
        tempo_alternate_continuous bottom pairs timestep interval args

breakpoints_arg :: Sig.Parser (DeriveT.Quoted, [(RealTime, DeriveT.Quoted)])
breakpoints_arg = Sig.check check $ (,)
    <$> Sig.required_env "bottom" Sig.None "Default alternate."
    <*> Sig.many_pairs "threshold,expr"
        "Evaluate the expr if the tempo is above the threshold.\
        \ The thresholds should be in ascending order, so the fastest alternate\
        \ is at the left."
    where
    check (_, pairs)
        | map fst pairs == List.sort (map fst pairs) = Nothing
        | otherwise = Just $ "thresholds should be in ascending order: "
            <> pretty (map fst pairs)

tempo_alternate_continuous :: DeriveT.Quoted -> [(RealTime, DeriveT.Quoted)]
    -> Meter.Rank -> Meter.Rank -> Derive.NoteArgs -> Derive.NoteDeriver
tempo_alternate_continuous bottom pairs timestep interval args = do
    interval <- Call.meter_duration (Args.start args) interval 1
    let starts = Seq.range (Args.start args) (Args.end args) interval
    indices <- alternate_indices starts timestep (map fst pairs)
    let (alts, alt_indices) = select_indices (bottom : map snd pairs) indices
    alts <- mapM (Eval.eval_quoted (Args.context args)) alts
    real_starts <- mapM Derive.real starts
    let breakpoints = Seq.drop_dups snd (zip real_starts alt_indices)
    -- Debug.tracepM "breakpoints" (starts, zip real_starts alt_indices)
    return $ case breakpoints of
        -- Optimize a single breakpoint at the start.
        [(t, i)] | [t] == take 1 real_starts -> alts !! i
        _ -> switch alts breakpoints

-- | Switch between note streams when the index changes.  Sounding notes
-- will be clipped, and dropped if they wind up at duration 0.
switch :: [Stream.Stream Score.Event] -> [(RealTime, Int)]
    -> Stream.Stream Score.Event
switch [] _ = mempty
switch streams bps = mconcatMap select (Seq.zip_next bps)
    where
    -- This is a little bit inefficient because it scans from the beginning of
    -- each stream, but the number of events and streams is likely small.
    select ((t, i), next) = case next of
        Nothing -> Stream.drop_while ((<t) . Score.event_start) (streams !! i)
        Just (next_t, _) -> extrect t next_t (streams !! i)
    extrect start end =
        fmap (clip end) . Stream.take_while ((<end) . Score.event_start)
            . Stream.drop_while ((<start) . Score.event_start)
    clip end event
        | Score.event_end event <= end = event
        | otherwise =
            Score.set_duration (max 0 (end - Score.event_start event)) event

alternate_indices :: [ScoreTime] -> Meter.Rank -> [RealTime]
    -> Derive.Deriver [Int]
    -- ^ time to switch to which index
alternate_indices starts timestep thresholds = do
    -- reals <- mapM Derive.real starts
    -- Debug.tracepM "reals" (zip starts reals)
    -- Get tempo at each start, in the duration of the timestep at each start.
    durs <- mapM (timestep_dur_at timestep) starts
    return $ -- Debug.trace_retp "alt_indices" (thresholds, zip starts durs) $
        map (index_under_threshold thresholds) durs

timestep_dur_at :: Meter.Rank -> ScoreTime -> Derive.Deriver RealTime
timestep_dur_at timestep p = do
    Call.real_duration p =<< Call.meter_duration p timestep 1

-- | Select the given indices from the list, and return a list with just the
-- indexed elements, and the original indices packed so they index into the
-- dense list.
select_indices :: [a] -> [Int] -> ([a], [Int])
select_indices xs is = (map (xs!!) unique, map pack is)
    where
    unique = Seq.unique_sort is
    pack i = fromMaybe 0 $ List.elemIndex i unique

index_under_threshold :: Ord a => [a] -> a -> Int
index_under_threshold ts val = go 0 ts
    where
    go i (t:ts)
        | val <= t = i
        | otherwise = go (i+1) ts
    go i [] = i

-- * val calls

c_val_alternate :: Derive.ValCall
c_val_alternate = Derive.val_call Module.prelude "alternate" Tags.random
    "Pick one of the arguments randomly."
    $ Sig.call (Sig.many1 "val" "Value of any type.") $ \vals _ ->
        Call.pick (vals :: NonEmpty DeriveT.Val) <$> Call.random

c_val_alternate_weighted :: Derive.ValCall
c_val_alternate_weighted = Derive.val_call Module.prelude "alternate-weighted"
    Tags.random "Pick one of the arguments randomly."
    $ Sig.call (Sig.many1_pairs "val" "(weight, val) pairs.") $
    \pairs _args -> do
        let vals :: NonEmpty DeriveT.Val
            (weights, vals) = NonEmpty.unzip pairs
        Call.pick_weighted (NonEmpty.zip weights vals) <$> Call.random

c_range :: Derive.ValCall
c_range = Derive.val_call Module.prelude "range" Tags.random
    "Pick a random number within a range." $ Sig.call ((,)
    <$> Sig.defaulted "low" (0 :: Int) "Bottom of range, inclusive."
    <*> Sig.defaulted "high" (1 :: Int) "Top of range, inclusive."
    ) $ \(low, high) _args -> Call.random_in low high :: Derive.Deriver Double
