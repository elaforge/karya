-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls for randomized scores.
module Derive.C.Prelude.Random where
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty

import qualified Util.Doc as Doc
import qualified Util.Lists as Lists
import qualified Util.Log as Log

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
    [ Library.polyGenerators
        [ ("alt", cAlternate)
        , ("alt-w", cAlternateWeighted)
        ]
    , Library.polyTransformers
        [ ("omit", cOmit)
        , ("log-seed", cLogSeed)
        ]
    , Library.generators
        [ ("alt-t", cAlternateTracks)
        , ("t-alt", cTempoAlternate)
        , ("t-alt-c", cTempoAlternateContinuous)
        ]
    , Library.vals
        [ ("alt", cValAlternate) -- or ?
        , ("alt-w", cValAlternateWeighted)
        , ("range", cRange) -- or -?
        ]
    ]

cOmit :: Derive.CallableExpr d => Derive.Transformer d
cOmit = Derive.transformer Module.prelude "omit" Tags.random
    "Omit the derived call a certain percentage of the time."
    $ Sig.callt
    (Sig.defaulted "chance" (0.5 :: Double)
        "Chance, from 0 to 1, that the transformed note will be omitted."
    ) $ \omit _args deriver ->
        ifM (Call.chance omit) (pure Stream.empty) deriver

cLogSeed :: Derive.CallableExpr d => Derive.Transformer d
cLogSeed = Derive.transformer Module.prelude "log-seed" mempty
    "Emit a log message with the seed at this point. If you like how a\
    \ generator realized, and want to freeze it, then you can use this to\
    \ get the seed, and then hardcode it with `seed=xyz`."
    $ Sig.call0t $ \_args deriver -> do
        seed <- fromMaybe 0 <$> Derive.lookupVal EnvKey.seed
        Log.warn $ "log-seed: " <> showt (seed :: Int)
        deriver

cAlternate :: Derive.CallableExpr d => Derive.Generator d
cAlternate = Derive.generator Module.prelude "alternate" Tags.random
    "Pick one of several expressions and evaluate it."
    $ Sig.call (Sig.many1 "expr" "Expression to evaluate.") $
    \exprs args -> do
        quoted <- Call.pick exprs <$> Call.random
        Eval.evalQuoted (Args.context args) quoted

cAlternateWeighted :: Derive.CallableExpr d => Derive.Generator d
cAlternateWeighted =
    Derive.generator Module.prelude "alternate-weighted" Tags.random
    "Pick one of several expressions and evaluate it."
    $ Sig.call (Sig.many1Pairs "expr" "(weight, expr) pairs.") $
    \pairs args ->
        Eval.evalQuoted (Args.context args) . Call.pickWeighted pairs
            =<< Call.random

cAlternateTracks :: Derive.Generator Derive.Note
cAlternateTracks = Derive.generator Module.prelude "alternate-tracks"
    (Tags.random <> Tags.subs) "Evaluate notes from one of the sub-tracks."
    $ Sig.call
    (Sig.many "weight" "Likelihood to choose each child track. Each number is\
        \ a relative weight, and tracks without a number default to 1. It's\
        \ an error to have more numbers than tracks.") $
    \weights args -> do
        subs <- Sub.subEvents args
        let err =  "more weights than tracks: " <> showt (length weights)
                <> " > " <> showt (length subs) <> " tracks"
        subWeights <- mapM (pair err) $ Lists.zipPadded weights subs
        case NonEmpty.nonEmpty subWeights of
            Nothing -> pure mempty
            Just subWeights ->
                Sub.derive . Call.pickWeighted subWeights =<< Call.random
    where
    pair _ (Lists.Both weight sub) = pure (weight, sub)
    pair err (Lists.First _) = Derive.throw err
    pair _ (Lists.Second sub) = pure (1, sub)

-- TODO This doesn't really belong in here since it's not random.  Also not in
-- NoteTransformer since it's not a transformer.  Maybe all this stuff should
-- move to an Alternate module?
cTempoAlternate :: Derive.Generator Derive.Note
cTempoAlternate = Derive.generator Module.prelude "tempo-alternate"
    mempty tempoAlternateDoc $ Sig.call ((,)
    <$> breakpointsArg
    <*> Sig.environ "timestep" Sig.Prefixed Meter.E
        "Use the duration of this timestep, in seconds."
    ) $ \((bottom, pairs), timestep) args -> do
        dur <- Derive.real =<< Call.meterDuration (Args.start args) timestep 1
        Eval.evalQuoted (Args.context args) $ underThreshold bottom pairs dur

underThreshold :: Ord key => val -> [(key, val)] -> key -> val
underThreshold bottom ((threshold, expr) : rest) dur
    | dur < threshold = bottom
    | otherwise = underThreshold expr rest dur
underThreshold bottom [] _ = bottom

tempoAlternateDoc :: Doc.Doc
tempoAlternateDoc =
    "Derive alternate calls depending on the tempo, for Javanese-style wirama\
    \ transitions. For instance, `a 1/8 b 1/4 c` will play `a` when an 8th\
    \ note is between 0 and 1/8s, `b` when it's between 1/8s and 1/4s, and\
    \ `c` when it's above 1/4s."

cTempoAlternateContinuous :: Derive.Generator Derive.Note
cTempoAlternateContinuous =
    Derive.generator Module.prelude "tempo-alternate-continuous" mempty
        (tempoAlternateDoc <> "\nThis variant will\
        \ switch between the alternates even in the middle of the call.\
        \ Long notes will be clipped at the transition point.")
    $ Sig.call ((,,)
    <$> breakpointsArg
    <*> Sig.environ "timestep" Sig.Prefixed Meter.E
        "Use the duration of this timestep, in seconds."
    <*> Sig.environ "interval" Sig.Prefixed Meter.Q
        "Switch between alternates at this time interval."
    ) $ \((bottom, pairs), timestep, interval) args ->
        tempoAlternateContinuous bottom pairs timestep interval args

breakpointsArg :: Sig.Parser (DeriveT.Quoted, [(RealTime, DeriveT.Quoted)])
breakpointsArg = Sig.check check $ (,)
    <$> Sig.requiredEnv "bottom" Sig.None "Default alternate."
    <*> Sig.manyPairs "threshold,expr"
        "Evaluate the expr if the tempo is above the threshold.\
        \ The thresholds should be in ascending order, so the fastest alternate\
        \ is at the left."
    where
    check (_, pairs)
        | map fst pairs == List.sort (map fst pairs) = Nothing
        | otherwise = Just $ "thresholds should be in ascending order: "
            <> pretty (map fst pairs)

tempoAlternateContinuous :: DeriveT.Quoted -> [(RealTime, DeriveT.Quoted)]
    -> Meter.Rank -> Meter.Rank -> Derive.NoteArgs -> Derive.NoteDeriver
tempoAlternateContinuous bottom pairs timestep interval args = do
    interval <- Call.meterDuration (Args.start args) interval 1
    let starts = Lists.range (Args.start args) (Args.end args) interval
    indices <- alternateIndices starts timestep (map fst pairs)
    let (alts, altIndices) = selectIndices (bottom : map snd pairs) indices
    alts <- mapM (Eval.evalQuoted (Args.context args)) alts
    realStarts <- mapM Derive.real starts
    let breakpoints = Lists.dropDups snd (zip realStarts altIndices)
    -- Debug.tracepM "breakpoints" (starts, zip real_starts alt_indices)
    pure $ case breakpoints of
        -- Optimize a single breakpoint at the start.
        [(t, i)] | [t] == take 1 realStarts -> alts !! i
        _ -> switch alts breakpoints

-- | Switch between note streams when the index changes.  Sounding notes
-- will be clipped, and dropped if they wind up at duration 0.
switch :: [Stream.Stream Score.Event] -> [(RealTime, Int)]
    -> Stream.Stream Score.Event
switch [] _ = mempty
switch streams bps = mconcatMap select (Lists.zipNext bps)
    where
    -- This is a little bit inefficient because it scans from the beginning of
    -- each stream, but the number of events and streams is likely small.
    select ((t, i), next) = case next of
        Nothing -> Stream.dropWhile ((<t) . Score.eventStart) (streams !! i)
        Just (nextT, _) -> extrect t nextT (streams !! i)
    extrect start end =
        fmap (clip end) . Stream.takeWhile ((<end) . Score.eventStart)
            . Stream.dropWhile ((<start) . Score.eventStart)
    clip end event
        | Score.eventEnd event <= end = event
        | otherwise =
            Score.setDuration (max 0 (end - Score.eventStart event)) event

alternateIndices :: [ScoreTime] -> Meter.Rank -> [RealTime]
    -> Derive.Deriver [Int]
    -- ^ time to switch to which index
alternateIndices starts timestep thresholds = do
    -- reals <- mapM Derive.real starts
    -- Debug.tracepM "reals" (zip starts reals)
    -- Get tempo at each start, in the duration of the timestep at each start.
    durs <- mapM (timestepDurAt timestep) starts
    pure $ -- Debug.traceRetp "alt_indices" (thresholds, zip starts durs) $
        map (indexUnderThreshold thresholds) durs

timestepDurAt :: Meter.Rank -> ScoreTime -> Derive.Deriver RealTime
timestepDurAt timestep p = do
    Call.realDuration p =<< Call.meterDuration p timestep 1

-- | Select the given indices from the list, and return a list with just the
-- indexed elements, and the original indices packed so they index into the
-- dense list.
selectIndices :: [a] -> [Int] -> ([a], [Int])
selectIndices xs is = (map (xs!!) unique, map pack is)
    where
    unique = Lists.uniqueSort is
    pack i = fromMaybe 0 $ List.elemIndex i unique

indexUnderThreshold :: Ord a => [a] -> a -> Int
indexUnderThreshold ts val = go 0 ts
    where
    go i (t:ts)
        | val <= t = i
        | otherwise = go (i+1) ts
    go i [] = i

-- * val calls

cValAlternate :: Derive.ValCall
cValAlternate = Derive.valCall Module.prelude "alternate" Tags.random
    "Pick one of the arguments randomly."
    $ Sig.call (Sig.many1 "val" "Value of any type.") $ \vals _ ->
        Call.pick (vals :: NonEmpty DeriveT.Val) <$> Call.random

cValAlternateWeighted :: Derive.ValCall
cValAlternateWeighted = Derive.valCall Module.prelude "alternate-weighted"
    Tags.random "Pick one of the arguments randomly."
    $ Sig.call (Sig.many1Pairs "val" "(weight, val) pairs.") $
    \pairs _args -> do
        let vals :: NonEmpty DeriveT.Val
            (weights, vals) = NonEmpty.unzip pairs
        Call.pickWeighted (NonEmpty.zip weights vals) <$> Call.random

cRange :: Derive.ValCall
cRange = Derive.valCall Module.prelude "range" Tags.random
    "Pick a random number within a range." $ Sig.call ((,)
    <$> Sig.defaulted "low" (0 :: Int) "Bottom of range, inclusive."
    <*> Sig.defaulted "high" (1 :: Int) "Top of range, inclusive."
    ) $ \(low, high) _args -> Call.randomIn low high :: Derive.Deriver Double
