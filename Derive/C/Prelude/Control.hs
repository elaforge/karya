-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Basic calls for control tracks.
module Derive.C.Prelude.Control (library) where
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map

import qualified Util.Doc as Doc
import qualified Util.Num as Num
import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Call.ControlUtil as ControlUtil
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Expr as Expr
import qualified Derive.Library as Library
import qualified Derive.Parse as Parse
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import           Global
import           Types


library :: Library.Library
library = mconcat
    [ Library.generators $
        [ ("set", cSet)
        , ("'", cSetPrev)
        , ("p", cPorta)
        , ("abs", cAbs)
        , ("dyn-pp", cDynamic "pp" 0.05)
        , ("dyn-p", cDynamic "p" 0.25)
        , ("dyn-mf", cDynamic "mf" 0.5)
        , ("dyn-f", cDynamic "f" 0.75)
        , ("dyn-ff", cDynamic "ff" 0.95)

        -- misc
        , ("bp>", cBreakpointNext)
        , ("bpm", cBpm)
        , ("n", cNeighbor)
        , ("d", cDown)
        , ("df", cDownFrom)
        , ("uf", cUpFrom)
        , ("u", cUp)

        -- not sure which one I'll like better
        , ("`ped`", cPedal)
        , ("ped", cPedal)
        , ("swell", cSwell)
        ] ++ ControlUtil.interpolatorVariations
    , Library.pattern patternGenerator
    , Library.pattern patternTransformer
    ]

-- | This is a special pattern for control tracks that lets you directly type
-- a number, and have that be interpreted as setting the control to that value.
-- In addition, it allows a special hex syntax
--
-- Formerly, control tracks used a slightly different parser to enable the same
-- thing, but that turned out to be awkward when I wanted to implement
-- 'Derive.Eval.eval_event'.
patternGenerator :: Derive.PatternCall (Derive.Generator Derive.Control)
patternGenerator = patternCall $ \val -> generator1 "set" mempty
    "Emit a sample with no interpolation. This accepts either decimal\
    \ numbers or hex numbers that look like `\\`0x\\`xx`.  The hex\
    \ is divided by 255, so they represent a number between 0 and 1.\n" $
    Sig.call0 $ \args -> do
        pos <- Args.realStart args
        pure $! Signal.fromSample pos val

patternTransformer :: Derive.PatternCall (Derive.Transformer Derive.Control)
patternTransformer = patternCall $ \val ->
    Derive.transformer Module.prelude "set" mempty
    "Prepend a sample to a signal. This is useful to create a discontinuity,\
    \ e.g. interpolate to a value and then jump to another one."
    $ Sig.call0t $ \args deriver -> do
        pos <- Args.realStart args
        Post.signal (<> Signal.fromSample pos val) deriver

patternCall :: (Signal.Y -> Derive.Call d)
    -> Derive.PatternCall (Derive.Call d)
patternCall call = Derive.PatternCall
    { patDescription = "numbers and hex"
    , patDoc = Derive.extractDoc (call 0)
    , patFunction = \(Expr.Symbol sym) -> pure $! case Parse.parseNum sym of
        Left _ -> Nothing
        Right val -> Just $ call val
    }

cSet :: Derive.Generator Derive.Control
cSet = generator1 "set" mempty "Emit a sample with no interpolation." $
    Sig.call (Sig.required "to" "Destination value.") $ \to args -> do
        pos <- Args.realStart args
        pure $! Signal.fromSample pos to

cSetPrev :: Derive.Generator Derive.Control
cSetPrev = Derive.generator Module.prelude "set-prev" Tags.prev
    "Re-set the previous value. This can be used to extend a breakpoint."
    $ Sig.call0 $ \args -> case Args.prevControl args of
        Nothing -> pure Stream.empty
        Just (x, y) -> do
            start <- Args.realStart args
            pure $ if start > x
                then Stream.fromEvent $ Signal.fromSample start y
                else Stream.empty

cPorta :: Derive.Generator Derive.Control
cPorta = generator1 "porta" mempty
    "Interpolate between two values. This is similar to `i>>`,  but intended\
    \ to be higher level, in that instruments or scores can override it to\
    \ represent an idiomatic portamento."
    $ Sig.call ((,,,,)
    <$> Sig.required "to" "Destination value."
    <*> (Typecheck._real <$> Sig.defaulted "time"
        ControlUtil.defaultInterpolationTime "Time to reach destination.")
    <*> Sig.defaultedEnv "place" Sig.Both (Typecheck.Normalized 1)
        "Placement, from before to after the call."
    <*> ControlUtil.fromEnv <*> ControlUtil.curveEnv
    ) $ \(to, time, place, from, curve) args -> do
        let maybeFrom = from <|> (snd <$> Args.prevControl args)
        time <- if Args.duration args == 0
            then pure time
            else DeriveT.RealDuration <$> Args.realDuration args
        (start, end) <- ControlUtil.placeRange place (Args.start args) time
        ControlUtil.makeSegmentFrom curve start maybeFrom end to

cAbs :: Derive.Generator Derive.Control
cAbs = Derive.generator1 Module.prelude "abs" mempty
    "Set the control to an absolute value, provided this control is combined\
    \ via multiplication."
    $ Sig.call (Sig.required "val" "Set to this value.") $ \val args ->
        setAbsolute val =<< Args.realStart args

cDynamic :: Derive.CallName -> Signal.Y -> Derive.Generator Derive.Control
cDynamic name val = Derive.generator1 Module.prelude name mempty
    "Set the control to an absolute value. This is useful for the `dyn`\
    \ control, so a part can override the dynamic in scope."
    $ Sig.call (Sig.defaulted "val" val "Set to this value.") $ \val args ->
        setAbsolute val =<< Args.realStart args

setAbsolute :: Signal.Y -> RealTime -> Derive.Deriver Signal.Control
setAbsolute val pos = do
    control <- Derive.lookupVal EnvKey.control
    merge <- Derive.lookupVal EnvKey.merge
    out <- set control merge
    pure $ Signal.fromSample pos out
    where
    set Nothing _ = pure val
    set (Just control) Nothing =
        Derive.throw $ "merge not set for " <> pretty control
    set (Just control) (Just merge) =
        maybe (pure val) (Derive.requireRight id . invertMerge merge val)
            =<< Derive.untypedControlAt (ScoreT.Control control) pos

-- | Figure out what value to emit which, once merged with the signal in scope,
-- will become the given absolute value.  TODO this is kind of a crazy hack and
-- I'm not sure if I'm in love with it.
invertMerge :: Text -> Signal.Y -> Signal.Y -> Either Text Signal.Y
invertMerge merge val currentVal = case Map.lookup merge inverters of
    Nothing -> Left $ "no way to invert merge type: " <> merge
    Just f -> Right $ f currentVal val
    where
    inverters = Map.fromList
        [ ("set", \_ new -> new)
        , (n Derive.mergeAdd, \old new -> new - old)
        , (n Derive.mergeSub, \old new -> old - new)
        , (n Derive.mergeMul, \old new -> if old == 0 then 0 else new / old)
        , (n Derive.mergeScale, Signal.scaleInvert)
        ]
    n (Derive.Merger name _ _) = name
    n Derive.Set = "set"
    n Derive.Unset = "unset"


-- * misc

-- TODO it's linear for now, but I could add an env val to set interpolation
cBreakpointNext :: Derive.Generator Derive.Control
cBreakpointNext = generator1 "breakpoint" mempty
    "Interpolate between the given values. Breakpoints start at this event and\
    \ end at the next one."
    $ Sig.call (Sig.many1 "val" "Breakpoints are distributed evenly between\
        \ this event and the next event.")
    $ \vals args -> do
        (start, end) <- Args.realRangeOrNext args
        srate <- Call.getSrate
        pure $ ControlUtil.breakpoints srate ControlUtil.Linear $
            ControlUtil.distribute start end (NonEmpty.toList vals)

cBpm :: Derive.Generator Derive.Control
cBpm = generator1 "bpm" mempty
    "Set a tempo value for the given bpm, which is just (/60)."
    $ Sig.call (Sig.required "bpm" "")
    $ \val args -> do
        pos <- Args.realStart args
        pure $! Signal.fromSample pos (val / 60)

cNeighbor :: Derive.Generator Derive.Control
cNeighbor = generator1 "neighbor" mempty
    ("Emit a slide from a value to 0 in absolute time. This is the control\
    \ equivalent of the neighbor pitch call."
    ) $ Sig.call ((,,)
    <$> Sig.defaulted "neighbor" (1 :: Double) "Start at this value."
    <*> Sig.defaulted "time" (Typecheck.real 0.1) "Time taken to get to 0."
    <*> ControlUtil.curveEnv
    ) $ \(neighbor, Typecheck.DefaultReal time, curve) args -> do
        (start, end) <- Call.durationFromStart args time
        ControlUtil.makeSegment curve start neighbor end 0

cUp :: Derive.Generator Derive.Control
cUp = generator1 "u" Tags.prev
    "Ascend at the given speed until the value reaches a limit or the next\
    \ event."
    $ Sig.call ((,,,)
    <$> Sig.defaulted "speed" (1 :: Double) "Ascend this amount per second."
    <*> Sig.defaulted "limit" (Nothing :: Maybe Double) "Stop at this value."
    <*> ControlUtil.fromEnv
    <*> Sig.environKey EnvKey.controlGt0 False
        "Whether or not to limit to 1 by default."
    ) $ \(speed, limit, from, gt0) args ->
        makeSlope args Nothing
            (if gt0 then limit else Just $ fromMaybe 1 limit)
            from speed

cDown :: Derive.Generator Derive.Control
cDown = generator1 "d" Tags.prev
    "Descend at the given speed until the value reaches a limit or the next\
    \ event."
    $ Sig.call ((,,)
    <$> Sig.defaulted "speed" (1 :: Double) "Descend this amount per second."
    <*> Sig.defaulted "limit" (0 :: Double) "Stop at this value."
    <*> ControlUtil.fromEnv
    ) $ \(speed, limit, from) args ->
        makeSlope args (Just limit) Nothing from (-speed)

cDownFrom :: Derive.Generator Derive.Control
cDownFrom = generator1 "df" mempty
    "Drop from a certain value. This is like `d` with `from`, but more\
    \ convenient to write."
    $ Sig.call ((,,)
    <$> Sig.defaulted "from" (1 :: Double) "Start at this value."
    <*> Sig.defaulted "speed" (1 :: Double) "Descend this amount per second."
    <*> Sig.defaulted "limit" (0 :: Double) "Stop at this value."
    ) $ \(from, speed, limit) args ->
        makeSlope args (Just limit) Nothing (Just from) (-speed)

cUpFrom :: Derive.Generator Derive.Control
cUpFrom = generator1 "uf" mempty "Like df, but up."
    $ Sig.call ((,,)
    <$> Sig.defaulted "from" (0 :: Double) "Start at this value."
    <*> Sig.defaulted "speed" (1 :: Double) "Ascend this amount per second."
    <*> Sig.defaulted "limit" (1 :: Double) "Stop at this value."
    ) $ \(from, speed, limit) args ->
        makeSlope args (Just limit) Nothing (Just from) speed

makeSlope :: Derive.ControlArgs -> Maybe Signal.Y -> Maybe Signal.Y
    -> Maybe Signal.Y -> Double -> Derive.Deriver Signal.Control
makeSlope args low high maybeFrom slope =
    case maybeFrom <|> ControlUtil.prevVal maybeFrom args of
        Nothing -> pure mempty
        Just from -> do
            (start, end) <- Args.realRangeOrNext args
            pure $ ControlUtil.slopeToLimit low high from slope start end

cPedal :: Derive.Generator Derive.Control
cPedal = generator1 "pedal" mempty
    ("Unlike most control events, this uses a duration. Set the control to\
    \ the given value for the event's duration, and reset to the old\
    \ value afterwards."
    ) $ Sig.call ((,)
    <$> Sig.defaulted "val" (1 :: Double) "Set to this value."
    <*> Sig.environ "dur" Sig.Prefixed (0.05 :: Double)
        "Use this duration if the event duration is 0."
    ) $ \(val, dur) args -> do
        (start, end) <- Args.realRange args
        end <- pure $ if start == end then end + dur else end
        let prev = maybe 0 snd $ Args.prevControl args
        pure $ Signal.fromPairs [(start, val), (end, val), (end, prev)]

cSwell :: Derive.Generator Derive.Control
cSwell = generator1 "swell" mempty
    "Start at the given value, interpolate to a peak, then back to the\
    \ original value. Uses duration."
    $ Sig.call ((,,)
    <$> Sig.required "val" "Start value."
    <*> Sig.defaulted "peak" (1 :: Double) "Interpolate to this value."
    <*> Sig.defaulted "bias" (0.5 :: Double)
        "0 puts the peak at the start, 1 at the end."
    ) $ \(val, peak, bias) args -> do
        (start, end) <- Args.realRange args
        let middle = Num.clamp start end $
                Num.scale start end (RealTime.seconds bias)
        srate <- Call.getSrate
        pure $ ControlUtil.breakpoints srate ControlUtil.Linear
            [(start, val), (middle, peak), (end, val)]

generator1 :: Derive.CallName -> Tags.Tags -> Doc.Doc
    -> Derive.WithArgDoc (Derive.PassedArgs d -> Derive.Deriver d)
    -> Derive.Call (Derive.GeneratorFunc d)
generator1 = Derive.generator1 Module.prelude
