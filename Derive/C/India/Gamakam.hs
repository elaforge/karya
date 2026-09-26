-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Carnatic style pitch ornaments.

    The names don't correspond directly with anything traditional, as far as
    I know, but are inspired by <http://www.gswift.com/article-2.html>.

    Pitch ornaments can be expressed either as pitch calls, or as control
    calls meant for a transpose track.  They both have pros and cons:

    Transposition control signal:

    - I can keep the pitches separate and clear and collapse the pitch
    track.  This correctly reflects the underlying swaram, with the gamakam
    as separate ornamentation.

    - Related to the above, each call doesn't need to repeat the the pitch arg,
    so there's less redundancy.  Calls are also simpler with one fewer
    argument.

    Pitch signal:

    - A pitch call can use absolute (@t-nn@) or scalar (@t-diatonic@)
    transposition based on the type of its arguments, while the transpose
    signal has to either use a separate track, or the somewhat awkward @->@
    call.

    - The pitch signal can represent an ornament involving multiple pitches,
    e.g. a slide frome one pitch to another.  A transposition signal can only
    represent offsets from an external pitch.

    So the pitch signal is more powerful, but the transposition signal is often
    more convenient, and can lead to less redundant notation.  Unless I can
    think of a way to get the advantages of both, I might have to have both
    around, with their own versions of the same calls.
-}
module Derive.C.India.Gamakam where
import qualified Data.List.NonEmpty as NonEmpty

import qualified Util.Doc as Doc
import qualified Util.Lists as Lists

import qualified Derive.Args as Args
import qualified Derive.C.Prelude.Trill as Trill
import qualified Derive.Call as Call
import qualified Derive.Call.ControlUtil as ControlUtil
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Expr as Expr
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Sig as Sig
import           Derive.Sig (defaulted, defaultedEnv, required)
import qualified Derive.Typecheck as Typecheck

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import           Global
import           Types


library :: Library.Library
library = mconcat
    [ Library.generators $
        [("dip", cDip)
        , ("jaru", cJaru)
        , ("sgr", cJaruIntervals ScoreT.TDiatonic [-1, 1])
        ] ++ kampitaVariations "kam" cKampita
    , Library.generators $
        [ ("dip", cDipC)
        , ("j)", jaruTransitionC "j)" Nothing
            "Time for each slide, defaults to `time`.")
        , ("j]", jaruTransitionC "j]" (Just (jaruTimeDefault / 2))
            "Time for each slide.")
        , ("sgr", cJaruIntervalsC [-1, 1])
        ]
        ++ kampitaVariations "kam" cKampitaC
        ++ kampitaVariations "nkam" cNkampitaC
    ]

module_ :: Module.Module
module_ = "india" <> "gamakam"

kampitaVariations :: Text
    -> (Maybe Trill.Direction -> Maybe Trill.Direction -> call)
    -> [(Expr.Symbol, call)]
kampitaVariations name call =
    [ (Expr.Symbol $ affix s <> name <> affix e, call s e)
    | s <- dirs, e <- dirs
    ]
    where
    affix = Trill.directionAffix
    dirs = [Nothing, Just Trill.Low, Just Trill.High]

-- * standard parameters

transitionDefault :: RealTime
transitionDefault = 0.08

jaruTimeDefault :: RealTime
jaruTimeDefault = 0.15

speedArg :: Sig.Parser Typecheck.RealTimeFunctionT
speedArg = defaulted "speed" (6 :: Int) "Alternate pitches at this speed."

neighborArg :: Sig.Parser ScoreT.Function
neighborArg = defaulted "neighbor" (1 :: Double)
    "Alternate between 0 and this value."

liltEnv :: Sig.Parser Double
liltEnv = Sig.environ "lilt" Sig.Both (0 :: Double)
    "Lilt is a horizontal bias to the\
    \ vibrato. A lilt of 1 would place each neighbor on top of the\
    \ following unison, while -1 would place it on the previous one.\
    \ So it should range from -1 < lilt < 1."

-- * pitch calls

cKampita :: Maybe Trill.Direction -> Maybe Trill.Direction
    -> Derive.Generator Derive.Pitch
cKampita startDir endDir = generator1 "kam" mempty
    "This is a kind of trill, but its interval defaults to NNs,\
    \ and transitions between the notes are smooth.  It's intended for\
    \ the vocal microtonal trills common in Carnatic music."
    $ Sig.call ((,,,,,,)
    <$> required "pitch" "Base pitch."
    <*> defaulted "neighbor" (1 :: Double)
        "Alternate with a pitch at this interval."
    <*> speedArg
    <*> defaultedEnv "transition" Sig.Both transitionDefault
        "Time for each slide."
    <*> Trill.holdEnv <*> liltEnv <*> Trill.adjustEnv
    ) $ \(pitch, Typecheck.NnTransposeFunctionT transposeT neighbor, speed,
            transition, hold, lilt, adjust) args -> do
        transpose <- kampita startDir endDir adjust neighbor speed
            transition hold lilt args
        start <- Args.realStart args
        pure $ PSignal.applyControl
            (Typecheck.transposeControl transposeT)
            (ScoreT.untyped transpose)
            (PSignal.fromSample start pitch)

trillTransitions :: Maybe Bool -> Trill.Adjust -> Double -> ScoreTime
    -> Typecheck.RealTimeFunctionT -> (ScoreTime, ScoreTime)
    -> Derive.Deriver [RealTime]
trillTransitions even adjust bias hold speed startEnd =
    Trill.adjustedTransitions config hold even startEnd
    where
    config = Trill.Config
        { _startDir = Nothing
        , _endDir = Nothing
        , _adjust = adjust
        , _hold = DeriveT.ScoreDuration 0
        , _speed = speed
        , _bias = bias
        -- Trills usually omit the transition that coincides with the end
        -- because that would create a zero duration note.  But these trills
        -- are smoothed and thus will still have a segment leading to the
        -- cut-off transition.
        , _includeEnd = True
        }

-- | Make a trill signal from a list of transition times.
trillFromTransitions :: ScoreT.Function -> ScoreT.Function
    -> [RealTime] -> [(RealTime, Signal.Y)]
trillFromTransitions val1 val2 transitions =
    [(x, sig x) | (x, sig) <- zip transitions (cycle [val1, val2])]

-- | Ok, this name is terrible but what else is better?
cDip :: Derive.Generator Derive.Pitch
cDip = generator1 "dip" mempty
    "Alternate two intervals, dropping `dyn` on the second. This is useful\
    \ when avoiding a swaram, since it doesn't necessarily emit the base\
    \ pitch."
    $ Sig.call ((,,,,,)
    <$> required "pitch" "Base pitch."
    <*> defaulted "high" (Typecheck.diatonic 1) "High interval."
    <*> defaulted "low" (-1 :: Double) "Low interval."
    <*> speedArg
    <*> defaulted "dyn" (0.5 :: Double) "Multiply dyn by this amount."
    <*> Sig.environ "transition" Sig.Both transitionDefault
        "Time for each slide."
    ) $ \(pitch, Typecheck.DefaultDiatonic high_, low, speed, dynScale,
            transition) args -> do
        let (high, control) = Controls.transposeControl high_
        transpose <- dip high low speed dynScale transition
            (Args.rangeOrNext args)
        start <- Args.realStart args
        pure $ PSignal.applyControl control (ScoreT.untyped transpose) $
            PSignal.fromSample start pitch

cJaru :: Derive.Generator Derive.Pitch
cJaru = generator1 "jaru" mempty
    "This is a series of grace notes whose pitches are relative to the given\
    \ base pitch."
    $ Sig.call ((,,,)
    <$> required "pitch" "Base pitch."
    <*> Sig.many1 "interval" "Intervals from base pitch."
    <*> Sig.environ "time" Sig.Both jaruTimeDefault "Time for each note."
    <*> Sig.environ "transition" Sig.Both (Nothing :: Maybe Sig.Dummy)
        "Time for each slide, defaults to `time`."
    ) $ \(pitch, intervals, time, maybeTransition) args -> do
        start <- Args.realStart args
        srate <- Call.getSrate
        (intervals, control) <- parse intervals
        let transition = fromMaybe time maybeTransition
        let sig = jaru srate start time transition (NonEmpty.toList intervals)
        pure $ PSignal.applyControl control (ScoreT.untyped sig) $
            PSignal.fromSample start pitch
    where
    parse intervals
        | all (==control) controls = pure (xs, control)
        | otherwise = Derive.throw "all intervals must have the same type"
        where
        (xs, control :| controls) = NonEmpty.unzip $ NonEmpty.map
            (Controls.transposeControl . Typecheck.defaultDiatonic)
            intervals

cJaruIntervals :: ScoreT.TransposeT -> [Signal.Y]
    -> Derive.Generator Derive.Pitch
cJaruIntervals transpose intervals = generator1 "jaru" mempty
    ("This is `jaru` hardcoded to " <> Doc.pretty intervals <> ".")
    $ Sig.call ((,,)
    <$> required "pitch" "Base pitch."
    <*> defaulted "time" jaruTimeDefault "Time for each note."
    <*> defaulted "transition" (Nothing :: Maybe Sig.Dummy)
        "Time for each slide, defaults to `time`."
    ) $ \(pitch, time, maybeTransition) args -> do
        start <- Args.realStart args
        srate <- Call.getSrate
        let sig = jaru srate start time (fromMaybe time maybeTransition)
                intervals
        pure $ PSignal.applyControl (Typecheck.transposeControl transpose)
            (ScoreT.untyped sig) (PSignal.fromSample start pitch)


-- * control calls

cKampitaC :: Maybe Trill.Direction -> Maybe Trill.Direction
    -> Derive.Generator Derive.Control
cKampitaC startDir endDir = generator1 "kam" mempty
    "This is a trill with smooth transitions between the notes. It's intended\
    \ for the microtonal vocal trills common in Carnatic music. `^` is high\
    \ and `_` is low, so `^kam_` starts on the upper note, and ends on the\
    \ lower one. Otherwise, it starts on the unison note and ends on either."
    $ Sig.call ((,,,,,)
    <$> neighborArg
    <*> speedArg
    <*> defaultedEnv "transition" Sig.Both transitionDefault
        "Time for each slide."
    <*> Trill.holdEnv <*> liltEnv <*> Trill.adjustEnv
    ) $ \(neighbor, speed, transition, hold, lilt, adjust) args ->
        kampita startDir endDir adjust neighbor speed transition hold lilt
            args

-- | You don't think there are too many arguments, do you?
kampita :: Maybe Trill.Direction -> Maybe Trill.Direction -> Trill.Adjust
    -> ScoreT.Function -> Typecheck.RealTimeFunctionT -> RealTime
    -> DeriveT.Duration -> Double -> Derive.PassedArgs a
    -> Derive.Deriver Signal.Control
kampita startDir endDir adjust neighbor speed transition hold lilt args = do
    start <- Args.realStart args
    let ((val1, val2), evenTransitions) = convertDirections start neighbor
            startDir endDir
    hold <- Call.scoreDuration (Args.start args) hold
    smoothTrill (-transition) val1 val2
        =<< trillTransitions evenTransitions adjust lilt hold speed
            (Args.rangeOrNext args)

smoothTrill :: RealTime -> ScoreT.Function -> ScoreT.Function
    -> [RealTime] -> Derive.Deriver Signal.Control
smoothTrill time val1 val2 transitions = do
    srate <- Call.getSrate
    pure $ ControlUtil.smoothAbsolute ControlUtil.Linear srate time $
        trillFromTransitions val1 val2 transitions

convertDirections :: RealTime -> ScoreT.Function -> Maybe Trill.Direction
    -> Maybe Trill.Direction -> ((ScoreT.Function, ScoreT.Function), Maybe Bool)
convertDirections startT neighbor start end = (vals, evenTransitions)
    where
    first = case start of
        Nothing -> Trill.Unison
        Just Trill.Low -> if neighborLow then Trill.Neighbor else Trill.Unison
        Just Trill.High -> if neighborLow then Trill.Unison else Trill.Neighbor
    vals = case first of
        Trill.Unison -> (const 0, neighbor)
        Trill.Neighbor -> (neighbor, const 0)
    -- If I end Low, and neighbor is low, and I started with Unison, then val2
    -- is low, so I want even transitions.  Why is it so complicated just to
    -- get a trill to end high or low?
    firstLow = case first of
        Trill.Unison -> not neighborLow
        Trill.Neighbor -> neighborLow
    evenTransitions = case end of
        Nothing -> Nothing
        Just Trill.Low -> Just (not firstLow)
        Just Trill.High -> Just firstLow
    neighborLow = neighbor startT < 0

cNkampitaC :: Maybe Trill.Direction -> Maybe Trill.Direction
    -> Derive.Generator Derive.Control
cNkampitaC startDir endDir = generator1 "nkam" mempty
    "`kam` with a set number of cycles. The speed adjusts to fit the cycles in\
    \ before the next event."
    $ Sig.call ((,,,,)
    <$> neighborArg
    <*> (Typecheck.positive <$> defaulted "cycles" (1 :: Double)
        "Number of cycles.")
    <*> liltEnv <*> Trill.holdEnv
    <*> Sig.environ "transition" Sig.Both transitionDefault
        "Time for each slide."
    ) $ \(neighbor, cycles, lilt, hold, transition) args -> do
        (start, end) <- Args.realRangeOrNext args
        let ((val1, val2), evenTransitions) = convertDirections start
                neighbor startDir endDir
        hold <- Call.scoreDuration (Args.start args) hold
        -- In order to hear the cycles clearly, I leave a one transition of
        -- flat space at the end.  This means nkam can't transition into the
        -- next note, but for now this seems more convenient.
        let numTransitions = 1 + cycles * 2
                + (if evenTransitions == Just True then 0 else 1)
        let speed = Typecheck.RealTimeFunctionT ScoreT.TReal
                (const $ (numTransitions - 1)
                    / RealTime.toSeconds (end - start))
        transitions <- trillTransitions Nothing Trill.Shorten lilt hold speed
                (Args.rangeOrNext args)
        smoothTrill (-transition) val1 val2 (Lists.dropEnd 1 transitions)

-- | Ok, this name is terrible but what else is better?
cDipC :: Derive.Generator Derive.Control
cDipC = generator1 "dip" mempty
    "Alternate two intervals, dropping `dyn` on the second. This is useful\
    \ when avoiding a swaram, since it doesn't necessarily emit the base\
    \ pitch."
    $ Sig.call ((,,,,)
    <$> defaulted "high" (1 :: Double) "High interval."
    <*> defaulted "low" (-1 :: Double) "Low interval."
    <*> speedArg
    <*> defaulted "dyn" (0.5 :: Double) "Multiply dyn by this amount."
    <*> Sig.environ "transition" Sig.Both transitionDefault
        "Time for each slide."
    ) $ \(high, low, speed, dynScale, transition) args ->
        dip high low speed dynScale transition (Args.rangeOrNext args)

dip :: Double -> Double -> Typecheck.RealTimeFunctionT -> Double
    -> RealTime -> (ScoreTime, ScoreTime) -> Derive.Deriver Signal.Control
dip high low speed dynScale transition (start, end) = do
    srate <- Call.getSrate
    transitions <- Trill.trillTransitions (start, end) False speed
    let smooth = ControlUtil.smoothAbsolute ControlUtil.Linear srate
            (-transition / 2)
        transpose = smooth $
            trillFromTransitions (const high) (const low) transitions
        dyn = smooth $
            trillFromTransitions (const 1) (const dynScale) transitions
    end <- Derive.real end
    ControlUtil.multiplyDyn end dyn
    pure transpose

jaruTransitionC :: Derive.CallName -> Maybe RealTime -> Doc.Doc
    -> Derive.Generator Derive.Control
jaruTransitionC name defaultTransition transitionDoc =
    generator1 name mempty
    "This is a series of grace notes with relative pitches."
    $ Sig.call ((,,)
    <$> Sig.many1 "interval" "Intervals from base pitch."
    <*> Sig.environ "time" Sig.Both jaruTimeDefault "Time for each note."
    <*> Sig.environ "transition" Sig.Both defaultTransition transitionDoc
    ) $ \(intervals, time, maybeTransition) args -> do
        start <- Args.realStart args
        srate <- Call.getSrate
        let transition = fromMaybe time maybeTransition
        pure $ jaru srate start time transition (NonEmpty.toList intervals)

cJaruIntervalsC :: [Signal.Y] -> Derive.Generator Derive.Control
cJaruIntervalsC intervals = generator1 "jaru" mempty
    ("This is `jaru` hardcoded to " <> Doc.pretty intervals <> ".")
    $ Sig.call ((,)
    <$> defaulted "time" jaruTimeDefault "Time for each note."
    <*> defaulted "transition" (Nothing :: Maybe Sig.Dummy)
        "Time for each slide, defaults to `time`."
    ) $ \(time, maybeTransition) args -> do
        start <- Args.realStart args
        srate <- Call.getSrate
        pure $ jaru srate start time (fromMaybe time maybeTransition)
            intervals

jaru :: RealTime -> RealTime -> RealTime -> RealTime -> [Signal.Y]
    -> Signal.Control
jaru srate start time transition intervals =
    ControlUtil.smoothAbsolute ControlUtil.Linear srate (-transition) $
        zip (Lists.range_ start time) (intervals ++ [0])

generator1 :: Derive.CallName -> Tags.Tags -> Doc.Doc
    -> Derive.WithArgDoc (Derive.PassedArgs d -> Derive.Deriver d)
    -> Derive.Call (Derive.GeneratorFunc d)
generator1 = Derive.generator1 module_
