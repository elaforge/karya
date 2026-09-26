-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Utilities for calls.

    The convention for calls is that there is a function @c_something@ which
    is type NoteCall or ControlCall or whatever.  It then extracts what is
    needed from the PassedArgs and passes those values to a function
    @something@ which is of type NoteDeriver or ControlDeriver or whatever.
    The idea is that PassedArgs is a large dependency and it should be reduced
    immediately to what is needed.
-}
module Derive.Call where
import qualified Data.List as List
import qualified Data.Set as Set
import qualified System.Random.Mersenne.Pure64 as Pure64

import qualified Util.Num as Num
import qualified Util.Random as Random
import qualified Cmd.TimeStep as TimeStep
import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Eval as Eval
import qualified Derive.Expr as Expr
import qualified Derive.Flags as Flags
import qualified Derive.PSignal as PSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Scale as Scale
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Symbols as Symbols
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import qualified Ui.Meter.Meter as Meter
import qualified Ui.ScoreTime as ScoreTime

import           Global
import           Types


-- * dynamic

-- | Unlike 'Derive.pitch_at', the transposition has already been applied.
transposed :: RealTime -> Derive.Deriver (Maybe PSignal.Transposed)
transposed pos =
    justm (Derive.pitchAt pos) $ fmap Just . Derive.resolvePitch pos

getTransposed :: RealTime -> Derive.Deriver PSignal.Transposed
getTransposed pos = Derive.require ("no pitch at " <> pretty pos)
    =<< transposed pos

-- | Pitch without the transposition applied.  You have to use this if you
-- create an event with a pitch based on this pitch, otherwise the
-- transposition will be applied twice.
getPitch :: RealTime -> Derive.Deriver PSignal.Pitch
getPitch pos = Derive.require ("no pitch at " <> pretty pos)
    =<< Derive.pitchAt pos

getPitchHere :: Derive.PassedArgs a -> Derive.Deriver PSignal.Pitch
getPitchHere = getPitch <=< Args.realStart

-- | Get the symbolic version of the transposed pitch.  Since it's transposed,
-- if you turn it back to a 'PSignal.Pitch', you should use
-- 'with_transposed_pitch'.
getParsedPitch :: (Pitch.Note -> Maybe Pitch.Pitch)
    -- ^ Parse pitch function, as returned by 'get_pitch_functions'.
    -- It's passed separately to avoid the overhead of calling
    -- get_pitch_functions multiple times.
    -> RealTime -> Derive.Deriver Pitch.Pitch
getParsedPitch parse = parsePitch parse <=< getTransposed

getSymbolicPitch :: RealTime -> Derive.Deriver Pitch.Note
getSymbolicPitch = Pitches.pitchNote <=< getTransposed

dynamic :: RealTime -> Derive.Deriver Signal.Y
dynamic pos = maybe Derive.defaultDynamic ScoreT.valOf <$>
    Derive.controlAt Controls.dynamic pos

withPitch :: PSignal.Pitch -> Derive.Deriver a -> Derive.Deriver a
withPitch = Derive.withConstantPitch

withTransposedPitch :: PSignal.Transposed -> Derive.Deriver a
    -> Derive.Deriver a
withTransposedPitch pitch =
    withoutTranspose . withPitch (PSignal.coerce pitch)

withoutTranspose :: Derive.Deriver a -> Derive.Deriver a
withoutTranspose = Derive.removeControls (Set.toList Controls.transposers)

withSymbolicPitch :: DeriveT.PitchCall -> ScoreTime -> Derive.Deriver a
    -> Derive.Deriver a
withSymbolicPitch call pos deriver = do
    pitch <- Eval.evalPitch pos call
    withPitch pitch deriver

-- | Replace the dynamic with the given one.
withDynamic :: Signal.Y -> Derive.Deriver a -> Derive.Deriver a
withDynamic = withConstant Controls.dynamic

multiplyDynamic :: Signal.Y -> Derive.Deriver a -> Derive.Deriver a
multiplyDynamic = multiplyConstant Controls.dynamic

withConstant :: ScoreT.Control -> Signal.Y -> Derive.Deriver a
    -> Derive.Deriver a
withConstant control = Derive.withControl control . ScoreT.untyped
    . Signal.constant

addControl, multiplyControl :: ScoreT.Control -> ScoreT.Typed Signal.Control
    -> Derive.Deriver a -> Derive.Deriver a
addControl = Derive.withMergedControl Derive.mergeAdd
multiplyControl = Derive.withMergedControl Derive.mergeMul

addConstant, multiplyConstant :: ScoreT.Control -> Signal.Y
    -> Derive.Deriver a -> Derive.Deriver a
multiplyConstant control val
    | val == 1 = id
    | otherwise = Derive.withMergedControl Derive.mergeMul control
        (ScoreT.untyped (Signal.constant val))
addConstant control val
    | val == 0 = id
    | otherwise = Derive.withMergedControl Derive.mergeAdd control
        (ScoreT.untyped (Signal.constant val))

-- * environ

getSrate :: Derive.Deriver RealTime
getSrate = RealTime.seconds <$> Derive.getVal EnvKey.srate

getScale :: Derive.Deriver Scale.Scale
getScale = Derive.getScale =<< getScaleId

lookupScale :: Derive.Deriver (Maybe Scale.Scale)
lookupScale = Derive.lookupScale =<< getScaleId

getScaleId :: Derive.Deriver Pitch.ScaleId
getScaleId = Expr.strToScaleId <$> Derive.getVal EnvKey.scale

lookupKey :: Derive.Deriver (Maybe Pitch.Key)
lookupKey = fmap Pitch.Key <$> Derive.lookupVal EnvKey.key

getInstrument :: Derive.Deriver ScoreT.Instrument
getInstrument = Derive.getVal EnvKey.instrument

lookupInstrument :: Derive.Deriver (Maybe ScoreT.Instrument)
lookupInstrument = Derive.lookupVal EnvKey.instrument

getAttributes :: Derive.Deriver Attrs.Attributes
getAttributes = fromMaybe mempty <$> Derive.lookupVal EnvKey.attributes

-- * parsing pitches

-- | Get symbolic pitch manipulating functions for the current scale.  This
-- is for calls that want to work with symbolic pitches.
getPitchFunctions :: Derive.Deriver
    ( Pitch.Note -> Maybe Pitch.Pitch
    , Pitch.Pitch -> Maybe Pitch.Note
    , Scale.Transposition -> Pitch.Step -> Pitch.Pitch -> Maybe Pitch.Pitch
    )
getPitchFunctions = do
    scale <- getScale
    env <- Derive.getEnviron
    let transpose transposition steps =
            toMaybe . Scale.scaleTranspose scale transposition env steps
    pure
        ( toMaybe . Scale.scaleRead scale env
        , toMaybe . Scale.scaleShow scale env
        , transpose
        )
    where toMaybe = either (const Nothing) Just

parsePitch :: (Pitch.Note -> Maybe a) -> PSignal.Transposed
    -> Derive.Deriver a
parsePitch parse pitch = do
    note <- Pitches.pitchNote pitch
    Derive.require "unparseable pitch" $ parse note

chromaticDifference :: PSignal.Transposed -> PSignal.Transposed
    -> Derive.Deriver Pitch.Semi
chromaticDifference = pitchDifference Scale.chromaticDifference

diatonicDifference :: PSignal.Transposed -> PSignal.Transposed
    -> Derive.Deriver Pitch.PitchClass
diatonicDifference = pitchDifference Scale.diatonicDifference

pitchDifference :: (Derive.Layout -> Pitch.Pitch -> Pitch.Pitch -> a)
    -> PSignal.Transposed -> PSignal.Transposed -> Derive.Deriver a
pitchDifference difference p1 p2 = do
    scale <- getScale
    env <- Derive.getEnviron
    let parse scale env = Scale.scaleRead scale env <=< PSignal.pitchNote
    let msg = pretty p1 <> " - " <> pretty p2 <> ": "
    Derive.requireRight ((msg<>) . pretty) $
        difference (Scale.scaleLayout scale) <$>
            parse scale env p1 <*> parse scale env p2

nnDifference :: RealTime -> PSignal.Pitch -> PSignal.Pitch
    -> Derive.Deriver Pitch.NoteNumber
nnDifference pos pitch1 pitch2 = do
    pitch1 <- Derive.resolvePitch pos pitch1
    pitch2 <- Derive.resolvePitch pos pitch2
    (-) <$> Pitches.pitchNn pitch1 <*> Pitches.pitchNn pitch2

-- * note

evalPitch_ :: ScoreTime -> Pitch.Pitch -> Derive.Deriver PSignal.Transposed
evalPitch_ start pitch = do
    (_, showPitch, _) <- getPitchFunctions
    evalPitch showPitch start pitch

-- | Evaluate a 'Pitch.Pitch'.  It returns a transposed pitch since
-- a 'Pitch.Pitch' is assumed to have been transposed (e.g. 'get_parsed_pitch'
-- uses a transposed pitch so range calculation works).
evalPitch :: (Pitch.Pitch -> Maybe Pitch.Note) -> ScoreTime -> Pitch.Pitch
    -> Derive.Deriver PSignal.Transposed
evalPitch showPitch start pitch = do
    note <- Derive.require ("scale doesn't have pitch: " <> pretty pitch)
        (showPitch pitch)
    evalNote start note

-- | Evaluate a symbolic pitch.  Like 'eval_pitch', I assume the Note was
-- Transposed, or at least should be an absolute pitch.
evalNote :: ScoreTime -> Pitch.Note -> Derive.Deriver PSignal.Transposed
evalNote pos note = Eval.evalPitch pos $
    Expr.call0 (Expr.Symbol (Pitch.noteText note))

-- | Generate a single note, from 0 to 1.
note :: Derive.NoteDeriver
note = Eval.evalOneCall True $ Expr.call0 Symbols.nullNote

-- | Like 'note', but the note reuses the Context, which means it will inherit
-- the caller's start and duration as well as sub-tracks and thus may apply
-- inversion.
--
-- This is appropriate when adding a wrapper around the default note call, but
-- not if you don't want to be overridden by sub-tracks.  See 'placed_note'
-- if you want to inherit the time, but not the rest.
reapplyNote :: Derive.NoteArgs -> Derive.NoteDeriver
reapplyNote args = Eval.reapplyCall (Args.context args) Symbols.nullNote []

-- | Override the pitch signal and generate a single note.
pitchedNote :: PSignal.Pitch -> Derive.NoteDeriver
pitchedNote pitch = withPitch pitch note

transposedPitchedNote :: PSignal.Transposed -> Derive.NoteDeriver
transposedPitchedNote pitch = withTransposedPitch pitch note

-- | Add an attribute and generate a single note.
attributeNote :: Attrs.Attributes -> Derive.NoteDeriver
attributeNote attrs = addAttributes attrs note

-- | A zero-duration 'note'.
triggeredNote :: Derive.NoteDeriver
triggeredNote =
    Eval.evalOneAt True 0 0 $ Expr.generator0 Symbols.nullNote

place :: Derive.PassedArgs d -> Derive.Deriver a -> Derive.Deriver a
place = uncurry Derive.place . Args.extent

placedNote :: Derive.PassedArgs d -> Derive.NoteDeriver
placedNote args = place args note

-- * transformer notes

-- | Derive with transformed Attributes.
withAttributes :: (Attrs.Attributes -> Attrs.Attributes) -> Derive.Deriver d
    -> Derive.Deriver d
withAttributes f deriver = do
    attrs <- getAttributes
    Derive.withVal EnvKey.attributes (f attrs) deriver

addAttributes :: Attrs.Attributes -> Derive.Deriver d -> Derive.Deriver d
addAttributes attrs
    | attrs == mempty = id
    | otherwise = withAttributes (<> attrs)

addFlags :: Flags.Flags -> Derive.NoteDeriver -> Derive.NoteDeriver
addFlags flags
    | flags == mempty = id
    | otherwise = fmap (fmap (Score.addFlags flags))

-- * random

-- | Get an infinite list of random numbers.  These are deterministic in that
-- they depend only on the random seed, but the random seed is hashed with
-- each stack entry.  So if you fix the random seed at a certain point, you
-- should get consistent results below it.
--
-- It's a class because both Doubles and Ints are useful and I'd like to use
-- the same function name for both.
class Random a where
    -- | Infinite list of random numbers.  These are deterministic in that
    -- they depend on the current track, current call position, and the random
    -- seed.
    randoms :: Derive.Deriver [a]
    -- | Infinite list of random numbers in the given range.
    randomsIn :: a -> a -> Derive.Deriver [a]

instance Random Double where
    -- | Random numbers in the range [0, 1).
    randoms = _makeRandoms Pure64.randomDouble
        -- Pure64.randomDouble doesn't document the range, but that's what it
        -- is.
    randomsIn low high = map (Num.scale low high) <$> randoms

instance Random Int where
    -- Random numbers between INT_MIN and INT_MAX.
    randoms = _makeRandoms Pure64.randomInt
    randomsIn low high = map (Num.restrict low high) <$> randoms

-- | Get a random Double or Int.  Ints will lose precision if converted to
-- double!
random :: Random a => Derive.Deriver a
random = head <$> randoms

randomIn :: (Random a, Real a) => a -> a -> Derive.Deriver a
randomIn low high
    | low == high = pure low
    | otherwise = head <$> randomsIn low high

-- | If the chance is 1, return true all the time, if it's 0.5, return it half
-- of the time.
chance :: Double -> Derive.Deriver Bool
chance v
    | v >= 1 = pure True
    | v <= 0 = pure False
    | otherwise = do
        r <- randomIn 0 1
        pure $ r <= v

shuffle :: [a] -> Derive.Deriver [a]
shuffle xs = Random.shuffle xs <$> randoms

_makeRandoms :: (Pure64.PureMT -> (a, Pure64.PureMT)) -> Derive.Deriver [a]
_makeRandoms f = List.unfoldr (Just . f) <$> _randomGenerator

_randomGenerator :: Derive.Deriver Pure64.PureMT
_randomGenerator = do
    seed <- fromMaybe 0 <$> Derive.lookupVal EnvKey.seed
    pure $ Pure64.pureMT (floor (seed :: Double))

pickWeighted :: NonEmpty (Double, a) -> Double -> a
pickWeighted weights rnd_ = go 0 weights
    where
    rnd = rnd_ * Num.sum (fmap fst weights)
    go collect ((weight, a) :| weights) = case weights of
        [] -> a
        w : ws
            | collect + weight > rnd -> a
            | otherwise -> go (collect + weight) (w :| ws)

-- | Like 'pick_weighted' when all the weights are equal.
pick :: NonEmpty a -> Double -> a
pick (x :| xs) rnd = (x:xs) !! i
    where i = round (rnd * fromIntegral (length xs))

-- TODO what I want is a bounded normal distribution.
-- Unfortunately it seems to be really complicated to actually sample that.
-- I could use anything with a similar shape actually.
normal :: Double -> Derive.Deriver Double
normal stddev = makeNormal stddev <$> randoms
{- notes:
    . Approximate normal distribution: sum (take n randoms) / n
    . normalCumulative mean stddev x =
          SpecFunctions.erfc ((mean - x) / ndCdfDenom) / 2
          where
          ndCdfDenom = Constants.m_sqrt_2 * stddev
    . Make a with_variation, so the choice is in the call, not the patch.
    . I want to give center and width, and then pick according to that
      distribution.  Alternately, if I can map a uniform 0-1.
    . Truncated normal distribution seems best, and I can map a uniformly
      distributed value through its cumulative probability function.
    . This is called "inverse transform sampling".  It's possible for
      truncated normal, but complicated:
      https://www.christophlassner.de/blog/2013/08/12/Generation-of-Truncated-Gaussian-Samples/
    . Rejection sampling just means I do a 2d normal distribution until
      I get something under the PDF.  Theoretically unbound time.
    . I don't care about the exact statistical properties, just that it
      has a similar shape.
-}

-- | Approximation to a normal distribution between 0 and 1, inclusive.
-- I can't use an actual normal distribution because I need it to be bounded.
makeNormal :: Double -> [Double] -> Double
makeNormal stddev rnds = Num.sum (take samples rnds) / fromIntegral samples
    where
    samples = 12

-- * conditional

ifEnv :: (Eq val, Typecheck.Typecheck val) => EnvKey.Key -> Maybe val
    -> Derive.Deriver a -> Derive.Deriver a -> Derive.Deriver a
ifEnv key val isSet notSet =
    ifM ((==val) <$> Derive.lookupVal key) isSet notSet

whenEnv :: (Eq val, Typecheck.Typecheck val) => EnvKey.Key -> Maybe val
    -> (Derive.Deriver a -> Derive.Deriver a)
    -> Derive.Deriver a -> Derive.Deriver a
whenEnv key val transformer deriver =
    ifEnv key val (transformer deriver) deriver

-- * time

-- | Get the real duration of time val at the given point in time.  RealTime is
-- linear, so 1 second is always 1 second no matter where it is, but ScoreTime
-- will map to different amounts of RealTime depending on where it is.
realDuration :: (Derive.Time t1, Derive.Time t2) => t1 -> t2
    -> Derive.Deriver RealTime
realDuration start dur = case Derive.toDuration dur of
    DeriveT.RealDuration t -> pure t
    DeriveT.ScoreDuration t
        | t == 0 -> pure 0
        | otherwise -> do
            -- I'm adding score to real, so I want the amount of real time in
            -- the future I am if I advance the given amount of score time from
            -- 'start'.
            scoreStart <- Derive.score start
            realStart <- Derive.real start
            end <- Derive.real $ scoreStart + t
            pure $ end - realStart

-- | Like 'real_duration', but get the duration in ScoreTime.  If you are
-- manipulating deriver abstractly instead of directly emitting events then you
-- will place them via 'Derive.at' and family, which are in ScoreTime.
scoreDuration :: (Derive.Time t1, Derive.Time t2) => t1 -> t2
    -> Derive.Deriver ScoreTime
scoreDuration start dur = case Derive.toDuration dur of
    DeriveT.ScoreDuration t -> pure t
    DeriveT.RealDuration t
        | t == 0 -> pure 0
        | otherwise -> do
            -- I'm adding real to score, so I want the amount of amount of
            -- score time I'd have to advance in order for the given amount
            -- of real time to pass.
            scoreStart <- Derive.score start
            realStart <- Derive.real start
            end <- Derive.score $ realStart + t
            pure $ end - scoreStart

-- | A time range from the event start until a given duration.
durationFromStart :: Derive.Time t => Derive.PassedArgs d -> t
    -> Derive.Deriver (RealTime, RealTime) -- ^ (start, start+dur)
durationFromStart args t = do
    start <- Args.realStart args
    dur <- realDuration start t
    pure (start, start + dur)

-- | Like 'duration_from_start', but subtract a duration from the end.
durationFromEnd :: Derive.Time t => Derive.PassedArgs d -> t
    -> Derive.Deriver (RealTime, RealTime) -- ^ (end-dur, end)
durationFromEnd args t = do
    end <- Args.realEnd args
    dur <- realDuration end t
    pure (end - dur, end)

-- | This is 'real_duration', but takes a ScoreT.Typed Signal.Y.
typedRealDuration :: Derive.Time t => ScoreT.TimeT -> t
    -> ScoreT.Typed Signal.Y -> Derive.Deriver RealTime
typedRealDuration defaultType from (ScoreT.Typed typ val)
    | typ == ScoreT.Real
        || typ == ScoreT.Untyped && defaultType == ScoreT.TReal =
            pure (RealTime.seconds val)
    | typ == ScoreT.Score
        || typ == ScoreT.Untyped && defaultType == ScoreT.TScore =
            realDuration from (ScoreTime.fromDouble val)
    | otherwise = Derive.throw $
        "expected time type for " <> ShowVal.showVal (ScoreT.Typed typ val)

realDurationAt :: ScoreT.TypedFunction -> RealTime
    -> Derive.Deriver RealTime
realDurationAt f t = typedRealDuration ScoreT.TReal t (($ t) <$> f)

-- ** timestep

-- | Take the given number of steps.  Negative means step back.
timestep :: ScoreTime -> TimeStep.TimeStep
    -> [Int] -- ^ pick the first steps that return Just
    -> Derive.Deriver ScoreTime
timestep start ts steps = do
    (blockId, tracknum) <- Internal.getCurrentTracknum
    Derive.require ("no valid timestep from " <> ShowVal.showVal start)
        =<< Derive.evalUi
            (firstJusts [TimeStep.stepFrom step ts blockId tracknum start |
                step <- steps])

-- | Get the timestep duration from the given point.  This tries first to
-- step forward, and then back.  This is because typically you use this to
-- configure duration for a call, and it's confusing when the call stops
-- working at the end of the block.
meterDuration :: ScoreTime -> Meter.Rank -> Int
    -> Derive.Deriver ScoreTime
meterDuration start rank steps = do
    end <- timestep start ts (map (*steps) [1, -1])
    pure $ abs (end - start)
    where
    ts = TimeStep.timeStep $ TimeStep.RelativeMark TimeStep.matchMeter rank

-- | Duration of a single timestep, starting here.
timestepDuration :: Derive.PassedArgs a -> Meter.Rank
    -> Derive.Deriver ScoreTime
timestepDuration args step = meterDuration (Args.start args) step 1


-- * general purpose types

-- | This is for arguments which can be high or low.
data UpDown = Up | Down deriving (Show, Enum, Bounded, Eq, Ord)

invert :: UpDown -> UpDown
invert Up = Down
invert Down = Up

instance Pretty UpDown where pretty = showt
instance Typecheck.Typecheck UpDown
instance Typecheck.ToVal UpDown
instance ShowVal.ShowVal UpDown where
    showVal Up = "u"
    showVal Down = "d"

data Hand = L | R deriving (Show, Enum, Bounded, Eq, Ord)

otherHand :: Hand -> Hand
otherHand L = R
otherHand R = L

instance Pretty Hand where pretty = showt
instance Typecheck.Typecheck Hand
instance Typecheck.ToVal Hand
instance ShowVal.ShowVal Hand where
    showVal L = "l"
    showVal R = "r"
