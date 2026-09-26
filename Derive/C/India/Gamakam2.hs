-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls for Carnatic gamakam.
module Derive.C.India.Gamakam2 where
import qualified Control.Monad.State.Strict as Monad.State
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text

import qualified Util.Doc as Doc
import qualified Util.Lists as Lists
import qualified Util.Pretty as Pretty

import qualified Derive.Args as Args
import qualified Derive.C.Prelude.Trill as Trill
import qualified Derive.Call as Call
import qualified Derive.Call.ControlUtil as ControlUtil
import qualified Derive.Call.Module as Module
import qualified Derive.Call.PitchUtil as PitchUtil
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Eval as Eval
import qualified Derive.Expr as Expr
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import qualified Ui.Event as Event

import           Global
import           Types


module_ :: Module.Module
module_ = "india" <> "gamakam2"

-- | Calls in these modules are meant to be used via the sequence call, so they
-- are only in scope under the relevant phase.
beginModule, middleModule, endModule :: Module.Module
beginModule = module_ <> "begin"
middleModule = module_ <> "middle"
endModule = module_ <> "end"

library :: Library.Library
library = mconcat
    [ Library.generators [("@", cSequence)]
    , Library.transformers
        [ ("@", cSequenceTransform)
        , ("sahitya", cSahitya)
        ]
    -- Pitch
    , Library.generators $ concat
        [ beginCalls
        , beginAliases
        , middleCalls
        , middleAliases
        , endCalls
        , endAliases
        ]
    ]

beginCalls :: [(Expr.Symbol, Derive.Generator Derive.Pitch)]
beginCalls =
    [ ("set-pitch", cSetPitch)
    , ("flat-start", cFlatStart)
    , ("cur", cFrom PitchFromCurrent NoFade)
    , ("cur<", cFrom PitchFromCurrent Fade)
    , ("prev", cFrom PitchFromPrev NoFade)
    , ("prev<", cFrom PitchFromPrev Fade)
    , ("jaru", cJaru False)
    , ("jaru0", cJaru True)
    , ("fade-in", cFade True)
    ]

-- | I don't want to take up short names for the whole track scope, but within
-- a sequence call it seems reasonable.  In addition, I know if it's a begin or
-- end call, and use the same name for logically similar things.
beginAliases :: [(Expr.Symbol, Derive.Generator Derive.Pitch)]
beginAliases = map (second (Derive.setModule beginModule))
    [ ("-", cFlatStart)
    , ("c", cFrom PitchFromCurrent NoFade)
    , ("c<", cFrom PitchFromCurrent Fade)
    , ("p", cFrom PitchFromPrev NoFade)
    , ("p<", cFrom PitchFromPrev Fade)
    , ("J", cJaru False)
    , ("j", cJaru True)
    , (fadeInCall, cFade True)
    ]

middleCalls :: [(Expr.Symbol, Derive.Generator Derive.Pitch)]
middleCalls = ("flat", cFlat)
    : kampitaVariations "kam" (cKampita "" neighbor)
    ++ kampitaVariations "kam2" (cKampita "" Kampita2)
    ++ kampitaVariations "nkam" (cNkampita "" neighbor)
    ++ kampitaVariations "nkam2" (cNkampita "" Kampita2)
    where neighbor = Kampita1 0

kampitaVariations :: Text -> (Maybe Trill.Direction -> call)
    -> [(Expr.Symbol, call)]
kampitaVariations name call =
    [ (Expr.Symbol $ name <> Trill.directionAffix end, call end)
    | end <- dirs
    ]
    where dirs = [Nothing, Just Trill.Low, Just Trill.High]

middleAliases :: [(Expr.Symbol, Derive.Generator Derive.Pitch)]
middleAliases = map (second (Derive.setModule middleModule)) $ concat $
    [ ("-", cFlat)
    ] :
    [ hardcoded "o^" (Kampita0 1 0) (Just Trill.High)
    , hardcoded "o_" (Kampita0 (-1) 0) (Just Trill.Low)
    , hardcoded "o*" (Kampita0 (-1) 1) (Just Trill.Low)
    , aliasPrefix "k" "kam" middleCalls
    , aliasPrefix "nk" "nkam" middleCalls
    ]
    where
    hardcoded name arg dir =
        [ (name, cKampita doc arg dir)
        , ("n" <> name, cNkampita doc arg dir)
        ]
    doc = Doc.Doc $ Text.unlines
        [ "These are hardcoded `k` variants:"
        , "`o^` touches the swaram from above, like `k2^ 1 0`."
        , "`o_` touches the swaram from below, like `k2_ -1 0`."
        , "`o*` avoids the swaram, like `k2_ -1 1`."
        ]

aliasPrefix :: Text -> Text -> [(Expr.Symbol, call)] -> [(Expr.Symbol, call)]
aliasPrefix from to calls = do
    (Expr.Symbol name, call) <- calls
    Just rest <- [Text.stripPrefix to name]
    pure (Expr.Symbol (from <> rest), call)

endCalls :: [(Expr.Symbol, Derive.Generator Derive.Pitch)]
endCalls =
    [ ("flat-end", cFlatEnd)
    , ("to", cTo NoFade)
    , ("to>", cTo Fade)
    , ("fade-out", cFade False)
    ]

endAliases :: [(Expr.Symbol, Derive.Generator Derive.Pitch)]
endAliases = map (second (Derive.setModule endModule))
    [ ("-", cFlatEnd)
    , ("t", cTo NoFade)
    , ("t>", cTo Fade)
    , (fadeOutCall, cFade False)
    ]

-- | Special behaviour documented in 'sequence_doc'.
fadeOutCall :: Expr.Symbol
fadeOutCall = "->" -- The leading dash makes these parse as symbols.

-- | Unlike 'fade_out_call', this doesn't need special treatment.
fadeInCall :: Expr.Symbol
fadeInCall = "-<"

-- * sequence

cSequence :: Derive.Generator Derive.Note
cSequence = Derive.generator module_ "sequence" mempty sequenceDoc
    $ Sig.call (Sig.manyVals "arg" "Expressions separated by `;`.")
    $ \_ -> Sub.inverting $ \args -> withSequence args (Call.placedNote args)

cSequenceTransform :: Derive.Transformer Derive.Note
cSequenceTransform = Derive.transformer module_ "sequence" mempty
    sequenceDoc $
        Sig.callt (Sig.manyVals "arg" "Expressions separated by `;`.") $
        \_ -> withSequence

sequenceDoc :: Doc.Doc
sequenceDoc = "Sequence several pitch calls. Calls are divided into\
    \ `begin ; middle1 ; middle2; ... ; end` phases. Calls are pitch\
    \ generators, and are sequenced such that the middle calls stretch\
    \ based on the duration of the note. The " <> doc beginModule <> ", "
    <> doc middleModule <> ", and " <> doc endModule <> " modules are\
    \ in scope during the begin, middle, and end phases. All calls\
    \ below the " <> doc module_ <> " module are designed for\
    \ sequencing. This just means they emit samples at the beginning and end\
    \ of their range, so the sequence call knows their extent. Normal pitch\
    \ calls may not do that.\
    \\nThere's a special hack for the " <> ShowVal.doc fadeInCall
    <> " and " <> ShowVal.doc fadeOutCall <> " calls: they have 0\
    \ duration, but are overlaid with their neighbors. This is so you can fade\
    \ in or out without having to flatten the pitch."
    where doc = Doc.pretty

withSequence :: Derive.PassedArgs Score.Event -> Derive.Deriver a
    -> Derive.Deriver a
withSequence args deriver = do
    let (begin, middle, end) = parseSequence (Derive.passedVals args)
    (pitch, mods) <- sequenceCalls (pitchContext (Args.context args))
        (Args.range args) begin middle end
    endTime <- Derive.real $ Args.next args
    Derive.withPitch pitch $ Derive.withControlMods mods endTime deriver

pitchContext :: Derive.Context Score.Event -> Derive.Context Derive.Pitch
pitchContext ctx = ctx
    { Derive.ctxPrevVal = Score.eventPitch <$> Derive.ctxPrevVal ctx }

type Signals = (PSignal.PSignal, [Derive.ControlMod])

{- Awkward things:

    - I set call duration with the event (start, dur) instead of warp, so
    I have to modify the event rather than use Derive.place.

    - I deduce the duration of a call by seeing what signal it returns rather
    that having some special call mode, so these calls need to emit samples at
    their start and end.  Actually, just end for begin and middle calls, and
    start for end calls.

    - Prev val is manually passed from one call to the next, so if I don't have
    TrackEval.derive_track handling that for me, I have to do it myself, and be
    careful to not pass it for the speculative evaluation of the end call.
-}

{- | I assume that start and end calls have a fixed duration and don't stretch
    to fill the given space, though they will shrink if necessary.  So
    I evaluate start and end to get their durations, and fit the middle calls
    into the remaining space, evenly divided.

    Actually, there's a circular problem in that I don't know how long the
    middle section can be until I know how long the end call is, but the end
    call likely relies on 'Derive.ctx_prev_val', so it has to be evaluated
    after the middle.  So I evaluate the end twice, once before the middle to
    find out its length, and again after evaluating the middle to get the pitch
    right.

    I considered a specially calling mode where calls could return their
    desired duration rather than a signal, but it seems much simpler to just
    have them do that by returning the signal itself and measuring that.

    A possibly useful extension would be to allow middle calls to be shorter
    than their allotted time, for instance a trill might want to complete
    a cycle and allow a @flat@ call to take up remaining space.  I could put
    the requested time in @Derive.real 1@ and the hard limit in @Args.next@,
    but that wouldn't allow time before the call to stretch, only time after.
    But I could indicate stretchiness with a special tag on the call.
-}
sequenceCalls :: Derive.Context Derive.Pitch -> (ScoreTime, ScoreTime)
    -> Expr -> [Expr] -> Maybe Expr -> Derive.Deriver Signals
sequenceCalls ctx (start, end) begin middles maybeEnd =
    fmap fst $ flip Monad.State.runStateT ctx $ do
        (beginPitch, beginMods) <- eval beginModule start end begin
        middleStart <- lift $ signalEnd start beginPitch
        -- This is a test eval of 'end', just to see how long it is.  The
        -- middle isn't evaluated yet, so it doesn't have the right
        -- ctx_prev_val.
        (test_end_pitch, _) <- detached $
            maybeEval (eval endModule middleStart end) maybeEnd
        endStart <- lift $ signalStart end test_end_pitch
        (middlePitch, middleMods)
            <- sequenceMiddles middleStart endStart middles
        (endPitch, endMods) <- maybeEval
            (evalEnd start endStart end) maybeEnd
        pure (beginPitch <> middlePitch <> endPitch,
            beginMods <> middleMods <> endMods)
    where
    maybeEval = maybe (pure (mempty, mempty))

-- | Special behaviour for the @fade-out@ call, as documented in 'sequence_doc'.
evalEnd :: ScoreTime -> ScoreTime -> ScoreTime -> Expr -> SequenceM Signals
evalEnd sequenceStart start end expr = case expr of
    EvaluatedExpr call _ | call == fadeOutCall ->
        eval endModule sequenceStart end expr
    _ -> eval endModule start end expr

-- | I need to thread Derive.ctx_prev_val from each call in the sequence.
type SequenceM = Monad.State.StateT (Derive.Context Derive.Pitch) Derive.Deriver

detached :: SequenceM a -> SequenceM a
detached m = do
    st <- Monad.State.get
    val <- m
    Monad.State.put st
    pure val

-- | Give each middle call an even division of the time left.  The call can
-- use less time if it wishes, and the remaining time will be divided among
-- the remaining middle calls.  It shouldn't use more time, because then it
-- could overlap with the end call, or go past the end of the note.
sequenceMiddles :: ScoreTime -> ScoreTime -> [Expr] -> SequenceM Signals
sequenceMiddles _ _ [] = pure (mempty, mempty)
sequenceMiddles start end _ | start >= end = pure (mempty, mempty)
sequenceMiddles start end (expr:exprs) = do
    let dur = (end - start) / fromIntegral (length exprs + 1)
    (pitch, mods) <- eval middleModule start (start + dur) expr
    sigEnd <- lift $ signalEnd start pitch
    (pitchRest, modsRest) <- sequenceMiddles sigEnd end exprs
    pure (pitch <> pitchRest, mods <> modsRest)

signalStart :: ScoreTime -> PSignal.PSignal -> Derive.Deriver ScoreTime
signalStart deflt = maybe (pure deflt) (Derive.score . fst) .  PSignal.head

signalEnd :: ScoreTime -> PSignal.PSignal -> Derive.Deriver ScoreTime
signalEnd deflt = maybe (pure deflt) (Derive.score . fst) .  PSignal.last

eval :: Module.Module -> ScoreTime -> ScoreTime -> Expr -> SequenceM Signals
eval module_ start end expr = do
    ctx <- Monad.State.get
    (result, cmods) <- lift $ withEmptyCollect $
        Derive.withImported True module_ $
        evalExpr (placeEvent start (end - start) ctx) expr
    signal <- mconcat <$> Stream.writeLogs result
    unless (PSignal.null signal) $
        Monad.State.put $ ctx { Derive.ctxPrevVal = Just signal }
    pure (signal, cmods)

placeEvent :: ScoreTime -> ScoreTime -> Derive.Context d -> Derive.Context d
placeEvent start dur ctx = ctx
    { Derive.ctxEvent = Event.place start dur (Derive.ctxEvent ctx) }

evalExpr :: Derive.CallableExpr d => Derive.Context d -> Expr
    -> Derive.Deriver (Stream.Stream d)
evalExpr ctx (QuotedExpr expr) = Eval.evalToplevel ctx expr
evalExpr ctx (EvaluatedExpr sym args) = do
    call <- Eval.getGenerator sym
    Eval.applyGenerator ctx call args

withEmptyCollect :: Derive.Deriver a
    -> Derive.Deriver (a, [Derive.ControlMod])
withEmptyCollect = fmap (second Derive.collectControlMods)
    . Internal.localCollect

-- ** parse

data Expr
    -- | This is a call which was embedded in the argument list of the sequence
    -- call, so its arguments have already been evaluated.
    = EvaluatedExpr Expr.Symbol [DeriveT.Val]
    -- | A call and its arguments can be protected from evaluation by quoting
    -- it.  This is also necessary to use a transformer, since @;@ has higher
    -- precedence than @|@ (actually it's just a value, not an operator).
    | QuotedExpr !DeriveT.Expr
    deriving Show

instance Pretty Expr where
    format (EvaluatedExpr sym vals) =
        ("(" <> Pretty.format sym) Pretty.<+> (Pretty.format vals <> ")")
    format (QuotedExpr quoted) = Pretty.text $ ShowVal.showVal quoted

-- | Parse the sequence call's arguments and substitute aliases.  If there is
-- no begin, @set-pitch@ will be added, and if there is no middle, @-@ will be
-- added.  This is so that if there is just an end call, there will still be
-- a signal from the beginning of the note.
--
-- The positions are inferred according to the number of sections:
--
-- > ; middle1;
-- > begin1; middle2
-- > begin1; middle2; middle3; ...; end_n
parseSequence :: [DeriveT.Val] -> (Expr, [Expr], Maybe Expr)
parseSequence exprs = postproc $
    case Lists.mapTail (drop 1) $ Lists.splitBefore isSeparator exprs of
        [] -> (Nothing, [], Nothing)
        begin : rest -> case reverse rest of
            [middle] -> (Just begin, [middle], Nothing)
            end : middle -> (Just begin, reverse middle, Just end)
            [] -> (Just begin, [], Nothing)
    where
    postproc (begin, middles, end) =
        ( fromMaybe (EvaluatedExpr "set-pitch" []) $ toExpr =<< begin
        , addHold $ mapMaybe toExpr middles
        , toExpr =<< end
        )
    addHold [] = [EvaluatedExpr "-" []]
    addHold xs = xs
    toExpr [] = Nothing
    toExpr (call : args) = Just $ case call of
        DeriveT.VQuoted (DeriveT.Quoted expr) -> QuotedExpr expr
        DeriveT.VStr (Expr.Str sym) -> EvaluatedExpr (Expr.Symbol sym) args
        _ -> EvaluatedExpr (Expr.Symbol (ShowVal.showVal call)) args
    isSeparator DeriveT.VSeparator = True
    isSeparator _ = False

-- * start

cFlatStart :: Derive.Generator Derive.Pitch
cFlatStart = generator1 "flat-start" mempty
    "Emit a flat pitch for the given duration."
    $ Sig.call ((,)
    <$> Sig.defaulted "pitch" (Nothing :: Maybe Sig.Dummy)
        "Emit this pitch, or continue the previous pitch if not given."
    <*> Sig.defaulted "time" (Typecheck.real 0.15)
        "Pitch lasts for this duration."
    ) $ \(maybePitch, Typecheck.DefaultReal time) args -> do
        start <- Args.realStart args
        end <- getEnd start time args
        pitch <- optionalPitch maybePitch <$> Call.getPitch start
        pure $ PSignal.fromPairs [(start, pitch), (end, pitch)]

cSetPitch :: Derive.Generator Derive.Pitch
cSetPitch = generator1 "set-pitch" mempty "Emit the current pitch.\
    \ Sequence pitch calls normally use the previous pitch, and this is an\
    \ implicit begin call so a sequence missing a begin doesn't inherit the\
    \ previous pitch."
    $ Sig.call0 $ \args -> do
        start <- Args.realStart args
        pitch <- Call.getPitch start
        pure $ PSignal.fromSample start pitch

data PitchFrom = PitchFromPrev | PitchFromCurrent deriving (Eq, Show)
data Fade = Fade | NoFade deriving (Eq, Show)

cFrom :: PitchFrom -> Fade -> Derive.Generator Derive.Pitch
cFrom pitchFrom fade = generator1 "from" mempty
    (case pitchFrom of
        PitchFromPrev -> "Come for the previous pitch, and possibly fade in."
        PitchFromCurrent -> "Come from a pitch, and possibly fade in.")
    $ Sig.call ((,,,)
    <$> case pitchFrom of
        PitchFromPrev -> pure Nothing
        PitchFromCurrent -> Sig.defaulted "from" (Nothing :: Maybe Sig.Dummy)
            "Come from this pitch, or the previous one."
    <*> Sig.defaulted "transition" defaultTransition "Time to destination."
    <*> Sig.defaulted "to" (Nothing :: Maybe Sig.Dummy)
        "Go to this pitch, or the current one."
    <*> ControlUtil.curveEnv
    ) $ \(fromPitch, Typecheck.DefaultReal time, maybeToPitch, curve)
            args -> do
        start <- Args.realStart args
        end <- getEnd start time args
        toPitch <- optionalPitch maybeToPitch <$> Call.getPitch start
        let from = resolvePitch args toPitch fromPitch
        case fade of
            Fade -> ControlUtil.multiplyDyn end
                =<< ControlUtil.makeSegment ControlUtil.Linear start 0 end 1
            NoFade -> pure ()
        PitchUtil.makeSegment curve start from end toPitch

-- | Get the end time, given a start and a duration.  Don't go beyond the
-- maximum, which is the event's duration, if given explicitly, or the next
-- event if it's 0.
getEnd :: RealTime -> DeriveT.Duration -> Derive.PassedArgs a
    -> Derive.Deriver RealTime
getEnd start dur args = do
    timeEnd <- (start +) <$> Call.realDuration start dur
    maxEnd <- if Args.duration args == 0
        then Derive.real $ Args.next args
        else Args.realEnd args
    pure $ min timeEnd maxEnd

{-
    Reliance on the underlying pitch is awkward.  E.g. jaru and p go to 0, but
    sometimes the hold is at -1 or something.  This also leads to kam not being
    able to assume the base.  Maybe there should be a generic way to transpose
    the base pitch.  Or set the middle:

    > j 1 2; -1; p> -- Jaru 1 2 to -1, hold -1, then to 0
    > j 1 2; k^ -1 1; p> -- Jaru 1 2, kam -1 1, to 0

    So maybe the 'from' calls need to know what the first middle pitch is, so
    they can go to it.

    Or kam can start with the previous pitch, just like hold does:

    > j 1 2 -1; - ; p> -- Jaru 1 2 to -1, hold -1, then to 0
    > j 1 2 -1; k^ 0 2; p> -- Jaru 1 2, kam -1 1, to 0

    Since it starts with 0 I can omit:

    > j 1 2 -1; k^ 2; p> -- Jaru 1 2, kam -1 1, to 0

    I think it doesn't read as nicely because each call depends on the previous
    one, and 'j' needs an extra arg.  On the other hand, how is 'start'
    supposed to know the starting pitch of 'middle', especially when 'middle'
    may want to rely on the previous pitch?  I guess the two approaches are
    incompatible.  So to do it that way, I'd need to make middle calls not
    rely on prev_pitch, do a speculative middle eval, and communicate next
    pitch to the 'begin' call via an env var or something.  Seems too
    complicated.
-}

cJaru :: Bool -> Derive.Generator Derive.Pitch
cJaru appendZero = generator1 "jaru" mempty
    "This is a series of grace notes whose pitches are relative to the\
    \ base pitch. The 0 variant appends a 0 on the end."
    $ Sig.call ((,,,)
    <$> Sig.many1 "interval" "Intervals from base pitch."
    <*> Sig.environ "time" Sig.Both defaultTransition "Time for each note."
    -- TODO This should also be a Duration
    <*> Sig.environ "transition" Sig.Both (Nothing :: Maybe Sig.Dummy)
        "Time for each slide, defaults to `time`."
    <*> ControlUtil.curveEnv
    ) $ \(intervals, Typecheck.DefaultReal time_, maybeTransition, curve)
            args -> do
        start <- Args.realStart args
        -- Adjust time per note based on the available duration.
        -- Since transitions can start at 0 and end at the end, I'm dividing
        -- the duration into intervals-1 parts.
        let len = NonEmpty.length intervals - 1
        end <- getEnd start
            (DeriveT.multiplyDuration time_ (fromIntegral len)) args
        let time = (end - start) / fromIntegral len
        pitch <- Call.getPitch start
        srate <- Call.getSrate
        (intervals, control) <- parse intervals
        let transition = fromMaybe time maybeTransition
        let sig = jaru curve srate start time transition $
                NonEmpty.toList intervals ++ if appendZero then [0] else []
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

jaru :: ControlUtil.Curve -> RealTime -> RealTime -> RealTime -> RealTime
    -> [Signal.Y] -> Signal.Control
jaru curve srate start time transition intervals =
    ControlUtil.smoothAbsolute curve srate (-transition) $
        zip (Lists.range_ start time) intervals

-- * middle

cFlat :: Derive.Generator Derive.Pitch
cFlat = generator1 "flat" mempty "Emit a flat pitch."
    $ Sig.call (Sig.defaulted "pitch" (Nothing :: Maybe Sig.Dummy)
        "Emit this pitch, or continue the previous pitch if not given.")
    $ \maybePitch args -> do
        start <- Args.realStart args
        end <- Args.realEnd args
        pitch <- case maybePitch of
            Nothing -> prevPitch start args
            Just transpose -> do
                pitch <- Call.getPitch start
                pure $ PitchUtil.resolvePitchTranspose pitch transpose
        pure $ PSignal.fromPairs [(start, pitch), (end, pitch)]

-- ** kampita

data KampitaArgs
    -- | Both interval arguments are hardcoded.
    = Kampita0 !Signal.Y !Signal.Y
    -- | The starting pitch is hardcoded.
    | Kampita1 !Signal.Y
    -- | Both arguments must be provided.
    | Kampita2
    deriving (Show)

cKampita:: Doc.Doc -> KampitaArgs -> Maybe Trill.Direction
    -> Derive.Generator Derive.Pitch
cKampita doc kamArgs endDir = generator1 "kam" mempty
    ("This is a kind of trill, but its interval defaults to NNs,\
    \ and transitions between the notes are smooth.  It's intended for\
    \ the vocal microtonal trills common in Carnatic music."
    <> if doc == "" then "" else "\n" <> doc)
    $ Sig.call ((,,,)
    <$> kampitaPitchArgs kamArgs
    <*> Sig.defaulted "speed" (6 :: Double) "Alternate pitches at this speed."
    <*> kampitaEnv <*> ControlUtil.curveEnv
    ) $ \(pitches, speed, (transition, hold, lilt, adjust), curve) args -> do
        (pitches, control) <- resolvePitches kamArgs pitches
        start <- Args.realStart args
        let even = endWantsEvenTransitions start pitches endDir
        transpose <- kampitaTranspose curve even adjust pitches speed
            transition hold lilt (Args.range args)
        kampita start args control transpose

cNkampita :: Doc.Doc -> KampitaArgs -> Maybe Trill.Direction
    -> Derive.Generator Derive.Pitch
cNkampita doc kamArgs endDir = generator1 "nkam" mempty
    ("`kam` with a set number of cycles. The speed adjusts to fit the cycles in\
    \ before the next event."
    <> if doc == "" then "" else "\n" <> doc)
    $ Sig.call ((,,,)
    <$> (Typecheck.positive <$> Sig.defaulted "cycles" (1 :: Double)
        "Number of cycles.")
    <*> kampitaPitchArgs kamArgs
    <*> kampitaEnv <*> ControlUtil.curveEnv
    ) $ \(cycles, pitches, (transition, hold, lilt, adjust), curve) args -> do
        (pitches, control) <- resolvePitches kamArgs pitches
        (start, end) <- Args.realRangeOrNext args
        let even = endWantsEvenTransitions start pitches endDir
        -- 1 cycle means a complete cycle, which is 3 transitions, but
        -- 'end_dir' may reduce the number of transitions, to a minimum of 2,
        -- which winds up sounding like a single transition: [0, 1].
        let numTransitions = cycles * 2 + if even == Just True then 0 else 1
        let speed = Typecheck.RealTimeFunctionT ScoreT.TReal
                (const $ (numTransitions - 1)
                    / RealTime.toSeconds (end - start))
        transpose <- kampitaTranspose curve even adjust pitches speed
            transition hold lilt (Args.range args)
        kampita start args control transpose

-- ** implementation

resolvePitches :: KampitaArgs
    -> (Typecheck.NnTransposeFunctionT, Typecheck.NnTransposeFunctionT)
    -> Derive.Deriver ((ScoreT.Function, ScoreT.Function), ScoreT.Control)
resolvePitches kamArgs
        ( Typecheck.NnTransposeFunctionT ttype1 pitch1
        , Typecheck.NnTransposeFunctionT ttype2 pitch2) = do
    let control1 = Typecheck.transposeControl ttype1
    let control2 = Typecheck.transposeControl ttype2
    let twoPitches = case kamArgs of
            Kampita2 -> False
            _ -> True
    when (twoPitches && control1 /= control2) $ Derive.throw $
        "pitch1 and pitch2 signals should have the same type: "
        <> pretty control1 <> " /= " <> pretty control2
    pure ((pitch1, pitch2), control1)

kampitaPitchArgs :: KampitaArgs
    -> Sig.Parser
        (Typecheck.NnTransposeFunctionT, Typecheck.NnTransposeFunctionT)
kampitaPitchArgs kamArgs = case kamArgs of
    Kampita0 p1 p2 -> (,) <$> pure (control p1) <*> pure (control p2)
    Kampita1 p1 -> (,) <$> pure (control p1)
        <*> Sig.defaulted "neighbor" (1 :: Double)
            "Alternate with a pitch at this interval."
    Kampita2 -> (,)
        <$> Sig.defaulted "pitch1" (0 :: Double) "First interval."
        <*> Sig.defaulted "pitch2" (1 :: Double) "Second interval."
    where
    control val = Typecheck.NnTransposeFunctionT ScoreT.TNn (const val)

kampitaEnv :: Sig.Parser (RealTime, DeriveT.Duration, Double, Trill.Adjust)
kampitaEnv = (,,,)
    <$> Sig.defaultedEnv "transition" Sig.Both defaultTransition_
        "Time for each slide."
    <*> Trill.holdEnv <*> liltEnv <*> Trill.adjustEnv
    where
    liltEnv :: Sig.Parser Double
    liltEnv = Sig.environ "lilt" Sig.Prefixed (0 :: Double)
        "Lilt is a horizontal bias to the vibrato. A lilt of 1 would place\
        \ each neighbor on top of the following unison, while -1 would place\
        \ it on the previous one. So it should range from -1 < lilt < 1."

defaultTransition :: Typecheck.DefaultReal
defaultTransition = Typecheck.real defaultTransition_

defaultTransition_ :: RealTime
defaultTransition_ = 0.12

kampita :: RealTime -> Derive.PitchArgs -> ScoreT.Control -> Signal.Control
    -> Derive.Deriver PSignal.PSignal
kampita start args control transpose = do
    pitch <- prevPitch start args
    pure $ PSignal.applyControl control (ScoreT.untyped transpose) $
        PSignal.fromSample start pitch

-- | You don't think there are too many arguments, do you?
kampitaTranspose :: ControlUtil.Curve -> Maybe Bool -> Trill.Adjust
    -> (ScoreT.Function, ScoreT.Function) -> Typecheck.RealTimeFunctionT
    -> RealTime -> DeriveT.Duration -> Double -> (ScoreTime, ScoreTime)
    -> Derive.Deriver Signal.Control
kampitaTranspose curve even adjust (pitch1, pitch2) speed transition hold lilt
        (start, end) = do
    hold <- Call.scoreDuration start hold
    smoothTrill curve (-transition) pitch1 pitch2
        =<< trillTransitions even adjust lilt hold speed (start, end)

smoothTrill :: ControlUtil.Curve -> RealTime -> ScoreT.Function
    -> ScoreT.Function -> [RealTime] -> Derive.Deriver Signal.Control
smoothTrill curve time val1 val2 transitions = do
    srate <- Call.getSrate
    pure $ ControlUtil.smoothAbsolute curve srate time $
        trillFromTransitions val1 val2 transitions

-- | Make a trill signal from a list of transition times.
trillFromTransitions :: ScoreT.Function -> ScoreT.Function
    -> [RealTime] -> [(RealTime, Signal.Y)]
trillFromTransitions val1 val2 transitions =
    [(x, sig x) | (x, sig) <- zip transitions (cycle [val1, val2])]

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

endWantsEvenTransitions :: RealTime -> (ScoreT.Function, ScoreT.Function)
    -> Maybe Trill.Direction -> Maybe Bool
endWantsEvenTransitions start (pitch1, pitch2) dir = case dir of
    Nothing -> Nothing
    Just Trill.Low -> Just (not pitch1Low)
    Just Trill.High -> Just pitch1Low
    where pitch1Low = pitch1 start <= pitch2 start

-- * end

cFlatEnd :: Derive.Generator Derive.Pitch
cFlatEnd = generator1 "flat-end" mempty
    "Emit a flat pitch for the given duration."
    $ Sig.call ((,)
    <$> Sig.defaulted "pitch" (Nothing :: Maybe Sig.Dummy)
        "Emit this pitch, or continue the previous pitch if not given."
    <*> Sig.defaulted "time" (Typecheck.real 0.15)
        "Pitch lasts for this duration."
    ) $ \(maybePitch, Typecheck.DefaultReal time) args -> do
        (start, end) <- Args.realRange args
        start <- alignToEnd start end time
        pitch <- optionalPitch maybePitch <$> prevPitch start args
        pure $ PSignal.fromPairs [(start, pitch), (end, pitch)]

cTo :: Fade -> Derive.Generator Derive.Pitch
cTo fade = generator1 "to" mempty "Go to a pitch, and possibly fade out."
    $ Sig.call ((,)
    <$> Sig.required "pitch" "Go to this pitch or interval."
    <*> Sig.defaulted "transition" defaultTransition
        "Time to destination pitch."
    ) $ \(toPitch, Typecheck.DefaultReal time) args -> do
        (start, end) <- Args.realRange args
        start <- alignToEnd start end time
        pitch <- prevPitch start args
        case fade of
            Fade -> ControlUtil.multiplyDyn end
                =<< ControlUtil.makeSegment ControlUtil.Linear start 1 end 0
            NoFade -> pure ()
        PitchUtil.makeSegment ControlUtil.Linear start pitch end
            (PitchUtil.resolvePitchTranspose pitch toPitch)

cFade :: Bool -> Derive.Generator Derive.Pitch
cFade fadeIn = generator1 "fade" mempty
    ((if fadeIn then "Fade in." else "Fade out.")
    <> " This will overlap with the pitch part of the "
    <> (if fadeIn then "next" else "previous") <> " call."
    ) $ Sig.call (Sig.defaulted "time" (Typecheck.real 0.15) "Time to fade.")
    $ \(Typecheck.DefaultReal time) args -> do
        (start, end) <- Args.realRange args
        (start, end) <- if fadeIn
            then (,) <$> pure start <*> getEnd start time args
            else (,) <$> alignToEnd start end time <*> pure end
        ControlUtil.multiplyDyn end
            =<< ControlUtil.makeSegment ControlUtil.Linear start 1 end 0
        pure mempty

-- | Subtract the duration from the given end time, but don't go past the
-- start.
alignToEnd :: RealTime -> RealTime -> DeriveT.Duration
    -> Derive.Deriver RealTime
alignToEnd start end dur = do
    dur <- min (end - start) <$> Call.realDuration start dur
    pure $ end - dur

-- * misc

cSahitya :: Derive.Taggable a => Derive.Transformer a
cSahitya = Derive.transformer module_ "sahitya" mempty
    "Ignore the transformed deriver. Put this on a track to ignore its\
    \ contents, and put in sahitya."
    $ Sig.call0t $ \_args _deriver -> pure Stream.empty

-- * util

-- | This defaults to the note's base pitch, in case this call is the first
-- one.  Also, the end call is called before the middle calls to find out how
-- long it is.
prevPitch :: RealTime -> Derive.PitchArgs -> Derive.Deriver PSignal.Pitch
prevPitch start args = case Args.prevPitch args of
    Nothing -> Call.getPitch start
    Just (_, pitch) -> pure pitch

resolvePitch :: Derive.PitchArgs -> PSignal.Pitch
    -> Maybe PitchUtil.PitchOrTranspose -> PSignal.Pitch
resolvePitch args thisPitch maybePitch = case maybePitch of
    Nothing -> case Args.prevPitch args of
        Nothing -> thisPitch
        Just (_, prev) -> prev
    Just (Left pitch) -> pitch
    Just (Right transpose) -> Pitches.transpose transpose thisPitch

-- | A number of calls take an optional pitch, and default to either
-- the current or previous pitch.
optionalPitch :: Maybe PitchUtil.PitchOrTranspose -> PSignal.Pitch
    -> PSignal.Pitch
optionalPitch maybePitch currentPitch =
    maybe currentPitch (PitchUtil.resolvePitchTranspose currentPitch)
        maybePitch

generator1 :: Derive.CallName -> Tags.Tags -> Doc.Doc
    -> Derive.WithArgDoc (Derive.PassedArgs d -> Derive.Deriver d)
    -> Derive.Call (Derive.GeneratorFunc d)
generator1 = Derive.generator1 module_
