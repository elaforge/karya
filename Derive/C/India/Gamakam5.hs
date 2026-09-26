-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-- | Calls for Carnatic gamakam.
module Derive.C.India.Gamakam5 (
    library
#ifdef TESTING
    , module Derive.C.India.Gamakam5
#endif
) where
import qualified Control.Monad.State as State
import qualified Data.Attoparsec.Text as A
import qualified Data.Char as Char
import qualified Data.DList as DList
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text

import qualified Util.Doc as Doc
import qualified Util.Lists as Lists
import qualified Util.Maps as Maps
import qualified Util.Num as Num
import qualified Util.ParseText as ParseText
import qualified Util.Pretty as Pretty
import qualified Util.Segment as Segment

import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Call.ControlUtil as ControlUtil
import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Expr as Expr
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.Parse as Parse
import qualified Derive.Pitches as Pitches
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import qualified Ui.Event as Event
import qualified Ui.ScoreTime as ScoreTime

import           Global
import           Types


module_ :: Module.Module
module_ = "india" <> "gamakam5"

library :: Library.Library
library = mconcat
    [ Library.generators
        [ (Parse.unparsedCall, cPitchSequence)
        , (Parse.unparsedCall, cDynSequence)
        ]
    , Library.transformers
        [ ("gamak", cImportPitch)
        , ("dyn", cImportDyn)
        ]
    , Library.transformers
        [ ("sahitya", cSahitya :: Derive.Transformer Derive.Note)
        ]
    ]

-- * sequence

cImportPitch :: Derive.Transformer Derive.Control
cImportPitch = Derive.transformer module_ "gamak" mempty
    "Import calls for a gamakam track."
    $ Sig.call0t $ \_args -> Derive.withImported False (module_ <> "pitch")

cImportDyn :: Derive.Transformer Derive.Control
cImportDyn = Derive.transformer module_ "dyn" mempty
    "Import calls for a dyn track."
    $ Sig.call0t $ \_args -> Derive.withImported False (module_ <> "dyn")

cPitchSequence :: Derive.Generator Derive.Control
cPitchSequence = Derive.generator1 (module_ <> "pitch")
    "sequence" mempty pitchSequenceDoc
    $ Sig.call ((,,)
        <$> Sig.required "sequence" "Pitch calls."
        <*> transitionEnv
        <*> Sig.environ "gamakam-above" Sig.Unprefixed False
            "Expect pitch and gamakam tracks above the note track."
    ) $ \(text, transition, pitchAbove) args -> do
        (start, end) <- if pitchAbove
            then (Args.start args,) <$> inferEnd args
            else Args.rangeOrNoteEnd args
        maybeState <- initialPitchState pitchAbove transition args
        case maybeState of
            Nothing -> pure mempty
            Just state -> do
                Result signals <- Derive.at start $
                    pitchSequence (end - start) state text
                realEnd <- Derive.real end
                -- End is the next pitch sample.  So if the next event
                -- coincides with or precedes it, it will want to come from
                -- this pitch, so don't append a 0.  Otherwise, the pitch is
                -- not "attached" to a gamakam, so I add a 0, otherwise it's
                -- out of tune.
                --
                -- I have to subtract ScoreTime.eta because the pitch sample
                -- has been warped back from RealTime, so it will lose some
                -- precision.  TODO ugh.
                let nextGamakam = maybe False
                        ((<=end) . subtract ScoreTime.eta) (nextEvent args)
                pure $ mconcat (DList.toList signals)
                    <> if nextGamakam then mempty
                        else Signal.fromSample realEnd 0
    where
    transitionEnv :: Sig.Parser Typecheck.Normalized
    transitionEnv =
        Sig.environ "transition" Sig.Both (Typecheck.Normalized 0.5) $
            "Time for each pitch movement, in proportion of the total time"
            <> " available."

-- | Start of the next event.  'Args.next' gets the end of the block if there
-- is no next event, but I don't want that.
nextEvent :: Derive.PassedArgs a -> Maybe TrackTime
nextEvent = fmap Event.start . Lists.head . Args.nextEvents

-- | Infer the end time for the gamakam as the next pitch in the pitch signal,
-- which should correspond to the next explicit swaram.
inferEnd :: Derive.PassedArgs a -> Derive.Deriver TrackTime
inferEnd args
    | Args.end args /= Args.start args = pure $ Args.end args
    | otherwise = do
        pitch <- Derive.getPitch
        start <- Args.realStart args
        case nextSample start pitch of
            Nothing -> pure $ Args.start args
            Just (x, _) -> do
                -- x could be the final sample at RealTime.large, so I need to
                -- limit it.
                end <- Derive.getVal EnvKey.blockEnd
                min end <$> Derive.score x

initialPitchState :: Bool -> Typecheck.Normalized
    -> Derive.PassedArgs Derive.Control
    -> Derive.Deriver (Maybe PitchState)
initialPitchState pitchAbove transition args = do
    start <- Args.realStart args
    -- If there's no pitch then this is likely at the edge of a slice, and can
    -- be ignored.  TODO I think?
    justm (lookupPitch start) $ \current -> do
        (prev, next, from) <- if pitchAbove
            then getNeighborPitchesAbove start
            else getNeighborPitches (Args.start args)

        let prevStep = maybe 0 snd $ Args.prevControl args
        let stepsFromCurrent = fmap (fromMaybe 0) . traverse
                ((`stepDifference` current) <=< Derive.resolvePitch start)
        prev <- stepsFromCurrent prev
        from <- stepsFromCurrent $
            Pitches.transposeNn (Pitch.nn prevStep) <$> from
        next <- stepsFromCurrent next

        pure $ Just $ PitchState
            { _swaram = current
            , _from = from
            , _prev = prev
            , _next = next
            , _transition = transition
            }
    where
    lookupPitch = Call.transposed

getNeighborPitches :: ScoreTime -> Derive.Deriver
    (Maybe PSignal.Pitch, Maybe PSignal.Pitch, Maybe PSignal.Pitch)
getNeighborPitches start = (,,)
    <$> Args.lookupPrevNotePitch start
    <*> Args.lookupNextNotePitch start
    <*> (getPrevPitch =<< Derive.real start)

getNeighborPitchesAbove :: RealTime -> Derive.Deriver
    (Maybe PSignal.Pitch, Maybe PSignal.Pitch, Maybe PSignal.Pitch)
getNeighborPitchesAbove start = do
    pitch <- Derive.getPitch
    let prev = PSignal.atNegative pitch start
    let next = snd <$> nextSample start pitch
    pure (prev, next, prev)

nextSample :: RealTime -> PSignal.PSignal -> Maybe (RealTime, PSignal.Pitch)
nextSample x pitch = do
    Segment.Segment _ _ x2 _ <- PSignal.segmentAt pitch x
    (x2,) <$> PSignal.at pitch x2

getPrevPitch :: RealTime -> Derive.Deriver (Maybe PSignal.Pitch)
getPrevPitch = Args.prevNotePitch

-- TODO this is how it used to work, which is complicated, and only mostly
-- worked.  If I move to *_above, then maybe I can get rid of this entirely.
-- get_prev_pitch_old :: RealTime -> Derive.Deriver (Maybe PSignal.Pitch)
-- get_prev_pitch_old start = do
--     -- If this is the first call in this note, then I have to look in the
--     -- previous slice.
--     prev_event_pitch <- Args.prev_note_pitch start
--     -- TODO gives different results for test_prev_pitch, but I don't understand
--     -- which is right.
--     -- pitch_signal <- PSignal.last . PSignal.drop_after start <$> Derive.get_pitch
--     pitch_signal <- before start <$> Derive.get_pitch
--     return $ case (prev_event_pitch, pitch_signal) of
--         (Just a, Just b) -> Just $ snd $ Lists.maxOn fst a b
--         (a, b) -> snd <$> (a <|> b)
--     -- Getting the previous pitch is kind of ridiculously complicated.
--     -- First, Args.prev_pitch means there was a preceding pitch call, so
--     -- I take that one if present.  Otherwise, the pitch can come from
--     -- either the previous event, or the previous pitch on the current
--     -- pitch signal, whichever is newer.  The current pitch signal will
--     -- be newer if there is a pitch on the parent pitch track with no
--     -- corresponding gamakam on this one.

before :: RealTime -> PSignal.PSignal -> Maybe (RealTime, PSignal.Pitch)
before x sig = case PSignal.segmentAt sig x of
    Just (Segment.Segment x y _ _) -> Just (x, y)
    Nothing -> Nothing

stepDifference :: PSignal.Transposed -> PSignal.Transposed
    -> Derive.Deriver Step
stepDifference p1 p2 = fmap realToFrac $
    (-) <$> Pitches.pitchNn p1 <*> Pitches.pitchNn p2

pitchSequenceDoc :: Doc.Doc
pitchSequenceDoc = Doc.Doc $
    "This is a mini-language, where each one or two characters is a call.\
    \ An upper-case call will take a single character argument. A special\
    \ parsing rule means that `-` and its following character is considered\
    \ a single character, so `-1` is a valid call or argument.\
    \ Most of these calls represent a pitch movement:"
    <> Text.unlines (map pitchCallDoc (Map.toAscList pitchCallMap))
    <> "Currently the transition curve is hardcoded to a sigmoid curve, but\
    \ I could add a curve env var if necessary."

pitchCallDoc :: (Char, [PitchCall]) -> Text
pitchCallDoc (name, pcalls) =
    "`" <> Text.singleton name <> "` - "
        <> Text.intercalate "; " (map docOf pcalls)
    where
    docOf pcall = pcallDoc pcall
        <> if pcallDuration pcall /= 1
            then " (dur " <> pretty (pcallDuration pcall) <> ")" else ""

pitchCallMap :: Map Char [PitchCall]
pitchCallMap = resolve $ Maps.unique $ concat
    [ [pcall '=' "Hold flat pitch." pcFlat]
    -- relative motion
    , [parseName $ pcall c "Relative motion." pcRelative | c <- "0123456789"]
    , [parseName $ pcall '-' "Negative relative motion." pcRelative]
    , [alias c 1 [showt n] | (c, n) <- zip "abc" [-1, -2 ..]]

    , [pcall 'e' "Pitch up by 1nn." (pcRelativeMove (Pitch.Nn 1))]
    , [pcall 'f' "Pitch down by 1nn." (pcRelativeMove (Pitch.Nn (-1)))]
    , [pcall 'g' "Pitch up by .5nn." (pcRelativeMove (Pitch.Nn 0.5))]
    , [pcall 'h' "Pitch down by .5nn." (pcRelativeMove (Pitch.Nn (-0.5)))]
    , [alias 'n' 0.5 ["e", "f"]]
    , [alias 'u' 0.5 ["f", "e"]]

    , [ pcall 'v' "Absolute motion to next pitch." (pcMoveDirection Next)
      -- set config
      , config '<' "Set from pitch to previous." (pcSetPitchFrom Previous)
      , config '^' "Set from pitch to current." (pcSetPitchFrom Current)
      , config '&' "Set from pitch to next." (pcSetPitchFrom Next)
      , config 'T' "Set from pitch relative to swaram." pcSetPitch
      , config 'F' "Fast transition time." (pcSetTransitionTime Fast)
      , config 'M' "Medium transition time." (pcSetTransitionTime Medium)
      , config 'S' "Slow transition time." (pcSetTransitionTime Slow)
      ]
    -- Just a placeholder, effects are actually applied by 'resolve_postfix'.
    , [ config c postfixDoc (PCall Sig.noArgs $ \() _ctx -> pure mempty)
      | c <- Map.keys postfixCalls
      ]
    ]
    where
    resolve (calls, duplicates)
        | null duplicates = either errorStack id (resolveAliases calls)
        | otherwise =
            errorStack $ "duplicate calls: " <> showt (map fst duplicates)
    parseName = second $ second $ \g -> g { pcallParseCallName = True }
    alias name duration to = (name, Left (duration, to))
    pcall name doc c = (name, Right $ PitchCall doc 1 False c)
    config name doc c = (name, Right $ PitchCall doc 0 False c)

-- * dyn-sequence

cDynSequence :: Derive.Generator Derive.Control
cDynSequence = Derive.generator1 (module_ <> "dyn") "dyn-sequence" mempty doc
    $ Sig.call (Sig.required "sequence" "Dyn calls.")
    $ \text args -> do
        (start, end) <- Args.rangeOrNoteEnd args
        let state = DynState
                { stateFromDyn = maybe 0 snd (Args.prevControl args) }
        Derive.at start $ dynSequence (end - start) state text
    where
    doc = Doc.Doc $
        "This is a mini-language, where each one or two characters is a call.\
        \ Each character can take an argument, which can only be a single\
        \ digit. Typically this represents a dyn level / 9, so 0 is 0 and\
        \ 9 is 1.  Calls:"
        <> Text.unlines (map dynCallDoc (Map.toAscList dynCallMap))

dynCallDoc :: (Char, DynCall) -> Text
dynCallDoc (name, dcall) = "`" <> Text.singleton name <> "` - "
    <> dcallDoc dcall

dynCallMap :: Map Char DynCall
dynCallMap = Map.fromList $
    [ ('=', dcFlat)
    , ('<', dcAttack)
    , ('a', dcAttackFrom)
    , ('>', dcDecay 0.8 0)
    , ('d', dcDecay 0 0.8)
    , ('T', dcSetDyn)
    , ('.', dcMoveTo)
    ]

newtype DynState = DynState { stateFromDyn :: Signal.Y }
    deriving (Show)

instance Pretty DynState where
    format (DynState fromDyn) = Pretty.recordTitle "DynState"
        [ ("from_dyn", Pretty.format fromDyn)
        ]

dynSequence :: ScoreTime -> DynState -> Text -> Derive.Deriver Signal.Control
dynSequence dur state arg = do
    exprs <- Derive.requireRight (("parsing " <> showt arg <> ": ")<>) $
        resolveDynCalls =<< parseDynSequence arg
    let starts = sliceTime dur (replicate (length exprs) 1)
        ranges = zip starts (drop 1 starts)
    (results, _) <- State.runStateT (mapM evalDyn (zip ranges exprs)) state
    pure $ mconcat results

evalDyn :: ((ScoreTime, ScoreTime), Call (DynCall, Char))
    -> M DynState Signal.Control
evalDyn ((start, end), (Call (DynCall _ sig func, name) arg)) = do
    let ctx = Context
            { ctxStart = start
            , ctxEnd = end
            , ctxCallName = Derive.CallName $ Text.singleton name
            }
    parsedArg <- parseArgs (Derive.CallName (Text.singleton name)) arg sig
    func parsedArg ctx

-- ** parse

parseDynSequence :: Text -> Either Text [Call Char]
parseDynSequence = ParseText.parse1 pDynCalls

pDynCalls :: Parser [Call Char]
pDynCalls = A.skipSpace *> A.many1 (pDynCall <* A.skipSpace)

pDynCall :: Parser (Call Char)
pDynCall = do
    c <- A.anyChar
    arg <- A.option "" (Text.singleton <$> A.digit)
    pure $ Call c arg

resolveDynCalls :: [Call Char] -> Either Text [Call (DynCall, Char)]
resolveDynCalls = mapM $ \(Call name arg) ->
    case Map.lookup name dynCallMap of
        Nothing -> Left $ "dyn call not found: " <> showt name
        Just call -> pure $ Call (call, name) arg

-- * State

type M s a = State.StateT s Derive.Deriver a

data PitchState = PitchState
    -- # maintained automatically
    -- | Absolute pitch of the underlying note.  The Steps are relative to
    -- this.
    { _swaram :: !PSignal.Transposed
    -- | Current pitch value.  Starts as the pitch at the end of the previous
    -- note.
    , _from :: !Step
    -- | Previous swaram.
    , _prev :: !Step
    -- | Next swaram.
    , _next :: !Step

    -- # maintained automatically
    -- | Transition time between pitch movements.
    , _transition :: !Typecheck.Normalized
    } deriving (Show)

-- | Relative NoteNumbers.
type Step = Double

instance Pretty PitchState where
    format (PitchState swaram from prev next transition) =
        Pretty.recordTitle "PitchState"
            [ ("swaram", Pretty.format swaram)
            , ("from", Pretty.format from)
            , ("prev", Pretty.format prev)
            , ("next", Pretty.format next)
            , ("transition", Pretty.format transition)
            ]

setPitch :: PSignal.Transposed -> M PitchState ()
setPitch p = do
    swaram <- State.gets _swaram
    steps <- lift $ stepDifference p swaram
    State.modify $ \state -> state { _from = steps }

getFrom :: M PitchState PSignal.Transposed
getFrom = do
    steps <- State.gets _from
    swaram <- State.gets _swaram
    pure $ Pitches.transposeNn (Pitch.nn steps) swaram

-- * sequence

newtype Result = Result (DList.DList Signal.Control)
    deriving (Show, Semigroup, Monoid)

pitchSequence :: ScoreTime -> PitchState -> Text -> Derive.Deriver Result
pitchSequence dur state arg = do
    calls <- Derive.requireRight (("parsing " <> pretty arg <> ": ")<>) $
        resolvePostfix =<< resolvePitchCalls =<< parsePitchSequence arg
    let starts = sliceTime dur (callDurations calls)
        ranges = zip starts (drop 1 starts)
    (results, _) <- State.runStateT (mapM evalPitch (zipCalls ranges calls))
        state
    pure $ mconcat results

sliceTime :: ScoreTime -> [Double] -> [ScoreTime]
sliceTime dur slices =
    scanl (+) 0 $ map ((*one) . ScoreTime.fromDouble) slices
    where one = dur / ScoreTime.fromDouble (Num.sum slices)

evalPitch :: Call ((ScoreTime, ScoreTime), (PitchCall, Char))
    -> M PitchState Result
evalPitch (Call ((start, end), (pcall, name)) arg_) = case pcallCall pcall of
    PCall signature func -> do
        parsedArg <- parseArgs (ctxCallName ctx) (pcallArg pcall name arg_)
            signature
        (Result . DList.singleton) <$> func parsedArg ctx
    where
    ctx = Context
        { ctxStart = start
        , ctxEnd = end
        , ctxCallName = Derive.CallName $ Text.cons name arg_
        }

data Call call = Call !call !Text
    deriving (Eq, Show, Functor)

callDurations :: [Call (PitchCall, a)] -> [Double]
callDurations = map $ pcallDuration . (\(Call (pcall, _) _) -> pcall)

zipCalls :: [a] -> [Call b] -> [Call (a, b)]
zipCalls xs calls = [Call (x, c) arg | (x, Call c arg) <- zip xs calls]

parsePitchSequence :: Text -> Either Text [ParsedPitch]
parsePitchSequence = ParseText.parse1 pExprs

resolvePitchCalls :: [ParsedPitch] -> Either Text [Call (PitchCall, Char)]
resolvePitchCalls = concatMapM resolve
    where
    resolve (PitchGroup calls) =
        map (modifyDuration (* (1 / fromIntegral (length calls)))) <$>
            concatMapM resolve calls
    resolve (CallArg name arg) = case Map.lookup name pitchCallMap of
        Nothing -> Left $ "pitch call not found: " <> showt name
        -- Apply the same argument to all of them.  But I should only get
        -- multiple PitchExprs for aliases, which expect no argument.
        Just calls -> Right [Call (c, name) arg | c <- calls]

resolvePostfix :: [Call (PitchCall, Char)]
    -> Either Text [Call (PitchCall, Char)]
resolvePostfix = resolve <=< ensureNoArgs
    where
    resolve [] = Right []
    resolve (call : calls)
        | Maybe.isJust (isPostfix call) =
            Left "postfix call with no preceding call"
        | otherwise = (modifyDuration modify call :) <$> resolve post
        where
        (pre, post) = Lists.spanWhile isPostfix calls
        modify dur = foldl' (flip ($)) dur pre
    -- The parser shouldn't look for args, but let's check anyway.
    ensureNoArgs calls
        | null errs = Right calls
        | otherwise = Left $
            "postfix calls can't have args: " <> Text.intercalate ", " errs
        where errs = concatMap hasArg calls
    hasArg call@(Call _ arg)
        | Maybe.isJust (isPostfix call) && arg /= mempty = [arg]
        | otherwise = []
    isPostfix (Call (_, name) _) = Map.lookup name postfixCalls

postfixCalls :: Map Char (Double -> Double)
postfixCalls = Map.fromList [('_', (+1)), ('.', (/2))]

postfixDoc :: Text
postfixDoc = "Postfix call that modifies the duration of the previous call."
    <> " `_` adds 1 to it, `.` divides by 2."

modifyDuration :: (Double -> Double) -> Call (PitchCall, a)
    -> Call (PitchCall, a)
modifyDuration modify = fmap $ first $ \call ->
    if pcallDuration call > 0
        then call { pcallDuration = modify (pcallDuration call) }
        else call

-- * DynCall

data DynCall = forall a. DynCall
    { dcallDoc :: Text
    , _dcallSignature :: Sig.Parser a
    , _dcallFunc :: a -> Context -> M DynState Signal.Control
    }

dcFlat :: DynCall
dcFlat = DynCall "No movement." Sig.noArgs $ \() ctx -> do
    prev <- State.gets stateFromDyn
    start <- lift $ Derive.real (ctxStart ctx)
    pure $ Signal.fromSample start prev

dcAttack :: DynCall
dcAttack = DynCall doc dynArg $ \maybeTo ctx ->
    makeDynCurve (dynCurve 0 0.8) 0 (fromMaybe 1 maybeTo) ctx
    where doc = "Attack from 0."

dcAttackFrom :: DynCall
dcAttackFrom = DynCall doc dynArg $ \maybeTo ctx -> do
    from <- State.gets stateFromDyn
    makeDynCurve (dynCurve 0 0.8) from (fromMaybe 1 maybeTo) ctx
    where doc = "Attack from previous value."

dcDecay :: Double -> Double -> DynCall
dcDecay w1 w2 = DynCall doc dynArg $ \maybeTo ctx -> do
    from <- State.gets stateFromDyn
    makeDynCurve (dynCurve w1 w2) from (fromMaybe 0 maybeTo) ctx
    where doc = "Decay to 0, with curve weights: " <> pretty (w1, w2)

dcMoveTo :: DynCall
dcMoveTo = DynCall "Move to dyn." requiredDynArg $ \to ctx -> do
    from <- State.gets stateFromDyn
    makeDynCurve (dynCurve 0.5 0.5) from to ctx

makeDynCurve :: ControlUtil.Curve -> Signal.Y -> Signal.Y -> Context
    -> M DynState Signal.Control
makeDynCurve curve from to ctx = do
    (start, end) <- ctxRange ctx
    State.modify $ \state -> state { stateFromDyn = to }
    lift $ ControlUtil.makeSegment curve start from end to

dynCurve :: Double -> Double -> ControlUtil.Curve
dynCurve w1 w2 = ControlUtil.Function $ ControlUtil.sigmoid w1 w2

dcSetDyn :: DynCall
dcSetDyn = DynCall "Set from dyn." requiredDynArg $ \to _ctx -> do
    State.modify $ \state -> state { stateFromDyn = to }
    pure mempty

dynArg :: Sig.Parser (Maybe Signal.Y)
dynArg = fmap argToDyn <$> Sig.defaulted "move" (Nothing :: Maybe Sig.Dummy)
    "Move to n/9."

requiredDynArg :: Sig.Parser Signal.Y
requiredDynArg = argToDyn <$> Sig.required "move" "Move to n/9."

argToDyn :: Int -> Signal.Y
argToDyn = (/9) . fromIntegral

-- * PitchCall

data PitchCall = PitchCall
    { pcallDoc :: !Text
    , pcallDuration :: !Double
    -- | If True, cons the call's name on to the arg before parsing it.
    , pcallParseCallName :: !Bool
    , pcallCall :: !PCall
    }

-- | Argument parser and call function.
data PCall = forall a. PCall
    (Sig.Parser a) (a -> Context -> M PitchState Signal.Control)

pcallArg :: PitchCall -> Char -> Text -> Text
pcallArg pcall name arg
    | pcallParseCallName pcall = Text.cons name arg
    | otherwise = arg

resolveAliases :: Map Char (Either (Double, [Text]) PitchCall)
    -> Either Text (Map Char [PitchCall])
resolveAliases callMap = Map.fromList <$> mapM resolve (Map.toList callMap)
    where
    resolve (name, Right call) = Right (name, [call])
    resolve (name, Left (duration, calls)) =
        (,) name . map (setDur duration) <$> mapM resolve1 calls
    setDur dur pcall = pcall { pcallDuration = dur }
    resolve1 to = do
        (c, arg) <- justErr "empty alias" $ Text.uncons to
        call <- justErr ("not found: " <> showt c) $ Map.lookup c callMap
        call <- first (("alias to alias: "<>) . showt) call
        Right $ applyArg call c arg

applyArg :: PitchCall -> Char -> Text -> PitchCall
applyArg call name arg = call
    { pcallCall = case pcallCall call of
        PCall signature func -> PCall ignore $ \_ ctx -> do
            parsed <- parseArgs (ctxCallName ctx) (pcallArg call name arg)
                signature
            func parsed ctx
    }
    where
    -- Accept anything for an argument but ignore it.  This is because
    -- I've already hardcoded the argument, but 'eval_pitch' will want to apply
    -- it anyway, since it can't tell the difference from an alias call and
    -- a normal call.
    ignore :: Sig.Parser DeriveT.Val
    ignore = Sig.defaulted "ignore" (DeriveT.num 0) ""

-- ** PitchCall implementation

parseArgs :: State.MonadTrans m => Derive.CallName -> Text -> Sig.Parser a
    -> m Derive.Deriver a
parseArgs name arg sig = lift $ do
    vals <- Derive.requireRight (("parsing " <> pretty name <> ": ") <>) $
        if Text.null arg then pure [] else (:[]) <$> Parse.parseVal arg
    Sig.requireRight
        =<< Sig.parseVals sig (Derive.dummyContext 0 1 (pretty name))
            name vals

-- | Here I am reinventing Derive.Call yet again.  This is the equivalent of
-- 'Derive.Context' and 'Derive.PassedArgs'.
data Context = Context
    { ctxStart :: !ScoreTime
    , ctxEnd :: !ScoreTime
    -- | Complete call name, first char consed to arg.
    , ctxCallName :: !Derive.CallName
    } deriving (Show)

ctxRange :: Context -> M s (RealTime, RealTime)
ctxRange ctx = lift $
    (,) <$> Derive.real (ctxStart ctx) <*> Derive.real (ctxEnd ctx)

pcFlat :: PCall
pcFlat = PCall Sig.noArgs $ \() ctx -> do
    step <- State.gets _from
    start <- lift $ Derive.real $ ctxStart ctx
    pure $ Signal.fromSample start step

-- | Move relative to the note's swaram.
pcRelative :: PCall
pcRelative = PCall (Sig.required "to" "To pitch.") $
    \(Typecheck.DefaultDiatonic transpose) ctx -> do
        from <- State.gets _swaram
        moveTo ctx (Pitches.transpose transpose from)

-- | Move relative to the current pitch.
pcRelativeMove :: Pitch.Transpose -> PCall
pcRelativeMove transpose = PCall Sig.noArgs $ \() ctx -> do
    fromPitch <- getFrom
    moveTo ctx (Pitches.transpose transpose fromPitch)

data PitchDirection = Previous | Current | Next deriving (Show, Eq)
instance Pretty PitchDirection where pretty = showt

pcMoveDirection :: PitchDirection -> PCall
pcMoveDirection dir = PCall Sig.noArgs $ \() ctx ->
    moveTo ctx =<< getDirectionPitch dir

pcSetPitchFrom :: PitchDirection -> PCall
pcSetPitchFrom dir = PCall Sig.noArgs $ \() _ctx -> do
    setPitch =<< getDirectionPitch dir
    pure mempty

getDirectionPitch :: PitchDirection -> M PitchState PSignal.Transposed
getDirectionPitch dir = case dir of
    Previous -> Pitches.transposeNn <$> (Pitch.nn <$> State.gets _prev)
        <*> State.gets _swaram
    Current -> State.gets _swaram
    Next -> Pitches.transposeNn <$> (Pitch.nn <$> State.gets _next)
        <*> State.gets _swaram

pcSetPitch :: PCall
pcSetPitch = PCall (Sig.required "to" "To pitch.") $
    \arg _ctx -> do
        transpose <- lift $ parseTranspose arg
        setPitch . Pitches.transpose transpose =<< State.gets _swaram
        pure mempty

parseTranspose :: Either Pitch.Transpose Expr.Str
    -> Derive.Deriver Pitch.Transpose
parseTranspose (Left t) = pure t
parseTranspose (Right (Expr.Str sym)) = case untxt sym of
    [c] | 'a' <= c && c <= 'z' -> pure $ Pitch.Diatonic $ fromIntegral $
        fromEnum 'a' - fromEnum c - 1
    _ -> Derive.throw $ "expected a lowercase letter: " <> showt sym

data TransitionTime = Slow | Medium | Fast deriving (Show, Eq)

pcSetTransitionTime :: TransitionTime -> PCall
pcSetTransitionTime time = PCall Sig.noArgs $ \() _ctx -> do
    State.modify $ \state -> state { _transition = ttime }
    pure mempty
    where
    -- TODO these could come from an environ value
    ttime = Typecheck.Normalized $ case time of
        Fast -> 0.1
        Medium -> 0.5
        Slow -> 0.9

-- ** util

moveTo :: Context -> PSignal.Transposed -> M PitchState Signal.Control
moveTo ctx to = do
    (start, end) <- ctxRange ctx
    from <- getFrom
    movePitch start from end to

movePitch :: RealTime -> PSignal.Transposed -> RealTime -> PSignal.Transposed
    -> M PitchState Signal.Control
movePitch start from end to = do
    Typecheck.Normalized transition <- State.gets _transition
    let curve = ControlUtil.Function $
            ControlUtil.sigmoid (1-transition) (1-transition)
    setPitch to
    swaram <- State.gets _swaram
    from <- lift $ stepDifference from swaram
    to <- lift $ stepDifference to swaram
    lift $ ControlUtil.makeSegment curve start from end to

-- * parser

data ParsedPitch = CallArg !Char !Text | PitchGroup ![ParsedPitch]
    deriving (Show, Eq)

type Parser a = A.Parser a

pExprs :: Parser [ParsedPitch]
pExprs = A.skipSpace *> A.many1 (pExpr <* A.skipSpace)

pExpr :: Parser ParsedPitch
pExpr = pGroup <|> pPitchExpr

pPitchExpr :: Parser ParsedPitch
pPitchExpr = do
    c <- A.satisfy $ \c -> c /=' ' && c /= '[' && c /= ']'
    if pitchHasArgument c
        then CallArg c <$> pPitchExprArg
        else pure $ CallArg c ""

pGroup :: Parser ParsedPitch
pGroup = PitchGroup <$> (A.char '[' *> pExprs <* A.char ']')

pPitchExprArg :: Parser Text
pPitchExprArg = do
    minus <- A.option False (A.char '-' >> pure True)
    c <- A.satisfy (/=' ')
    pure $ (if minus then ("-"<>) else id) (Text.singleton c)

pitchHasArgument :: Char -> Bool
pitchHasArgument c = Char.isUpper c || c == '-'



-- * misc

cSahitya :: Derive.Taggable a => Derive.Transformer a
cSahitya = Derive.transformer module_ "sahitya" mempty
    "Ignore the transformed deriver. Put this on a track to ignore its\
    \ contents, and put in sahitya."
    $ Sig.call0t $ \_args _deriver -> pure Stream.empty
