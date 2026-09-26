-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.Prelude.Val (library) where
import qualified Data.Attoparsec.Text as A
import qualified Data.Map as Map

import qualified Util.Doc as Doc
import qualified Util.Lists as Lists
import qualified Util.ParseText as ParseText

import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Call.ControlUtil as ControlUtil
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Module as Module
import qualified Derive.Call.PitchUtil as PitchUtil
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Eval as Eval
import qualified Derive.Expr as Expr
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Pitches as Pitches
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Theory as Theory
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck
import qualified Derive.ValType as ValType

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import qualified Ui.Event as Event
import qualified Ui.ScoreTime as ScoreTime

import           Global
import           Types


library :: Library.Library
library = Library.vals
    [ (">", cNextVal)
    , ("<", cPrevVal)
    , ("next-event", cNextEvent)
    , ("bpm", cBpm)
    , ("env", cEnv)
    , ("tempo", cTempo)
    , ("ts", cTimestep)
    , ("ts/", cTimestepReciprocal)
    , ("1/", cReciprocal)
    , ("nn", cNn)
    , ("hz", cHz)
    -- literals
    , ("list", cList)
    , ("lrange", cLrange)
    , ("st", cScoretime)
    , ("rt", cRealtime)
    , ("pitch", cPitch)
    , ("#", cPcontrolRef)
    , ("signal", cSignal)
    , ("psignal", cPsignal)
    -- , ("control", c_control)
    -- , ("pcontrol", c_pcontrol)
    -- lookup
    , ("<-#", cGetPitch)
    -- generate signals
    , ("i>", cLinearNext)
    , ("e>", cExpNext)
    , ("df", cDownFrom)
    ]

cNextVal :: Derive.ValCall
cNextVal = valCall "next-val" Tags.next
    "Evaluate the next event. Only works on pitch and control tracks, and\
    \ if the next event doesn't need its previous event."
    $ Sig.call0 $ \args -> do
        event <- Derive.require "no next event" $
            Lists.head (Args.nextEvents args)
        start <- Derive.real (Event.start event)
        nextVal event start (Derive.ctxTrackType (Derive.passedCtx args))

nextVal :: Event.Event -> RealTime -> Maybe ParseTitle.Type
    -> Derive.Deriver DeriveT.Val
nextVal event start ttype = case ttype of
    Just ParseTitle.ControlTrack -> evalControl start event
    Just ParseTitle.TempoTrack -> evalControl start event
    Just ParseTitle.PitchTrack -> do
        signal <- eval event
        case PSignal.at signal start of
            Nothing -> Derive.throw "next pitch event didn't emit a pitch"
            Just pitch -> pure $ DeriveT.VPSignal $ PSignal.constant pitch
    Just ParseTitle.NoteTrack ->
        Derive.throw "can't get next value for note tracks"
    Nothing -> Derive.throw "no track type"
    where
    evalControl start event = do
        signal <- eval event
        pure $ DeriveT.num $ Signal.at (signal :: Signal.Control) start
    eval event = mconcat . Stream.eventsOf <$>
        (either Derive.throw pure =<< Eval.evalEvent event)

cPrevVal :: Derive.ValCall
cPrevVal = valCall "prev-val" Tags.prev
    "Return the previous value. Only works on pitch and control tracks."
    $ Sig.call0 $ \args -> do
        start <- Args.realStart args
        case Args.prevVal args of
            Just (Derive.TagControl sig) ->
                pure $ DeriveT.num $ Signal.at sig start
            Just (Derive.TagPitch sig) ->
                maybe (Derive.throw "no previous pitch")
                    (pure . DeriveT.VPSignal . PSignal.constant)
                    (PSignal.at sig start)
            _ -> Derive.throw "no previous value"

cNextEvent :: Derive.ValCall
cNextEvent = valCall "next-event" Tags.next
    "Start RealTime of the next event. Only used for tests."
    $ Sig.call0 $ \args -> do
        event <- Derive.require "no next event" $
            Lists.head (Args.nextEvents args)
        Derive.scoreToReal $ Event.start event

cBpm :: Derive.ValCall
cBpm = valCall "bpm" mempty "Convert bpm to tempo.  This is just (/60)."
    $ Sig.call (Sig.defaulted "bpm" (Nothing :: Maybe Double) "")
    $ \mbBpm args -> case mbBpm of
        Just bpm -> pure $ (bpm :: Double) / 60
        Nothing -> (*60) <$> Derive.getTempo (Args.start args)

cTempo :: Derive.ValCall
cTempo = valCall "tempo" mempty "Get tempo at this point."
    $ Sig.call0 $ \args -> Derive.getTempo (Args.start args)

cEnv :: Derive.ValCall
cEnv = valCall "env" mempty
    "Look up the given val in the environ."
    $ Sig.call ((,)
    <$> Sig.required "name" "Look up the value of this key."
    <*> Sig.defaulted "default" (Nothing :: Maybe DeriveT.Val)
        "If given, this is the default value when the key isn't present. If\
        \ not given, a missing key will throw an exception. The presence of\
        \ a default will also make the lookup expect the same type as the\
        \ default."
    ) $ \(name, maybeDeflt) _args -> case maybeDeflt of
        Nothing -> Derive.getVal name
        Just deflt -> check name deflt =<< Derive.lookupVal name
    where
    check _ deflt Nothing = pure deflt
    check name deflt (Just val)
        | DeriveT.typesEqual deflt val = pure val
        | otherwise = Derive.throw $ "env " <> pretty name
            <> " expected " <> pretty (ValType.generalTypeOf deflt)
            <> " but got " <> pretty (ValType.generalTypeOf val)

cTimestep :: Derive.ValCall
cTimestep = valCall "timestep" mempty
    ("Compute the duration of the given RelativeMark timestep at the current\
    \ position. This is for durations, so it only works with RelativeMark."
    ) $ Sig.call ((,)
    <$> Sig.required "rank" "Emit a duration of this rank."
    <*> Sig.defaulted "steps" (1 :: Int) "This number of steps of that rank."
    ) $ \(rank, steps) args -> DeriveT.scoreTime <$>
        Call.meterDuration (Args.start args) rank steps

cTimestepReciprocal :: Derive.ValCall
cTimestepReciprocal = Make.modifyVcall cTimestep Module.prelude
    "timestep-reciprocal"
    ("This is the same as `timestep` except it returns the reciprocal. This is\
    \ useful for e.g. trills which take cycles per second rather than duration."
    ) reciprocal
    where
    reciprocal val
        | Just num <- DeriveT.constantVal val =
            DeriveT.VSignal $ Signal.constant . recip <$> num
        | otherwise = val

cReciprocal :: Derive.ValCall
cReciprocal = valCall "reciprocal" mempty
    "Find the reciprocal of a number. Useful for tempo, e.g. set the tempo to\
    \ 1/time." $ Sig.call (Sig.required "num" "") $ \num _ ->
        if num == 0 then Derive.throw "1/0"
            else pure (1 / num :: Double)

cNn :: Derive.ValCall
cNn = valCall "nn" mempty
    "Convert a pitch, hz, or twelve-tone pitch name to a NoteNumber.\
    \ A pitch name looks like `[a-g]s?[-1-9]`."
    $ Sig.call (Sig.required "val" "") $ \val _ -> case val of
        Left pitch -> Pitches.pitchNn (PSignal.coerce (pitch :: PSignal.Pitch))
        Right (Left hz) -> pure $ Pitch.hzToNn hz
        Right (Right name) -> getNameNn name

cHz :: Derive.ValCall
cHz = valCall "hz" mempty
    "Convert a pitch, twelve-tone pitch name, or NoteNumber to hz.\
    \ A pitch name looks like `[a-g]s?[-1-9]`. If the octave isn't given, it\
    \ defaults to 0.  This is useful for `%just-base`, which ignores the\
    \ octave."
    $ Sig.call (Sig.required "val" "") $ \val _ -> case val of
        Left pitch -> Pitch.nnToHz <$>
            Pitches.pitchNn (PSignal.coerce (pitch :: PSignal.Pitch))
            -- Not transposed because they asked for a specific pitch.
        Right (Left name) -> Pitch.nnToHz <$> getNameNn name
        Right (Right nn) ->
            pure (Pitch.nnToHz (Pitch.NoteNumber nn) :: Double)

getNameNn :: Expr.Str -> Derive.Deriver Pitch.NoteNumber
getNameNn name =
    Derive.require ("unknown pitch: " <> ShowVal.showVal name) $
        parsePitchName (Expr.unstr name)

-- | c-1 is 0, g9 is 127.  The octave is optional, and defaults to 1.
parsePitchName :: Text -> Maybe Pitch.NoteNumber
parsePitchName = either (const Nothing) Just . ParseText.parse1 parse
    where
    parse = do
        pc <- maybe mzero pure . (`Map.lookup` pcs) =<< A.anyChar
        sharp <- A.option 0 $ A.char 's' >> pure 1
        oct <- A.option 1 $ ParseText.pInt
        pure $ Pitch.nn $ pc + sharp + (oct+1) * 12
    pcs = Map.fromList $ zip "cdefgab" (scanl (+) 0 Theory.pianoIntervals)

cList :: Derive.ValCall
cList = valCall "list" mempty "Create a list." $
    Sig.call (Sig.many "val" "Value.") $ \vals _ ->
        pure $ DeriveT.VList vals

cLrange :: Derive.ValCall
cLrange = valCall "range" mempty "Make a list range of numbers." $
    Sig.call ((,,)
    <$> Sig.required "start" "Start value."
    <*> Sig.required "step" "Step."
    <*> Sig.required "count" "Number of values."
    ) $ \(start, step, count) _ ->
        pure $ DeriveT.VList $ map DeriveT.num $ Lists.range start step count

cScoretime :: Derive.ValCall
cScoretime = valCall "scoretime" mempty
    "Convert a number to ScoreTime. This just changes the type annotation, the\
    \ value remains the same." $
    Sig.call (Sig.requiredEnv "val" Sig.None "") $ \val _ ->
        pure $ ScoreTime.fromDouble val

cRealtime :: Derive.ValCall
cRealtime = valCall "realtime" mempty
    "Convert a number to RealTime. This just changes the type annotation, the\
    \ value remains the same." $
    Sig.call (Sig.requiredEnv "val" Sig.None "") $ \val _ ->
        pure $ RealTime.seconds val

cPitch :: Derive.ValCall
cPitch = valCall "pitch" mempty "Create a 'Perform.Pitch.Pitch'."
    $ Sig.call ((,,)
    <$> Sig.defaultedEnv "oct" Sig.None (Left 0 :: Either Int Int)
        "Octave, or a pitch name or pitch. If it's a pitch name or pitch, the\
        \ `pc` and `accs` args must be 0."
    <*> Sig.defaultedEnv "pc" Sig.None (0 :: Int) "Pitch class."
    <*> Sig.defaultedEnv "accs" Sig.None (0 :: Int) "Accidentals."
    ) $ \(oct, pc, accs) _ -> makePitch oct pc accs

makePitch :: Either Pitch.Octave (Either Text PSignal.Pitch)
    -> Pitch.PitchClass -> Pitch.Accidentals -> Derive.Deriver Pitch.Pitch
makePitch (Left oct) pc accs = pure $ Pitch.Pitch oct (Pitch.Degree pc accs)
makePitch (Right namePitch) pc accs
    | pc /= 0 || accs /= 0 = Derive.throw $
        "pc and accs args must be 0 when a pitch is given: " <> showt (pc, accs)
    | otherwise = do
        (note, scale) <- case namePitch of
            Left name -> (,) <$> pure (Pitch.Note name) <*> Call.getScale
            Right pitch -> (,)
                <$> Pitches.pitchNote (PSignal.coerce pitch)
                <*> Derive.getScale (PSignal.pitchScaleId pitch)
        env <- Derive.getEnviron
        either (Derive.throw . pretty) pure $
            Scale.scaleRead scale env note

cPcontrolRef :: Derive.ValCall
cPcontrolRef = valCall "pcontrol-ref" mempty
    "Create a 'Derive.DeriveT.PControlRef'. For control literals, the\
    \ `#name` syntax suffices, but if you want to give a default pitch,\
    \ you need this call. This is because pitches are calls, and while\
    \ `%c,1` can be parsed as-is, `#p,(4c)` needs an evaluation."
    $ Sig.call ((,)
    <$> Sig.required "name" "Name of pitch signal."
    <*> Sig.defaulted "default" (Nothing :: Maybe PSignal.Pitch)
        "Default pitch, if the signal is not set."
    ) $ \(name, mbDefault) _ -> pure $
        DeriveT.Ref (ScoreT.PControl name) (PSignal.constant <$> mbDefault)

-- | This should be the inverse of ShowVal (ScoreT.Typed Signal.Control).
-- See 'DeriveT.show_signal'.
cSignal :: Derive.ValCall
cSignal = valCall "signal" mempty
    "Create a signal. This is the control signal literal."
    $ Sig.call ((,)
        <$> Sig.optionalEnv "type" Derive.None "" "Type code."
        <*> Sig.manyPairs "breakpoints" "Breakpoints."
    ) $ \(typeCode, bps) _ -> do
        typ <- Derive.require ("unknown type code: " <> typeCode)
            (ScoreT.codeToType typeCode)
        pure $ DeriveT.VSignal $ ScoreT.Typed typ $ Signal.fromPairs bps

-- | Like c_signal, should be the inverse of ShowVal PSignal.PSignal, except
-- it can't because I can't show pitches.
-- See 'DeriveT.show_psignal'.
cPsignal :: Derive.ValCall
cPsignal = valCall "psignal" mempty "Create a pitch signal."
    $ Sig.call (Sig.manyPairs "breakpoints" "Breakpoints.")
    $ \bps _ -> pure $ DeriveT.VPSignal $ PSignal.fromPairs bps

{-
c_control :: Derive.ValCall
c_control = val_call "control" mempty
    "Reference to a signal. Like 'env' but specialized to a signal.\
    \ This is the literal backing the %c,1 syntax sugar."
    -- TODO except it's not, not yet
    $ Sig.call ((,)
    <$> Sig.required "name" "Look up a signal under this key."
    <*> Sig.defaulted "default" (Nothing :: Maybe (ScoreT.Typed Signal.Y))
        "Returned when the name isn't present."
    ) $ \(name, maybe_default) _args -> do
        Typecheck.lookup_signal name >>= \case
            Nothing -> Derive.require
                ("no control and no default: " <> ShowVal.show_val name)
                maybe_default
            Just tsig -> return tsig

c_pcontrol :: Derive.ValCall
c_pcontrol = val_call "pcontrol" mempty
    "Reference to a pitch signal. Like 'env' but specialized to a pitch signal.\
    \ This is the literal backing the #c,(4c) syntax sugar."
    $ Sig.call ((,)
    <$> Sig.required "name" "Look up a signal under this key."
    <*> Sig.defaulted "default" (Nothing :: Maybe Sig.Dummy)
        "Returned when the name isn't present."
    ) $ \(name, maybe_default) _args ->
        Typecheck.lookup_pitch_signal name >>= \case
            Nothing -> Derive.require
                ("no pitch control and no default: " <> ShowVal.show_val name)
                maybe_default
            Just psig -> return psig
-}

-- * lookup

cGetPitch :: Derive.ValCall
cGetPitch = valCall "pitch" mempty "Get the current pitch." $
    Sig.call (Sig.defaulted "control" ("" :: Text)
        "The default pitch if empty, otherwise, get the named pitch.") $
    \control args -> Derive.require "pitch"
        =<< Derive.namedPitchAt control =<< Args.realStart args

-- * generate signals

cLinearNext :: Derive.ValCall
cLinearNext = valCall "linear-next" mempty
    "Create straight lines between the given breakpoints."
    $ Sig.call breakpointsArg $ \vals args ->
        breakpoints 0 ControlUtil.Linear vals args

cExpNext :: Derive.ValCall
cExpNext = valCall "exp-next" mempty
    "Create curved lines between the given breakpoints."
    $ Sig.call ((,)
    <$> Sig.defaulted "exp" (2 :: Double) ControlUtil.exponentialDoc
    <*> breakpointsArg
    ) $ \(exp, vals) args ->
        breakpoints 1 (ControlUtil.Function $ ControlUtil.expon exp) vals args

breakpointsArg :: Sig.Parser (NonEmpty DeriveT.Val)
breakpointsArg = Sig.many1 "bp" "Breakpoints are distributed evenly between\
    \ the event start and the next event. They can be all numbers, or all\
    \ pitches."

cDownFrom :: Derive.ValCall
cDownFrom = valCall "down-from" mempty
    "Go down from a starting value at a certain rate."
    $ Sig.call ((,)
    <$> Sig.defaulted "from" (1 :: Double) "Start at this value."
    <*> Sig.defaulted "speed" (1 :: Double) "Descend this amount per second."
    ) $ \(from, speed) args -> do
        (start, end) <- Args.realRangeOrNext args
        pure $ ScoreT.untyped $
            ControlUtil.slopeToLimit (Just 0) Nothing from (-speed) start end

-- ** implementation

breakpoints :: Int -> ControlUtil.Curve -> NonEmpty DeriveT.Val
    -> Derive.PassedArgs a -> Derive.Deriver DeriveT.Val
breakpoints argnum curve vals args = do
    (start, end) <- Args.realRangeOrNext args
    srate <- Call.getSrate
    vals <- numOrPitch (Args.start args) argnum vals
    pure $ case vals of
        Left nums -> DeriveT.VSignal $
            ScoreT.untyped $ ControlUtil.breakpoints srate curve $
            ControlUtil.distribute start end nums
        Right pitches -> DeriveT.VPSignal $
            PitchUtil.breakpoints srate curve $
            ControlUtil.distribute start end pitches

-- | Insist that the vals be either all numbers or pitches.
--
-- TODO If 'Sig.Parser' supported Alternative, maybe I could build this as
-- a parser and get both shorter code and documentation.
numOrPitch :: ScoreTime -> Int -> NonEmpty DeriveT.Val
    -> Derive.Deriver (Either [Signal.Y] [PSignal.Pitch])
numOrPitch start argnum (val :| vals)
    | Just (ScoreT.Typed ScoreT.Untyped n) <- DeriveT.constantVal val = do
        vals <- mapM (expect tsig) (zip [argnum + 1 ..] vals)
        pure $ Left (n : vals)
    | Just pitch <- PSignal.pitchVal val = do
        vals <- mapM (expect ValType.TPitch) (zip [argnum + 1 ..] vals)
        pure $ Right (pitch : vals)
    | otherwise = typeError argnum "bp"
        (ValType.TEither tsig ValType.TPitch) val
    where
    tsig = ValType.TSignal ValType.TUntyped ValType.TAny
    expect typ (argnum, val) =
        maybe (typeError argnum "bp" typ val) pure
            =<< Typecheck.fromValEval start val

typeError :: Int -> Derive.ArgName -> ValType.Type -> DeriveT.Val
    -> Derive.Deriver a
typeError argnum name expected received =
    Derive.throwError $ Derive.CallError $ Derive.TypeError $ Derive.TypeErrorT
        { errorPlace = Derive.TypeErrorArg argnum
        , errorSource = Derive.Literal
        , errorArgName = name
        , errorExpected = expected
        , errorReceived = Just received
        , errorDerive = Nothing
        }


-- * util

valCall :: Typecheck.ToVal a => Derive.CallName -> Tags.Tags -> Doc.Doc
    -> Derive.WithArgDoc (Derive.PassedArgs Derive.Tagged -> Derive.Deriver a)
    -> Derive.ValCall
valCall = Derive.valCall Module.prelude
