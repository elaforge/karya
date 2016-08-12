-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls for Carnatic gamakam.
module Derive.Call.India.Gamakam2 where
import qualified Control.Monad.State.Strict as Monad.State
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text

import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Ui.Event as Event
import qualified Derive.Args as Args
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call as Call
import qualified Derive.Call.ControlUtil as ControlUtil
import qualified Derive.Call.Module as Module
import qualified Derive.Call.PitchUtil as PitchUtil
import qualified Derive.Call.Prelude.SignalTransform as SignalTransform
import qualified Derive.Call.Prelude.Trill as Trill
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Eval as Eval
import qualified Derive.PSignal as PSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Global
import Types


module_ :: Module.Module
module_ = "india" <> "gamakam2"

-- | Calls in these modules are meant to be used via the sequence call, so they
-- are only in scope under the relevant phase.
begin_module, middle_module, end_module :: Module.Module
begin_module = module_ <> "begin"
middle_module = module_ <> "middle"
end_module = module_ <> "end"

note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.call_maps
    [("@", c_sequence)]
    [ ("@", c_sequence_transform)
    , ("sahitya", c_sahitya)
    ]

pitch_calls :: Derive.CallMaps Derive.Pitch
pitch_calls = Derive.generator_call_map $ concat
    [ begin_calls
    , begin_aliases
    , middle_calls
    , middle_aliases
    , end_calls
    , end_aliases
    ]

begin_calls :: [(BaseTypes.CallId, Derive.Generator Derive.Pitch)]
begin_calls =
    [ ("set-pitch", c_set_pitch)
    , ("flat-start", c_flat_start)
    , ("cur", c_from PitchFromCurrent NoFade)
    , ("cur<", c_from PitchFromCurrent Fade)
    , ("prev", c_from PitchFromPrev NoFade)
    , ("prev<", c_from PitchFromPrev Fade)
    , ("jaru", c_jaru False)
    , ("jaru0", c_jaru True)
    , ("fade-in", c_fade True)
    ]

-- | I don't want to take up short names for the whole track scope, but within
-- a sequence call it seems reasonable.  In addition, I know if it's a begin or
-- end call, and use the same name for logically similar things.
begin_aliases :: [(BaseTypes.CallId, Derive.Generator Derive.Pitch)]
begin_aliases = map (second (Derive.set_module begin_module))
    [ ("-", c_flat_start)
    , ("c", c_from PitchFromCurrent NoFade)
    , ("c<", c_from PitchFromCurrent Fade)
    , ("p", c_from PitchFromPrev NoFade)
    , ("p<", c_from PitchFromPrev Fade)
    , ("J", c_jaru False)
    , ("j", c_jaru True)
    , (fade_in_call, c_fade True)
    ]

middle_calls :: [(BaseTypes.CallId, Derive.Generator Derive.Pitch)]
middle_calls = ("flat", c_flat)
    : kampita_variations "kam" (c_kampita "" neighbor)
    ++ kampita_variations "kam2" (c_kampita "" Kampita2)
    ++ kampita_variations "nkam" (c_nkampita "" neighbor)
    ++ kampita_variations "nkam2" (c_nkampita "" Kampita2)
    where neighbor = Kampita1 0

kampita_variations :: Text -> (Maybe Trill.Direction -> call)
    -> [(BaseTypes.CallId, call)]
kampita_variations name call =
    [ (BaseTypes.Symbol $ name <> Trill.direction_affix end, call end)
    | end <- dirs
    ]
    where dirs = [Nothing, Just Trill.Low, Just Trill.High]

middle_aliases :: [(BaseTypes.CallId, Derive.Generator Derive.Pitch)]
middle_aliases = map (second (Derive.set_module middle_module)) $ concat $
    [ ("-", c_flat)
    ] :
    [ hardcoded "o^" (Kampita0 1 0) (Just Trill.High)
    , hardcoded "o_" (Kampita0 (-1) 0) (Just Trill.Low)
    , hardcoded "o*" (Kampita0 (-1) 1) (Just Trill.Low)
    , alias_prefix "k" "kam" middle_calls
    , alias_prefix "nk" "nkam" middle_calls
    ]
    where
    hardcoded name arg dir =
        [ (name, c_kampita doc arg dir)
        , (BaseTypes.Symbol $ "n" <> BaseTypes.unsym name,
            c_nkampita doc arg dir)
        ]
    doc = Derive.Doc $ Text.unlines
        [ "These are hardcoded `k` variants:"
        , "`o^` touches the swaram from above, like `k2^ 1 0`."
        , "`o_` touches the swaram from below, like `k2_ -1 0`."
        , "`o*` avoids the swaram, like `k2_ -1 1`."
        ]

alias_prefix :: Text -> Text -> [(BaseTypes.CallId, call)]
    -> [(BaseTypes.CallId, call)]
alias_prefix from to calls = do
    (BaseTypes.Symbol name, call) <- calls
    Just rest <- [Text.stripPrefix to name]
    return (BaseTypes.Symbol (from <> rest), call)

end_calls :: [(BaseTypes.CallId, Derive.Generator Derive.Pitch)]
end_calls =
    [ ("flat-end", c_flat_end)
    , ("to", c_to NoFade)
    , ("to>", c_to Fade)
    , ("fade-out", c_fade False)
    ]

end_aliases :: [(BaseTypes.CallId, Derive.Generator Derive.Pitch)]
end_aliases = map (second (Derive.set_module end_module))
    [ ("-", c_flat_end)
    , ("t", c_to NoFade)
    , ("t>", c_to Fade)
    , (fade_out_call, c_fade False)
    ]

-- | Special behaviour documented in 'sequence_doc'.
fade_out_call :: BaseTypes.CallId
fade_out_call = "->" -- The leading dash makes these parse as symbols.

-- | Unlike 'fade_out_call', this doesn't need special treatment.
fade_in_call :: BaseTypes.CallId
fade_in_call = "-<"

-- * sequence

c_sequence :: Derive.Generator Derive.Note
c_sequence = Derive.generator module_ "sequence" mempty sequence_doc
    $ Sig.call (Sig.many_vals "arg" "Expressions separated by `;`.")
    $ \_ -> Sub.inverting $ \args -> with_sequence args (Call.placed_note args)

c_sequence_transform :: Derive.Transformer Derive.Note
c_sequence_transform = Derive.transformer module_ "sequence" mempty
    sequence_doc $
        Sig.callt (Sig.many_vals "arg" "Expressions separated by `;`.") $
        \_ -> with_sequence

sequence_doc :: Derive.Doc
sequence_doc = "Sequence several pitch calls. Calls are divided into\
    \ `begin ; middle1 ; middle2; ... ; end` phases. Calls are pitch\
    \ generators, and are sequenced such that the middle calls stretch\
    \ based on the duration of the note. The " <> doc begin_module <> ", "
    <> doc middle_module <> ", and " <> doc end_module <> " modules are\
    \ in scope during the begin, middle, and end phases. All calls\
    \ below the " <> doc module_ <> " module are designed for\
    \ sequencing. This just means they emit samples at the beginning and end\
    \ of their range, so the sequence call knows their extent. Normal pitch\
    \ calls may not do that.\
    \\nThere's a special hack for the " <> ShowVal.doc fade_in_call
    <> " and " <> ShowVal.doc fade_out_call <> " calls: they have 0\
    \ duration, but are overlaid with their neighbors. This is so you can fade\
    \ in or out without having to flatten the pitch."
    where doc v = Derive.Doc $ "`" <> pretty v <> "`"

with_sequence :: Derive.PassedArgs Score.Event -> Derive.Deriver a
    -> Derive.Deriver a
with_sequence args deriver = do
    let (begin, middle, end) = parse_sequence (Derive.passed_vals args)
    (pitch, mods) <- sequence_calls (pitch_context (Args.context args))
        (Args.range args) begin middle end
    end_time <- Derive.real $ Args.next args
    Derive.with_pitch pitch $ Derive.with_control_mods mods end_time deriver

pitch_context :: Derive.Context Score.Event -> Derive.Context Derive.Pitch
pitch_context ctx = ctx
    { Derive.ctx_prev_val = Score.event_untransformed_pitch <$>
        Derive.ctx_prev_val ctx
    }

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
sequence_calls :: Derive.Context Derive.Pitch -> (ScoreTime, ScoreTime)
    -> Expr -> [Expr] -> Maybe Expr -> Derive.Deriver Signals
sequence_calls ctx (start, end) begin middles maybe_end =
    fmap fst $ flip Monad.State.runStateT ctx $ do
        (begin_pitch, begin_mods) <- eval begin_module start end begin
        middle_start <- lift $ signal_end start begin_pitch
        -- This is a test eval of 'end', just to see how long it is.  The
        -- middle isn't evaluated yet, so it doesn't have the right
        -- ctx_prev_val.
        (test_end_pitch, _) <- detached $
            maybe_eval (eval end_module middle_start end) maybe_end
        end_start <- lift $ signal_start end test_end_pitch
        (middle_pitch, middle_mods)
            <- sequence_middles middle_start end_start middles
        (end_pitch, end_mods) <- maybe_eval
            (eval_end start end_start end) maybe_end
        return (begin_pitch <> middle_pitch <> end_pitch,
            begin_mods <> middle_mods <> end_mods)
    where
    maybe_eval = maybe (return (mempty, mempty))

-- | Special behaviour for the @fade-out@ call, as documented in 'sequence_doc'.
eval_end :: ScoreTime -> ScoreTime -> ScoreTime -> Expr -> SequenceM Signals
eval_end sequence_start start end expr = case expr of
    EvaluatedExpr call _ | call == fade_out_call ->
        eval end_module sequence_start end expr
    _ -> eval end_module start end expr

-- | I need to thread Derive.ctx_prev_val from each call in the sequence.
type SequenceM = Monad.State.StateT (Derive.Context Derive.Pitch) Derive.Deriver

detached :: SequenceM a -> SequenceM a
detached m = do
    st <- Monad.State.get
    val <- m
    Monad.State.put st
    return val

-- | Give each middle call an even division of the time left.  The call can
-- use less time if it wishes, and the remaining time will be divided among
-- the remaining middle calls.  It shouldn't use more time, because then it
-- could overlap with the end call, or go past the end of the note.
sequence_middles :: ScoreTime -> ScoreTime -> [Expr] -> SequenceM Signals
sequence_middles _ _ [] = return (mempty, mempty)
sequence_middles start end _ | start >= end = return (mempty, mempty)
sequence_middles start end (expr:exprs) = do
    let dur = (end - start) / fromIntegral (length exprs + 1)
    (pitch, mods) <- eval middle_module start (start + dur) expr
    sig_end <- lift $ signal_end start pitch
    (pitch_rest, mods_rest) <- sequence_middles sig_end end exprs
    return (pitch <> pitch_rest, mods <> mods_rest)

signal_start :: ScoreTime -> PSignal.PSignal -> Derive.Deriver ScoreTime
signal_start deflt = maybe (return deflt) (Derive.score . fst) .  PSignal.head

signal_end :: ScoreTime -> PSignal.PSignal -> Derive.Deriver ScoreTime
signal_end deflt = maybe (return deflt) (Derive.score . fst) .  PSignal.last

eval :: Module.Module -> ScoreTime -> ScoreTime -> Expr -> SequenceM Signals
eval module_ start end expr = do
    ctx <- Monad.State.get
    (result, cmods) <- lift $ with_empty_collect $
        Derive.with_imported True module_ $
        eval_expr (place_event start (end - start) ctx) expr
    signal <- mconcat <$> Stream.write_logs result
    unless (PSignal.null signal) $
        Monad.State.put $ ctx { Derive.ctx_prev_val = Just signal }
    return (signal, cmods)

place_event :: ScoreTime -> ScoreTime -> Derive.Context d -> Derive.Context d
place_event start dur ctx = ctx
    { Derive.ctx_event = Event.place start dur (Derive.ctx_event ctx) }

eval_expr :: Derive.Callable d => Derive.Context d -> Expr
    -> Derive.Deriver (Stream.Stream d)
eval_expr ctx (QuotedExpr expr) = Eval.eval_toplevel ctx expr
eval_expr ctx (EvaluatedExpr call_id args) = do
    call <- Eval.get_generator call_id
    Eval.apply_generator ctx call args

with_empty_collect :: Derive.Deriver a
    -> Derive.Deriver (a, [Derive.ControlMod])
with_empty_collect = fmap (second Derive.collect_control_mods)
    . Internal.local_collect

-- ** parse

data Expr =
    -- | This is a call which was embedded in the argument list of the sequence
    -- call, so its arguments have already been evaluated.
    EvaluatedExpr BaseTypes.CallId [BaseTypes.Val]
    -- | A call and its arguments can be protected from evaluation by quoting
    -- it.  This is also necessary to use a transformer, since @;@ has higher
    -- precedence than @|@ (actually it's just a value, not an operator).
    | QuotedExpr !BaseTypes.Expr
    deriving Show

instance Pretty.Pretty Expr where
    format (EvaluatedExpr call_id vals) =
        ("(" <> Pretty.format call_id) Pretty.<+> (Pretty.format vals <> ")")
    format (QuotedExpr quoted) = Pretty.format quoted

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
parse_sequence :: [BaseTypes.Val] -> (Expr, [Expr], Maybe Expr)
parse_sequence exprs = postproc $
    case Seq.map_tail (drop 1) $ Seq.split_with is_separator exprs of
        [] -> (Nothing, [], Nothing)
        begin : rest -> case reverse rest of
            [middle] -> (Just begin, [middle], Nothing)
            end : middle -> (Just begin, reverse middle, Just end)
            [] -> (Just begin, [], Nothing)
    where
    postproc (begin, middles, end) =
        ( fromMaybe (EvaluatedExpr "set-pitch" []) $ to_expr =<< begin
        , add_hold $ mapMaybe to_expr middles
        , to_expr =<< end
        )
    add_hold [] = [EvaluatedExpr "-" []]
    add_hold xs = xs
    to_expr [] = Nothing
    to_expr (call : args) = Just $ case call of
        BaseTypes.VQuoted (BaseTypes.Quoted expr) -> QuotedExpr expr
        BaseTypes.VSymbol sym -> EvaluatedExpr sym args
        _ -> EvaluatedExpr (BaseTypes.Symbol (ShowVal.show_val call)) args
    is_separator BaseTypes.VSeparator = True
    is_separator _ = False

-- * start

c_flat_start :: Derive.Generator Derive.Pitch
c_flat_start = generator1 "flat-start" mempty
    "Emit a flat pitch for the given duration."
    $ Sig.call ((,)
    <$> Sig.defaulted "pitch" Nothing
        "Emit this pitch, or continue the previous pitch if not given."
    <*> Sig.defaulted "time" (Typecheck.real 0.15)
        "Pitch lasts for this duration."
    ) $ \(maybe_pitch, Typecheck.DefaultReal time) args -> do
        start <- Args.real_start args
        end <- get_end start time args
        pitch <- optional_pitch maybe_pitch <$> Call.get_pitch start
        return $ PSignal.signal [(start, pitch), (end, pitch)]

c_set_pitch :: Derive.Generator Derive.Pitch
c_set_pitch = generator1 "set-pitch" mempty "Emit the current pitch.\
    \ Sequence pitch calls normally use the previous pitch, and this is an\
    \ implicit begin call so a sequence missing a begin doesn't inherit the\
    \ previous pitch."
    $ Sig.call0 $ \args -> do
        start <- Args.real_start args
        pitch <- Call.get_pitch start
        return $ PSignal.signal [(start, pitch)]

data PitchFrom = PitchFromPrev | PitchFromCurrent deriving (Eq, Show)
data Fade = Fade | NoFade deriving (Eq, Show)

c_from :: PitchFrom -> Fade -> Derive.Generator Derive.Pitch
c_from pitch_from fade = generator1 "from" mempty
    (case pitch_from of
        PitchFromPrev -> "Come for the previous pitch, and possibly fade in."
        PitchFromCurrent -> "Come from a pitch, and possibly fade in.")
    $ Sig.call ((,,,)
    <$> case pitch_from of
        PitchFromPrev -> pure Nothing
        PitchFromCurrent -> Sig.defaulted "from" Nothing
            "Come from this pitch, or the previous one."
    <*> Sig.defaulted "transition" default_transition "Time to destination."
    <*> Sig.defaulted "to" Nothing "Go to this pitch, or the current one."
    <*> ControlUtil.curve_env
    ) $ \(from_pitch, Typecheck.DefaultReal time, maybe_to_pitch, curve)
            args -> do
        start <- Args.real_start args
        end <- get_end start time args
        to_pitch <- optional_pitch maybe_to_pitch <$> Call.get_pitch start
        let from = resolve_pitch args to_pitch from_pitch
        case fade of
            Fade -> ControlUtil.multiply_dyn end
                =<< ControlUtil.make_segment id start 0 end 1
            NoFade -> return ()
        PitchUtil.make_segment curve start from end to_pitch

-- | Get the end time, given a start and a duration.  Don't go beyond the
-- maximum, which is the event's duration, if given explicitly, or the next
-- event if it's 0.
get_end :: RealTime -> BaseTypes.Duration -> Derive.PassedArgs a
    -> Derive.Deriver RealTime
get_end start dur args = do
    time_end <- (start +) <$> Call.real_duration start dur
    max_end <- if Args.duration args == 0
        then Derive.real $ Args.next args
        else Args.real_end args
    return $ min time_end max_end

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

c_jaru :: Bool -> Derive.Generator Derive.Pitch
c_jaru append_zero = generator1 "jaru" mempty
    "This is a series of grace notes whose pitches are relative to the\
    \ base pitch. The 0 variant appends a 0 on the end."
    $ Sig.call ((,,,)
    <$> Sig.many1 "interval" "Intervals from base pitch."
    <*> Sig.environ "time" Sig.Both default_transition "Time for each note."
    -- TODO This should also be a Duration
    <*> Sig.environ "transition" Sig.Both Nothing
        "Time for each slide, defaults to `time`."
    <*> ControlUtil.curve_env
    ) $ \(intervals, Typecheck.DefaultReal time_, maybe_transition, curve)
            args -> do
        start <- Args.real_start args
        -- Adjust time per note based on the available duration.
        -- Since transitions can start at 0 and end at the end, I'm dividing
        -- the duration into intervals-1 parts.
        let len = NonEmpty.length intervals - 1
        end <- get_end start
            (BaseTypes.multiply_duration time_ (fromIntegral len)) args
        let time = (end - start) / fromIntegral len
        pitch <- Call.get_pitch start
        srate <- Call.get_srate
        (intervals, control) <- parse intervals
        let transition = fromMaybe time maybe_transition
        let sig = jaru curve srate start time transition $
                NonEmpty.toList intervals ++ if append_zero then [0] else []
        return $ PSignal.apply_control control (Score.untyped sig) $
            PSignal.signal [(start, pitch)]
    where
    parse intervals
        | all (==control) controls = return (xs, control)
        | otherwise = Derive.throw "all intervals must have the same type"
        where
        (xs, control :| controls) = NonEmpty.unzip $ NonEmpty.map
            (Controls.transpose_control . Typecheck.default_diatonic)
            intervals

jaru :: ControlUtil.Curve -> RealTime -> RealTime -> RealTime -> RealTime
    -> [Signal.Y] -> Signal.Control
jaru curve srate start time transition intervals =
    SignalTransform.smooth curve srate (-transition) $
        Signal.signal (zip (Seq.range_ start time) intervals)

-- * middle

c_flat :: Derive.Generator Derive.Pitch
c_flat = generator1 "flat" mempty "Emit a flat pitch."
    $ Sig.call (Sig.defaulted "pitch" Nothing
        "Emit this pitch, or continue the previous pitch if not given.")
    $ \maybe_pitch args -> do
        start <- Args.real_start args
        end <- Args.real_end args
        pitch <- case maybe_pitch of
            Nothing -> prev_pitch start args
            Just transpose -> do
                pitch <- Call.get_pitch start
                return $ PitchUtil.resolve_pitch_transpose pitch transpose
        return $ PSignal.signal [(start, pitch)]
            <> PSignal.signal [(end, pitch)]

-- ** kampita

data KampitaArgs =
    -- | Both interval arguments are hardcoded.
    Kampita0 !Signal.Y !Signal.Y
    -- | The starting pitch is hardcoded.
    | Kampita1 !Signal.Y
    -- | Both arguments must be provided.
    | Kampita2
    deriving (Show)

c_kampita:: Derive.Doc -> KampitaArgs -> Maybe Trill.Direction
    -> Derive.Generator Derive.Pitch
c_kampita doc kam_args end_dir = generator1 "kam" mempty
    ("This is a kind of trill, but its interval defaults to NNs,\
    \ and transitions between the notes are smooth.  It's intended for\
    \ the vocal microtonal trills common in Carnatic music."
    <> if doc == "" then "" else "\n" <> doc)
    $ Sig.call ((,,,)
    <$> kampita_pitch_args kam_args
    <*> Sig.defaulted "speed" (Sig.typed_control "kam-speed" 6 Score.Real)
        "Alternate pitches at this speed."
    <*> kampita_env <*> ControlUtil.curve_env
    ) $ \(pitches, speed, (transition, hold, lilt, adjust), curve) args -> do
        (pitches, control) <- resolve_pitches kam_args pitches
        start <- Args.real_start args
        let even = end_wants_even_transitions start pitches end_dir
        transpose <- kampita_transpose curve even adjust pitches speed
            transition hold lilt (Args.range args)
        kampita start args control transpose

c_nkampita :: Derive.Doc -> KampitaArgs -> Maybe Trill.Direction
    -> Derive.Generator Derive.Pitch
c_nkampita doc kam_args end_dir = generator1 "nkam" mempty
    ("`kam` with a set number of cycles. The speed adjusts to fit the cycles in\
    \ before the next event."
    <> if doc == "" then "" else "\n" <> doc)
    $ Sig.call ((,,,)
    <$> (Typecheck.positive <$> Sig.defaulted "cycles" 1 "Number of cycles.")
    <*> kampita_pitch_args kam_args
    <*> kampita_env <*> ControlUtil.curve_env
    ) $ \(cycles, pitches, (transition, hold, lilt, adjust), curve) args -> do
        (pitches, control) <- resolve_pitches kam_args pitches
        (start, end) <- Args.real_range_or_next args
        let even = end_wants_even_transitions start pitches end_dir
        -- 1 cycle means a complete cycle, which is 3 transitions, but
        -- 'end_dir' may reduce the number of transitions, to a minimum of 2,
        -- which winds up sounding like a single transition: [0, 1].
        let num_transitions = cycles * 2 + if even == Just True then 0 else 1
        let speed = BaseTypes.constant_control $
                (num_transitions - 1) / RealTime.to_seconds (end - start)
        transpose <- kampita_transpose curve even adjust pitches speed
            transition hold lilt (Args.range args)
        kampita start args control transpose

-- ** implementation

resolve_pitches :: KampitaArgs -> (BaseTypes.ControlRef, BaseTypes.ControlRef)
    -> Derive.Deriver ((Typecheck.Function, Typecheck.Function), Score.Control)
resolve_pitches kam_args (pitch1, pitch2) = do
    (pitch1, control1) <- Call.to_transpose_function Typecheck.Nn pitch1
    (pitch2, control2) <- Call.to_transpose_function Typecheck.Nn pitch2
    let two_pitches = case kam_args of
            Kampita2 -> False
            _ -> True
    when (two_pitches && control1 /= control2) $ Derive.throw $
        "pitch1 and pitch2 signals should have the same type: "
        <> pretty control1 <> " /= " <> pretty control2
    return ((pitch1, pitch2), control1)

kampita_pitch_args :: KampitaArgs
    -> Sig.Parser (BaseTypes.ControlRef, BaseTypes.ControlRef)
kampita_pitch_args kam_args = case kam_args of
    Kampita0 p1 p2 -> (,) <$> pure (control p1) <*> pure (control p2)
    Kampita1 p1 -> (,) <$> pure (control p1)
        <*> Sig.defaulted "neighbor" (sig "kam-neighbor" 1)
            "Alternate with a pitch at this interval."
    Kampita2 -> (,)
        <$> Sig.defaulted "pitch1" (sig "kam-pitch1" 0) "First interval."
        <*> Sig.defaulted "pitch2" (sig "kam-pitch2" 1) "Second interval."
    where
    control val =
        BaseTypes.ControlSignal $ Score.untyped (Signal.constant val)
    sig name deflt = Sig.typed_control name deflt Score.Nn

kampita_env :: Sig.Parser (RealTime, BaseTypes.Duration, Double, Trill.Adjust)
kampita_env = (,,,)
    <$> Sig.defaulted_env "transition" Sig.Both default_transition_
        "Time for each slide."
    <*> Trill.hold_env <*> lilt_env <*> Trill.adjust_env
    where
    lilt_env :: Sig.Parser Double
    lilt_env = Sig.environ "lilt" Sig.Prefixed 0
        "Lilt is a horizontal bias to the vibrato. A lilt of 1 would place\
        \ each neighbor on top of the following unison, while -1 would place\
        \ it on the previous one. So it should range from -1 < lilt < 1."

default_transition :: Typecheck.DefaultReal
default_transition = Typecheck.real default_transition_

default_transition_ :: RealTime
default_transition_ = 0.12

kampita :: RealTime -> Derive.PitchArgs -> Score.Control -> Signal.Control
    -> Derive.Deriver PSignal.PSignal
kampita start args control transpose = do
    pitch <- prev_pitch start args
    return $ PSignal.apply_control control
        (Score.untyped transpose) $ PSignal.signal [(start, pitch)]

-- | You don't think there are too many arguments, do you?
kampita_transpose :: ControlUtil.Curve -> Maybe Bool -> Trill.Adjust
    -> (Typecheck.Function, Typecheck.Function) -> BaseTypes.ControlRef
    -> RealTime -> BaseTypes.Duration -> Double -> (ScoreTime, ScoreTime)
    -> Derive.Deriver Signal.Control
kampita_transpose curve even adjust (pitch1, pitch2) speed transition hold lilt
        (start, end) = do
    hold <- Call.score_duration start hold
    smooth_trill curve (-transition) pitch1 pitch2
        =<< trill_transitions even adjust lilt hold speed (start, end)

smooth_trill :: ControlUtil.Curve -> RealTime -> Typecheck.Function
    -> Typecheck.Function -> [RealTime] -> Derive.Deriver Signal.Control
smooth_trill curve time val1 val2 transitions = do
    srate <- Call.get_srate
    return $ SignalTransform.smooth curve srate time $
        trill_from_transitions val1 val2 transitions

-- | Make a trill signal from a list of transition times.
trill_from_transitions :: Typecheck.Function -> Typecheck.Function
    -> [RealTime] -> Signal.Control
trill_from_transitions val1 val2 transitions = Signal.signal
    [(x, sig x) | (x, sig) <- zip transitions (cycle [val1, val2])]

trill_transitions :: Maybe Bool -> Trill.Adjust -> Double -> ScoreTime
    -> BaseTypes.ControlRef -> (ScoreTime, ScoreTime)
    -> Derive.Deriver [RealTime]
trill_transitions = Trill.adjusted_transitions include_end
    where
    -- Trills usually omit the transition that coincides with the end because
    -- that would create a zero duration note.  But these trills are smoothed
    -- and thus will still have a segment leading to the cut-off transition.
    include_end = True

end_wants_even_transitions :: RealTime
    -> (Typecheck.Function, Typecheck.Function)
    -> Maybe Trill.Direction -> Maybe Bool
end_wants_even_transitions start (pitch1, pitch2) dir = case dir of
    Nothing -> Nothing
    Just Trill.Low -> Just (not pitch1_low)
    Just Trill.High -> Just pitch1_low
    where pitch1_low = pitch1 start <= pitch2 start

-- * end

c_flat_end :: Derive.Generator Derive.Pitch
c_flat_end = generator1 "flat-end" mempty
    "Emit a flat pitch for the given duration."
    $ Sig.call ((,)
    <$> Sig.defaulted "pitch" Nothing
        "Emit this pitch, or continue the previous pitch if not given."
    <*> Sig.defaulted "time" (Typecheck.real 0.15)
        "Pitch lasts for this duration."
    ) $ \(maybe_pitch, Typecheck.DefaultReal time) args -> do
        (start, end) <- Args.real_range args
        start <- align_to_end start end time
        pitch <- optional_pitch maybe_pitch <$> prev_pitch start args
        return $ PSignal.signal [(start, pitch), (end, pitch)]

c_to :: Fade -> Derive.Generator Derive.Pitch
c_to fade = generator1 "to" mempty "Go to a pitch, and possibly fade out."
    $ Sig.call ((,)
    <$> Sig.required "pitch" "Go to this pitch or interval."
    <*> Sig.defaulted "transition" default_transition
        "Time to destination pitch."
    ) $ \(to_pitch, Typecheck.DefaultReal time) args -> do
        (start, end) <- Args.real_range args
        start <- align_to_end start end time
        pitch <- prev_pitch start args
        case fade of
            Fade -> ControlUtil.multiply_dyn end
                =<< ControlUtil.make_segment id start 1 end 0
            NoFade -> return ()
        PitchUtil.make_segment id start pitch end
            (PitchUtil.resolve_pitch_transpose pitch to_pitch)

c_fade :: Bool -> Derive.Generator Derive.Pitch
c_fade fade_in = generator1 "fade" mempty
    ((if fade_in then "Fade in." else "Fade out.")
    <> " This will overlap with the pitch part of the "
    <> (if fade_in then "next" else "previous") <> " call."
    ) $ Sig.call (Sig.defaulted "time" (Typecheck.real 0.15) "Time to fade.")
    $ \(Typecheck.DefaultReal time) args -> do
        (start, end) <- Args.real_range args
        (start, end) <- if fade_in
            then (,) <$> return start <*> get_end start time args
            else (,) <$> align_to_end start end time <*> return end
        ControlUtil.multiply_dyn end
            =<< ControlUtil.make_segment id start 1 end 0
        return mempty

-- | Subtract the duration from the given end time, but don't go past the
-- start.
align_to_end :: RealTime -> RealTime -> BaseTypes.Duration
    -> Derive.Deriver RealTime
align_to_end start end dur = do
    dur <- min (end - start) <$> Call.real_duration start dur
    return $ end - dur

-- * misc

c_sahitya :: Derive.Taggable a => Derive.Transformer a
c_sahitya = Derive.transformer module_ "sahitya" mempty
    "Ignore the transformed deriver. Put this on a track to ignore its\
    \ contents, and put in sahitya."
    $ Sig.call0t $ \_args _deriver -> return Stream.empty

-- * util

-- | This defaults to the note's base pitch, in case this call is the first
-- one.  Also, the end call is called before the middle calls to find out how
-- long it is.
prev_pitch :: RealTime -> Derive.PitchArgs -> Derive.Deriver PSignal.Pitch
prev_pitch start args = case Args.prev_pitch args of
    Nothing -> Call.get_pitch start
    Just (_, pitch) -> return pitch

resolve_pitch :: Derive.PitchArgs -> PSignal.Pitch
    -> Maybe PitchUtil.PitchOrTranspose -> PSignal.Pitch
resolve_pitch args this_pitch maybe_pitch = case maybe_pitch of
    Nothing -> case Args.prev_pitch args of
        Nothing -> this_pitch
        Just (_, prev) -> prev
    Just (Left pitch) -> pitch
    Just (Right transpose) -> Pitches.transpose transpose this_pitch

-- | A number of calls take an optional pitch, and default to either
-- the current or previous pitch.
optional_pitch :: Maybe PitchUtil.PitchOrTranspose -> PSignal.Pitch
    -> PSignal.Pitch
optional_pitch maybe_pitch current_pitch =
    maybe current_pitch (PitchUtil.resolve_pitch_transpose current_pitch)
        maybe_pitch

generator1 :: Derive.CallName -> Tags.Tags -> Derive.Doc
    -> Derive.WithArgDoc (Derive.PassedArgs d -> Derive.Deriver d)
    -> Derive.Call (Derive.GeneratorFunc d)
generator1 = Derive.generator1 module_
