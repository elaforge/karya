-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.India.Gamakam2 where
import qualified Control.Applicative as Applicative
import qualified Control.Monad.State.Strict as Monad.State
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Text as Text

import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Ui.Event as Event
import qualified Derive.Args as Args
import qualified Derive.Call.ControlUtil as ControlUtil
import qualified Derive.Call.India.Gamakam as Gamakam
import qualified Derive.Call.Module as Module
import qualified Derive.Call.PitchUtil as PitchUtil
import qualified Derive.Call.SignalTransform as SignalTransform
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Trill as Trill
import qualified Derive.Call.Util as Util
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Eval as Eval
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Types


module_ :: Module.Module
module_ = "india" <> "gamakam2"

note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.call_maps [("!", c_sequence)] [("!", c_sequence_transform)]

pitch_calls :: Derive.CallMaps Derive.Pitch
pitch_calls = Derive.generator_call_map $
    begin_calls ++ middle_calls ++ end_calls

begin_calls :: [(TrackLang.CallId, Derive.Generator Derive.Pitch)]
begin_calls =
    [ ("from-p", c_from False False)
    , ("from-p<", c_from False True)
    , ("from", c_from True False)
    , ("from<", c_from True True)
    , ("jaru", c_jaru)
    ]

middle_calls :: [(TrackLang.CallId, Derive.Generator Derive.Pitch)]
middle_calls = ("hold", c_hold)
    : kampita_variations "kam" (c_kampita False)
    ++ kampita_variations "kam2" (c_kampita True)
    ++ kampita_variations "nkam" (c_nkampita False)
    ++ kampita_variations "nkam2" (c_nkampita True)

end_calls :: [(TrackLang.CallId, Derive.Generator Derive.Pitch)]
end_calls =
    [ ("to", c_to False)
    , ("to>", c_to True)
    ]

-- avoid below: ! p< -1 ; - ; p
-- avoid below, kam: ! p< -1 ; nk 2 ; p>
--
-- kharaharapriya avarohana:
-- sa: j 0 -2
-- ni: ; k_^ -1 ;
-- da: -
-- pa: -
-- ma: j 1 ; - ; j 1
-- ga: p -1 ; - ; p
-- ri: p
-- sa: -

-- | I don't want to take up short names for the whole track scope, but within
-- a sequence call it seems reasonable.  In addition, I know if it's a begin or
-- end call, and use the same name for logically similar things.
begin_aliases :: Map.Map TrackLang.CallId TrackLang.CallId
begin_aliases = Map.fromList
    [ ("p", "from-p")
    , ("p<", "from-p<")
    , ("-^", "from")
    , ("-^<", "from<")
    , ("j", "jaru")
    ]

middle_aliases :: Map.Map TrackLang.CallId TrackLang.CallId
middle_aliases = Map.fromList $ ("-", "hold")
    : alias_prefix "k" "kam" (map fst middle_calls)
    ++ alias_prefix "nk" "nkam" (map fst middle_calls)

end_aliases :: Map.Map TrackLang.CallId TrackLang.CallId
end_aliases = Map.fromList
    [ ("p", "to")
    , ("p>", "to>")
    ]

alias_prefix :: Text -> Text -> [TrackLang.CallId]
    -> [(TrackLang.CallId, TrackLang.CallId)]
alias_prefix from to calls = do
    TrackLang.Symbol call <- calls
    Just rest <- [Text.stripPrefix to call]
    return (TrackLang.Symbol (from <> rest), TrackLang.Symbol call)

kampita_variations :: Text -> (Maybe Trill.Direction -> call)
    -> [(TrackLang.CallId, call)]
kampita_variations name call =
    [ (TrackLang.Symbol $ name <> Trill.direction_affix end, call end)
    | end <- dirs
    ]
    where dirs = [Nothing, Just Trill.Low, Just Trill.High]

-- * sequence

c_sequence :: Derive.Generator Derive.Note
c_sequence = Derive.make_call module_ "sequence" mempty sequence_doc
    $ Sig.parsed_manually "Expressions separated by `;`."
    $ Sub.inverting $ \args -> with_sequence args (Util.note_here args)

c_sequence_transform :: Derive.Transformer Derive.Note
c_sequence_transform = Derive.transformer module_ "sequence" mempty
    sequence_doc $ Sig.parsed_manually "Expressions separated by `;`."
        with_sequence

sequence_doc :: Text
sequence_doc = "Sequence several pitch calls. Calls are divided into\
    \ begin ; middle1 ; middle2; ... ; end calls. Calls are pitch generators,\
    \ and are sequenced such that the middle calls stretch based on the\
    \ duration of the note. There are short aliases for calls that are\
    \ designed for sequencing."
    <> "\nBegin aliases: " <> prettyt begin_aliases
    <> "\nMiddle aliases: " <> prettyt middle_aliases
    <> "\nEnd aliases: " <> prettyt end_aliases

with_sequence :: Derive.PassedArgs Score.Event -> Derive.Deriver a
    -> Derive.Deriver a
with_sequence args deriver = do
    let (begin, middle, end) = parse_sequence (Derive.passed_vals args)
    (pitch, mods) <- sequence_calls (pitch_call_info (Args.info args))
        (Args.range args) begin middle end
    Derive.with_pitch Nothing pitch $ Derive.with_control_mods mods deriver

pitch_call_info :: Derive.CallInfo Score.Event -> Derive.CallInfo Derive.Pitch
pitch_call_info cinfo = cinfo
    { Derive.info_prev_val = Score.event_pitch <$> Derive.info_prev_val cinfo }

type Signals = (PitchSignal.Signal, [Derive.ControlMod])

-- Awkward things:
-- - I set call duration with the event (start, dur) instead of warp, so I have
-- to modify the event rather than use Derive.place.
-- - I deduce the duration of a call by seeing what signal it returns rather
-- that having some special call mode, so these calls need to emit samples at
-- their start and end.  Actually, just end for begin and middle calls, and
-- start for end calls.
-- - Prev val is manually passed from one call to the next, so if I don't have
-- TrackEval.derive_track handling that for me, I have to do it myself, and
-- be careful to not pass it for the speculative evaluation of the end call.

{- | I assume that start and end calls have a fixed duration and don't stretch
    to fill the given space, though they will shrink if necessary.  So
    I evaluate start and end to get their durations, and fit the middle calls
    into the remaining space, evenly divided.

    Actually, there's a circular problem in that I don't know how long the
    middle section can be until I know how long the end call is, but the end
    call likely relies on 'Derive.info_prev_val', so it has to be evaluated
    after the middle.  is going to consume.  So I evaluate the end twice, once
    before the middle to find out its length, and again after evaluating the
    middle.

    I considered a specially calling mode where calls could return their
    desired duration rather than a signal, but it seems much simpler to just
    have them do that by returning the signal itself and measuring that.

    A possibly useful extension would be to allow middle calls to be shorter
    than their allotted time, for instance a trill might want to complete
    a cycle and allow a hold call to take up remaining space.  I could put the
    requested time in @Derive.real 1@ and the hard limit it @Args.next@, but
    that wouldn't allow time before the call to stretch, only time after.
    But I could indicate stretchiness with a special tag on the call.
-}
sequence_calls :: Derive.CallInfo Derive.Pitch -> (ScoreTime, ScoreTime)
    -> Maybe Expr -> [Expr] -> Maybe Expr -> Derive.Deriver Signals
sequence_calls cinfo (start, end) maybe_begin middles maybe_end =
    fmap fst $ flip Monad.State.runStateT cinfo $ do
        (begin_pitch, begin_mods) <- maybe_eval start end maybe_begin
        middle_start <- lift $ signal_end start begin_pitch
        -- This is a test eval of 'end', just to see how long it is.  The
        -- middle isn't evaluated yet, so it doesn't have the right
        -- info_prev_val.
        (test_end_pitch, _) <-
            detached $ maybe_eval middle_start end maybe_end
        end_start <- lift $ signal_start end test_end_pitch
        (middle_pitch, middle_mods) <-
            sequence_middles middle_start end_start middles
        (end_pitch, end_mods) <- maybe_eval end_start end maybe_end
        return (begin_pitch <> middle_pitch <> end_pitch,
            begin_mods <> middle_mods <> end_mods)
    where
    maybe_eval start end = maybe (return (mempty, mempty)) (eval start end)

-- | I need to thread Derive.info_prev_val from each call in the sequence.
type SequenceM =
    Monad.State.StateT (Derive.CallInfo Derive.Pitch) Derive.Deriver

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
    (pitch, mods) <- eval start (start + dur) expr
    sig_end <- lift $ signal_end start pitch
    (pitch_rest, mods_rest) <- sequence_middles sig_end end exprs
    return (pitch <> pitch_rest, mods <> mods_rest)

signal_start :: ScoreTime -> PitchSignal.Signal -> Derive.Deriver ScoreTime
signal_start deflt =
    maybe (return deflt) (Derive.score . fst) .  PitchSignal.head

signal_end :: ScoreTime -> PitchSignal.Signal -> Derive.Deriver ScoreTime
signal_end deflt = maybe (return deflt) (Derive.score . fst) .  PitchSignal.last

eval :: ScoreTime -> ScoreTime -> Expr -> SequenceM Signals
eval start end expr = do
    cinfo <- Monad.State.get
    (result, cmods) <- lift $ with_empty_collect $
        eval_expr (place_event start (end - start) cinfo) expr
    let (chunks, logs) = LEvent.partition result
    mapM_ Log.write logs
    let signal = mconcat chunks
    unless (PitchSignal.null signal) $
        Monad.State.put $ cinfo { Derive.info_prev_val = Just signal }
    return (signal, cmods)

place_event :: ScoreTime -> ScoreTime -> Derive.CallInfo d -> Derive.CallInfo d
place_event start dur cinfo = cinfo
    { Derive.info_event = Event.place start dur (Derive.info_event cinfo) }

eval_expr :: Derive.Callable d => Derive.CallInfo d -> Expr
    -> Derive.LogsDeriver d
eval_expr cinfo (QuotedExpr expr) = Eval.apply_toplevel cinfo expr
eval_expr cinfo (EvaluatedExpr call_id args) =
    Eval.reapply_generator cinfo call_id args ""
    -- TODO I shouldn't need inversion, so I shouldn't need the expr.
    -- Can I put in Nothing, so I can get a nice error if someone tries?

with_empty_collect :: Derive.Deriver a
    -> Derive.Deriver (a, [Derive.ControlMod])
with_empty_collect = fmap (second Derive.collect_control_mods)
    . Internal.local_collect

-- ** parse

data Expr =
    -- | This is a call which was embedded in the argument list of the sequence
    -- call, so its arguments have already been evaluated.
    EvaluatedExpr TrackLang.CallId [TrackLang.Val]
    -- | A call and its arguments can be protected from evaluation by quoting
    -- it.  This is also necessary to use a transformer, since @;@ has higher
    -- precedence than @|@ (actually it's just a value, not an operator).
    | QuotedExpr !TrackLang.Expr
    deriving Show

instance Pretty.Pretty Expr where
    format (EvaluatedExpr call_id vals) =
        ("(" <> Pretty.format call_id) Pretty.<+> (Pretty.format vals <> ")")
    format (QuotedExpr quoted) = Pretty.format quoted

-- | Parse the sequence call's arguments and substitute aliases.  If there is
-- no middle, @-@ will be added.  This is so that if there is just an end call,
-- there will still be a signal from the beginning of the note.
--
-- > begin ; middle1 ; middle2; ...; end
-- > begin; end
-- > ; middle ;
parse_sequence :: [TrackLang.Val] -> (Maybe Expr, [Expr], Maybe Expr)
parse_sequence exprs = postproc $
    case Seq.map_tail (drop 1) $ Seq.split_with is_separator exprs of
        [] -> (Nothing, [], Nothing)
        begin : rest -> case reverse rest of
            end : middle -> (Just begin, reverse middle, Just end)
            [] -> (Just begin, [], Nothing)
    where
    postproc (begin, middles, end) =
        ( fmap (substitute_aliases begin_aliases) . to_expr =<< begin
        , map (substitute_aliases middle_aliases) $
            add_hold $ mapMaybe to_expr middles
        , fmap (substitute_aliases end_aliases) . to_expr =<< end
        )
    add_hold [] = [EvaluatedExpr "-" []]
    add_hold xs = xs
    to_expr [] = Nothing
    to_expr (call : args) = Just $ case call of
        TrackLang.VQuoted (TrackLang.Quoted expr) -> QuotedExpr expr
        TrackLang.VSymbol sym -> EvaluatedExpr sym args
        _ -> EvaluatedExpr (TrackLang.Symbol (ShowVal.show_val call)) args
    is_separator TrackLang.VSeparator = True
    is_separator _ = False

substitute_aliases :: Map.Map TrackLang.CallId TrackLang.CallId -> Expr -> Expr
substitute_aliases aliases expr = case expr of
    EvaluatedExpr call_id vals -> EvaluatedExpr (subst call_id) vals
    QuotedExpr expr -> QuotedExpr $ TrackLang.map_generator subst expr
    where subst call_id = Map.findWithDefault call_id call_id aliases

-- * start

c_from :: Bool -> Bool -> Derive.Generator Derive.Pitch
c_from from_prev fade_in = generator1 "from" mempty
    (if from_prev
        then "Come for the previous pitch, and possibly fade in."
        else "Come from a pitch, and possibly fade in.")
    $ Sig.call ((,,)
    <$> (if from_prev then Applicative.pure Nothing
        else Sig.defaulted "from" Nothing
            "Come from this pitch, or the previous one.")
    <*> Sig.defaulted "time" (TrackLang.real 0.25) "Time to destination pitch."
    <*> Sig.defaulted "to" Nothing "Go to this pitch, or the current one."
    ) $ \(from_pitch, TrackLang.DefaultReal time, maybe_to_pitch) args -> do
        start <- Args.real_start args
        end <- get_end start time args
        current_pitch <- Util.get_pitch start
        let to_pitch = maybe current_pitch
                (PitchUtil.resolve_pitch_transpose current_pitch)
                maybe_to_pitch
        let from = resolve_pitch args to_pitch from_pitch
        when fade_in $
            ControlUtil.multiply_dyn end
                =<< ControlUtil.make_signal id start 0 end 1
        PitchUtil.make_interpolator id True start from end to_pitch

-- | Get the end time, given a start and a duration.  Don't go beyond the
-- maximum, which is the event's duration, if given explicitly, or the next
-- event if it's 0.
get_end :: RealTime -> TrackLang.Duration -> Derive.PassedArgs a
    -> Derive.Deriver RealTime
get_end start dur args = do
    time_end <- (start +) <$> Util.real_duration start dur
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

c_jaru :: Derive.Generator Derive.Pitch
c_jaru = generator1 "jaru" mempty
    "This is a series of grace notes whose pitches are relative to the\
    \ base pitch."
    $ Sig.call ((,,)
    <$> Sig.many1 "interval" "Intervals from base pitch."
    <*> Sig.environ "time" Sig.Both jaru_time_default "Time for each note."
    -- TODO This should also be a Duration
    <*> Sig.environ "transition" Sig.Both Nothing
        "Time for each slide, defaults to `time`."
    ) $ \(intervals, TrackLang.DefaultReal time_, maybe_transition) args -> do
        start <- Args.real_start args
        -- Adjust time per note based on the available duration.
        -- Since transitions can start at 0 and end at the end, I'm dividing
        -- the duration into intervals-1 parts.
        let len = NonEmpty.length intervals - 1
        end <- get_end start
            (TrackLang.multiply_duration time_ len) args
        let time = (end - start) / fromIntegral len
        pitch <- Util.get_pitch start
        srate <- Util.get_srate
        (intervals, control) <- parse intervals
        let transition = fromMaybe time maybe_transition
        let sig = jaru srate start time transition (NonEmpty.toList intervals)
        return $ PitchSignal.apply_control control (Score.untyped sig) $
            PitchSignal.signal [(start, pitch)]
    where
    parse intervals
        | all (==control) controls = return (xs, control)
        | otherwise = Derive.throw "all intervals must have the same type"
        where
        (xs, control :| controls) = NonEmpty.unzip $ NonEmpty.map
            (Controls.transpose_control . TrackLang.default_diatonic)
            intervals
    jaru_time_default = TrackLang.real 0.15

jaru :: RealTime -> RealTime -> RealTime -> RealTime -> [Signal.Y]
    -> Signal.Control
jaru srate start time transition intervals =
    -- TODO use segments from PitchUtil?  That way I can use the
    -- interpolate-type.  Of course, 'smooth' takes a function too...
    SignalTransform.smooth id srate (-transition) $
        Signal.signal (zip (Seq.range_ start time) intervals)

-- * middle

c_hold :: Derive.Generator Derive.Pitch
c_hold = generator1 "hold" mempty "Emit a flat pitch."
    $ Sig.call (Sig.defaulted "pitch" Nothing
        "Emit this pitch, or continue the previous pitch if not given.")
    $ \maybe_pitch args -> do
        start <- Args.real_start args
        end <- Args.real_end args
        pitch <- case maybe_pitch of
            Nothing -> prev_pitch start args
            Just transpose -> do
                pitch <- Util.get_pitch start
                return $ PitchUtil.resolve_pitch_transpose pitch transpose
        return $ PitchSignal.signal [(start, pitch)]
            <> PitchSignal.signal [(end, pitch)]

-- ** kampita

c_kampita:: Bool -> Maybe Trill.Direction -> Derive.Generator Derive.Pitch
c_kampita two_pitches end_dir = generator1 "kam" mempty
    "This is a kind of trill, but its interval defaults to NNs,\
    \ and transitions between the notes are smooth.  It's intended for\
    \ the vocal microtonal trills common in Carnatic music."
    $ Sig.call ((,,)
    <$> kampita_pitch_args two_pitches
    <*> Sig.defaulted "speed" (Sig.typed_control "kam-speed" 6 Score.Real)
        "Alternate pitches at this speed."
    <*> kampita_env
    ) $ \(pitches, speed, (transition, hold, lilt, adjust)) args -> do
        (pitches, control) <- resolve_pitches two_pitches pitches
        start <- Args.real_start args
        let even = end_wants_even_transitions start pitches end_dir
        transpose <- kampita_transpose even adjust pitches speed
            transition hold lilt (Args.range args)
        kampita start args control transpose

c_nkampita :: Bool -> Maybe Trill.Direction -> Derive.Generator Derive.Pitch
c_nkampita two_pitches end_dir = generator1 "nkam" mempty
    "`kam` with a set number of cycles. The speed adjusts to fit the cycles in\
    \ before the next event."
    $ Sig.call ((,,)
    <$> Sig.defaulted "cycles" (TrackLang.Positive 1) "Number of cycles."
    <*> kampita_pitch_args two_pitches
    <*> kampita_env
    ) $ \(TrackLang.Positive cycles, pitches, (transition, hold, lilt, adjust))
            args -> do
        (pitches, control) <- resolve_pitches two_pitches pitches
        (start, end) <- Args.real_range_or_next args
        let even = end_wants_even_transitions start pitches end_dir
        -- 1 cycle means a complete cycle, which is 3 transitions, but
        -- 'end_dir' may reduce the number of transitions, to a minimum of 2,
        -- which winds up sounding like a single transition: [0, 1].
        let num_transitions = cycles * 2 + if even == Just True then 0 else 1
        let speed = TrackLang.constant_control $
                (num_transitions - 1) / RealTime.to_seconds (end - start)
        transpose <- kampita_transpose even adjust pitches speed
            transition hold lilt (Args.range args)
        kampita start args control transpose

-- ** implementation

resolve_pitches :: Bool -> (TrackLang.ValControl, TrackLang.ValControl)
    -> Derive.Deriver ((Util.Function, Util.Function), Score.Control)
resolve_pitches two_pitches (pitch1, pitch2) = do
    (pitch1, control1) <- Util.to_transpose_function Util.Nn pitch1
    (pitch2, control2) <- Util.to_transpose_function Util.Nn pitch2
    when (two_pitches && control1 /= control2) $ Derive.throw $
        "pitch1 and pitch2 signals should have the same type: "
        <> pretty control1 <> " /= " <> pretty control2
    return ((pitch1, pitch2), control1)

kampita_pitch_args :: Bool
    -> Sig.Parser (TrackLang.ValControl, TrackLang.ValControl)
kampita_pitch_args two_pitches
    | two_pitches = (,)
        <$> Sig.defaulted "pitch1" (sig "kam-pitch1" 0) "First interval."
        <*> Sig.defaulted "pitch2" (sig "kam-pitch2" 1) "Second interval."
    | otherwise = (,)
        <$> Applicative.pure (TrackLang.constant_control 0)
        <*> Sig.defaulted "neighbor" (sig "kam-neighbor" 1)
            "Alternate with a pitch at this interval."
    where sig name deflt = Sig.typed_control name deflt Score.Nn

kampita_env :: Sig.Parser (RealTime, TrackLang.Duration, Double, Trill.Adjust)
kampita_env = (,,,)
    <$> Sig.defaulted_env "transition" Sig.Both 0.08 "Time for each slide."
    <*> Trill.hold_env <*> lilt_env <*> Trill.adjust_env
    where
    lilt_env :: Sig.Parser Double
    lilt_env = Sig.environ "lilt" Sig.Prefixed 0
        "Lilt is a horizontal bias to the vibrato. A lilt of 1 would place\
        \ each neighbor on top of the following unison, while -1 would place\
        \ it on the previous one. So it should range from -1 < lilt < 1."

kampita :: RealTime -> Derive.PitchArgs -> Score.Control -> Signal.Control
    -> Derive.Deriver PitchSignal.Signal
kampita start args control transpose = do
    pitch <- prev_pitch start args
    return $ PitchSignal.apply_control control
        (Score.untyped transpose) $ PitchSignal.signal [(start, pitch)]

-- | You don't think there are too many arguments, do you?
kampita_transpose :: Maybe Bool -> Trill.Adjust
    -> (Util.Function, Util.Function) -> TrackLang.ValControl -> RealTime
    -> TrackLang.Duration -> Double -> (ScoreTime, ScoreTime)
    -> Derive.Deriver Signal.Control
kampita_transpose even adjust (pitch1, pitch2) speed transition hold lilt
        (start, end) = do
    hold <- Util.score_duration start hold
    Gamakam.smooth_trill (-transition) pitch1 pitch2
        =<< Gamakam.trill_transitions even adjust lilt hold speed (start, end)

end_wants_even_transitions :: RealTime -> (Util.Function, Util.Function)
    -> Maybe Trill.Direction -> Maybe Bool
end_wants_even_transitions start (pitch1, pitch2) dir = case dir of
    Nothing -> Nothing
    Just Trill.Low -> Just (not pitch1_low)
    Just Trill.High -> Just pitch1_low
    where pitch1_low = pitch1 start <= pitch2 start

-- * end

c_to :: Bool -> Derive.Generator Derive.Pitch
c_to fade_out = generator1 "to" mempty "Go to a pitch, and possibly fade out."
    $ Sig.call ((,)
    <$> Sig.required "pitch" "Go to this pitch or interval."
    <*> Sig.defaulted "time" (TrackLang.real 0.25) "Time to destination pitch."
    ) $ \(to_pitch, TrackLang.DefaultReal time) args -> do
        start <- Args.real_start args
        end <- Args.real_end args
        start <- align_to_end start end time
        pitch <- prev_pitch start args
        when fade_out $
            ControlUtil.multiply_dyn end
                =<< ControlUtil.make_signal id start 1 end 0
        PitchUtil.make_interpolator id True start pitch end
            (PitchUtil.resolve_pitch_transpose pitch to_pitch)

align_to_end :: RealTime -> RealTime -> TrackLang.Duration
    -> Derive.Deriver RealTime
align_to_end start end dur = do
    dur <- min (end - start) <$> Util.real_duration start dur
    return $ end - dur

-- * util

-- | This defaults to the note's base pitch, in case this call is the first
-- one.  Also, the end call is called before the middle calls to find out how
-- long it is.
prev_pitch :: RealTime -> Derive.PitchArgs -> Derive.Deriver PitchSignal.Pitch
prev_pitch start args = case Args.prev_pitch args of
    Nothing -> Util.get_pitch start
    Just (_, pitch) -> return pitch

resolve_pitch :: Derive.PitchArgs -> PitchSignal.Pitch
    -> Maybe PitchUtil.PitchOrTranspose -> PitchSignal.Pitch
resolve_pitch args this_pitch maybe_pitch = case maybe_pitch of
    Nothing -> case Args.prev_pitch args of
        Nothing -> this_pitch
        Just (_, prev) -> prev
    Just (Left pitch) -> pitch
    Just (Right transpose) -> Pitches.transpose transpose this_pitch

generator1 :: Text -> Tags.Tags -> Text
    -> Derive.WithArgDoc (Derive.PassedArgs d -> Derive.Deriver d)
    -> Derive.Call (Derive.GeneratorFunc d)
generator1 = Derive.generator1 module_
