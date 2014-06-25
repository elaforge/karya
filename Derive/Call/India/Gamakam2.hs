-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.India.Gamakam2 where
import qualified Control.Applicative as Applicative
import qualified Control.Monad.State.Strict as Monad.State
import qualified Data.Map as Map

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
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Trill as Trill
import qualified Derive.Call.Util as Util
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

import qualified Perform.Signal as Signal
import Types


-- add as pitch calls
-- as note calls it's only one per note, plus placement for the high level
-- syntax

note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.call_maps
    [ ("!", c_sequence)
    ]
    [ ("!", c_sequence_transform)
    ]

pitch_calls :: Derive.CallMaps Derive.Pitch
pitch_calls = Derive.call_maps
    ([ ("from-p", c_from False False)
    , ("from-p<", c_from False True)
    , ("from", c_from True False)
    , ("from<", c_from True True)
    , ("hold", c_hold)
    , ("to", c_to False)
    , ("to>", c_to True)
    ] ++ kampita_variations "k" c_kampita)
    [
    ]

-- avoid below: ! p< -1 ; - ; p
-- avoid below, kam: ! p< -1 ; nk 2 ; p>
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
    ]

middle_aliases :: Map.Map TrackLang.CallId TrackLang.CallId
middle_aliases = Map.fromList [("-", "hold")]

end_aliases :: Map.Map TrackLang.CallId TrackLang.CallId
end_aliases = Map.fromList
    [ ("p", "to")
    , ("p>", "to>")
    ]

module_ :: Module.Module
module_ = "india" <> "gamakam2"

kampita_variations :: Text
    -> (Maybe Trill.Direction -> Maybe Trill.Direction -> call)
    -> [(TrackLang.CallId, call)]
kampita_variations name call =
    [ (TrackLang.Symbol $ affix s <> name <> affix e, call s e)
    | s <- dirs, e <- dirs
    ]
    where
    affix = Trill.direction_affix
    dirs = [Nothing, Just Trill.Low, Just Trill.High]

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
sequence_doc = "Sequence several pitch calls.  TODO"

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
        -- Debug.tracepM "sequence times" (start, middle_start, end_start, end)
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
    -- Debug.tracepM "sequence_middles" (start, end, expr, exprs)
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
    $ Sig.call ((,)
    <$> (if from_prev then Applicative.pure Nothing
        else Sig.defaulted "pitch" Nothing
            "Come from this pitch, or the previous one.")
    <*> Sig.defaulted "time" (TrackLang.real 0.25) "Time to destination pitch."
    ) $ \(from_pitch, TrackLang.DefaultReal time) args -> do
        start <- Args.real_start args
        end <- get_end start time args
        to_pitch <- Util.get_pitch start
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

-- . Attack, e.g. 0 -2 0.  Only at beginning of note.  Doesn't shorten,
--   though it might speed up a bit.  Diatonic.  Jaru.
--   > j) 0 -2, j] -1, sgr

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

c_kampita:: Maybe Trill.Direction -> Maybe Trill.Direction
    -> Derive.Generator Derive.Pitch
c_kampita start_dir end_dir = generator1 "kam" mempty
    "This is a kind of trill, but its interval defaults to NNs,\
    \ and transitions between the notes are smooth.  It's intended for\
    \ the vocal microtonal trills common in Carnatic music."
    $ Sig.call ((,,,,,)
    <$> Sig.defaulted "neighbor" (Sig.typed_control "tr-neighbor" 1 Score.Nn)
        "Alternate with a pitch at this interval."
    <*> speed_arg
    <*> Sig.defaulted_env "transition" Sig.Both 0.08 "Time for each slide."
    <*> Trill.hold_env <*> lilt_env <*> Trill.adjust_env
    ) $ \(neighbor, speed, transition, hold, lilt, adjust) args -> do
        (neighbor, control) <- Util.to_transpose_function Util.Nn neighbor
        transpose <- kampita start_dir end_dir adjust neighbor speed
            transition hold lilt (Args.range args)
        start <- Args.real_start args
        pitch <- Util.get_pitch start
        return $ PitchSignal.apply_control control
            (Score.untyped transpose) $ PitchSignal.signal [(start, pitch)]

-- | You don't think there are too many arguments, do you?
kampita :: Maybe Trill.Direction -> Maybe Trill.Direction -> Trill.Adjust
    -> Util.Function -> TrackLang.ValControl -> RealTime
    -> TrackLang.DefaultReal -> Double -> (ScoreTime, ScoreTime)
    -> Derive.Deriver Signal.Control
kampita start_dir end_dir adjust neighbor speed transition
        (TrackLang.DefaultReal hold) lilt (start, end) = do
    real_start <- Derive.real start
    let ((val1, val2), even_transitions) = Gamakam.convert_directions real_start
            neighbor start_dir end_dir
    hold <- Util.score_duration start hold
    Gamakam.smooth_trill (-transition) val1 val2
        =<< Gamakam.trill_transitions even_transitions adjust lilt hold speed
            (start, end)

-- one that takes n number of cycles, doesn't stretch
-- nkam, align to begin or end

-- avoid is -1 1, or 1 -1

speed_arg :: Sig.Parser TrackLang.ValControl
speed_arg = Sig.defaulted "speed" (Sig.typed_control "tr-speed" 6 Score.Real)
    "Alternate pitches at this speed."

lilt_env :: Sig.Parser Double
lilt_env = Sig.environ "lilt" Sig.Both 0 "Lilt is a horizontal bias to the\
    \ vibrato. A lilt of 1 would place each neighbor on top of the\
    \ following unison, while -1 would place it on the previous one.\
    \ So it should range from -1 < lilt < 1."

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
