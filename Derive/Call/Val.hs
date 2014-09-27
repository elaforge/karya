-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Val where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified System.Random.Mersenne.Pure64 as Pure64

import Util.Control
import qualified Util.Num as Num
import qualified Util.Seq as Seq

import qualified Ui.Event as Event
import qualified Ui.Ruler as Ruler
import qualified Ui.ScoreTime as ScoreTime

import qualified Cmd.Meter as Meter
import qualified Derive.Args as Args
import qualified Derive.Call.ControlUtil as ControlUtil
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Module as Module
import qualified Derive.Call.PitchUtil as PitchUtil
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Environ as Environ
import qualified Derive.Eval as Eval
import qualified Derive.LEvent as LEvent
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Scale as Scale
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import Derive.Sig (defaulted, required)
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import Types


val_calls :: [Derive.LookupCall Derive.ValCall]
val_calls = Derive.call_map
    [ (">", c_next_val)
    , ("<", c_prev_val)
    , ("e", c_env)
    , ("ts", c_timestep)
    , ("ts/", c_timestep_reciprocal)
    , ("1/", c_reciprocal)
    , ("nn", c_nn)
    , ("hz", c_hz)
    -- literals
    , ("list", c_list)
    , ("st", c_scoretime)
    , ("rt", c_realtime)
    , ("pitch", c_pitch)
    , ("#", c_pitch_control)
    -- lookup
    , ("<-#", c_get_pitch)
    -- generate signals
    , ("i>", c_linear_next)
    , ("e>", c_exp_next)
    -- control functions
    , ("cf-rnd", c_cf_rnd const)
    , ("cf-rnd+", c_cf_rnd (+))
    , ("cf-rnd*", c_cf_rnd (*))
    , ("cf-swing", c_cf_swing)
    ]

c_next_val :: Derive.ValCall
c_next_val = val_call "next-val" Tags.next
    "Evaluate the next event. Only works on pitch and control tracks, and\
    \ if the next event doesn't need its previous event."
    $ Sig.call0 $ \args -> do
        event <- Derive.require "no next event" $
            Seq.head (Args.next_events args)
        start <- Derive.real (Event.start event)
        next_val event start (Derive.info_track_type (Derive.passed_info args))

next_val :: Event.Event -> RealTime -> Maybe ParseTitle.Type
    -> Derive.Deriver TrackLang.Val
next_val event start ttype = case ttype of
    Just ParseTitle.ControlTrack -> eval_control start event
    Just ParseTitle.TempoTrack -> eval_control start event
    Just ParseTitle.PitchTrack -> do
        signal <- eval event
        case PitchSignal.at start signal of
            Nothing -> Derive.throw "next pitch event didn't emit a pitch"
            Just pitch -> return $ TrackLang.VPitch pitch
    Just ParseTitle.NoteTrack ->
        Derive.throw "can't get next value for note tracks"
    Nothing -> Derive.throw "no track type"
    where
    eval_control start event = do
        signal <- eval event
        return $ TrackLang.VNum $ Score.untyped $
            Signal.at start (signal :: Signal.Control)
    eval event = mconcat . LEvent.events_of <$>
        (either Derive.throw return =<< Eval.eval_event event)

c_prev_val :: Derive.ValCall
c_prev_val = val_call "prev-val" Tags.prev
    "Return the previous value. Only works on pitch and control tracks."
    $ Sig.call0 $ \args -> do
        start <- Args.real_start args
        case Args.prev_val args of
            Just (Derive.TagControl sig) ->
                return $ TrackLang.num $ Signal.at start sig
            Just (Derive.TagPitch sig) ->
                maybe (Derive.throw "no previous pitch")
                    (return . TrackLang.VPitch) (PitchSignal.at start sig)
            _ -> Derive.throw "no previous value"

c_env :: Derive.ValCall
c_env = val_call "env" mempty
    "Look up the given val in the environ."
    $ Sig.call ((,)
    <$> required "name" "Look up the value of this key."
    <*> defaulted "default" Nothing "If given, this is the default value when\
        \ the key isn't present. If not given, a missing key will throw an\
        \ exception. The presence of a default will also make the lookup\
        \ expect the same type as the default."
    ) $ \(name, maybe_deflt) _args -> case maybe_deflt of
        Nothing -> Derive.get_val name
        Just deflt -> check name deflt =<< Derive.lookup_val name
    where
    check _ deflt Nothing = return deflt
    check name deflt (Just val)
        | TrackLang.type_of val == TrackLang.type_of deflt = return val
        | otherwise = Derive.throw $ "env " <> pretty name
            <> " expected " <> pretty (TrackLang.type_of deflt)
            <> " but got " <> pretty (TrackLang.type_of val)

c_timestep :: Derive.ValCall
c_timestep = val_call "timestep" mempty
    ("Compute the duration of the given RelativeMark timestep at the current\
    \ position. This is for durations, so it only works with RelativeMark."
    ) $ Sig.call ((,)
    <$> required "rank" "Emit a duration of this rank."
    <*> defaulted "multiply" 1 "Multiply duration."
    ) $ \(rank, steps) args ->
        TrackLang.score_time <$>
            Util.meter_duration (Args.start args) rank steps

c_timestep_reciprocal :: Derive.ValCall
c_timestep_reciprocal = Make.modify_vcall c_timestep Module.prelude
    "timestep-reciprocal"
    ("This is the same as `timestep` except it returns the reciprocal. This is\
    \ useful for e.g. trills which take cycles per second rather than duration."
    ) reciprocal
    where
    reciprocal (TrackLang.VNum num) = TrackLang.VNum $ recip <$> num
    reciprocal val = val

c_reciprocal :: Derive.ValCall
c_reciprocal = val_call "reciprocal" mempty
    "Find the reciprocal of a number. Useful for tempo, e.g. set the tempo to\
    \ 1/time." $ Sig.call (required "num" "") $ \num _ ->
        if num == 0 then Derive.throw "1/0"
            else return (1 / num :: Double)

c_nn :: Derive.ValCall
c_nn = val_call "nn" mempty
    "Convert a pitch or hz to a NoteNumber." $ Sig.call (required "val" "") $
    \val _ -> case val of
        Left pitch -> realToFrac <$> Pitches.pitch_nn
            (PitchSignal.coerce (pitch :: PitchSignal.Pitch))
        Right hz -> return (realToFrac (Pitch.hz_to_nn hz) :: Double)

c_hz :: Derive.ValCall
c_hz = val_call "hz" mempty
    "Convert a pitch or NoteNumber to hz." $ Sig.call (required "val" "") $
    \val _ -> case val of
        Left pitch -> Pitch.nn_to_hz <$>
            Pitches.pitch_nn (PitchSignal.coerce (pitch :: PitchSignal.Pitch))
            -- Not transposed because they asked for a specific pitch.
        Right nn -> return (Pitch.nn_to_hz (Pitch.NoteNumber nn) :: Double)

c_list :: Derive.ValCall
c_list = val_call "list" mempty "Create a list." $
    Sig.call (Sig.many "val" "Value.") $ \vals _ ->
        return $ TrackLang.VList vals

c_scoretime :: Derive.ValCall
c_scoretime = val_call "scoretime" mempty
    "Convert a number to ScoreTime. This just changes the type annotation, the\
    \ value remains the same." $
    Sig.call (Sig.required_env "val" Sig.None "") $ \val _ ->
        return $ ScoreTime.double val

c_realtime :: Derive.ValCall
c_realtime = val_call "realtime" mempty
    "Convert a number to RealTime. This just changes the type annotation, the\
    \ value remains the same." $
    Sig.call (Sig.required_env "val" Sig.None "") $ \val _ ->
        return $ RealTime.seconds val

c_pitch :: Derive.ValCall
c_pitch = val_call "pitch" mempty "Create a 'Perform.Pitch.Pitch'."
    $ Sig.call ((,,)
    <$> Sig.defaulted_env "oct" Sig.None (Left 0)
        "Octave, or a pitch name or pitch. If it's a pitch name or pitch, the\
        \ `pc` and `accs` args must be 0."
    <*> Sig.defaulted_env "pc" Sig.None 0 "Pitch class."
    <*> Sig.defaulted_env "accs" Sig.None 0 "Accidentals."
    ) $ \(oct, pc, accs) _ -> make_pitch oct pc accs

make_pitch :: Either Pitch.Octave (Either Text PitchSignal.Pitch)
    -> Pitch.PitchClass -> Pitch.Accidentals -> Derive.Deriver Pitch.Pitch
make_pitch (Left oct) pc accs = return $ Pitch.Pitch oct (Pitch.Degree pc accs)
make_pitch (Right name_pitch) pc accs
    | pc /= 0 || accs /= 0 = Derive.throw $
        "pc and accs args must be 0 when a pitch is given: " ++ show (pc, accs)
    | otherwise = do
        (note, scale) <- case name_pitch of
            Left name -> (,) <$> return (Pitch.Note name) <*> Util.get_scale
            Right pitch -> (,)
                <$> Pitches.pitch_note (PitchSignal.coerce pitch)
                <*> Derive.get_scale (PitchSignal.pitch_scale_id pitch)
        key <- Util.lookup_key
        either (Derive.throw . pretty) return $
            Scale.scale_read scale key note

c_pitch_control :: Derive.ValCall
c_pitch_control = val_call "pitch-control" mempty
    "Create a 'Derive.TrackLang.PitchControl'. For control literals, the\
    \ `#name` syntax suffices, but if you want to give a default pitch,\
    \ you need this call."
    $ Sig.call ((,)
    <$> Sig.required "name" "Name of pitch signal."
    <*> Sig.defaulted "default" Nothing
        "Default pitch, if the signal is not set."
    ) $ \(name, maybe_default) _ -> return $ case maybe_default of
        Nothing -> TrackLang.LiteralControl (Score.control name)
        Just pitch -> TrackLang.DefaultedControl (Score.control name)
            (PitchSignal.constant pitch)

-- * lookup

c_get_pitch :: Derive.ValCall
c_get_pitch = val_call "pitch" mempty
    "Get the current pitch." $ Sig.call (defaulted "control" ""
        "The default pitch if empty, otherwise, get the named pitch.") $
    \control args ->
        Derive.require "pitch" =<< get control =<< Args.real_start args
    where
    get control
        | control == "" = Derive.pitch_at
        | otherwise = Derive.named_pitch_at (Score.control control)

-- * generate signals

c_linear_next :: Derive.ValCall
c_linear_next = val_call "linear-next" mempty
    "Create straight lines between the given breakpoints."
    $ Sig.call breakpoints_arg $ \vals args ->
        c_breakpoints 0 id vals args

c_exp_next :: Derive.ValCall
c_exp_next = val_call "exp-next" mempty
    "Create curved lines between the given breakpoints."
    $ Sig.call ((,)
    <$> defaulted "exp" 2 ControlUtil.exp_doc
    <*> breakpoints_arg
    ) $ \(exp, vals) args ->
        c_breakpoints 1 (ControlUtil.expon exp) vals args

breakpoints_arg :: Sig.Parser (NonEmpty TrackLang.Val)
breakpoints_arg = Sig.many1 "bp" "Breakpoints are distributed evenly between\
    \ the event start and the next event. They can be all numbers, or all\
    \ pitches."

-- ** implementation

c_breakpoints :: Int -> (Double -> Double) -> NonEmpty TrackLang.Val
    -> Derive.PassedArgs a -> Derive.Deriver TrackLang.Val
c_breakpoints argnum f vals args = do
    (start, end) <- Args.real_range_or_next args
    srate <- Util.get_srate
    vals <- num_or_pitch argnum vals
    return $ case vals of
        Left nums -> TrackLang.VControl $ TrackLang.ControlSignal $
            Score.untyped $ ControlUtil.breakpoints srate f start end nums
        Right pitches -> TrackLang.VPitchControl $ TrackLang.ControlSignal $
            PitchUtil.breakpoints srate f start end pitches

-- | Insist that the vals be either all numbers or pitches.
--
-- TODO If 'Sig.Parser' supported Alternative, maybe I could build this as
-- a parser and get both shorter code and documentation.
num_or_pitch :: Int -> NonEmpty TrackLang.Val
    -> Derive.Deriver (Either [Signal.Y] [PitchSignal.Pitch])
num_or_pitch argnum (val :| vals) = case val of
    TrackLang.VNum num -> do
        vals <- mapM (expect tnum) (zip [argnum + 1 ..] vals)
        return $ Left (Score.typed_val num : vals)
    TrackLang.VPitch pitch -> do
        vals <- mapM (expect TrackLang.TPitch) (zip [argnum + 1 ..] vals)
        return $ Right (pitch : vals)
    _ -> type_error argnum "bp" (TrackLang.TEither tnum TrackLang.TPitch) val
    where
    tnum = TrackLang.TNum TrackLang.TUntyped TrackLang.TAny
    expect typ (argnum, val) = maybe (type_error argnum "bp" typ val) return $
        TrackLang.from_val val

type_error :: Int -> Text -> TrackLang.Type -> TrackLang.Val -> Derive.Deriver a
type_error argnum name expected received =
    Derive.throw_error $ Derive.CallError $
        Derive.TypeError (Derive.TypeErrorArg argnum) Derive.Literal name
            expected (Just received)

-- * control function

data Distribution =
    Uniform
    -- | Approximate a bounded normal distribution.
    | Normal
    -- | This is like Normal, but rotated, so the peaks are at the extremities.
    | Bimodal
    deriving (Bounded, Eq, Enum, Show)
instance ShowVal.ShowVal Distribution where
    show_val = TrackLang.default_show_val
instance TrackLang.Typecheck Distribution
instance TrackLang.TypecheckSymbol Distribution

c_cf_rnd :: (Signal.Y -> Signal.Y -> Signal.Y) -> Derive.ValCall
c_cf_rnd combine = val_call "cf-rnd"
    (Tags.control_function <> Tags.random)
    "Randomize a control. Normally it replaces the control of the same name,\
    \ while the `+` and `*` variants add to and multiply with it."
    $ Sig.call ((,,)
    <$> required "low" "Low end of the range."
    <*> required "high" "High end of the range."
    <*> Sig.environ "distribution" Sig.Prefixed Normal
        "Random distribution."
    ) $ \(low, high, distribution) _args -> return $!
        TrackLang.ControlFunction "cf-rnd" $ \control dyn pos ->
            Score.untyped $ combine
                (cf_rnd distribution low high (random_stream (dyn_seed dyn)))
                (dyn_control dyn control pos)

cf_rnd :: Distribution -> Double -> Double -> [Double] -> Double
cf_rnd dist low high rnds = Num.scale low high $ case dist of
    Uniform -> head rnds
    Normal -> normal rnds
    Bimodal
        | v >= 0.5 -> v - 0.5
        | otherwise -> v + 0.5
        where v = normal rnds

-- | Approximation to a normal distribution between 0 and 1, inclusive.
-- This is similar to a gaussian distribution, but is bounded between 0 and 1.
normal :: [Double] -> Double
normal rnds = sum (take 12 rnds) / 12

random_stream :: Double -> [Double]
random_stream =
    List.unfoldr (Just . Pure64.randomDouble) . Pure64.pureMT . floor


-- * cf-swing

c_cf_swing :: Derive.ValCall
c_cf_swing = val_call "cf-swing" Tags.control_function
    ("Add a curved  offset to the control, suitable for swing tempo when added\
    \ to " <> ShowVal.doc_val Controls.start_s <> ". The curve is a sine wave,\
    \ from trough to trough.")
    $ Sig.call ((,)
    <$> defaulted "rank" Meter.Q
        "The time steps are on the beat, and midway between offset by the\
        \ given amount."
    <*> defaulted "amount" (TrackLang.real_control "swing" (1/3))
        "Swing amount, multiplied by the rank duration / 2."
    ) $ \(rank, amount) _args -> return $!
        TrackLang.ControlFunction "cf-swing" (cf_swing_ rank amount)
    where
    cf_swing_ rank amount control dyn pos
        | Just marks <- maybe_marks = Score.untyped $
            dyn_control dyn control pos + RealTime.to_seconds
                (cf_swing (real dyn) (Meter.name_to_rank rank)
                    (to_function dyn 0 amount) marks (score dyn pos))
        | otherwise = Score.untyped 0
        where
        maybe_marks = snd <$> Map.lookup Ruler.meter (TrackLang.dyn_ruler dyn)

cf_swing :: (ScoreTime -> RealTime) -> Ruler.Rank -> Util.Function
    -> Ruler.Marklist -> ScoreTime -> RealTime
cf_swing to_real rank amount marks pos = case marks_around rank marks pos of
    Nothing -> 0
    Just (pre, post) -> (to_real post - to_real pre) / 2
        * RealTime.seconds (amount (to_real pos))
        * swing (Num.normalize pre post pos)

marks_around :: Ruler.Rank -> Ruler.Marklist -> ScoreTime
    -> Maybe (ScoreTime, ScoreTime)
marks_around rank marks pos =
    (,) <$> get (Ruler.descending pos marks) <*> get (Ruler.ascending pos marks)
    where get = fmap fst . Seq.head . filter ((<=rank) . Ruler.mark_rank . snd)

swing :: ScoreTime -- ^ time from this beat to the next, normalized 0 to 1
    -> RealTime -- ^ amount of swing offset, also normalized 0 to 1
swing = RealTime.seconds . Num.normalize (-1) 1 . sin . (*pi)
    . Num.scale (-0.5) 1.5 . ScoreTime.to_double


-- * TrackLang.Dynamic

-- These functions are starting to reinvent the Deriver functions, which is
-- annoying.  Also they are missing logging and exceptions, which would be
-- convenient.  Unfortunately ControlFunction can't just use a Deriver without
-- running into circular imports, or lumping thousands of lines into
-- Derive.Deriver.Monad.

dyn_seed :: TrackLang.Dynamic -> Double
dyn_seed = fromMaybe 0
    . TrackLang.maybe_val Environ.seed . TrackLang.dyn_environ

dyn_control :: TrackLang.Dynamic -> Score.Control -> RealTime -> Double
dyn_control dyn control pos = maybe 0 (Signal.at pos . Score.typed_val) $
    Map.lookup control $ TrackLang.dyn_controls dyn

real :: TrackLang.Dynamic -> ScoreTime -> RealTime
real dyn = Score.warp_pos (TrackLang.dyn_warp dyn)

score :: TrackLang.Dynamic -> RealTime -> ScoreTime
score dyn = Score.unwarp_pos (TrackLang.dyn_warp dyn)

-- ** ValControl

to_function :: TrackLang.Dynamic -> Signal.Y -> TrackLang.ValControl
    -> Util.Function
to_function dyn deflt =
    (Score.typed_val .) . to_typed_function dyn (Score.untyped deflt)

to_typed_function :: TrackLang.Dynamic -> Score.TypedVal -> TrackLang.ValControl
    -> Util.TypedFunction
to_typed_function dyn deflt control =
    case to_signal_or_function dyn control of
        Nothing -> const deflt
        Just (Left sig) -> Derive.signal_function sig
        Just (Right f) -> TrackLang.call_control_function f score_control dyn
    where
    score_control = case control of
        TrackLang.ControlSignal {} -> Controls.null
        TrackLang.DefaultedControl cont _ -> cont
        TrackLang.LiteralControl cont -> cont

to_signal_or_function :: TrackLang.Dynamic -> TrackLang.ValControl
    -> Maybe (Either Score.TypedControl TrackLang.ControlFunction)
to_signal_or_function dyn control = case control of
    TrackLang.ControlSignal sig -> return $ Left sig
    TrackLang.DefaultedControl cont deflt ->
        get_control (Score.type_of deflt) (return $ Left deflt) cont
    TrackLang.LiteralControl cont ->
        get_control Score.Untyped Nothing cont
    where
    get_control default_type deflt cont = case get_function cont of
        Just f -> return $ Right $
            TrackLang.apply_control_function (inherit_type default_type .) f
        Nothing -> case get_signal cont of
            Just sig -> return $ Left sig
            Nothing -> deflt

    get_function cont = Map.lookup cont $ TrackLang.dyn_control_functions dyn
    get_signal cont = Map.lookup cont $ TrackLang.dyn_controls dyn

    -- If the signal was untyped, it gets the type of the default, since
    -- presumably the caller expects that type.
    inherit_type default_type val =
        val { Score.type_of = Score.type_of val <> default_type }


val_call :: TrackLang.Typecheck a => Text -> Tags.Tags -> Text
    -> Derive.WithArgDoc (Derive.PassedArgs Derive.Tagged -> Derive.Deriver a)
    -> Derive.ValCall
val_call = Derive.val_call Module.prelude
