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
    [ (">", c_next_val)
    , ("<", c_prev_val)
    , ("next-event", c_next_event)
    , ("bpm", c_bpm)
    , ("env", c_env)
    , ("ts", c_timestep)
    , ("ts/", c_timestep_reciprocal)
    , ("1/", c_reciprocal)
    , ("nn", c_nn)
    , ("hz", c_hz)
    -- literals
    , ("list", c_list)
    , ("lrange", c_lrange)
    , ("st", c_scoretime)
    , ("rt", c_realtime)
    , ("pitch", c_pitch)
    , ("#", c_pcontrol_ref)
    , ("signal", c_signal)
    , ("psignal", c_psignal)
    -- , ("control", c_control)
    -- , ("pcontrol", c_pcontrol)
    -- lookup
    , ("<-#", c_get_pitch)
    -- generate signals
    , ("i>", c_linear_next)
    , ("e>", c_exp_next)
    , ("df", c_down_from)
    ]

c_next_val :: Derive.ValCall
c_next_val = val_call "next-val" Tags.next
    "Evaluate the next event. Only works on pitch and control tracks, and\
    \ if the next event doesn't need its previous event."
    $ Sig.call0 $ \args -> do
        event <- Derive.require "no next event" $
            Lists.head (Args.next_events args)
        start <- Derive.real (Event.start event)
        next_val event start (Derive.ctx_track_type (Derive.passed_ctx args))

next_val :: Event.Event -> RealTime -> Maybe ParseTitle.Type
    -> Derive.Deriver DeriveT.Val
next_val event start ttype = case ttype of
    Just ParseTitle.ControlTrack -> eval_control start event
    Just ParseTitle.TempoTrack -> eval_control start event
    Just ParseTitle.PitchTrack -> do
        signal <- eval event
        case PSignal.at start signal of
            Nothing -> Derive.throw "next pitch event didn't emit a pitch"
            Just pitch -> return $ DeriveT.VPitch pitch
    Just ParseTitle.NoteTrack ->
        Derive.throw "can't get next value for note tracks"
    Nothing -> Derive.throw "no track type"
    where
    eval_control start event = do
        signal <- eval event
        return $ DeriveT.num $ Signal.at start (signal :: Signal.Control)
    eval event = mconcat . Stream.events_of <$>
        (either Derive.throw return =<< Eval.eval_event event)

c_prev_val :: Derive.ValCall
c_prev_val = val_call "prev-val" Tags.prev
    "Return the previous value. Only works on pitch and control tracks."
    $ Sig.call0 $ \args -> do
        start <- Args.real_start args
        case Args.prev_val args of
            Just (Derive.TagControl sig) ->
                return $ DeriveT.num $ Signal.at start sig
            Just (Derive.TagPitch sig) ->
                maybe (Derive.throw "no previous pitch")
                    (return . DeriveT.VPitch) (PSignal.at start sig)
            _ -> Derive.throw "no previous value"

c_next_event :: Derive.ValCall
c_next_event = val_call "next-event" Tags.next
    "Start RealTime of the next event. Only used for tests."
    $ Sig.call0 $ \args -> do
        event <- Derive.require "no next event" $
            Lists.head (Args.next_events args)
        Derive.score_to_real $ Event.start event

c_bpm :: Derive.ValCall
c_bpm = val_call "bpm" mempty "Convert bpm to tempo.  This is just (/60)."
    $ Sig.call (Sig.required "bpm" "")
    $ \bpm _args -> return $ (bpm :: Double) / 60

c_env :: Derive.ValCall
c_env = val_call "env" mempty
    "Look up the given val in the environ."
    $ Sig.call ((,)
    <$> Sig.required "name" "Look up the value of this key."
    <*> Sig.defaulted "default" (Nothing :: Maybe DeriveT.Val)
        "If given, this is the default value when the key isn't present. If\
        \ not given, a missing key will throw an exception. The presence of\
        \ a default will also make the lookup expect the same type as the\
        \ default."
    ) $ \(name, maybe_deflt) _args -> case maybe_deflt of
        Nothing -> Derive.get_val name
        Just deflt -> check name deflt =<< Derive.lookup_val name
    where
    check _ deflt Nothing = return deflt
    check name deflt (Just val)
        | DeriveT.types_equal deflt val = return val
        | otherwise = Derive.throw $ "env " <> pretty name
            <> " expected " <> pretty (ValType.general_type_of deflt)
            <> " but got " <> pretty (ValType.general_type_of val)

c_timestep :: Derive.ValCall
c_timestep = val_call "timestep" mempty
    ("Compute the duration of the given RelativeMark timestep at the current\
    \ position. This is for durations, so it only works with RelativeMark."
    ) $ Sig.call ((,)
    <$> Sig.required "rank" "Emit a duration of this rank."
    <*> Sig.defaulted "steps" (1 :: Int) "This number of steps of that rank."
    ) $ \(rank, steps) args -> DeriveT.score_time <$>
        Call.meter_duration (Args.start args) rank steps

c_timestep_reciprocal :: Derive.ValCall
c_timestep_reciprocal = Make.modify_vcall c_timestep Module.prelude
    "timestep-reciprocal"
    ("This is the same as `timestep` except it returns the reciprocal. This is\
    \ useful for e.g. trills which take cycles per second rather than duration."
    ) reciprocal
    where
    reciprocal val
        | Just num <- DeriveT.constant_val val =
            DeriveT.VSignal $ Signal.constant . recip <$> num
        | otherwise = val

c_reciprocal :: Derive.ValCall
c_reciprocal = val_call "reciprocal" mempty
    "Find the reciprocal of a number. Useful for tempo, e.g. set the tempo to\
    \ 1/time." $ Sig.call (Sig.required "num" "") $ \num _ ->
        if num == 0 then Derive.throw "1/0"
            else return (1 / num :: Double)

c_nn :: Derive.ValCall
c_nn = val_call "nn" mempty
    "Convert a pitch, hz, or twelve-tone pitch name to a NoteNumber.\
    \ A pitch name looks like `[a-g]s?[-1-9]`."
    $ Sig.call (Sig.required "val" "") $ \val _ -> case val of
        Left pitch -> Pitches.pitch_nn (PSignal.coerce (pitch :: PSignal.Pitch))
        Right (Left hz) -> return $ Pitch.hz_to_nn hz
        Right (Right name) -> get_name_nn name

c_hz :: Derive.ValCall
c_hz = val_call "hz" mempty
    "Convert a pitch, twelve-tone pitch name, or NoteNumber to hz.\
    \ A pitch name looks like `[a-g]s?[-1-9]`. If the octave isn't given, it\
    \ defaults to 0.  This is useful for `%just-base`, which ignores the\
    \ octave."
    $ Sig.call (Sig.required "val" "") $ \val _ -> case val of
        Left pitch -> Pitch.nn_to_hz <$>
            Pitches.pitch_nn (PSignal.coerce (pitch :: PSignal.Pitch))
            -- Not transposed because they asked for a specific pitch.
        Right (Left name) -> Pitch.nn_to_hz <$> get_name_nn name
        Right (Right nn) ->
            return (Pitch.nn_to_hz (Pitch.NoteNumber nn) :: Double)

get_name_nn :: Expr.Str -> Derive.Deriver Pitch.NoteNumber
get_name_nn name =
    Derive.require ("unknown pitch: " <> ShowVal.show_val name) $
        parse_pitch_name (Expr.unstr name)

-- | c-1 is 0, g9 is 127.  The octave is optional, and defaults to 1.
parse_pitch_name :: Text -> Maybe Pitch.NoteNumber
parse_pitch_name = either (const Nothing) Just . ParseText.parse1 parse
    where
    parse = do
        pc <- maybe mzero return . (`Map.lookup` pcs) =<< A.anyChar
        sharp <- A.option 0 $ A.char 's' >> return 1
        oct <- A.option 1 $ ParseText.p_int
        return $ Pitch.nn $ pc + sharp + (oct+1) * 12
    pcs = Map.fromList $ zip "cdefgab" (scanl (+) 0 Theory.piano_intervals)

c_list :: Derive.ValCall
c_list = val_call "list" mempty "Create a list." $
    Sig.call (Sig.many "val" "Value.") $ \vals _ ->
        return $ DeriveT.VList vals

c_lrange :: Derive.ValCall
c_lrange = val_call "range" mempty "Make a list range of numbers." $
    Sig.call ((,,)
    <$> Sig.required "start" "Start value."
    <*> Sig.required "step" "Step."
    <*> Sig.required "count" "Number of values."
    ) $ \(start, step, count) _ ->
        return $ DeriveT.VList $ map DeriveT.num $ Lists.range start step count

c_scoretime :: Derive.ValCall
c_scoretime = val_call "scoretime" mempty
    "Convert a number to ScoreTime. This just changes the type annotation, the\
    \ value remains the same." $
    Sig.call (Sig.required_env "val" Sig.None "") $ \val _ ->
        return $ ScoreTime.from_double val

c_realtime :: Derive.ValCall
c_realtime = val_call "realtime" mempty
    "Convert a number to RealTime. This just changes the type annotation, the\
    \ value remains the same." $
    Sig.call (Sig.required_env "val" Sig.None "") $ \val _ ->
        return $ RealTime.seconds val

c_pitch :: Derive.ValCall
c_pitch = val_call "pitch" mempty "Create a 'Perform.Pitch.Pitch'."
    $ Sig.call ((,,)
    <$> Sig.defaulted_env "oct" Sig.None (Left 0 :: Either Int Int)
        "Octave, or a pitch name or pitch. If it's a pitch name or pitch, the\
        \ `pc` and `accs` args must be 0."
    <*> Sig.defaulted_env "pc" Sig.None (0 :: Int) "Pitch class."
    <*> Sig.defaulted_env "accs" Sig.None (0 :: Int) "Accidentals."
    ) $ \(oct, pc, accs) _ -> make_pitch oct pc accs

make_pitch :: Either Pitch.Octave (Either Text PSignal.Pitch)
    -> Pitch.PitchClass -> Pitch.Accidentals -> Derive.Deriver Pitch.Pitch
make_pitch (Left oct) pc accs = return $ Pitch.Pitch oct (Pitch.Degree pc accs)
make_pitch (Right name_pitch) pc accs
    | pc /= 0 || accs /= 0 = Derive.throw $
        "pc and accs args must be 0 when a pitch is given: " <> showt (pc, accs)
    | otherwise = do
        (note, scale) <- case name_pitch of
            Left name -> (,) <$> return (Pitch.Note name) <*> Call.get_scale
            Right pitch -> (,)
                <$> Pitches.pitch_note (PSignal.coerce pitch)
                <*> Derive.get_scale (PSignal.pitch_scale_id pitch)
        env <- Derive.get_environ
        either (Derive.throw . pretty) return $
            Scale.scale_read scale env note

c_pcontrol_ref :: Derive.ValCall
c_pcontrol_ref = val_call "pcontrol-ref" mempty
    "Create a 'Derive.DeriveT.PControlRef'. For control literals, the\
    \ `#name` syntax suffices, but if you want to give a default pitch,\
    \ you need this call. This is because pitches are calls, and while\
    \ `%c,1` can be parsed as-is, `#p,(4c)` needs an evaluation."
    $ Sig.call ((,)
    <$> Sig.required "name" "Name of pitch signal."
    <*> Sig.defaulted "default" (Nothing :: Maybe PSignal.Pitch)
        "Default pitch, if the signal is not set."
    ) $ \(name, mb_default) _ -> return $
        DeriveT.Ref (ScoreT.PControl name) (PSignal.constant <$> mb_default)

-- | This should be the inverse of ShowVal (ScoreT.Typed Signal.Control).
-- See 'DeriveT.show_signal'.
c_signal :: Derive.ValCall
c_signal = val_call "signal" mempty
    "Create a signal. This is the control signal literal."
    $ Sig.call ((,)
        <$> Sig.optional_env "type" Derive.None "" "Type code."
        <*> Sig.many_pairs "breakpoints" "Breakpoints."
    ) $ \(type_code, bps) _ -> do
        typ <- Derive.require ("unknown type code: " <> type_code)
            (ScoreT.code_to_type type_code)
        return $ DeriveT.VSignal $ ScoreT.Typed typ $ Signal.from_pairs bps

-- | Like c_signal, should be the inverse of ShowVal PSignal.PSignal, except
-- it can't because I can't show pitches.
-- See 'DeriveT.show_psignal'.
c_psignal :: Derive.ValCall
c_psignal = val_call "psignal" mempty "Create a pitch signal."
    $ Sig.call (Sig.many_pairs "breakpoints" "Breakpoints.")
    $ \bps _ -> return $ DeriveT.VPSignal $ PSignal.from_pairs bps

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

c_get_pitch :: Derive.ValCall
c_get_pitch = val_call "pitch" mempty "Get the current pitch." $
    Sig.call (Sig.defaulted "control" ("" :: Text)
        "The default pitch if empty, otherwise, get the named pitch.") $
    \control args -> Derive.require "pitch"
        =<< Derive.named_pitch_at control =<< Args.real_start args

-- * generate signals

c_linear_next :: Derive.ValCall
c_linear_next = val_call "linear-next" mempty
    "Create straight lines between the given breakpoints."
    $ Sig.call breakpoints_arg $ \vals args ->
        breakpoints 0 ControlUtil.Linear vals args

c_exp_next :: Derive.ValCall
c_exp_next = val_call "exp-next" mempty
    "Create curved lines between the given breakpoints."
    $ Sig.call ((,)
    <$> Sig.defaulted "exp" (2 :: Double) ControlUtil.exponential_doc
    <*> breakpoints_arg
    ) $ \(exp, vals) args ->
        breakpoints 1 (ControlUtil.Function $ ControlUtil.expon exp) vals args

breakpoints_arg :: Sig.Parser (NonEmpty DeriveT.Val)
breakpoints_arg = Sig.many1 "bp" "Breakpoints are distributed evenly between\
    \ the event start and the next event. They can be all numbers, or all\
    \ pitches."

c_down_from :: Derive.ValCall
c_down_from = val_call "down-from" mempty
    "Go down from a starting value at a certain rate."
    $ Sig.call ((,)
    <$> Sig.defaulted "from" (1 :: Double) "Start at this value."
    <*> Sig.defaulted "speed" (1 :: Double) "Descend this amount per second."
    ) $ \(from, speed) args -> do
        (start, end) <- Args.real_range_or_next args
        return $ ScoreT.untyped $
            ControlUtil.slope_to_limit (Just 0) Nothing from (-speed) start end

-- ** implementation

breakpoints :: Int -> ControlUtil.Curve -> NonEmpty DeriveT.Val
    -> Derive.PassedArgs a -> Derive.Deriver DeriveT.Val
breakpoints argnum curve vals args = do
    (start, end) <- Args.real_range_or_next args
    srate <- Call.get_srate
    vals <- num_or_pitch (Args.start args) argnum vals
    return $ case vals of
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
num_or_pitch :: ScoreTime -> Int -> NonEmpty DeriveT.Val
    -> Derive.Deriver (Either [Signal.Y] [PSignal.Pitch])
num_or_pitch start argnum (val :| vals) = case val of
    val | Just (ScoreT.Typed ScoreT.Untyped n) <- DeriveT.constant_val val -> do
        vals <- mapM (expect tsig) (zip [argnum + 1 ..] vals)
        return $ Left (n : vals)
    DeriveT.VPitch pitch -> do
        vals <- mapM (expect ValType.TPitch) (zip [argnum + 1 ..] vals)
        return $ Right (pitch : vals)
    _ -> type_error argnum "bp" (ValType.TEither tsig ValType.TPitch) val
    where
    tsig = ValType.TSignal ValType.TUntyped ValType.TAny
    expect typ (argnum, val) =
        maybe (type_error argnum "bp" typ val) return
            =<< Typecheck.from_val_eval start val

type_error :: Int -> Derive.ArgName -> ValType.Type -> DeriveT.Val
    -> Derive.Deriver a
type_error argnum name expected received =
    Derive.throw_error $ Derive.CallError $ Derive.TypeError $ Derive.TypeErrorT
        { error_place = Derive.TypeErrorArg argnum
        , error_source = Derive.Literal
        , error_arg_name = name
        , error_expected = expected
        , error_received = Just received
        , error_derive = Nothing
        }


-- * util

val_call :: Typecheck.ToVal a => Derive.CallName -> Tags.Tags -> Doc.Doc
    -> Derive.WithArgDoc (Derive.PassedArgs Derive.Tagged -> Derive.Deriver a)
    -> Derive.ValCall
val_call = Derive.val_call Module.prelude
