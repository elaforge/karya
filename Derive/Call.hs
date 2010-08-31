{- | Utilities for writing calls.  This is higher-level than TrackLang, so
    it can import "Derive.Derive".

    It should also have DeriveT utilities that could go in Derive, but are more
    specific to calls.

    Calls are evaluated in normalized time, which means that they start at
    @score_to_real 0@ and end at @score_to_real 1@.  The events passed to the
    generator are also in this time.

    The way expression evaluation works is a little irregular.  The toplevel
    expression returns a parameterized deriver, so this part of the type is
    exported to the haskell type system.  The values and non-toplevel calls
    return dynamically typed Vals though.  The difference between a generator
    and a transformer is that the latter takes an extra deriver arg, but since
    the type of the deriver is statically determined at the haskell level, it
    isn't passed as a normal arg but is instead hardcoded into the evaluation
    scheme for the toplevel expression.  So only the toplevel calls can take
    and return derivers.

    I experimented with a system that added a VDeriver type, but there were
    several problems:

    - If I don't parameterize Val I wind up with separate VEventDeriver,
    VPitchDeriver, etc. constructors.  Every call that takes a deriver must
    validate the type and there is no static guarantee that event deriver calls
    won't wind up the pitch deriver symbol table.  It seems nice that the
    CallMap and Environ can all be replaced with a single symbol table, but in
    practice they should have different calls available so they would need to
    be separated anyway.

    - If I do parameterize Val, I need some complicated typeclass gymnastics
    and a lot of redundant Typecheck instances to make the new VDeriver type
    fit in with the calling scheme.  I have to differentiate PassedVals, which
    includev VDeriver, from Vals, which don't, so Environ can remain
    unparameterized.  Otherwise I would need a separate Environ per track,
    and copy over vals which should be shared, like srate.  The implication is
    that Environ should really have dynamically typed deriver vals.

    - Replacing @a | b | c@ with @a (b (c))@ is appealing, but if the deriver
    is the final argument then I have a problem where a required argument wants
    to follow an optional one.  Solutions would be to implement some kind of
    keyword args that allow the required arg to remain at the end, or simply
    put it as the first arg, so that @a 1 | b 2 | c 3@ is sugar for
    @a (b (c 3) 2) 1@.

    - But, most importantly, I don't have a clear use for making derivers first
    class.  Examples would be:

        * A call that takes two derivers: @do-something (block1) (block2)@.
        I can't think of a @do-something@.

        * Derivers in the environment: @default-something = (block1)@.  I
        can't think of a @default-something@.

    I could move more in the direction of a real language by unifying all
    symbols into Environ, looking up Symbols in @eval@, and making a VCall
    type.  That way I could rebind calls with @tr = absolute-trill@ or
    do argument substitution with @d = (block1); transpose 1 | d@.  However,
    I don't have any uses in mind for that, and /haskell/ is supposed to be
    the real language.  I should focus more on making it easy to write your own
    calls in haskell.

    - Making derivers first class would likely mean dropping \"consumed\"
    unless I can stuff it into an environ value or something.  I don't mind
    that though, because I don't have any convincing uses for it yet either.
-}
module Derive.Call where
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import Ui
import qualified Ui.Event as Event
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Types as Types

import qualified Derive.Cache as Cache
import qualified Derive.CallSig as CallSig
import Derive.CallSig (required)
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.PitchSignal as PitchSignal
import qualified Perform.Signal as Signal


-- * signals

-- | A function to generate a pitch curve.  It's convenient to define this
-- as a type alias so it can be easily passed to various functions that want
-- to draw curves.
type PitchInterpolator = Pitch.ScaleId
    -> Bool -- ^ include the initial sample or not
    -> RealTime -> Pitch.Degree -> RealTime -> Pitch.Degree
    -> PitchSignal.PitchSignal

-- | Like 'PitchInterpolator' but for control signals.
type ControlInterpolator = Bool -- ^ include the initial sample or not
    -> RealTime -> Signal.Y -> RealTime -> Signal.Y
    -> Signal.Control

with_controls :: [TrackLang.Control] -> ([Signal.Y] -> Derive.Deriver a)
    -> Derive.Deriver a
with_controls controls f = do
    now <- Derive.now
    f =<< mapM (control_at now) controls

-- | To accomodate both normal calls, which are in score time, and post
-- processing calls, which are in real time, these functions take RealTimes.
control_at :: RealTime -> TrackLang.Control -> Derive.Deriver Signal.Y
control_at pos control = case control of
    TrackLang.ConstantControl deflt -> return deflt
    TrackLang.DefaultedControl cont deflt ->
        maybe deflt id <$> Derive.control_at cont pos
    TrackLang.Control cont ->
        maybe (Derive.throw $ "not found and no default: " ++ show cont) return
            =<< Derive.control_at cont pos

-- | Convert a 'TrackLang.Control' to a signal.
to_signal :: TrackLang.Control -> Derive.Deriver Signal.Control
to_signal control = case control of
    TrackLang.ConstantControl deflt -> return $ Signal.constant deflt
    TrackLang.DefaultedControl cont deflt -> do
        sig <- Derive.get_control cont
        return $ maybe (Signal.constant deflt) id sig
    TrackLang.Control cont ->
        maybe (Derive.throw $ "not found: " ++ show cont) return
            =<< Derive.get_control cont

pitch_at :: RealTime -> TrackLang.PitchControl -> Derive.Deriver PitchSignal.Y
pitch_at pos control = case control of
    TrackLang.ConstantControl deflt ->
        PitchSignal.degree_to_y <$> eval_note deflt
    TrackLang.DefaultedControl cont deflt -> do
        maybe_y <- Derive.named_pitch_at cont pos
        maybe (PitchSignal.degree_to_y <$> eval_note deflt) return maybe_y
    TrackLang.Control cont -> do
        maybe_y <- Derive.named_pitch_at cont pos
        maybe (Derive.throw $ "pitch not found and no default given: "
            ++ show cont) return maybe_y

to_pitch_signal :: TrackLang.PitchControl
    -> Derive.Deriver PitchSignal.PitchSignal
to_pitch_signal control = case control of
    TrackLang.ConstantControl deflt -> constant deflt
    TrackLang.DefaultedControl cont deflt -> do
        sig <- Derive.get_named_pitch cont
        maybe (constant deflt) return sig
    TrackLang.Control cont ->
        maybe (Derive.throw $ "not found: " ++ show cont) return
            =<< Derive.get_named_pitch cont
    where
    constant note = do
        scale <- get_scale
        PitchSignal.constant (Pitch.scale_id scale) <$> eval_note note

degree_at :: RealTime -> TrackLang.PitchControl -> Derive.Deriver Pitch.Degree
degree_at pos control = PitchSignal.y_to_degree <$> pitch_at pos control

-- * util

one_note :: Either TrackLang.TypeError Derive.EventDeriver
    -> Either TrackLang.TypeError (Derive.EventDeriver, Int)
one_note = fmap $ \d -> (d, 1)

-- | Return True if I'm in a relative scale context.
--
-- There are a set of pitch calls that need a \"note\" arg when called in an
-- absolute context, but can more usefully default to @(Note "0")@ in
-- a relative track.
in_relative_scale :: Derive.PassedArgs derived -> Bool
in_relative_scale args = case TrackLang.lookup_val TrackLang.v_scale environ of
        Right scale -> Pitch.is_relative (Pitch.scale_id scale)
        _ -> False
    where environ = Derive.passed_environ args

-- | There are a set of pitch calls that need a \"note\" arg when called in an
-- absolute context, but can more usefully default to @(Note "0")@ in
-- a relative track.  This will prepend a note arg if the scale in the environ
-- is relative.
--
-- TODO this is much easier to use than 'in_relative_scale' but doesn't work
-- since it needs to be in Deriver.  If I put TypeError into DeriveError then
-- I can do it
default_relative_note :: Derive.PassedArgs derived
    -> Derive.Deriver (Derive.PassedArgs derived)
default_relative_note args
    | is_relative = do
        degree <- CallSig.cast "relative pitch 0"
            =<< eval (TrackLang.val_call "0")
        return $ args { Derive.passed_vals =
            TrackLang.VDegree degree : Derive.passed_vals args }
    | otherwise = return args
    where
    environ = Derive.passed_environ args
    is_relative = case TrackLang.lookup_val TrackLang.v_scale environ of
        Right scale -> Pitch.is_relative (Pitch.scale_id scale)
        _ -> False

-- ** derive ops

get_srate :: Derive.Deriver RealTime
get_srate = RealTime <$> Derive.require_val TrackLang.v_srate

get_scale :: Derive.Deriver Pitch.Scale
get_scale = Derive.require_val TrackLang.v_scale

get_scale_id :: Derive.Deriver Pitch.ScaleId
get_scale_id = Pitch.scale_id <$> get_scale

-- * eval

type LookupCall call = TrackLang.CallId -> Derive.Deriver (Maybe call)

-- | Evaluate a single note as a generator.  Fake up an event with no prev or
-- next lists.
eval_one :: ScoreTime -> ScoreTime -> TrackLang.Expr -> Derive.EventDeriver
eval_one start dur expr = do
    -- Since the event was fake, I don't care if it wants to consume.
    Derive.d_at start $ Derive.d_stretch dur $
        apply_toplevel (dinfo, cinfo) expr
    where
    -- TODO use pretty instead of show
    cinfo = Derive.dummy_call_info ("eval_one: " ++ show expr)
    dinfo = DeriveInfo Derive.no_events lookup_note_call

-- | A version of 'eval' specialized to evaluate note calls.
eval_note :: Pitch.Note -> Derive.Deriver Pitch.Degree
eval_note note = CallSig.cast ("eval note " ++ show note)
    =<< eval (TrackLang.val_call (Pitch.note_text note))

-- | Evaluate the root block in a performance.  Making this an ordinary call
-- means it participates in the derive cache just like all other calls.
--
-- TODO put global postproc here, like negative duration
eval_root_block :: BlockId -> Derive.EventDeriver
    -- Derive.d_tempo does a bit of magic to stretch all blocks to length 1,
    -- except the root one.  The root block should operate in real time, so
    -- no stretching here.  Otherwise, a tempo of '2' is the same as '1'.
eval_root_block block_id = eval_one 0 1 [call_from_block_id block_id]

-- ** eval implementation

data DeriveInfo derived = DeriveInfo {
    -- | This deriver's empty value to return on failure.
    info_empty :: derived
    , info_lookup :: LookupCall (Derive.Call derived)
    }

type PreProcess = TrackLang.Expr -> TrackLang.Expr
type Info derived = (DeriveInfo derived, Derive.CallInfo derived)

derive_track :: (Derive.Derived derived) =>
    ScoreTime -> DeriveInfo derived -> PreProcess
    -> (Maybe (RealTime, Derive.Elem derived) -> derived
        -> Maybe (RealTime, Derive.Elem derived))
    -> [Track.PosEvent] -> Derive.Deriver [derived]
derive_track block_end dinfo preproc get_last_sample events =
    go Nothing [] events
    where
    go _ _ [] = return []
    go prev_sample prev (cur@(pos, event) : rest) = do
        chunk <- with_catch (info_empty dinfo) pos event $
            derive_event block_end dinfo preproc prev_sample prev cur rest
        rest <- go (get_last_sample prev_sample chunk) (cur:prev) rest
        return $ chunk : rest

    with_catch deflt pos evt =
        fmap (Maybe.fromMaybe deflt) . Derive.catch_warn id . with_stack pos evt
    with_stack pos evt =
        Derive.with_stack_region pos (pos + Event.event_duration evt)

derive_event :: (Derive.Derived derived) =>
    ScoreTime -> DeriveInfo derived -> PreProcess
    -> Maybe (RealTime, Derive.Elem derived)
    -> [Track.PosEvent] -- ^ previous events, in reverse order
    -> Track.PosEvent -- ^ cur event
    -> [Track.PosEvent] -- ^ following events
    -> Derive.Deriver derived
derive_event block_end dinfo preproc prev_val prev cur@(pos, event) next
    | Event.event_string event == "--" = return (info_empty dinfo)
    | otherwise = case TrackLang.parse (Event.event_string event) of
        Left err -> Derive.warn err >> return (info_empty dinfo)
        Right expr -> run_call (preproc expr)
    where
    -- TODO move with_catch down here
    run_call expr =
        Derive.d_place start stretch $ apply_toplevel (dinfo, cinfo) expr
        where
        cinfo = Derive.CallInfo prev_val
            evt0 (map warp prev) (map warp next) ((block_end-start) / stretch)
            (pos, Event.event_duration event) prev next
        -- Derivation happens according to the extent of the note, not the
        -- duration.  This is how negative duration events begin deriving
        -- before arriving at the trigger.  Note generating calls that wish to
        -- actually generate negative duration may check the event_duration and
        -- reverse this by looking at a (start, end) of (1, 0) instead of
        -- (0, 1).
        (start, end) = (Track.event_min cur, Track.event_max cur)
        -- A 0 dur event can't be normalized, so don't try.
        stretch = if start == end then 1 else end - start
        -- Warp all the events to be local to the warp established by 'place'.
        -- TODO optimize the stretch=1 case and see if that affects performance
        warp (pos, evt) =
            ((pos-start) / stretch, Event.modify_duration (/stretch) evt)
        evt0 = Event.modify_duration (/stretch) (snd cur)

-- | Apply a toplevel expression.
apply_toplevel :: (Derive.Derived derived) => Info derived -> TrackLang.Expr
    -> Derive.Deriver derived
apply_toplevel info expr = case Seq.break_last expr of
    (transform_calls, Just generator_call) ->
        apply_transformer info transform_calls $
            apply_generator info generator_call
    _ -> Derive.throw "event with no calls at all (this shouldn't happen)"

apply_generator :: (Derive.Derived derived) => Info derived -> TrackLang.Call
    -> Derive.Deriver derived
apply_generator (dinfo, cinfo) (TrackLang.Call call_id args) = do
    maybe_call <- info_lookup dinfo call_id
    (call, vals) <- case maybe_call of
        Just call -> do
            vals <- mapM eval args
            return (call, vals)
        Nothing -> do
            maybe_call <- lookup_val_call call_id
            case maybe_call of
                Nothing -> Derive.throw $ unknown_call_id call_id
                Just vcall -> do
                    val <- apply call_id vcall args
                    -- We only do this fallback thing once.
                    fb_call <- with_call fallback_call_id (info_lookup dinfo)
                    return (fb_call, [val])

    env <- Derive.gets Derive.state_environ
    let args = Derive.PassedArgs vals env call_id cinfo
    state <- Derive.get
    let with_stack = Derive.with_stack_call (Derive.call_name call)
    with_stack $ case Derive.call_generator call of
        Just gen -> do
            result <- Cache.cached_generator (Derive.state_cache_state state)
                (Derive.state_stack state) gen args
            case result of
                (Left err, _) -> Derive.throw $ Pretty.pretty err
                (Right deriver, new_cache) -> do
                    maybe (return ()) Derive.put_cache new_cache
                    deriver
        Nothing -> Derive.throw $ "non-generator in generator position: "
            ++ Derive.call_name call

apply_transformer :: (Derive.Derived derived) => Info derived -> TrackLang.Expr
    -> Derive.Deriver derived -> Derive.Deriver derived
apply_transformer _ [] deriver = deriver
apply_transformer info@(dinfo, cinfo) (TrackLang.Call call_id args : calls)
        deriver = do
    vals <- mapM eval args
    let new_deriver = apply_transformer info calls deriver
    call <- with_call call_id (info_lookup dinfo)
    env <- Derive.gets Derive.state_environ
    let args = Derive.PassedArgs vals env call_id cinfo
    state <- Derive.get
    let cached = Cache.cached_transformer
            (Derive.state_cache (Derive.state_cache_state state))
            (Derive.state_stack state)
    let with_stack = Derive.with_stack_call (Derive.call_name call)
    with_stack $ case Derive.call_transformer call of
        Just trans -> do
            result <- cached trans args new_deriver
            case result of
                (Left err, _) -> Derive.throw $ Pretty.pretty err
                (Right result, new_cache) -> do
                    maybe (return ()) Derive.put_cache new_cache
                    result
        Nothing -> Derive.throw $ "non-transformer in transformer position: "
            ++ Derive.call_name call

eval :: TrackLang.Term -> Derive.Deriver TrackLang.Val
eval (TrackLang.Literal val) = return val
eval (TrackLang.ValCall (TrackLang.Call call_id terms)) = do
    call <- with_call call_id lookup_val_call
    apply call_id call terms

apply :: TrackLang.CallId -> Derive.ValCall -> [TrackLang.Term]
    -> Derive.Deriver TrackLang.Val
apply call_id call args = do
    env <- Derive.gets Derive.state_environ
    let with_msg = Derive.with_msg $ "val call " ++ Derive.vcall_name call
    vals <- mapM eval args
    let args = Derive.PassedArgs vals env call_id
            (Derive.dummy_call_info "val-call")
    case Derive.vcall_call call args of
        Left err -> with_msg $ Derive.throw $ Pretty.pretty err
        Right result -> with_msg result

with_call :: TrackLang.CallId
    -> (TrackLang.CallId -> Derive.Deriver (Maybe call)) -> Derive.Deriver call
with_call call_id lookup =
    maybe (Derive.throw (unknown_call_id call_id)) return =<< lookup call_id

unknown_call_id :: TrackLang.CallId -> String
unknown_call_id call_id = "call not found: " ++ Pretty.pretty call_id

fallback_call_id :: TrackLang.CallId
fallback_call_id = TrackLang.Symbol ""

-- * lookup call

lookup_note_call :: LookupCall Derive.NoteCall
lookup_note_call call_id = do
    st <- Derive.get
    let default_ns = State.state_project (Derive.state_ui st)
        block_id = Types.BlockId (make_id default_ns call_id)
    if block_id `Map.member` State.state_blocks (Derive.state_ui st)
        then return $ Just $ c_block block_id
        else lookup_call Derive.calls_note call_id

c_block :: BlockId -> Derive.NoteCall
c_block block_id = Derive.caching_generator "block" $ \args ->
    if null (Derive.passed_vals args)
        then Right $ block_call block_id
        else Left $ TrackLang.ArgError "args for block call not implemented yet"

block_call :: BlockId -> Derive.EventDeriver
block_call block_id =
    Derive.d_subderive Derive.no_events (Derive.d_block block_id)

-- | Given a block id, produce a call expression that will call that block.
call_from_block_id :: BlockId -> TrackLang.Call
call_from_block_id block_id =
    TrackLang.call (Id.show_id (Id.unpack_id block_id))

-- | Make an Id from a string, relative to the current ns if it doesn't already
-- have one.
--
-- TODO move this to a more generic place since LanguageCmds may want it too?
make_id :: String -> TrackLang.CallId -> Id.Id
make_id default_ns (TrackLang.Symbol ident_str) = Id.id ns ident
    where
    (w0, w1) = break (=='/') ident_str
    (ns, ident) = if null w1 then (default_ns, w0) else (w0, drop 1 w1)

lookup_control_call :: LookupCall Derive.ControlCall
lookup_control_call = lookup_call Derive.calls_control

lookup_pitch_call :: LookupCall Derive.PitchCall
lookup_pitch_call = lookup_call Derive.calls_pitch

lookup_val_call :: LookupCall Derive.ValCall
lookup_val_call call_id = do
    -- Looking up calls_val first means that you can locally override a note.
    maybe_call <- lookup_call Derive.calls_val call_id
    case maybe_call of
        Just vcall -> return (Just vcall)
        Nothing -> do
            scale <- get_scale
            return $ Pitch.scale_note_to_call scale (to_note call_id)
    where to_note (TrackLang.Symbol sym) = Pitch.Note sym

lookup_call :: (Derive.CallMap -> Map.Map TrackLang.CallId call)
    -> LookupCall call
lookup_call get_cmap call_id = do
    cmap <- Derive.gets (get_cmap . Derive.state_call_map)
    return (Map.lookup call_id cmap)

-- * c_equal

c_equal :: (Derive.Derived derived) => derived -> Derive.Call derived
c_equal empty = Derive.Call "equal"
    (Just (Derive.GeneratorCall
        (\args -> with_args args generate) Derive.NonCachingGenerator))
    (Just (Derive.TransformerCall
        (\args deriver -> with_args args (transform deriver))
        Derive.NonIncremental))
    where
    with_args args = CallSig.call2 args
        (required "symbol", required "value" :: CallSig.Arg TrackLang.Val)
    transform deriver sym val = Derive.with_val sym val deriver
    generate sym val = Derive.put_val sym val >> return empty

-- * map score events

-- Functions here force a Deriver into its Score.Events and process them
-- directly, and then repackage them as a Deriver.  This can accomplish
-- concrete post-processing type effects but has the side-effect of collapsing
-- the Deriver, which will no longer respond to the environment.


-- | Reinstate warp behaviour on a list of events.  The name is from nyquist.
--
-- Only offset is implemented, not stretch or general warp.
cue :: [Score.Event] -> Derive.EventDeriver
cue events = do
    offset <- Derive.now
    return $ map (Score.move (+offset)) events

-- | Map a function with state over events and lookup pitch and controls vals
-- for each event.  Exceptions are not caught.
--
-- TODO needs a length encoded vector to be safer.
map_signals :: [TrackLang.Control] -> [TrackLang.PitchControl]
    -> ([Signal.Y] -> [Pitch.Degree] -> state -> Score.Event
        -> Derive.Deriver (state, result))
    -> state -> [Score.Event] -> Derive.Deriver (state, [result])
map_signals controls pitch_controls f state events =
    map_accuml_m go state events
    where
    go state event = do
        let pos = Score.event_start event
        control_vals <- mapM (control_at pos) controls
        pitch_vals <- mapM (degree_at pos) pitch_controls
        f control_vals pitch_vals state event

{-
What I would really like is:

map_signals :: Vec TrackLang.Control clen -> Vec TrackLang.PitchControl plen
    -> (state -> Vec Signal.Y clen -> Vec Pitch.Degree plen -> Score.Event
        -> Derive.Deriver (state, result))
    -> state -> [Score.Event] -> Derive.Deriver (state, [result])

then access with 'f (c1 :, c2 :, ()) (p1 :, ()) = ...'
-}
