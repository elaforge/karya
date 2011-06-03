{- | Basic module for call evaluation.

    It should also have Deriver utilities that could go in Derive, but are more
    specific to calls.

    It used to be that events were evaluated in \"normalized time\", which to
    say each one was shifted and stretched into place so that it always
    begins at 0t and ends at 1t.  While elegant, this was awkward in
    practice.  Some calls take ScoreTimes as arguments, and for those to
    be in the track's ScoreTime they have to be warped too.  Calls that
    look at the time of the next event on the track must warp that too.
    The result is that calls have to work in two time references
    simultaneously, which is confusing.  But the main thing is that note
    calls with subtracks need to slice the relevant events out of the
    subtracks, and those events are naturally in track time.  So the slice
    times would have to be unwarped, and then the sliced events warped.
    It was too complicated.

    Now events are evaluated in track time.  Block calls still warp the
    call into place, so blocks are still in normalized time, but other
    calls must keep track of their start and end times.

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
import qualified Data.ByteString.Char8 as B
import qualified Data.Maybe as Maybe

import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import Ui
import qualified Ui.Event as Event
import qualified Ui.State as State
import qualified Ui.Track as Track

import qualified Derive.Cache as Cache
import qualified Derive.CallSig as CallSig
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.ParseBs as Parse
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch


-- * eval

-- | Evaluate a single note as a generator.  Fake up an event with no prev or
-- next lists.
eval_one :: ScoreTime -> ScoreTime -> TrackLang.Expr -> Derive.EventDeriver
eval_one start dur expr =
    Derive.d_place start dur (eval_expr (note_dinfo, cinfo) expr)
    where cinfo = Derive.dummy_call_info ("eval_one: " ++ Pretty.pretty expr)

-- | Apply an expr with the current call info.
reapply :: Derive.PassedArgs Score.Event -> TrackLang.Expr
    -> Derive.EventDeriver
reapply args expr = eval_expr (note_dinfo, cinfo) expr
    where cinfo = Derive.passed_info args

-- | A version of 'eval' specialized to evaluate note calls.
eval_note :: Pitch.Note -> Derive.Deriver Pitch.Degree
eval_note note = CallSig.cast ("eval note " ++ show note)
    =<< eval (TrackLang.val_call (Pitch.note_text note))

-- | Evaluate a single expression.
eval_expr :: (Derive.Derived derived) => Info derived -> TrackLang.Expr
    -> Derive.LogsDeriver derived
eval_expr info expr = do
    state <- Derive.get
    let (res, logs, collect, cache) = apply_toplevel state info expr
    Derive.modify $ \st -> st
        { Derive.state_collect = collect, Derive.state_cache_state = cache }
    return $ Derive.merge_logs res logs

-- ** eval implementation

data DeriveInfo derived = DeriveInfo {
    -- | TODO If I could get rid of this I could make functions like 'eval_one'
    -- and 'reapply' polymorphic.  It seems like this should be possible
    -- because there can be a static mapping between the derived type and the
    -- lookup.  Unfortunately ValCall is a problem since sinced it's not
    -- a derived type like the others.
    info_lookup :: Derive.LookupCall (Derive.Call derived)
    -- | Name of the lookup scope.  It would be cleaner to get this out of
    -- 'derived' by using the typeclass, but this is simpler.  I could do this
    -- with 'Derive.with_msg' but this seems more direct.
    , info_name :: String
    }

note_dinfo :: DeriveInfo Score.Event
note_dinfo = DeriveInfo lookup_note_call "note"

type Info derived = (DeriveInfo derived, Derive.CallInfo derived)

type GetLastSample d =
    Maybe (RealTime, Derive.Elem d) -> d -> Maybe (RealTime, Derive.Elem d)

derive_track :: (Derive.Derived derived) =>
    Derive.State -> ScoreTime -> DeriveInfo derived
    -> Parse.ParseExpr -> GetLastSample derived
    -> State.EventsTree -> [Track.PosEvent]
    -> ([LEvent.LEvents derived], Derive.Collect, Derive.CacheState)
derive_track state block_end dinfo parse get_last_sample subs events =
    go (Derive.record_track_environ state) (Derive.state_cache_state state)
        Nothing [] events
    where
    go collect cache _ _ [] = ([], collect, cache)
    go collect cache prev_sample prev (cur : rest) =
        -- TODO is this no longer tail recursive because it has to keep
        -- final_modify around?  Can I improve it with an accumulator?  Does it
        -- matter if the events are lazily consumed?
        (map LEvent.Log logs : score_events : rest_events,
            last_collect, last_cache)
        where
        (result, logs, next_collect, next_cache) =
            -- trace ("derive " ++ show_pos state (fst cur) ++ "**") $
            derive_event
                (state { Derive.state_collect = collect,
                    Derive.state_cache_state = cache })
                block_end dinfo parse prev_sample subs prev cur rest
        (rest_events, last_collect, last_cache) = go next_collect next_cache
            sample (cur:prev) rest
        score_events = case result of
            Right stream -> stream
            Left err -> [LEvent.Log (Derive.error_to_warn err)]
        sample = case result of
            Right derived ->
                case Seq.last (Maybe.mapMaybe LEvent.event derived) of
                    Just elt -> get_last_sample prev_sample elt
                    Nothing -> prev_sample
            Left _ -> prev_sample

-- show_pos :: Derive.State -> ScoreTime -> String
-- show_pos state pos = stack ++ ": " ++ Pretty.pretty now
--     where
--     now = Score.warp_pos pos (Derive.state_warp state)
--     stack = Seq.join ", " $ map Stack.unparse_ui_frame $
--         Stack.to_ui (Derive.state_stack state)


derive_event :: (Derive.Derived d) =>
    Derive.State -> ScoreTime -> DeriveInfo d -> Parse.ParseExpr
    -> Maybe (RealTime, Derive.Elem d)
    -> State.EventsTree
    -> [Track.PosEvent] -- ^ previous events, in reverse order
    -> Track.PosEvent -- ^ cur event
    -> [Track.PosEvent] -- ^ following events
    -> (Either Derive.DeriveError (LEvent.LEvents d), [Log.Msg],
        Derive.Collect, Derive.CacheState)
derive_event st block_end dinfo parse prev_val subs prev cur@(pos, event) next
    | Event.event_bs event == B.pack "--" =
        (Right mempty, [], Derive.state_collect st, Derive.state_cache_state st)
    | otherwise = case parse (Event.event_bs event) of
        Left err -> (Right mempty, [parse_error err],
            Derive.state_collect st, Derive.state_cache_state st)
        Right expr -> run_call expr
    where
    parse_error = Log.msg Log.Warn (Just (Derive.state_stack st))
    run_call expr = apply_toplevel state (dinfo, cinfo expr) expr

    state = st {
        Derive.state_stack =
            Stack.add (Stack.Region pos (pos + Event.event_duration event))
                (Derive.state_stack st)
        }
    cinfo expr = Derive.CallInfo expr prev_val cur prev next block_end subs

-- | Apply a toplevel expression.
apply_toplevel :: (Derive.Derived d) => Derive.State -> Info d
    -> TrackLang.Expr
    -> (Either Derive.DeriveError (LEvent.LEvents d), [Log.Msg],
        Derive.Collect, Derive.CacheState)
apply_toplevel state info expr = case Seq.break_last expr of
        (transform_calls, Just generator_call) -> run $
            apply_transformer info transform_calls $
                apply_generator info generator_call
        _ -> (Right mempty, [err],
            Derive.state_collect state, Derive.state_cache_state state)
    where
    run d = case Derive.run state d of
        (result, state, logs) -> (result, logs, Derive.state_collect state,
            Derive.state_cache_state state)
    err = Log.msg Log.Warn (Just (Derive.state_stack state))
        "event with no calls at all (this shouldn't happen)"

apply_generator :: (Derive.Derived derived) => Info derived -> TrackLang.Call
    -> Derive.LogsDeriver derived
apply_generator (dinfo, cinfo) (TrackLang.Call call_id args) = do
    maybe_call <- info_lookup dinfo call_id
    (call, vals) <- case maybe_call of
        Just call -> do
            vals <- mapM eval args
            return (call, vals)
        Nothing -> do
            maybe_call <- lookup_val_call call_id
            case maybe_call of
                Nothing -> Derive.throw $
                    unknown_call_id (info_name dinfo) call_id
                Just vcall -> do
                    val <- apply call_id vcall args
                    -- We only do this fallback thing once.
                    fb_call <- with_call fallback_call_id
                        (info_name dinfo) (info_lookup dinfo)
                    return (fb_call, [val])

    env <- Derive.gets Derive.state_environ
    ns <- State.state_namespace <$> Derive.get_ui_state
    let args = Derive.PassedArgs vals env call_id cinfo
        gen = Derive.call_generator call
        with_stack = case (($ (ns, args)) . Derive.gcall_block) =<< gen of
            Just block_id -> Derive.with_stack_block block_id
            Nothing -> Derive.with_stack_call (Derive.call_name call)
    with_stack $ case Derive.gcall_func <$> Derive.call_generator call of
        Just call -> call args
        Nothing -> Derive.throw $ "non-generator in generator position: "
            ++ Derive.call_name call

apply_transformer :: (Derive.Derived derived) => Info derived -> TrackLang.Expr
    -> Derive.LogsDeriver derived -> Derive.LogsDeriver derived
apply_transformer _ [] deriver = deriver
apply_transformer info@(dinfo, cinfo) (TrackLang.Call call_id args : calls)
        deriver = do
    vals <- mapM eval args
    let new_deriver = apply_transformer info calls deriver
    call <- with_call call_id (info_name dinfo) (info_lookup dinfo)
    env <- Derive.gets Derive.state_environ
    let args = Derive.PassedArgs vals env call_id cinfo
    state <- Derive.get
    let cached = Cache.cached_transformer
            (Derive.state_cache (Derive.state_cache_state state))
            (Derive.state_stack state)
    let with_stack = Derive.with_stack_call (Derive.call_name call)
    with_stack $ case Derive.call_transformer call of
        Just trans -> do
            (transformed, new_cache) <- cached trans args new_deriver
            when_just new_cache Derive.put_cache
            transformed
        Nothing -> Derive.throw $ "non-transformer in transformer position: "
            ++ Derive.call_name call

eval :: TrackLang.Term -> Derive.Deriver TrackLang.Val
eval (TrackLang.Literal val) = return val
eval (TrackLang.ValCall (TrackLang.Call call_id terms)) = do
    call <- with_call call_id "val" lookup_val_call
    apply call_id call terms

apply :: TrackLang.CallId -> Derive.ValCall -> [TrackLang.Term]
    -> Derive.Deriver TrackLang.Val
apply call_id call args = do
    env <- Derive.gets Derive.state_environ
    vals <- mapM eval args
    let args = Derive.PassedArgs vals env call_id
            (Derive.dummy_call_info "val-call")
    Derive.with_msg ("val call " ++ Derive.vcall_name call) $
        Derive.vcall_call call args

with_call :: TrackLang.CallId -> String
    -> (TrackLang.CallId -> Derive.Deriver (Maybe call)) -> Derive.Deriver call
with_call call_id name lookup =
    maybe (Derive.throw (unknown_call_id name call_id)) return
        =<< lookup call_id

unknown_call_id :: String -> TrackLang.CallId -> String
unknown_call_id name call_id =
    name ++ " call not found: " ++ Pretty.pretty call_id

fallback_call_id :: TrackLang.CallId
fallback_call_id = TrackLang.Symbol ""



-- * lookup call

-- | First priority is the blocks.  So a block with a certain name will shadow
-- everything else with that name.
lookup_note_call :: Derive.LookupCall Derive.NoteCall
lookup_note_call = lookup_with Derive.scope_note

lookup_control_call :: Derive.LookupCall Derive.ControlCall
lookup_control_call = lookup_with Derive.scope_control

lookup_pitch_call :: Derive.LookupCall Derive.PitchCall
lookup_pitch_call = lookup_with Derive.scope_pitch

lookup_val_call :: Derive.LookupCall Derive.ValCall
lookup_val_call = lookup_with Derive.scope_val

lookup_with :: (Derive.Scope -> Derive.ScopeType call)
    -> Derive.LookupCall call
lookup_with get call_id = do
    lookups <- get_scopes get
    lookup_scopes lookups call_id

get_scopes :: (Derive.Scope -> Derive.ScopeType call)
    -> Derive.Deriver [Derive.LookupCall call]
get_scopes get = do
    Derive.ScopeType inst scale builtin <-
        get <$> Derive.gets Derive.state_scope
    return $ inst ++ scale ++ builtin

-- | Convert a list of lookups into a single lookup by returning the first
-- one to yield a Just.
lookup_scopes :: [Derive.LookupCall call] -> Derive.LookupCall call
lookup_scopes [] _ = return Nothing
lookup_scopes (lookup:rest) call_id =
    maybe (lookup_scopes rest call_id) (return . Just) =<< lookup call_id
