{- | Utilities for writing calls.  This is higher-level than TrackLang, so
    it can import "Derive.Derive".

    It should also have DeriveT utilities that could go in Derive, but are more
    specific to calls.

    Calls are evaluated in normalized time, which means that they start at
    @score_to_real 0@ and end at @score_to_real 1@.  The events passed to the
    generator are also in this time.
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

import qualified Derive.CallSig as CallSig
import Derive.CallSig (required)
import qualified Derive.Derive as Derive
import qualified Derive.TrackLang as TrackLang
import qualified Derive.Scale.Relative as Relative

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal


-- * signals

with_controls :: [TrackLang.Control] -> ([Signal.Y] -> Derive.Deriver a)
    -> Derive.Deriver a
with_controls controls f = f =<< mapM (control_at 0) controls

control_at :: ScoreTime -> TrackLang.Control -> Derive.Deriver Signal.Y
control_at pos control = case control of
    TrackLang.ConstantControl deflt -> return deflt
    TrackLang.DefaultedControl cont deflt ->
        Derive.control_at cont (Just deflt) pos
    TrackLang.Control cont -> Derive.control_at cont Nothing pos

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

-- * util

one_note :: Either TrackLang.TypeError Derive.EventDeriver
    -> Either TrackLang.TypeError (Derive.EventDeriver, Int)
one_note = fmap $ \d -> (d, 1)

-- | There are a set of pitch calls that need a "note" arg when called in an
-- absolute context, but can more usefully default to (Note "0") in a relative
-- track.  This will prepend a note arg if the scale in the environ is
-- relative.
default_relative_note :: Derive.PassedArgs derived -> Derive.PassedArgs derived
default_relative_note args
    | is_relative = args { Derive.passed_vals =
        TrackLang.VNote (Pitch.Note "0") : Derive.passed_vals args }
    | otherwise = args
    where
    environ = Derive.passed_environ args
    is_relative = case TrackLang.lookup_val TrackLang.v_scale environ of
        Right scale -> Relative.is_relative (Pitch.scale_id scale)
        _ -> False

-- ** derive ops

-- | Find the given note in the current scale, or throw.
note_to_degree :: Pitch.Note -> Derive.Deriver Pitch.Degree
note_to_degree note = do
    scale <- Derive.require_val TrackLang.v_scale
    lookup_note scale note

-- | Look up the given note in the given scale, or throw.
lookup_note :: Pitch.Scale -> Pitch.Note -> Derive.Deriver Pitch.Degree
lookup_note scale note = case Pitch.scale_note_to_degree scale note of
    Nothing -> Derive.throw $
        show note ++ " not in " ++ show (Pitch.scale_id scale)
    Just degree -> return degree

get_srate :: Derive.Deriver RealTime
get_srate = RealTime <$> Derive.require_val TrackLang.v_srate

-- * eval

type GeneratorReturn derived = Derive.Deriver (Derive.Deriver derived, Int)

skip :: derived -> GeneratorReturn derived
skip empty = return (return empty, 1)

-- | Evaluate a single note as a generator.  Fake up an event with no prev or
-- next lists.
eval_one :: String -> ScoreTime -> ScoreTime -> TrackLang.Expr
    -> Derive.EventDeriver
eval_one caller start dur expr = do
    -- Since the event was fake, I don't care if it wants to consume.
    (deriver, _) <- apply_toplevel (dinfo, cinfo) expr
    Derive.d_at start (Derive.d_stretch dur deriver)
    where
    cinfo = Derive.CallInfo 1 Nothing
        (Event.event ("expr: " ++ show expr) 1) [] []
    dinfo = DeriveInfo caller Derive.no_events lookup_note_call

-- ** eval implementation

data DeriveInfo derived = DeriveInfo {
    -- | Used in errors and warnings.
    info_caller :: String
    -- | This deriver's empty value to return on failure.
    , info_empty :: derived
    , info_lookup :: TrackLang.CallId
        -> Derive.Deriver (Maybe (Derive.Call derived))
    }

type PreProcess = TrackLang.Expr -> TrackLang.Expr
type Info derived = (DeriveInfo derived, Derive.CallInfo derived)

derive_track :: DeriveInfo derived -> PreProcess
    -> (Maybe (RealTime, Derive.Element derived) -> derived
        -> Maybe (RealTime, Derive.Element derived))
    -> [Track.PosEvent] -> Derive.Deriver [derived]
derive_track dinfo preproc get_last_sample events = do
    chunks <- go Nothing [] events
    return chunks
    where
    go _ _ [] = return []
    go prev_sample prev (cur@(pos, event) : rest) = do
        (chunk, consumed) <- with_catch (info_empty dinfo, 1) pos event $ do
            (deriver, consumed) <- derive_event dinfo preproc
                prev_sample prev cur rest
            chunk <- deriver
            return (chunk, consumed)
        -- TODO is this really an optimization?  profile later
        let (prev2, next2) = if consumed == 1
                then (cur : prev, rest)
                else let (pre, post) = splitAt consumed (cur : rest)
                    in (reverse pre ++ prev, post)
        rest <- go (get_last_sample prev_sample chunk) prev2 next2
        return $ chunk : rest

    with_catch deflt pos evt =
        fmap (Maybe.fromMaybe deflt) . Derive.catch_warn id . with_stack pos evt
    with_stack pos evt = Derive.with_stack_pos pos (Event.event_duration evt)

derive_event :: DeriveInfo derived -> PreProcess
    -> Maybe (RealTime, Derive.Element derived)
    -> [Track.PosEvent] -- ^ previous events, in reverse order
    -> Track.PosEvent -- ^ cur event
    -> [Track.PosEvent] -- ^ following events
    -> GeneratorReturn derived
derive_event dinfo preproc prev_val prev cur@(_, event) next
    | Event.event_string event == "--" = skip (info_empty dinfo)
    | otherwise = case TrackLang.parse (Event.event_string event) of
        Left err -> Derive.warn err >> skip (info_empty dinfo)
        Right expr -> run_call (preproc expr)
    where
    -- TODO move with_catch down here
    run_call expr = do
            (deriver, consumed) <- apply_toplevel (dinfo, cinfo) expr
            return (place deriver, consumed)
        where
        cinfo = Derive.CallInfo stretch prev_val
            evt0 (map warp prev) (map warp next)
        -- Derivation happens according to the extent of the note, not the
        -- duration.  This is how negative duration events begin deriving
        -- before arriving at the trigger.  Note generating calls that wish to
        -- actually generate negative duration may check the event_duration and
        -- reverse this by looking at a (start, end) of (1, 0) instead of
        -- (0, 1).
        place = Derive.d_at start . Derive.d_stretch stretch
        (start, end) = (Track.event_min cur, Track.event_max cur)
        -- A 0 dur event can't be normalized, so don't try.
        stretch = if start == end then 1 else end - start
        -- Warp all the events to be local to the warp established by 'place'.
        -- TODO optimize the stretch=1 case and see if that affects performance
        warp (pos, evt) =
            ((pos-start) / stretch, Event.modify_duration (/stretch) evt)
        evt0 = Event.modify_duration (/stretch) (snd cur)

apply_toplevel :: Info derived -> TrackLang.Expr -> GeneratorReturn derived
apply_toplevel info expr = case Seq.break_last expr of
    (transform_calls, Just generator_call) -> do
        (deriver, consumed) <- apply_generator info generator_call
        return (apply_transformer info transform_calls deriver, consumed)
    _ -> Derive.throw "event with no calls at all (this shouldn't happen)"

apply_generator :: Info derived -> TrackLang.Call -> GeneratorReturn derived
apply_generator info@(dinfo, cinfo) (TrackLang.Call call_id args) = do
    maybe_call <- info_lookup dinfo call_id
    (call, vals) <- case maybe_call of
        Just call -> do
            vals <- mapM (eval info) args
            return (call, vals)
        Nothing -> do
            maybe_call <- lookup_val_call call_id
            case maybe_call of
                Nothing -> Derive.throw $ unknown_call_id call_id
                Just vcall -> do
                    val <- apply info call_id vcall args
                    -- We only do this fallback thing once.
                    fb_call <- with_call fallback_call_id (info_lookup dinfo)
                    return (fb_call, [val])

    env <- Derive.gets Derive.state_environ
    let passed = Derive.PassedArgs vals env call_id cinfo
    case Derive.call_generator call of
        Just c -> case c passed of
            Left err -> with_msg call $ Derive.throw $ Pretty.pretty err
            Right (deriver, consumed) ->
                return (with_msg call deriver, consumed)
        Nothing -> with_msg call $
            Derive.throw "non-generator in generator position"
    where
    with_msg call = Derive.with_msg $
        info_caller dinfo ++ " generate " ++ Derive.call_name call

apply_transformer :: Info derived -> TrackLang.Expr
    -> Derive.Deriver derived -> Derive.Deriver derived
apply_transformer _ [] deriver = deriver
apply_transformer info@(dinfo, cinfo) (TrackLang.Call call_id args : calls)
        deriver = do
    vals <- mapM (eval info) args
    let new_deriver = apply_transformer info calls deriver
    call <- with_call call_id (info_lookup dinfo)
    env <- Derive.gets Derive.state_environ
    let with_msg = Derive.with_msg $
            info_caller dinfo ++ " transform " ++ Derive.call_name call
    let passed = Derive.PassedArgs vals env call_id cinfo
    case Derive.call_transformer call of
        Just c -> case c passed new_deriver of
            Left err -> with_msg $ Derive.throw $ Pretty.pretty err
            Right result -> with_msg result
        Nothing -> with_msg $
            Derive.throw "non-transformer in transformer position"

eval :: Info derived -> TrackLang.Term -> Derive.Deriver TrackLang.Val
eval _ (TrackLang.Literal val) = return val
eval info (TrackLang.ValCall (TrackLang.Call call_id terms)) = do
    call <- with_call call_id lookup_val_call
    apply info call_id call terms

apply :: Info derived -> TrackLang.CallId -> Derive.ValCall -> [TrackLang.Term]
    -> Derive.Deriver TrackLang.Val
apply info@(dinfo, _) call_id call args = do
    env <- Derive.gets Derive.state_environ
    let with_msg = Derive.with_msg $
            info_caller dinfo ++ " val call " ++ Derive.vcall_name call
    vals <- mapM (eval info) args
    let passed = Derive.PassedArgs vals env call_id Derive.dummy_call_info
    case Derive.vcall_call call passed of
        Left err -> with_msg $ Derive.throw $ Pretty.pretty err
        Right result -> with_msg result

with_call :: TrackLang.CallId
    -> (TrackLang.CallId -> Derive.Deriver (Maybe call)) -> Derive.Deriver call
with_call call_id lookup =
    maybe (Derive.throw (unknown_call_id call_id)) return =<< lookup call_id

unknown_call_id :: TrackLang.CallId -> String
unknown_call_id call_id = "unknown " ++ show call_id

fallback_call_id :: TrackLang.CallId
fallback_call_id = TrackLang.Symbol ""

-- * lookup call

lookup_note_call :: TrackLang.CallId -> Derive.Deriver (Maybe Derive.NoteCall)
lookup_note_call call_id = do
    st <- Derive.get
    let default_ns = State.state_project (Derive.state_ui st)
        block_id = Types.BlockId (make_id default_ns call_id)
    if block_id `Map.member` State.state_blocks (Derive.state_ui st)
        then return $ Just $ c_block block_id
        else lookup_call Derive.calls_note call_id

c_block :: BlockId -> Derive.NoteCall
c_block block_id = Derive.generate_one "block" $ \args ->
    if null (Derive.passed_vals args)
        then Right $ block_call block_id
        else Left $ TrackLang.ArgError "args for block call not implemented yet"

block_call :: BlockId -> Derive.EventDeriver
block_call block_id =
    Derive.d_subderive Derive.no_events (Derive.d_block block_id)

-- | Make an Id from a string, relative to the current ns if it doesn't already
-- have one.
--
-- TODO move this to a more generic place since LanguageCmds may want it too?
make_id :: String -> TrackLang.CallId -> Id.Id
make_id default_ns (TrackLang.Symbol ident_str) = Id.id ns ident
    where
    (w0, w1) = break (=='/') ident_str
    (ns, ident) = if null w1 then (default_ns, w0) else (w0, drop 1 w1)

lookup_control_call :: TrackLang.CallId
    -> Derive.Deriver (Maybe Derive.ControlCall)
lookup_control_call = lookup_call Derive.calls_control

lookup_pitch_call :: TrackLang.CallId -> Derive.Deriver (Maybe Derive.PitchCall)
lookup_pitch_call = lookup_call Derive.calls_pitch

lookup_val_call :: TrackLang.CallId -> Derive.Deriver (Maybe Derive.ValCall)
lookup_val_call = lookup_call Derive.calls_val

lookup_call :: (Derive.CallMap -> Map.Map TrackLang.CallId call)
    -> TrackLang.CallId -> Derive.Deriver (Maybe call)
lookup_call get_cmap call_id = do
    cmap <- Derive.gets (get_cmap . Derive.state_call_map)
    return (Map.lookup call_id cmap)

-- * c_equal

c_equal :: derived -> Derive.Call derived
c_equal empty = Derive.Call "equal"
    (Just $ \args -> with_args args generate)
    (Just $ \args deriver -> with_args args (transform deriver))
    where
    with_args args = CallSig.call2 args
        (required "symbol", required "value" :: CallSig.Arg TrackLang.Val)
    transform deriver sym val = Derive.with_val sym val deriver
    generate sym val = (Derive.put_val sym val >> return empty, 1)

-- * map score events

-- Functions here force a Deriver into its Score.Events and process them
-- directly, and then repackage them as a Deriver.  This can accomplish
-- concrete post-processing type effects but has the side-effect of collapsing
-- the Deriver, which will no longer respond to the environment.
--
-- The warp behaviour is reinstated though.

{-
-- | Transform an event list.  As a convenience, you can optionally pass a list
-- of signals which will be looked up at each event start.
--
-- The iteratee can return any number of events in any order.  This is flexible
-- but destroys laziness.
map_any :: (Monad m) => [Score.Event] -> st
    -> (EventContext st -> Result m st) -> Derive.DeriveT m [Score.Event]
map_any events st f = do
    (emap, _) <- fold_events go (Map.empty, st) events
    return (Map.elems emap)
    where
    go (emap, st) prev event next = do
        (new_st, new_events) <- f (st, prev, event, next)
        let epos = [(Score.start e, e) | e <- new_events]
        return (Map.insert_list epos emap, new_st)

-- | This is the same as 'map_any' except that the iteratee promises that
-- @last r0 <= head r1@ where @r0@ and @r1@ are consecutive result lists.  This
-- is less flexible but preserves laziness.
map_asc :: (Monad m) => [Score.Event] -> st
    -> (EventContext st -> Result m st) -> Derive.DeriveT m [Score.Event]
map_asc events st f = do
    (new_events, _) <- fold_events go (DList.empty, st) events
    return (DList.toList new_events)
    where
    go (collect, st) prev event next = do
        (new_st, new_events) <- f (st, prev, event, next)
        return (collect `DList.append` (DList.fromList new_events), new_st)

-- TODO: a variant map that promises to return ascending lists of events that
-- can overlap, e.g. 'head r0 <= head r1'.  If I can write a lazy
-- merge_sublists function...  I could use this one to make c_echo lazy.

type EventContext st = (st, [Score.Event], Score.Event, [Score.Event])
type Result m st = Derive.DeriveT m (st, [Score.Event])

with_directive :: (Monad m) => (TrackLang.CallId -> Bool)
    -> (Note.Call -> EventContext st -> Result m st)
    -> EventContext st -> Result m st
with_directive is_dir f context@(st, _, event, _) =
    case Note.parse_directive event of
        Nothing -> return (st, [event])
        Just (Left msg) -> do
            Log.warn $ "with_directive: error parsing directive: " ++ msg
            return (st, [event])
        Just (Right call)
            | is_dir (Note.call_id call) -> f call context
            | otherwise -> return (st, [event])

with_directive_calls call_ids = with_directive (is_call call_ids)

is_call :: [String] -> TrackLang.CallId -> Bool
is_call call_id_strs call_id = Set.member call_id call_ids
    where call_ids = Set.fromList (map TrackLang.Symbol call_id_strs)

-- * util

-- TODO merge this with Derive.map_events, which is doing the same kind of
-- thing

fold_events :: (Monad m, Score.Eventlike e) =>
    (st -> [e] -> e -> [e] -> Derive.DeriveT m st)
    -> st -> [e] -> Derive.DeriveT m st
fold_events f st events = foldM_neighbors go st events
    where go st prev event next = Derive.with_event event (f st prev event next)

-- | This is like 'foldM', but additionally pass the iteratee a list of
-- previous events in reverse order and a list of following events.
foldM_neighbors :: (Monad m) =>
    (st -> [a] -> a -> [a] -> m st) -> st -> [a] -> m st
foldM_neighbors f st xs = go st [] xs
    where
    go st _ [] = return st
    go st prev (x:xs) = do
        new_st <- f st prev x xs
        go new_st (x:prev) xs
-}
