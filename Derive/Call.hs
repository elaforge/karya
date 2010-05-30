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

import Ui
import qualified Ui.Event as Event
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Types as Types

import qualified Derive.Derive as Derive
import qualified Derive.TrackLang as TrackLang
import qualified Derive.Score as Score
import qualified Derive.Call.Note as Note
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

-- | Get the next \"relevant\" event beginning.  Intended to be used by calls
-- to determine their extent, especially control calls, which have no explicit
-- duration.
--
-- This will skip 'x = y' calls, which are not indended to affect note scope.
-- TODO implement that
next_event_begin :: [Track.PosEvent] -> Maybe ScoreTime
next_event_begin ((pos, _) : _) = Just pos
next_event_begin _ = Nothing

-- | There are a set of pitch calls that need a "note" arg when called in an
-- absolute context, but can more usefully default to (Note "0") in a relative
-- track.  This will prepend a note arg if the scale in the environ is
-- relative.
default_relative_note :: TrackLang.PassedArgs y -> TrackLang.PassedArgs y
default_relative_note args
    | is_relative = args { TrackLang.passed_vals =
        TrackLang.VNote (Pitch.Note "0") : TrackLang.passed_vals args }
    | otherwise = args
    where
    environ = TrackLang.passed_environ args
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
    (deriver, _) <- eval_note_generator caller expr Nothing []
        (event start dur ("expr: " ++ show expr)) []
    deriver

event :: ScoreTime -> ScoreTime -> String -> Track.PosEvent
event start dur text = (start, Event.event text dur)

-- ** eval implementation

-- | (caller, empty, lookup_call, preproc)
--
-- @caller@ is used in errors and warnings.
-- @preproc@ is applied to each event text before parsing.  It's a hack for
-- the pitch track mangling.
type DeriveInfo y derived = (String, derived,
    TrackLang.CallId -> Derive.Deriver (Derive.Call y derived),
    String -> String)

derive_track :: DeriveInfo y derived
    -> (Maybe (RealTime, y) -> derived -> Maybe (RealTime, y))
    -> [Track.PosEvent] -> Derive.Deriver [derived]
derive_track info@(_, empty, _, _) get_last_sample events = do
    chunks <- go Nothing [] events
    return chunks
    where
    go _ _ [] = return []
    go prev_sample prev (cur@(pos, event) : rest) = do
        (chunk, consumed) <- with_catch (empty, 1) pos event $ do
            (deriver, consumed) <- derive_event info prev_sample
                prev cur rest
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

derive_event :: DeriveInfo y derived
    -> Maybe (RealTime, y)
    -> [Track.PosEvent] -- ^ previous events, in reverse order
    -> Track.PosEvent -- ^ cur event
    -> [Track.PosEvent] -- ^ following events
    -> Derive.Deriver (Derive.Deriver derived, Int)
derive_event info@(_, empty, _, preproc) prev_sample prev cur@(_, event) next
    | Event.event_string event == "--" = skip empty
    | otherwise = case TrackLang.parse (preproc (Event.event_string event)) of
        Left err -> Derive.warn err >> skip empty
        Right expr -> eval_generator info expr prev_sample prev cur next

eval_note_generator :: String -> TrackLang.Expr
    -> Maybe (RealTime, Score.Event) -> [Track.PosEvent]
    -> Track.PosEvent -> [Track.PosEvent] -> GeneratorReturn Derive.Events
eval_note_generator caller =
    eval_generator (caller, Derive.no_events, lookup_note_call, id)

eval_generator :: DeriveInfo y derived
    -> TrackLang.Expr -> Maybe (RealTime, y)
    -> [Track.PosEvent] -> Track.PosEvent -> [Track.PosEvent]
    -> Derive.Deriver (Derive.Deriver derived, Int)
eval_generator info@(caller, empty, lookup_call, _)
        (TrackLang.Call call_id args : rest) prev_val prev cur next = do
    call <- lookup_call call_id
    env <- Derive.gets Derive.state_environ
    let passed = TrackLang.PassedArgs args env call_id stretch prev_val
    let msg = eval_msg "generator" caller call
    case Derive.call_generator call of
        Nothing -> do
            Derive.with_msg msg
                (Derive.warn "non-generator in generator position")
            skip empty
        Just c -> case c passed (map warp prev) evt0 (map warp next) of
            Left err -> do
                Derive.with_msg msg $ Derive.warn $ Pretty.pretty err
                skip empty
            Right (generate_deriver, consumed) -> do
                deriver <- eval_transformer info stretch rest
                    (handle_exc msg empty generate_deriver)
                return (place deriver, consumed)
    where
    -- Derivation happens according to the extent of the note, not the
    -- duration.  This is how negative duration events begin deriving before
    -- arriving at the trigger.  Note generating calls that wish to actually
    -- generate negative duration may check the event_duration and reverse this
    -- by looking at a (start, end) of (1, 0) instead of (0, 1).
    place = Derive.d_at start . Derive.d_stretch stretch
    (start, end) = (Track.event_min cur, Track.event_max cur)
    -- A 0 dur event can't be normalized, so don't try.
    stretch = if start == end then 1 else end - start
    -- Warp all the events to be local to the warp established by 'place'.
    -- TODO optimize the stretch=1 case and see if that affects performance
    warp (pos, evt) =
        ((pos-start) / stretch, Event.modify_duration (/stretch) evt)
    evt0 = Event.modify_duration (/stretch) (snd cur)
eval_generator _ [] _ _ cur _ = Derive.throw $
    "event with no calls at all (this shouldn't happen): " ++ show cur


eval_note_transformer :: String -> ScoreTime -> TrackLang.Expr
    -> Derive.EventDeriver -> Derive.Deriver Derive.EventDeriver
eval_note_transformer caller =
    eval_transformer (caller, Derive.no_events, lookup_note_call, id)

eval_transformer :: DeriveInfo y derived
    -> ScoreTime -> TrackLang.Expr
    -> Derive.Deriver derived -> Derive.Deriver (Derive.Deriver derived)
eval_transformer info@(caller, empty, lookup_call, _) stretch
        (TrackLang.Call call_id args : rest) deriver = do
    call <- lookup_call call_id
    env <- Derive.gets Derive.state_environ
    let passed = TrackLang.PassedArgs args env call_id stretch Nothing
    let msg = eval_msg "transformer" caller call
    case Derive.call_transformer call of
        Nothing -> do
            Derive.with_msg msg $
                Derive.warn "non-transformer in transformer position"
            return (return empty)
        Just c -> case c passed deriver of
            Left err -> do
                Derive.with_msg msg $ Derive.warn $ Pretty.pretty err
                return (return empty)
            Right deriver -> eval_transformer info stretch rest
                (handle_exc msg empty deriver)
eval_transformer _ _ [] deriver = return deriver

handle_exc :: String -> a -> Derive.Deriver a -> Derive.Deriver a
handle_exc msg empty deriver = fmap (maybe empty id) $
    -- TODO with_msg is not quite right here, because then nested calls wind up
    -- pushing nested msgs on to the stack and cluttering up the error msgs,
    -- when I really only want the last.  Not sure why this only happens for
    -- the title expr though.  I don't know what a better solution would be,
    -- short of both a nested and non-nested contexts.
    Derive.catch_warn id (Derive.with_msg msg deriver)

eval_msg :: String -> String -> Derive.Call y derived -> String
eval_msg eval_type caller call = "eval "
    ++ caller ++ " " ++ eval_type ++ " " ++ Derive.call_name call

-- * lookup_note_call

-- TODO Can I move this to Derive.Note?  The only thing is that Calls want
-- to do their own sub-derivation, e.g. Rambat.c_tick.

-- | This is here instead of Derive because note calls first look at the block
-- ids to derive a block.
lookup_note_call :: TrackLang.CallId -> Derive.Deriver Derive.NoteCall
lookup_note_call call_id = do
    st <- Derive.get
    let default_ns = State.state_project (Derive.state_ui st)
        block_id = Types.BlockId (make_id default_ns call_id)
    let call_map = Derive.calls_note (Derive.state_call_map st)
    if block_id `Map.member` State.state_blocks (Derive.state_ui st)
        then return $ Note.c_block block_id
        else case Map.lookup call_id call_map of
            Nothing -> return (c_not_found call_id)
            Just call -> return call

-- | I don't want to abort all of derivation by throwing, but I do want to
-- abort evaluation of this expression, so consider this a kind of type error,
-- which does just that.
c_not_found :: TrackLang.CallId -> Derive.NoteCall
c_not_found call_id = Derive.Call "not_found"
    (Just $ \_ _ _ _ -> err) (Just $ \_ _ -> err)
    where err = Left (TrackLang.CallNotFound call_id)

-- | Make an Id from a string, relative to the current ns if it doesn't already
-- have one.
--
-- TODO move this to a more generic place since LanguageCmds may want it to?
make_id :: String -> TrackLang.CallId -> Id.Id
make_id default_ns (TrackLang.Symbol ident_str) = Id.id ns ident
    where
    (w0, w1) = break (=='/') ident_str
    (ns, ident) = if null w1 then (default_ns, w0) else (w0, drop 1 w1)


-- * c_equal

c_equal :: derived -> Derive.Call y derived
c_equal = Note.c_equal

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
