-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
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
    validate the type and there is no static guarantee that event deriver
    calls won't wind up the pitch deriver symbol table.  It seems nice that
    the CallMap and Environ can all be replaced with a single symbol table,
    but in practice they represent different scopes, so they would need to be
    separated anyway.

    - If I do parameterize Val, I need some complicated typeclass gymnastics
    and a lot of redundant Typecheck instances to make the new VDeriver type
    fit in with the calling scheme.  I have to differentiate PassedVals, which
    include VDeriver, from Vals, which don't, so Environ can remain
    unparameterized.  Otherwise I would need a separate Environ per track, and
    copy over vals which should be shared, like srate.  The implication is
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
-}
module Derive.Call where
import qualified Data.ByteString.Char8 as B
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text

import Util.Control
import qualified Util.Log as Log
import qualified Util.Seq as Seq

import qualified Ui.Event as Event
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Types as Types

import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.LEvent as LEvent
import qualified Derive.ParseBs as ParseBs
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Stack as Stack
import qualified Derive.TrackInfo as TrackInfo
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Signal as Signal
import Types


type CallInfo d = Derive.CallInfo (Derive.Elem d)
type PassedArgs d = Derive.PassedArgs (Derive.Elem d)

-- * eval

-- | Evaluate a single note as a generator.  Fake up an event with no prev or
-- next lists.
eval_one :: (Derive.Callable d) => TrackLang.Expr -> Derive.LogsDeriver d
eval_one = eval_one_at 0 1

eval_one_call :: (Derive.Callable d) => TrackLang.Call -> Derive.LogsDeriver d
eval_one_call = eval_one . (:| [])

eval_one_at :: (Derive.Callable d) => ScoreTime -> ScoreTime -> TrackLang.Expr
    -> Derive.LogsDeriver d
eval_one_at start dur expr = eval_expr cinfo expr
    where
    -- Set the event start and duration instead of using Derive.d_place since
    -- this way I can have zero duration events.
    cinfo = Derive.dummy_call_info start dur $
        untxt $ "eval_one: " <> ShowVal.show_val expr

-- | Like 'derive_event' but evaluate the event outside of its track context.
-- This is useful if you want to evaluate things out of order, i.e. evaluate
-- the /next/ pitch.
eval_event :: (Derive.Callable d) => Event.Event
    -> Derive.Deriver (Either String (LEvent.LEvents d))
eval_event event = case ParseBs.parse_expr (Event.event_bytestring event) of
    Left err -> return $ Left err
    Right expr -> Right <$>
        -- TODO eval it separately to catch any exception?
        eval_one_at (Event.start event) (Event.duration event) expr

-- | Apply an expr with the current call info.
reapply :: (Derive.Callable d) => PassedArgs d -> TrackLang.Expr
    -> Derive.LogsDeriver d
reapply args = eval_expr (Derive.passed_info args)

-- | Like 'reapply', but parse the string first.
reapply_string :: (Derive.Callable d) => PassedArgs d -> Text
    -> Derive.LogsDeriver d
reapply_string args s = case ParseBs.parse_expr (ParseBs.from_text s) of
    Left err -> Derive.throw $ "parse error: " ++ err
    Right expr -> reapply args expr

reapply_call :: (Derive.Callable d) => PassedArgs d -> Text -> [TrackLang.Term]
    -> Derive.LogsDeriver d
reapply_call args call_id call_args =
    reapply args (TrackLang.call call_id call_args :| [])

-- | A version of 'eval' specialized to evaluate pitch calls.
eval_pitch :: ScoreTime -> TrackLang.PitchCall
    -> Derive.Deriver PitchSignal.Pitch
eval_pitch pos call =
    Sig.cast ("eval pitch " <> ShowVal.show_val call)
        =<< eval cinfo (TrackLang.ValCall call)
    where
    cinfo :: Derive.CallInfo PitchSignal.Pitch
    cinfo = Derive.dummy_call_info pos 0 "<eval_pitch>"
    -- Pitch calls shouldn't care about their pos.

-- | This is like 'eval_pitch' when you already know the call, presumably
-- because you asked 'Derive.scale_note_to_call'.
apply_pitch :: ScoreTime -> Derive.ValCall -> Derive.Deriver TrackLang.Val
apply_pitch pos call = apply cinfo call []
    where cinfo = Derive.dummy_call_info pos 0 "<apply_pitch>"

-- | Evaluate a single expression.
eval_expr :: (Derive.Callable d) => CallInfo d -> TrackLang.Expr
    -> Derive.LogsDeriver d
eval_expr cinfo expr = do
    state <- Derive.get
    let (res, logs, collect) = apply_toplevel state cinfo expr
    -- I guess this could set collect to mempty and then merge it back in,
    -- but I think this is the same with less work.
    Derive.modify $ \st -> st { Derive.state_collect = collect }
    return $ Derive.merge_logs res logs

-- | Parse and apply a transform expression.
apply_transform :: (Derive.Callable d) => String -> Text
    -> Derive.LogsDeriver d -> Derive.LogsDeriver d
apply_transform name expr_str deriver = do
    expr <- case ParseBs.parse_expr (ParseBs.from_text expr_str) of
        Left err -> Derive.throw $ name ++ ": " ++ err
        Right expr -> return expr
    let transform = if Text.null expr_str then id
            else apply_transformers info (NonEmpty.toList expr)
        info = Derive.dummy_call_info 0 1 name
    transform deriver

-- * derive_track

-- | Just a spot to stick all the per-track parameters.
data TrackInfo = TrackInfo {
    -- | Either the end of the block, or the next event after the slice.
    -- These fields are take directly from 'State.TrackEvents'.
    tinfo_events_end :: !ScoreTime
    , tinfo_track_range :: !(ScoreTime, ScoreTime)
    , tinfo_shifted :: !ScoreTime
    , tinfo_sub_tracks :: !TrackTree.EventsTree
    , tinfo_events_around :: !([Event.Event], [Event.Event])
    , tinfo_type :: !TrackInfo.Type
    } deriving (Show)

-- | Given the previous sample and derivation results, get the last sample from
-- the results.
--
-- Technically only the last sample part varies, this signature allows note
-- calls to avoid the work in 'get_last'.
type GetLastSample d = forall x.  PrevVal d -> Either x (LEvent.LEvents d)
    -> PrevVal d
type PrevVal d = Maybe (RealTime, Derive.Elem d)

pitch_last_sample :: GetLastSample PitchSignal.Signal
pitch_last_sample =
    get_last (\prev chunk -> PitchSignal.last chunk `mplus` prev)

control_last_sample :: GetLastSample Signal.Control
control_last_sample = get_last (\prev chunk -> Signal.last chunk `mplus` prev)

get_last :: (val -> d -> val) -> val -> Either x [LEvent.LEvent d] -> val
get_last _ prev (Left _) = prev
get_last f prev (Right derived) =
    case Seq.last (mapMaybe LEvent.event derived) of
        Just elt -> f prev elt
        Nothing -> prev

-- | This is the toplevel function to derive a track.  It's responsible for
-- actually evaluating each event.
--
-- There's a certain amount of hairiness in here because note and control
-- tracks are mostly but not quite the same and because calls get a lot of
-- auxiliary data in 'Derive.CallInfo'.
derive_track :: forall d. (Derive.Callable d) =>
    -- forall and ScopedTypeVariables needed for the inner 'go' signature
    Derive.State -> TrackInfo -> GetLastSample d
    -> [Event.Event] -> ([LEvent.LEvents d], Derive.Collect)
derive_track state tinfo get_last_sample events =
    go (Internal.record_track_dynamic state) Nothing [] events
    where
    -- This threads the collect through each event.  I would prefer to map and
    -- mconcat, but profiling showed that to be quite a bit slower.
    go :: Derive.Collect -> PrevVal d -> [Event.Event] -> [Event.Event]
        -> ([LEvent.LEvents d], Derive.Collect)
    go collect _ _ [] = ([], collect)
    go collect prev_sample prev (cur : rest) =
        (events : rest_events, final_collect)
        where
        (result, logs, next_collect) =
            derive_event (state { Derive.state_collect = collect })
                tinfo prev_sample prev cur rest
        (rest_events, final_collect) =
            go next_collect next_sample (cur : prev) rest
        events = map LEvent.Log logs ++ case result of
            Right stream -> stream
            Left err -> [LEvent.Log (Derive.error_to_warn err)]
        next_sample = get_last_sample prev_sample result

derive_event :: (Derive.Callable d) =>
    Derive.State -> TrackInfo -> PrevVal d
    -> [Event.Event] -- ^ previous events, in reverse order
    -> Event.Event -- ^ cur event
    -> [Event.Event] -- ^ following events
    -> (Either Derive.Error (LEvent.LEvents d), [Log.Msg], Derive.Collect)
derive_event st tinfo prev_sample prev event next
    | "--" `B.isPrefixOf` B.dropWhile (==' ') text =
        (Right mempty, [], Derive.state_collect st)
    | otherwise = case ParseBs.parse_expr text of
        Left err ->
            (Right mempty, [parse_error (txt err)], Derive.state_collect st)
        Right expr -> run_call expr
    where
    text = Event.event_bytestring event
    parse_error = Log.msg Log.Warn $
        Just (Stack.to_strings (Derive.state_stack (Derive.state_dynamic st)))
    run_call expr = apply_toplevel state (cinfo expr) expr
    state = st
        { Derive.state_dynamic = Internal.add_stack_frame
            region (Derive.state_dynamic st)
        }
    region = Stack.Region (shifted + Event.min event)
        (shifted + Event.max event)
    cinfo expr = Derive.CallInfo
        { Derive.info_expr = expr
        , Derive.info_prev_val = prev_sample
        , Derive.info_event = event
        -- Augment prev and next with the unevaluated "around" notes from
        -- 'State.tevents_around'.
        , Derive.info_prev_events = tprev ++ prev
        , Derive.info_next_events = next ++ tnext
        , Derive.info_event_end = case next ++ tnext of
            [] -> events_end
            event : _ -> Event.start event
        , Derive.info_track_range = track_range
        , Derive.info_sub_tracks = subs
        , Derive.info_sub_events = Nothing
        , Derive.info_track_type = Just ttype
        }
    TrackInfo events_end track_range shifted subs (tprev, tnext) ttype = tinfo

repeat_call_of :: B.ByteString -> B.ByteString -> B.ByteString
repeat_call_of prev cur
    | not (B.null cur) && B.takeWhile (/=' ') cur /= "\"" = cur
    | otherwise = prev

-- | Apply a toplevel expression.
apply_toplevel :: (Derive.Callable d) => Derive.State
    -> CallInfo d -> TrackLang.Expr
    -> (Either Derive.Error (LEvent.LEvents d), [Log.Msg], Derive.Collect)
apply_toplevel state cinfo expr =
    run $ apply_transformers cinfo transform_calls $
        apply_generator cinfo generator_call
    where
    (transform_calls, generator_call) = Seq.ne_viewr expr
    run d = case Derive.run state d of
        (result, state, logs) -> (result, logs, Derive.state_collect state)

apply_generator :: forall d. (Derive.Callable d) => CallInfo d
    -> TrackLang.Call -> Derive.LogsDeriver d
apply_generator cinfo (TrackLang.Call call_id args) = do
    maybe_call <- Derive.lookup_generator call_id
    (call, vals) <- case maybe_call of
        Just call -> do
            vals <- mapM (eval cinfo) args
            return (call, vals)
        -- If I didn't find a call, look for a val call and pass its result to
        -- "".  This is what makes pitch tracks work, since scales are val
        -- calls.
        Nothing -> do
            -- Use the outer name, not val call's "val", otherwise every failed
            -- lookup says it's a failed val lookup.
            vcall <- require_call call_id (name <> " generator or val")
                =<< Derive.lookup_val_call call_id
            val <- apply (tag_call_info cinfo) vcall args
            -- We only do this fallback thing once.
            call <- get_generator fallback_call_id
            return (call, [val])

    let passed = Derive.PassedArgs
            { Derive.passed_vals = vals
            , Derive.passed_call_name = Derive.call_name call
            , Derive.passed_info = cinfo
            }
    Internal.with_stack_call (Derive.call_name call) $
        Derive.call_func call passed
    where
    name = Derive.callable_name
        (error "Derive.callable_name shouldn't evaluate its argument." :: d)

apply_transformers :: (Derive.Callable d) => CallInfo d
    -> [TrackLang.Call] -> Derive.LogsDeriver d
    -> Derive.LogsDeriver d
apply_transformers _ [] deriver = deriver
apply_transformers cinfo (TrackLang.Call call_id args : calls) deriver = do
    vals <- mapM (eval cinfo) args
    call <- get_transformer call_id
    let passed = Derive.PassedArgs
            { Derive.passed_vals = vals
            , Derive.passed_call_name = Derive.call_name call
            , Derive.passed_info = cinfo
            }
    Internal.with_stack_call (Derive.call_name call) $
        Derive.call_func call passed $ apply_transformers cinfo calls deriver

-- | Like 'apply_transformers', but apply only one, and apply to already
-- evaluated 'TrackLang.Val's.  This is useful when you want to re-apply an
-- already parsed set of vals.
--
-- TODO yuck, can I do it with less copy-paste?
reapply_transformer :: (Derive.Callable d) => CallInfo d
    -> TrackLang.CallId -> [TrackLang.Val] -> Derive.LogsDeriver d
    -> Derive.LogsDeriver d
reapply_transformer cinfo call_id vals deriver = do
    call <- get_transformer call_id
    let passed = Derive.PassedArgs
            { Derive.passed_vals = vals
            , Derive.passed_call_name = Derive.call_name call
            , Derive.passed_info = cinfo
            }
    Internal.with_stack_call (Derive.call_name call) $
        Derive.call_func call passed deriver

eval :: (Derive.ToTagged a) => Derive.CallInfo a -> TrackLang.Term
    -> Derive.Deriver TrackLang.Val
eval cinfo (TrackLang.Literal val) = eval_val cinfo val
eval cinfo (TrackLang.ValCall (TrackLang.Call call_id terms)) = do
    call <- get_val_call call_id
    apply (tag_call_info cinfo) call terms

eval_val :: (Derive.ToTagged a) => Derive.CallInfo a -> TrackLang.RawVal
    -> Derive.Deriver TrackLang.Val
eval_val cinfo val = case val of
    TrackLang.VPitchControl p ->
        TrackLang.VPitchControl <$> eval_pitch_control cinfo p
    -- Ack, I wish there were a better way.
    TrackLang.VNum a -> return $ TrackLang.VNum a
    TrackLang.VAttributes a -> return $ TrackLang.VAttributes a
    TrackLang.VControl a -> return $ TrackLang.VControl a
    TrackLang.VPitch a -> return $ TrackLang.VPitch a
    TrackLang.VInstrument a -> return $ TrackLang.VInstrument a
    TrackLang.VSymbol a -> return $ TrackLang.VSymbol a
    TrackLang.VNotGiven -> return $ TrackLang.VNotGiven

eval_pitch_control :: (Derive.ToTagged a) => Derive.CallInfo a
    -> TrackLang.RawPitchControl -> Derive.Deriver TrackLang.PitchControl
eval_pitch_control cinfo c = case c of
    TrackLang.ControlSignal note ->
        TrackLang.ControlSignal <$> eval_pitch note
    TrackLang.DefaultedControl ctl note ->
        TrackLang.DefaultedControl ctl <$> eval_pitch note
    TrackLang.LiteralControl ctl -> return $ TrackLang.LiteralControl ctl
    where
    eval_pitch call = fmap PitchSignal.constant $
        Sig.cast ("eval_pitch_control " <> ShowVal.show_val call)
            =<< eval cinfo (TrackLang.ValCall call)

apply :: Derive.CallInfo Derive.Tagged -> Derive.ValCall
    -> [TrackLang.Term] -> Derive.Deriver TrackLang.Val
apply cinfo call args = do
    vals <- mapM (eval cinfo) args
    let passed = Derive.PassedArgs
            { Derive.passed_vals = vals
            , Derive.passed_call_name = Derive.vcall_name call
            , Derive.passed_info = cinfo
            }
    Derive.with_msg ("val call " <> Derive.vcall_name call) $
        Derive.vcall_call call passed

-- | Tag the polymorphic part of the CallInfo so it can be given to
-- a 'Derive.ValCall'.  Otherwise, ValCall would have to be polymorphic too,
-- which means it would hard to write generic ones.
tag_call_info :: (Derive.ToTagged a) => Derive.CallInfo a
    -> Derive.CallInfo Derive.Tagged
tag_call_info cinfo = cinfo
    { Derive.info_prev_val =
        second Derive.to_tagged <$> Derive.info_prev_val cinfo
    }

event_start :: Derive.CallInfo d -> ScoreTime
event_start = Event.start . Derive.info_event

get_val_call :: TrackLang.CallId -> Derive.Deriver Derive.ValCall
get_val_call call_id =
    require_call call_id "val" =<< Derive.lookup_val_call call_id

get_generator :: forall d. (Derive.Callable d) =>
    TrackLang.CallId -> Derive.Deriver (Derive.Generator d)
get_generator call_id =
    require_call call_id (name <> " generator")
        =<< Derive.lookup_generator call_id
    where
    name = Derive.callable_name
        (error "Derive.callable_name shouldn't evaluate its argument." :: d)

get_transformer :: forall d. (Derive.Callable d) =>
    TrackLang.CallId -> Derive.Deriver (Derive.Transformer d)
get_transformer call_id =
    require_call call_id (name <> " transformer")
        =<< Derive.lookup_transformer call_id
    where
    name = Derive.callable_name
        (error "Derive.callable_name shouldn't evaluate its argument." :: d)

require_call :: TrackLang.CallId -> Text -> Maybe a -> Derive.Deriver a
require_call _ _ (Just a) = return a
require_call call_id name Nothing = do
    -- If the call wasn't found, it can be seen as a block call whose block
    -- doesn't exist yet.  If it is created later, I have to know that this
    -- block depends on it, otherwise it won't be rederived and hence won't
    -- realize that the bad call is now valid.
    -- TODO but just for note calls!
    block_id <- symbol_to_block_id call_id
    whenJust block_id Internal.add_block_dep
    Derive.throw $ untxt (unknown_call_id name call_id)

unknown_call_id :: Text -> TrackLang.CallId -> Text
unknown_call_id name (TrackLang.Symbol sym) =
    name <> " not found: " <> sym

fallback_call_id :: TrackLang.CallId
fallback_call_id = ""

-- | Given a CallId, try to come up with the BlockId of the block it could be
-- a call for.
symbol_to_block_id :: TrackLang.CallId -> Derive.Deriver (Maybe BlockId)
symbol_to_block_id sym
    | sym == "" = return Nothing
    | otherwise = do
        ui_state <- Derive.get_ui_state id
        let ns = State.config_namespace (State.state_config ui_state)
        return $ make_block_id ns sym

make_block_id :: Id.Namespace -> TrackLang.Symbol -> Maybe BlockId
make_block_id namespace (TrackLang.Symbol call) =
    Types.BlockId <$> Id.read_short namespace (untxt call)
