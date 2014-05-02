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
module Derive.Call (
    -- * eval
    eval_one, eval_one_call, eval_one_at
    , eval_event, reapply_gen, reapply_gen_normalized
    , reapply, reapply_string, reapply_call, eval_pitch, apply_pitch
    , eval_expr, apply_transform

    -- * derive_track
    , TrackInfo(..)
    , pitch_last_sample, control_last_sample
    , derive_control_track, derive_note_track
    , defragment_track_signals, unwarp

    -- * eval / apply
    , eval
    , reapply_generator, apply_transformers, reapply_transformer

    -- * lookup call
    , unknown_call_id, symbol_to_block_id, is_relative_call

    -- * misc
    , cast
) where
import qualified Data.Char as Char
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Text as Text

import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Ui.Event as Event
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree

import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.LEvent as LEvent
import qualified Derive.Parse as Parse
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Slice as Slice
import qualified Derive.TrackLang as TrackLang

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Types


type CallInfo d = Derive.CallInfo (Derive.Elem d)
type PassedArgs d = Derive.PassedArgs (Derive.Elem d)

-- * eval

-- | Evaluate a single note as a generator.  Fake up an event with no prev or
-- next lists.
eval_one :: Derive.Callable d => Bool -> TrackLang.Expr -> Derive.LogsDeriver d
eval_one collect = eval_one_at collect 0 1

eval_one_call :: Derive.Callable d => Bool -> TrackLang.Call
    -> Derive.LogsDeriver d
eval_one_call collect = eval_one collect . (:| [])

eval_one_at :: Derive.Callable d => Bool -> ScoreTime -> ScoreTime
    -> TrackLang.Expr -> Derive.LogsDeriver d
eval_one_at collect start dur expr = eval_expr collect cinfo expr
    where
    -- Set the event start and duration instead of using Derive.place since
    -- this way I can have zero duration events.
    cinfo = Derive.dummy_call_info start dur $
        "eval_one: " <> ShowVal.show_val expr

-- | Like 'derive_event' but evaluate the event outside of its track context.
-- This is useful if you want to evaluate things out of order, i.e. evaluate
-- the /next/ pitch.
eval_event :: Derive.Callable d => Event.Event
    -> Derive.Deriver (Either String (LEvent.LEvents d))
eval_event event = case Parse.parse_expr (Event.event_text event) of
    Left err -> return $ Left err
    Right expr -> Right <$>
        -- TODO eval it separately to catch any exception?
        eval_one_at False (Event.start event) (Event.duration event) expr

-- | Evaluate a generator, reusing the passed args but replacing the CallId.
-- Generators can use this to delegate to other generators.
reapply_gen :: Derive.Callable d => PassedArgs d -> TrackLang.CallId
    -> Derive.LogsDeriver d
reapply_gen args call_id = do
    let cinfo = Derive.passed_info args
    -- As documented in 'reapply_generator', I need the expr that
    -- (call_id, passed_vals) corresponds to.  Since I'm reusing an existing
    -- call, it's probably safe to reuse its expr, swapping out the call_id.
    expr <- Derive.require_right ("reapply_gen: unparseable info_expr: "<>) $
        replace_generator call_id (Derive.info_expr cinfo)
    reapply_generator cinfo call_id (Derive.passed_vals args) expr

-- | Like 'reapply_gen', but the note is given normalized time, 0--1, instead
-- of inheriting the start and duration from the args.  This is essential if
-- you want to shift or stretch the note.
reapply_gen_normalized :: Derive.Callable d => PassedArgs d -> TrackLang.CallId
    -> Derive.LogsDeriver d
reapply_gen_normalized args = reapply_gen $ args
    { Derive.passed_info = cinfo
        { Derive.info_event = (Derive.info_event cinfo)
            { Event.start = 0
            , Event.duration = 1
            }
        , Derive.info_event_end = 1
        }
    }
    where cinfo = Derive.passed_info args

replace_generator :: TrackLang.CallId -> Event.Text -> Either String Event.Text
replace_generator call_id = fmap replace . Parse.parse_expr
    where
    replace = ShowVal.show_val . TrackLang.map_generator (const call_id)

-- | Apply an expr with the current call info.  This discards the parsed
-- arguments in the 'PassedArgs' since it gets args from the 'TrackLang.Expr'.
reapply :: (Derive.Callable d) => PassedArgs d -> TrackLang.Expr
    -> Derive.LogsDeriver d
reapply args = eval_expr False (Derive.passed_info args)

-- | Like 'reapply', but parse the string first.
reapply_string :: (Derive.Callable d) => PassedArgs d -> Text
    -> Derive.LogsDeriver d
reapply_string args s = case Parse.parse_expr s of
    Left err -> Derive.throw $ "parse error: " ++ err
    Right expr -> reapply args expr

reapply_call :: (Derive.Callable d) => PassedArgs d -> TrackLang.Symbol
    -> [TrackLang.Term] -> Derive.LogsDeriver d
reapply_call args call_id call_args =
    reapply args (TrackLang.call call_id call_args :| [])

-- | A version of 'eval' specialized to evaluate pitch calls.
eval_pitch :: ScoreTime -> TrackLang.PitchCall
    -> Derive.Deriver PitchSignal.Pitch
eval_pitch pos call =
    cast ("eval pitch " <> ShowVal.show_val call)
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

-- | Evaluate a single expression, catching an exception if it throws.
eval_expr :: Derive.Callable d => Bool -> CallInfo d -> TrackLang.Expr
    -> Derive.LogsDeriver d
eval_expr collect cinfo expr =
    fromMaybe [] <$> Derive.catch collect (apply_toplevel cinfo expr)

-- | Parse and apply a transform expression.
apply_transform :: (Derive.Callable d) => Text -> Text
    -> Derive.LogsDeriver d -> Derive.LogsDeriver d
apply_transform name expr_str deriver
    | Text.all Char.isSpace expr_str = deriver
    | otherwise = do
        expr <- case Parse.parse_expr expr_str of
            Left err -> Derive.throw $ untxt name ++ ": " ++ err
            Right expr -> return expr
        let info = Derive.dummy_call_info 0 1 name
        apply_transformers info (NonEmpty.toList expr) deriver

-- * derive_track

-- | Per-track parameters, to cut down on the number of arguments taken by
-- 'derive_track'.
data TrackInfo = TrackInfo {
    tinfo_track :: !TrackTree.Track
    , tinfo_sub_tracks :: !TrackTree.EventsTree
    , tinfo_type :: !ParseTitle.Type
    } deriving (Show)

instance Pretty.Pretty TrackInfo where
    format (TrackInfo track subs ttype) = Pretty.record_title "TrackInfo"
        [ ("track", Pretty.format track)
        , ("sub_tracks", Pretty.format subs)
        , ("type", Pretty.format ttype)
        ]

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

-- | This is the toplevel function to derive control tracks.  It's responsible
-- for actually evaluating each event.
derive_control_track :: forall d. Derive.Callable d =>
    Derive.State -> TrackInfo -> GetLastSample d
    -> [Event.Event] -> ([LEvent.LEvents d], Derive.Collect)
derive_control_track = derive_track_ derive_section
    where derive_section _ _ _ deriver = deriver

-- | Derive a note track.  This is different from 'derive_control_track' in
-- two ways: it doesn't keep track of the previous sample, and it evaluates
-- orphans.
--
-- Orphans are uncovered events in note tracks in the sub-tracks.  They are
-- extracted with 'Slice.checked_slice_notes' and evaluated as-is.  The effect
-- is that note transformers can be stacked horizontally, and tracks left empty
-- have no effect, except whatever transformers they may have in their titles.
derive_note_track :: (TrackTree.EventsTree -> Derive.NoteDeriver)
    -> Derive.State -> TrackInfo -> [Event.Event]
    -> ([[LEvent.LEvent Score.Event]], Derive.Collect)
derive_note_track derive_tracks state tinfo =
    derive_track_ derive_section state tinfo get_last_sample
    where
    get_last_sample _ _ = Nothing
    derive_section tinfo prev events deriver =
        maybe id (<>) (get_sub_notes tinfo prev events) deriver
    get_sub_notes tinfo prev events =
        derive_sub_notes derive_tracks prev
            (maybe (TrackTree.track_end (tinfo_track tinfo)) Event.start
                (Seq.head events))
            (tinfo_sub_tracks tinfo)

-- | Awkwardly factor out the common parts of 'derive_control_track' and
-- 'derive_note_track'.
derive_track_ :: forall d. Derive.Callable d =>
    -- forall and ScopedTypeVariables needed for the inner 'go' signature
    (TrackInfo -> Maybe Event.Event -> [Event.Event]
        -> Derive.LogsDeriver d -> Derive.LogsDeriver d)
    -> Derive.State -> TrackInfo -> GetLastSample d
    -> [Event.Event] -> ([LEvent.LEvents d], Derive.Collect)
derive_track_ derive_section state tinfo get_last_sample =
    go (record state) Nothing []
    where
    record = record_track_dynamic (not (null (tinfo_sub_tracks tinfo)))
        (tinfo_track tinfo)
    -- This threads the collect through each event.  I would prefer to map and
    -- mconcat, but profiling showed that to be quite a bit slower.
    go :: Derive.Collect -> PrevVal d -> [Event.Event] -> [Event.Event]
        -> ([LEvent.LEvents d], Derive.Collect)
    go collect prev_sample prev events =
        -- Without 'seq', the events are parsed in reverse order, presumably
        -- because the collect is forced to whnf when stashing it in the
        -- Derive.State.  The out of order lasts only until the
        -- get_generator call_id, but presumably creates a bit of drag.
        -- I don't know if it's a significant amount, but in any case this
        -- might get rid of it.
        (levents : rest_events, events `seq` final_collect)
        where
        (result, out_state, logs) = Derive.run
            (state { Derive.state_collect = collect }) $
                derive_section tinfo (Seq.head prev) events derive
        derive = case events of
            [] -> return []
            cur : rest -> derive_event tinfo prev_sample prev cur rest
        (rest_events, final_collect) = case events of
            [] -> ([], Derive.state_collect out_state)
            cur : rest -> go (Derive.state_collect out_state) next_sample
                (cur : prev) rest
        levents = map LEvent.Log logs ++ case result of
            Right stream -> stream
            Left err -> [LEvent.Log (Derive.error_to_warn err)]
        next_sample = get_last_sample prev_sample result

derive_sub_notes :: (TrackTree.EventsTree -> Derive.NoteDeriver)
    -> Maybe Event.Event -> TrackTime -> TrackTree.EventsTree
    -> Maybe Derive.NoteDeriver
    -- ^ The Maybe is a micro-optimization to avoid returning 'mempty'.  This
    -- is because 'Derive.d_merge' doesn't know that one of its operands is
    -- empty, and does all the splitting of and restoring collect bother.
    -- I expect lots of empties here so maybe it makes a difference.
derive_sub_notes derive_tracks prev end subs
    | start == end = Nothing
    | otherwise = case concat <$> sliced of
        Left err -> Just $ Log.warn err >> return []
        Right [] -> Nothing
        Right notes -> Just $ mconcatMap place notes
    where
    sliced = Slice.checked_slice_notes exclude_start start end subs
    exclude_start = maybe False ((==0) . Event.duration) prev
    start = maybe 0 Event.end prev
    -- The events have been shifted back to 0 by 'Slice.slice_notes', but are
    -- still their original lengths.
    place (shift, _, tree) = Derive.at shift $ derive_tracks tree

-- Notes on recording TrackDynamic at NOTE [record-track-dynamics].
record_track_dynamic :: Bool -> TrackTree.Track -> Derive.State
    -> Derive.Collect
record_track_dynamic _has_children track state
    -- Use the controls from the parent track.
    -- How to get them?  The parent can see the children, but how does the
    -- sliced child know who its parent was?
    -- When the parent finishes, its children should already be recorded, so
    -- it could replace them.
    | TrackTree.track_sliced track = collect $
        -- Debug.tracep ("sliced: " ++ show (TrackTree.track_inverted track)) $
        Internal.record_track_dynamic $
        dyn { Derive.state_controls = mempty }
    | otherwise = collect $
        -- Debug.tracep "unsliced" $
        Internal.record_track_dynamic dyn
    -- where dyn = Derive.state_dynamic state
    where
    -- k = (TrackTree.track_block_id track, TrackTree.track_track_id track)
    -- dyn = Debug.traceps "record" k $ Derive.state_dynamic state
    dyn = Derive.state_dynamic state
    collect = maybe mempty (\d -> mempty { Derive.collect_track_dynamic = d })

defragment_track_signals :: Score.Warp -> Derive.Collect -> Derive.Collect
defragment_track_signals warp collect
    | Map.null fragments = collect
    | otherwise = collect
        { Derive.collect_track_signals = Derive.collect_track_signals collect
            <> Map.map defragment fragments
        , Derive.collect_signal_fragments = mempty
        }
    where
    fragments = Derive.collect_signal_fragments collect
    defragment = unwarp warp . Signal.merge

unwarp :: Score.Warp -> Signal.Control -> Track.TrackSignal
unwarp warp control = case is_linear_warp warp of
    Just (shift, stretch) ->
        Track.TrackSignal (Signal.coerce control) shift stretch
    Nothing -> Track.TrackSignal unwarped 0 1
        where
        Score.Warp warp_sig shift stretch = warp
        unwarped = Signal.unwarp_fused warp_sig (RealTime.score shift)
            (RealTime.score stretch) control

-- | Return (shift, stretch) if the tempo is linear.  This relies on an
-- optimization in 'Derive.d_tempo' to notice when the tempo is constant and
-- give it 'Score.id_warp_signal'.
is_linear_warp :: Score.Warp -> Maybe (ScoreTime, ScoreTime)
is_linear_warp warp
    | Score.warp_signal warp == Score.id_warp_signal =
        Just (Score.warp_shift warp, Score.warp_stretch warp)
    | otherwise = Nothing

derive_event :: Derive.Callable d => TrackInfo -> PrevVal d
    -> [Event.Event] -- ^ previous events, in reverse order
    -> Event.Event -- ^ cur event
    -> [Event.Event] -- ^ following events
    -> Derive.LogsDeriver d
derive_event tinfo prev_sample prev event next
    | "--" `Text.isPrefixOf` Text.dropWhile (==' ') text = return []
    | otherwise = case Parse.parse_expr text of
        Left err -> Log.warn err >> return []
        Right expr -> Internal.with_stack_region (Event.min event + shifted)
            (Event.max event + shifted) $ apply_toplevel cinfo expr
    where
    shifted = TrackTree.track_shifted track
    text = Event.event_text event
    cinfo = Derive.CallInfo
        { Derive.info_expr = text
        , Derive.info_prev_val = prev_sample
        , Derive.info_event = event
        -- Augment prev and next with the unevaluated "around" notes from
        -- 'State.track_around'.
        , Derive.info_prev_events = tprev ++ prev
        , Derive.info_next_events = next ++ tnext
        , Derive.info_event_end = case next ++ tnext of
            [] -> TrackTree.track_end track
            event : _ -> Event.start event
        , Derive.info_track_shifted = TrackTree.track_shifted track
        , Derive.info_inverted = TrackTree.track_inverted track
        , Derive.info_sub_tracks = subs
        , Derive.info_sub_events = Nothing
        , Derive.info_track_type = Just ttype
        }
    TrackInfo track subs ttype = tinfo
    (tprev, tnext) = TrackTree.track_around track

-- * eval / apply

-- | Apply a toplevel expression.
apply_toplevel :: Derive.Callable d => CallInfo d -> TrackLang.Expr
    -> Derive.LogsDeriver d
apply_toplevel cinfo expr = apply_transformers cinfo transform_calls $
    apply_generator cinfo generator_call
    where (transform_calls, generator_call) = Seq.ne_viewr expr

apply_generator :: forall d. Derive.Callable d => CallInfo d
    -> TrackLang.Call -> Derive.LogsDeriver d
apply_generator cinfo (TrackLang.Call call_id args) = do
    vals <- mapM (eval cinfo) args
    reapply_generator cinfo call_id vals (Derive.info_expr cinfo)

-- | Like 'apply_generator', but for when the args are already parsed and
-- evaluated.  This is useful when one generator wants to dispatch to another.
--
-- In addition to a call and args, this also requires the expression that
-- the call and args represents.  The reason is complicated and annoying:
-- 'Derive.info_expr' must have the expression currently being evaluated,
-- because it's used later by inversion.  Since I'm evaluating a new call_id
-- and args, and the cinfo is likely reused from another call, the @info_expr@
-- is probably wrong.  Unfortunately, I can't "unparse" a call_id and args back
-- to an expr, because pitches are code and can't necessarily be returned to
-- the expression from whence they sprang.
--
-- The reason @info_expr@ is unparsed text is also thanks to pitch signal
-- expressions.  Maybe I should get rid of them?
reapply_generator :: (Derive.Callable d) => CallInfo d
    -> TrackLang.CallId -> [TrackLang.Val] -> Event.Text -> Derive.LogsDeriver d
reapply_generator cinfo call_id args expr = do
    call <- get_generator call_id
    let passed = Derive.PassedArgs
            { Derive.passed_vals = args
            , Derive.passed_call_name = Derive.call_name call
            , Derive.passed_info = cinfo { Derive.info_expr = expr  }
            }
    Internal.with_stack_call (Derive.call_name call) $
        Derive.call_func call passed

apply_transformers :: (Derive.Callable d) => CallInfo d
    -> [TrackLang.Call] -> Derive.LogsDeriver d
    -> Derive.LogsDeriver d
apply_transformers _ [] deriver = deriver
apply_transformers cinfo (TrackLang.Call call_id args : calls) deriver = do
    vals <- mapM (eval cinfo) args
    call <- get_transformer call_id
    let under_invert = Derive.cdoc_tags (Derive.call_doc call)
            `Tags.contains` Tags.under_invert
        inverted = Derive.info_inverted cinfo
        -- If there are no subs and I'm not inverted, then inversion won't
        -- happen, so I'd better run it anyway.
        skip = not $ inverted == under_invert
            || null (Derive.info_sub_tracks cinfo) && not inverted
    let passed = Derive.PassedArgs
            { Derive.passed_vals = vals
            , Derive.passed_call_name = Derive.call_name call
            , Derive.passed_info = cinfo
            }
    let rest = apply_transformers cinfo calls deriver
    if skip then rest
        else Internal.with_stack_call (Derive.call_name call) $
            Derive.call_func call passed rest

-- | The transformer version of 'reapply_generator'.  Like
-- 'apply_transformers', but apply only one, and apply to already
-- evaluated 'TrackLang.Val's.  This is useful when you want to re-apply an
-- already parsed set of vals.
reapply_transformer :: (Derive.Callable d) => CallInfo d
    -> TrackLang.CallId -> [TrackLang.Val] -> Derive.LogsDeriver d
    -> Derive.LogsDeriver d
reapply_transformer cinfo call_id args deriver = do
    call <- get_transformer call_id
    let passed = Derive.PassedArgs
            { Derive.passed_vals = args
            , Derive.passed_call_name = Derive.call_name call
            , Derive.passed_info = cinfo
            }
    Internal.with_stack_call (Derive.call_name call) $
        Derive.call_func call passed deriver

eval :: (Derive.ToTagged a) => Derive.CallInfo a -> TrackLang.Term
    -> Derive.Deriver TrackLang.Val
eval _ (TrackLang.Literal val) = return val
eval cinfo (TrackLang.ValCall (TrackLang.Call call_id terms)) = do
    call <- get_val_call call_id
    apply (Derive.tag_call_info cinfo) call terms

apply :: Derive.CallInfo Derive.Tagged -> Derive.ValCall
    -> [TrackLang.Term] -> Derive.Deriver TrackLang.Val
apply cinfo call args = do
    vals <- mapM (eval cinfo) args
    let passed = Derive.PassedArgs
            { Derive.passed_vals = vals
            , Derive.passed_call_name = Derive.vcall_name call
            , Derive.passed_info = cinfo
            }
    Derive.vcall_call call passed

-- * lookup call

get_val_call :: TrackLang.CallId -> Derive.Deriver Derive.ValCall
get_val_call call_id =
    require_call False call_id "val call" =<< Derive.lookup_val_call call_id

get_generator :: forall d. (Derive.Callable d) =>
    TrackLang.CallId -> Derive.Deriver (Derive.Generator d)
get_generator call_id =
    require_call True call_id (name <> " generator")
        =<< Derive.lookup_generator call_id
    where name = Derive.callable_name (Proxy :: Proxy d)

get_transformer :: forall d. (Derive.Callable d) =>
    TrackLang.CallId -> Derive.Deriver (Derive.Transformer d)
get_transformer call_id =
    require_call False call_id (name <> " transformer")
        =<< Derive.lookup_transformer call_id
    where name = Derive.callable_name (Proxy :: Proxy d)

require_call :: Bool -> TrackLang.CallId -> Text -> Maybe a -> Derive.Deriver a
require_call _ _ _ (Just a) = return a
require_call is_generator call_id name Nothing = do
    -- If the call wasn't found, it can be seen as a block call whose block
    -- doesn't exist yet.  If it is created later, I have to know that this
    -- block depends on it, otherwise it won't be rederived and hence won't
    -- realize that the bad call is now valid.
    when is_generator $ do
        caller <- Internal.lookup_current_block_id
        ns <- Derive.get_ui_state $ State.config_namespace . State.state_config
        whenJust (symbol_to_block_id ns caller call_id) Internal.add_block_dep
    Derive.throw $ untxt (unknown_call_id name call_id)

unknown_call_id :: Text -> TrackLang.CallId -> Text
unknown_call_id name (TrackLang.Symbol sym) = name <> " not found: " <> sym

-- | Given a CallId, try to come up with the BlockId of the block it could be
-- a call for.
symbol_to_block_id :: Id.Namespace -> Maybe BlockId
    -- ^ If the symbol starts with ., this block is prepended to it.
    -> TrackLang.CallId -> Maybe BlockId
symbol_to_block_id ns maybe_caller sym
    | sym == "" = Nothing
    | otherwise = Just $ Id.BlockId $ Id.read_short ns relative
    where
    relative
        | Just caller <- maybe_caller, is_relative_call sym =
            Id.ident_text caller <> TrackLang.unsym sym
        | otherwise = TrackLang.unsym sym

is_relative_call :: TrackLang.CallId -> Bool
is_relative_call (TrackLang.Symbol sym) = "." `Text.isPrefixOf` sym

-- * misc

-- | Cast a Val to a haskell val, or throw if it's the wrong type.
cast :: forall a. TrackLang.Typecheck a => Text -> TrackLang.Val
    -> Derive.Deriver a
cast name val = case TrackLang.from_val val of
    Nothing -> Derive.throw $ untxt $
        name <> ": expected " <> prettyt return_type
        <> " but val was " <> prettyt (TrackLang.type_of val)
        <> " " <> TrackLang.show_val val
    Just a -> return a
    where return_type = TrackLang.to_type (Proxy :: Proxy a)
