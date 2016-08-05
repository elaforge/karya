-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Postprocs that change note start and duration.
module Derive.Call.Post.Postproc where
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe

import qualified Util.ApproxEq as ApproxEq
import qualified Util.Map
import qualified Util.Seq as Seq

import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Prelude.ControlFunction as ControlFunction
import qualified Derive.Call.Prelude.Equal as Equal
import qualified Derive.Call.Prelude.Note as Note
import qualified Derive.Call.StaticMacro as StaticMacro
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Flags as Flags
import qualified Derive.LEvent as LEvent
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Global
import Types


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.transformer_call_map
    [ ("apply-start-offset", c_apply_start_offset)
    , ("cancel", c_cancel)
    , ("randomize-start", c_randomize_start)
    ]

-- * cancel

c_cancel :: Derive.Transformer Derive.Note
c_cancel = Derive.transformer Module.prelude "cancel" Tags.postproc
    "Process the 'Derive.Flags.strong' and 'Derive.Flags.weak' flags.\
    \ This will cause notes to be dropped."
    $ make_cancel (cancel_strong_weak infer_duration_merged) Post.hand_key

-- | Given a set of coincident notes, return either an error, or merge them
-- into a set of output notes.
type Cancel = [Score.Event] -> Either Text [Score.Event]

-- | The key identifies another event which is in the same voice.  This could
-- be 'Post.hand_key', but it could also match polos to sangsih, since they
-- form a composite part.
type Key key = Score.Event -> key

make_cancel :: Ord key => Cancel -> Key key
    -> Derive.WithArgDoc (Derive.TransformerF Derive.Note)
make_cancel cancel key =
    Sig.callt final_duration_arg $ \final_dur _args deriver ->
        Derive.require_right id . group_and_cancel cancel key final_dur
            =<< deriver

final_duration_arg :: Sig.Parser RealTime
final_duration_arg = Sig.defaulted_env "final-duration" Sig.Unprefixed 1
    "If there is no following note, infer this duration."

group_and_cancel :: Ord key => Cancel -> Key key -> RealTime
    -> Events -> Either Text Events
group_and_cancel cancel key final_dur =
    fmap (infer_duration_single key final_dur . suppress_notes)
    . merge_groups cancel . group_coincident key

-- | Merge notes with 'Flags.strong' and 'Flags.weak'.  The rules are that
-- strong notes merge with weaker ones, in the order strong, normal, weak.
--
-- Previously I considered multiple weaks or strongs ambiguous, but it turns
-- out I get multiple strongs with two hand strokes at the end of a block,
-- and I might as well allow the rest too, for simplicity.
cancel_strong_weak :: (Score.Event -> [Score.Event] -> Score.Event)
    -> [Score.Event] -> Either Text [Score.Event]
cancel_strong_weak merge events = case partition events of
    (strongs@(_:_), weaks, normals) ->
        Right [merge strong (normals ++ weaks) | strong <- strongs]
    ([], weaks, normals@(_:_)) -> Right [merge normal weaks | normal <- normals]
    ([], weaks, []) -> Right weaks
    where
    partition = Seq.partition2 (Score.has_flags Flags.strong)
        (Score.has_flags Flags.weak)

-- | Handle 'Flags.infer_duration' for notes merged together.  This is the case
-- where a final note replaces a coincident initial note.  The strong note gets
-- the duration of the longest weak notes, if there are any.  If there are no
-- weaks, then there are no coincedent notes to merge, so return the event
-- unchanged so 'infer_duration_single' can handle it.
infer_duration_merged :: Score.Event -> [Score.Event] -> Score.Event
infer_duration_merged strong weaks =
    case Seq.maximum (map Score.event_end weaks) of
        Just end | Score.has_flags Flags.infer_duration strong ->
            Score.add_log ("set duration to max of weak notes: "
                <> pretty (map Score.event_end weaks)) $
            Score.remove_flags Flags.infer_duration $
            Score.set_duration (end - Score.event_start strong) strong
        _ -> strong

-- | Handle 'Flags.infer_duration' for a note by itself.  When there is no
-- coincedent note to replace, the duration extends to the start of the next
-- matching event, according to the 'Key'.
--
-- This actually finds the next matching event which starts later than this
-- one.  Normally notes of the same key are not expected to occur
-- simultaneously, but may still do so, for example pasang parts which are
-- normally considered a single voice but may still contain unison or kempyung.
infer_duration_single :: Eq key => Key key -> RealTime
    -> Stream.Stream Score.Event -> Stream.Stream Score.Event
infer_duration_single key final_dur = Post.emap1_ infer . Post.nexts_by key id
    where
    infer (event, _) | not (Score.has_flags Flags.infer_duration event) = event
    infer (event, nexts) =
        Score.remove_flags Flags.infer_duration $ case next of
            Just next -> Score.add_log "set duration to next start" $
                set_end (Score.event_start next) event
            Nothing -> Score.add_log "set duration to final dur" $
                Score.set_duration final_dur event
        where
        next = List.find ((> Score.event_start event) . Score.event_start) nexts

    set_end end event = Score.set_duration (end - Score.event_start event) event

merge_groups :: ([a] -> Either Text [a]) -> [Either [LEvent.LEvent a] [a]]
    -> Either Text (Stream.Stream a)
merge_groups merge = fmap Stream.from_sorted_list . concatMapM go
    where
    go (Left ungrouped) = Right ungrouped
    go (Right []) = Right []
    go (Right [e]) = Right [LEvent.Event e]
    go (Right es) = map LEvent.Event <$> merge es

type Events = Stream.Stream Score.Event

-- | Group events with the same start time.  Events in Left are not grouped.
group_coincident :: Ord key => (Score.Event -> key) -> Events
    -> [Either [LEvent.LEvent Score.Event] [Score.Event]]
group_coincident key = go . Stream.to_list
    where
    go [] = []
    go (log@(LEvent.Log {}) : es) = Left [log] : go es
    go (LEvent.Event e : es) =
        (if null logs then id else (Left (map LEvent.Log logs) :)) $
            groups ++ go after
        where
        ((during, logs), after) = first LEvent.partition $
            span (LEvent.log_or $ same_start e) es
        -- [e] is going to be a common case, since most notes don't group.
        groups = map Right (Seq.group_sort key (e : during))
    same_start e1 e2 = Score.event_start e1 RealTime.== Score.event_start e2

-- | Filter out events that fall at and before the 'EnvKey.suppress_until'
-- range of an event with the same (instrument, hand).  Only events that don't
-- have a suppress_until are suppressed.
--
-- This is complicated by the fact that an event should suppress coincident
-- events even if the supressor follows the suppressee in the list, so I have
-- to look into the future for the greatest suppress_until.
suppress_notes :: Stream.Stream Score.Event -> Stream.Stream Score.Event
suppress_notes =
    snd . Post.emap go Map.empty . Stream.zip_on Post.nexts
        . Stream.zip3_on (map Post.hand_key) (map get_suppress)
    where
    go suppressed (nexts, (key, suppress, event)) = case suppress of
        Nothing -> (,) suppressed $ case suppress_until of
            Just until | until >= Score.event_start event - RealTime.eta -> []
            _ -> [event]
        Just until -> (Map.insert key until suppressed, [event])
        where
        suppress_until = Seq.maximum $ Maybe.catMaybes $
            (Map.lookup key suppressed :) $ map suppress_of $
            takeWhile (coincident . event_of) $
            filter ((==key) . key_of) nexts
        coincident e = Score.event_start e
            <= Score.event_start event + RealTime.eta
    get_suppress :: Score.Event -> Maybe RealTime
    get_suppress = Env.maybe_val EnvKey.suppress_until . Score.event_environ
    key_of (k, _, _) = k
    suppress_of (_, s, _) = s
    event_of (_, _, e) = e

{- | A note with inferred duration gets its start from the end of the previous
    block, but its duration and the rest of its controls come from the
    corresponding note at the beginning of the next block.

    TODO currently this is unused.  Formerly it was used when an infer-duration
    note replaced a note.  The intent was that the strong note at the end of
    the block would determine the initial pitch and dynamic of the note, but
    control curves would still be picked up from the replaced note, since there
    is no room to put them on the final note.  It seems a little ad-hoc and
    grody, but it still makes a kind of sense and I may still want it, so I'll
    leave this function here for now.
-}
replace_note :: Score.Event -> Score.Event -> Score.Event
replace_note next event = event
    { Score.event_duration = Score.event_end next - start
    , Score.event_untransformed_pitch = pitch event
        <> PSignal.drop_before_at start (pitch next)
    , Score.event_untransformed_pitches = Util.Map.mappend
        (pitches event)
        (PSignal.drop_before_at start <$> pitches next)
    , Score.event_untransformed_controls = Util.Map.mappend
        (controls event)
        (fmap (Signal.drop_before_at start) <$> controls next)
    }
    where
    pitch = Score.event_transformed_pitch
    pitches = Score.event_transformed_pitches
    controls = Score.event_transformed_controls
    start = Score.event_start event

-- * apply start offset

c_randomize_start :: Derive.Transformer Derive.Note
c_randomize_start = StaticMacro.check "c_randomize_start" $
    StaticMacro.transformer Module.prelude "randomize-start" Tags.postproc ""
        -- apply-start-offset | %start-s = (cf-rnd-a+ $)
        [ StaticMacro.Call c_apply_start_offset []
        , StaticMacro.Call Equal.c_equal
            [ StaticMacro.literal (ShowVal.show_val Controls.start_s)
            , StaticMacro.call (ControlFunction.c_cf_rnd_around (+))
                [StaticMacro.Var]
            ]
        ]

{- | Previously I applied the @%start-s@ and @%start-t@ controls in the note
    generator, but I wound up with notes getting out of sync with their
    controls.  Even if I apply the controls before inversion, it still doesn't
    work other calls, like say block calls, and I can't apply the controls
    before the block call
-}
c_apply_start_offset :: Derive.Transformer Derive.Note
c_apply_start_offset =
    Derive.transformer Module.prelude "apply-start-offset" Tags.postproc
    ("Apply the " <> ShowVal.doc EnvKey.start_offset_val <> " env var.\
     \ This is set by note deriver from the "
     <> ShowVal.doc Controls.start_s <> " and "
     <> ShowVal.doc Controls.start_t <> " controls, so if you want those\
     \ controls to have an effect, you have to use this postproc."
    ) $ Sig.callt (
        Sig.defaulted "min-duration" Nothing "If given, notes on the same hand\
            \ won't be moved closer than this time. Otherwise, hand and\
            \ instrument is ignored."
    ) $ \min_dur _args deriver -> apply_start_offset min_dur <$> deriver

apply_start_offset :: Maybe RealTime -> Stream.Stream Score.Event
    -> Stream.Stream Score.Event
apply_start_offset maybe_min_dur =
    apply_offset . tweak_offset . Stream.zip_on (map offset_of)
    where
    tweak_offset = case maybe_min_dur of
        Nothing -> id
        Just min_dur ->
            Post.emap1_ (tweak min_dur) . Post.neighbors_by Post.hand_key snd
    tweak min_dur (prev, (offset, event), next) = (new_offset, event)
        where
        new_offset = adjust_offset min_dur (extract <$> prev) (extract <$> next)
            offset (Score.event_start event)
        extract (offset, event) = (offset, Score.event_start event)

    apply_offset = Post.emap1_ord_ apply . Post.neighbors_by Post.hand_key snd
    apply (_, (offset, event), maybe_next) =
        set_dur $ Score.move_start (fromMaybe Note.min_duration maybe_min_dur)
            offset event
        where
        set_dur event = case maybe_next of
            Nothing -> event
            Just (next_offset, next) -> Score.duration (const dur) event
                where
                dur = adjust_duration (Score.event_start next)
                    (Score.event_start next + next_offset) event

{- | Conceptually, all notes move together until they bump into each
    other.  Or, they move without restriction, and then go to midway of the
    overlap.  But the note's start is a hard lower or upper limit, so one note
    moving can never cause another note to move, it can just cause it to not
    move as much as it wanted.

    TODO actually "half of the overlap" is not the same as "all move together".
    For the latter, the overlap split depends on how far the note moved to get
    there.  So instead of overlap/2 it's 'max 0 (overlap - n) / 2', where 'n'
    is the imbalance between their move offsets.

    TODO this is still broken if an offset causes an note to skip over another.
    But that should be stopped by the next event, right?
-}
adjust_offset :: RealTime -- ^ don't move notes any closer than this
    -> Maybe (RealTime, RealTime) -> Maybe (RealTime, RealTime)
    -> RealTime -> RealTime -> RealTime
adjust_offset min_dur prev next offset start
    | offset == 0 = offset
    | offset > 0 = case next of
        Nothing -> offset
            -- 0   1   2   3   4
            -- [----=+=>
            --     <-+-----|
            -- [---====+===)--->
            --     <---+---|
            -- [------->   )
            --     |------->
        Just (next_offset, next_start)
            | overlap <= 0 -> min (next_end - min_dur) end - start
            | otherwise -> (end - overlap + overlap / 2 - min_dur) - start
            where
            overlap = end - next_end
            end = min (max next_start next_end) (start + offset)
            next_end = max start (next_start + next_offset)
    | otherwise = case prev of
        Nothing -> offset
            -- 0   1   2   3   4
            -- <-------|
            -- (   <-------|
        Just (prev_offset, prev_start)
            -- If the prev_offset is positive, then it will have already given
            -- the min_dur space.
            | overlap <= 0 -> if prev_offset > 0
                then offset
                else max (prev_end + min_dur) end - start
            | otherwise -> (end + overlap - overlap / 2) - start
            where
            overlap = prev_end - end
            end = max (min prev_start prev_end) (start + offset)
            prev_end = min start (prev_start + prev_offset)

-- | Change the duration based on the movement of the next event.
--
-- If the event end touches the next start, then adjust dur by next_offset.  If
-- it's less, then shorten but don't lengthen.  If it overlaps the next note,
-- then leave it alone.
adjust_duration :: RealTime -> RealTime -> Score.Event -> RealTime
adjust_duration next new_next event =
    subtract (Score.event_start event) $ case ApproxEq.compare 0.001 end next of
        EQ -> new_next
        LT -> min new_next end
        GT -> end
    where end = Score.event_end event

offset_of :: Score.Event -> RealTime
offset_of = fromMaybe 0 . Env.maybe_val EnvKey.start_offset_val
    . Score.event_environ
