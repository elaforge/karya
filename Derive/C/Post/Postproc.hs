-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Postprocs that change note start and duration.
module Derive.C.Post.Postproc (
    library
    , Key
    , makeCancel
    , adjustOffset
    , cancelStrongWeak
    , groupAndCancel
    , inferDurationMerged
) where
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe

import qualified Util.Lists as Lists
import qualified Util.Test.ApproxEq as ApproxEq
import qualified Derive.C.Prelude.ControlFunction as ControlFunction
import qualified Derive.C.Prelude.Equal as Equal
import qualified Derive.C.Prelude.Note as Note
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.StaticMacro as StaticMacro
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Flags as Flags
import qualified Derive.LEvent as LEvent
import qualified Derive.Library as Library
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream

import qualified Perform.RealTime as RealTime

import           Global
import           Types


library :: Library.Library
library = Library.transformers
    [ ("apply-start-offset", cApplyStartOffset)
    , ("cancel", cCancel)
    , ("infer-negative", cInferNegative)
    , ("randomize-start", cRandomizeStart)
    , ("strong", Make.addFlag module_ "strong"
        "Add the 'Derive.Flags.strong' flag, which will cancel coincident\
            \ non-strong notes."
        Flags.strong)
    , ("weak", Make.addFlag module_ "weak"
        "Add the 'Derive.Flags.weak' flag, which will cause this to be\
            \ cancelled by coincident non-weak notes."
        Flags.weak)
    ]

module_ :: Module.Module
module_ = Module.prelude

-- * infer-negative

cInferNegative :: Derive.Transformer Derive.Note
cInferNegative = Derive.transformer module_ "infer-negative" Tags.postproc
    "Infer durations for negative events, by instrument."
    $ Sig.callt ((,)
    <$> Sig.defaulted "heuristic" ("note" :: Text) "doc"
    <*> finalDurationArg
    ) $ \(heuristic, finalDur) _args deriver -> do
        process <- Derive.require ("invalid heuristic: " <> heuristic) $
            Map.lookup (heuristic :: Text) inferHeuristics
        process finalDur <$> deriver
    where
    inferHeuristics = Map.fromList
        [ ("hand", nextHand)
        , ("note", nextNote)
        ]

-- | Until the next note with the same hand.  Suitable for gender barung.
-- If the next event doesn't touch this one, stop at its (negative) start time.
-- This is a bit weird conceptually but in practice seems to be useful for
-- controlling duration.
nextHand :: RealTime -> Stream.Stream Score.Event -> Stream.Stream Score.Event
nextHand = inferDuration Post.handKey $ \here nexts ->
    subtract here . until here <$> Lists.head nexts
    where
    until here next
        | Score.eventMin next >= here + threshold = Score.eventMin next
        | otherwise = Score.eventMax next
    threshold = 0.05

-- | Until the next note with the same instrument.  Suitable for gender
-- panerus.
nextNote :: RealTime -> Stream.Stream Score.Event -> Stream.Stream Score.Event
nextNote = inferDuration Score.eventInstrument $ \here nexts ->
    subtract here . start <$>
        Lists.head (dropWhile (\n -> start n - here < threshold) nexts)
    where
    threshold = 0.15
    start = Score.eventStart

inferDuration :: Eq key => Key key
    -> (RealTime -> [Score.Event] -> Maybe RealTime)
    -> RealTime
    -> Stream.Stream Score.Event
    -> Stream.Stream Score.Event
inferDuration nextKey infer finalDur =
    Post.emap1_ infer1 . Post.nextsBy nextKey
    where
    infer1 (event, nexts)
        | null nexts = Score.setDuration finalDur event
        | Score.eventDuration event >= 0 = event
        | otherwise = case infer (Score.eventStart event) nexts of
            Nothing -> Score.setDuration finalDur event
            Just dur -> Score.setDuration dur event

-- * cancel

cCancel :: Derive.Transformer Derive.Note
cCancel = Derive.transformer module_ "cancel" Tags.postproc
    "Process the 'Derive.Flags.strong' and 'Derive.Flags.weak' flags.\
    \ This will cause notes to be dropped."
    $ makeCancel (cancelStrongWeak inferDurationMerged) Post.handKey

-- | Given a set of coincident notes, return either an error, or merge them
-- into a set of output notes.
type Cancel = [Score.Event] -> Either Text [Score.Event]

-- | The key identifies another event which is in the same voice.  This could
-- be 'Post.hand_key', but it could also match polos to sangsih, since they
-- form a composite part.
type Key key = Score.Event -> key

makeCancel :: Ord key => Cancel -> Key key
    -> Derive.WithArgDoc (Derive.TransformerF Derive.Note)
makeCancel cancel key =
    Sig.callt finalDurationArg $ \finalDur _args deriver ->
        Derive.requireRight id . groupAndCancel cancel key finalDur
            =<< deriver

finalDurationArg :: Sig.Parser RealTime
finalDurationArg = Sig.defaultedEnv "final-duration" Sig.Unprefixed
    (1 :: RealTime) "If there is no following note, infer this duration."

groupAndCancel :: Ord key => Cancel -> Key key -> RealTime
    -> Events -> Either Text Events
groupAndCancel cancel key finalDur =
    fmap (inferDurationSingle key finalDur . suppressNotes)
    . mergeGroups cancel . groupCoincident key

-- | Merge notes with 'Flags.strong' and 'Flags.weak'.  The rules are that
-- strong notes merge with weaker ones, in the order strong, normal, weak.
--
-- Previously I considered multiple weaks or strongs ambiguous, but it turns
-- out I get multiple strongs with two hand strokes at the end of a block,
-- and I might as well allow the rest too, for simplicity.
cancelStrongWeak :: (Score.Event -> [Score.Event] -> Score.Event)
    -> [Score.Event] -> Either Text [Score.Event]
cancelStrongWeak merge events = case partition events of
    (strongs@(_:_), weaks, normals) ->
        Right [merge strong (normals ++ weaks) | strong <- strongs]
    ([], weaks, normals@(_:_)) -> Right [merge normal weaks | normal <- normals]
    ([], weaks, []) -> Right weaks
    where
    partition = Lists.partition2 (Score.hasFlags Flags.strong)
        (Score.hasFlags Flags.weak)

-- | Handle 'Flags.infer_duration' for notes merged together.  This is the case
-- where a final note replaces a coincident initial note.  The strong note gets
-- the duration of the longest weak notes, if there are any.  If there are no
-- weaks, then there are no coincedent notes to merge, so return the event
-- unchanged so 'infer_duration_single' can handle it.
inferDurationMerged :: Score.Event -> [Score.Event] -> Score.Event
inferDurationMerged strong weaks =
    case Lists.maximum (map Score.eventEnd weaks) of
        Just end | Score.hasFlags Flags.inferDuration strong ->
            Score.addLog ("set duration to max of weak notes: "
                <> Score.shortEvents weaks) $
            Score.removeFlags Flags.inferDuration $
            Score.setDuration (end - Score.eventStart strong) strong
        _ -> strong

-- | Handle 'Flags.infer_duration' for a note by itself.  When there is no
-- coincident note to replace, the duration extends to the start of the next
-- matching event, according to the 'Key'.
--
-- This actually finds the next matching event which starts later than this
-- one.  Normally notes of the same key are not expected to occur
-- simultaneously, but may still do so, for example pasang parts which are
-- normally considered a single voice but may still contain unison or kempyung.
inferDurationSingle :: Eq key => Key key -> RealTime
    -> Stream.Stream Score.Event -> Stream.Stream Score.Event
inferDurationSingle key finalDur = Post.emap1_ infer . Post.nextsBy key
    where
    infer (event, _) | not (Score.hasFlags Flags.inferDuration event) = event
    infer (event, nexts) =
        Score.removeFlags Flags.inferDuration $ case next of
            Just next -> Score.addLog "set duration to next start" $
                setEnd (Score.eventStart next) event
            Nothing -> Score.addLog "set duration to final dur" $
                Score.setDuration finalDur event
        where
        next = List.find ((> Score.eventStart event) . Score.eventStart) nexts
    setEnd end event = Score.setDuration (end - Score.eventStart event) event

mergeGroups :: ([a] -> Either Text [a]) -> [Either [LEvent.LEvent a] [a]]
    -> Either Text (Stream.Stream a)
mergeGroups merge = fmap Stream.fromSortedList . concatMapM go
    where
    go (Left ungrouped) = Right ungrouped
    go (Right []) = Right []
    go (Right [e]) = Right [LEvent.Event e]
    go (Right es) = map LEvent.Event <$> merge es

type Events = Stream.Stream Score.Event

-- | Group events with the same start time.  Events in Left are not grouped.
groupCoincident :: Ord key => (Score.Event -> key) -> Events
    -> [Either [LEvent.LEvent Score.Event] [Score.Event]]
groupCoincident key = go . Stream.toList
    where
    go [] = []
    go (log@(LEvent.Log {}) : es) = Left [log] : go es
    go (LEvent.Event e : es) =
        (if null logs then id else (Left (map LEvent.Log logs) :)) $
            groups ++ go after
        where
        ((during, logs), after) = first LEvent.partition $
            span (LEvent.logOr $ sameStart e) es
        -- [e] is going to be a common case, since most notes don't group.
        groups = map Right (Lists.groupSort key (e : during))
    sameStart e1 e2 = Score.eventStart e1 RealTime.== Score.eventStart e2

-- | Filter out events that fall at and before the 'EnvKey.suppress_until'
-- range of an event with the same (instrument, hand).  Only events that don't
-- have a suppress_until are suppressed.
--
-- This is complicated by the fact that an event should suppress coincident
-- events even if the supressor follows the suppressee in the list, so I have
-- to look into the future for the greatest suppress_until.
suppressNotes :: Stream.Stream Score.Event -> Stream.Stream Score.Event
suppressNotes =
    snd . Post.emap go Map.empty . Stream.zipOn Post.nexts
        . Stream.zip3On (map Post.handKey) (map getSuppress)
    where
    go suppressed (nexts, (key, suppress, event)) = case suppress of
        Nothing -> (,) suppressed $ case suppressUntil of
            Just until | until >= Score.eventStart event - RealTime.eta -> []
            _ -> [event]
        Just until -> (Map.insert key until suppressed, [event])
        where
        suppressUntil = Lists.maximum $ Maybe.catMaybes $
            (Map.lookup key suppressed :) $ map suppressOf $
            takeWhile (coincident . eventOf) $
            filter ((==key) . keyOf) nexts
        coincident e = Score.eventStart e
            <= Score.eventStart event + RealTime.eta
    getSuppress :: Score.Event -> Maybe RealTime
    getSuppress = Env.maybeVal EnvKey.suppressUntil . Score.eventEnviron
    keyOf (k, _, _) = k
    suppressOf (_, s, _) = s
    eventOf (_, _, e) = e

-- * apply start offset

cRandomizeStart :: Derive.Transformer Derive.Note
cRandomizeStart = StaticMacro.check "c_randomize_start" $
    StaticMacro.transformer module_ "randomize-start" Tags.postproc ""
        -- apply-start-offset | start-s = (cf-rnd-a+ $)
        [ StaticMacro.Call cApplyStartOffset []
        , StaticMacro.Call Equal.cEqual
            [ StaticMacro.literal (ShowVal.showVal Controls.startS)
            , StaticMacro.call (ControlFunction.cCfRndAround (+))
                [StaticMacro.Var]
            ]
        ]

{- | Previously I applied the @%start-s@ and @%start-t@ controls in the note
    generator, but I wound up with notes getting out of sync with their
    controls.  Even if I apply the controls before inversion, it still doesn't
    work other calls, like say block calls, and I can't apply the controls
    before the block call
-}
cApplyStartOffset :: Derive.Transformer Derive.Note
cApplyStartOffset =
    Derive.transformer module_ "apply-start-offset" Tags.postproc
    ("Apply the " <> ShowVal.doc EnvKey.startOffsetVal <> " env var.\
     \ This is set by note deriver from the "
     <> ShowVal.doc Controls.startS <> " and "
     <> ShowVal.doc Controls.startT <> " controls, so if you want those\
     \ controls to have an effect, you have to use this postproc."
    ) $ Sig.callt (
        Sig.defaulted "min-duration" (Nothing :: Maybe RealTime)
            "If given, notes on the same hand\
            \ won't be moved closer than this time. Otherwise, hand and\
            \ instrument is ignored."
    ) $ \minDur _args deriver -> applyStartOffset minDur <$> deriver

applyStartOffset :: Maybe RealTime -> Stream.Stream Score.Event
    -> Stream.Stream Score.Event
applyStartOffset maybeMinDur =
    applyOffset . tweakOffset . Stream.zipOn (map offsetOf)
    where
    tweakOffset = case maybeMinDur of
        Nothing -> id
        Just minDur -> Post.emap1_ (tweak minDur)
            . Post.neighborsBy (Post.handKey . snd)
    tweak minDur (prev, (offset, event), next) = (newOffset, event)
        where
        newOffset = adjustOffset minDur (extract <$> prev) (extract <$> next)
            offset (Score.eventStart event)
        extract (offset, event) = (offset, Score.eventStart event)

    applyOffset =
        Post.emap1Ord_ apply . Post.neighborsBy (Post.handKey . snd)
    apply (_, (offset, event), maybeNext) =
        setDur $ Score.moveStart (fromMaybe Note.minDuration maybeMinDur)
            offset event
        where
        setDur event = case maybeNext of
            Nothing -> event
            Just (nextOffset, next) -> Score.duration (const dur) event
                where
                dur = adjustDuration (Score.eventStart next)
                    (Score.eventStart next + nextOffset) event

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
adjustOffset :: RealTime -- ^ don't move notes any closer than this
    -> Maybe (RealTime, RealTime) -> Maybe (RealTime, RealTime)
    -> RealTime -> RealTime -> RealTime
adjustOffset minDur prev next offset start
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
        Just (nextOffset, nextStart)
            | overlap <= 0 -> min (nextEnd - minDur) end - start
            | otherwise -> (end - overlap + overlap / 2 - minDur) - start
            where
            overlap = end - nextEnd
            end = min (max nextStart nextEnd) (start + offset)
            nextEnd = max start (nextStart + nextOffset)
    | otherwise = case prev of
        Nothing -> offset
            -- 0   1   2   3   4
            -- <-------|
            -- (   <-------|
        Just (prevOffset, prevStart)
            -- If the prev_offset is positive, then it will have already given
            -- the min_dur space.
            | overlap <= 0 -> if prevOffset > 0
                then offset
                else max (prevEnd + minDur) end - start
            | otherwise -> (end + overlap - overlap / 2) - start
            where
            overlap = prevEnd - end
            end = max (min prevStart prevEnd) (start + offset)
            prevEnd = min start (prevStart + prevOffset)

-- | Change the duration based on the movement of the next event.
--
-- If the event end touches the next start, then adjust dur by next_offset.  If
-- it's less, then shorten but don't lengthen.  If it overlaps the next note,
-- then leave it alone.
adjustDuration :: RealTime -> RealTime -> Score.Event -> RealTime
adjustDuration next newNext event =
    subtract (Score.eventStart event) $ case ApproxEq.compare 0.001 end next of
        EQ -> newNext
        LT -> min newNext end
        GT -> end
    where end = Score.eventEnd event

offsetOf :: Score.Event -> RealTime
offsetOf = fromMaybe 0 . Env.maybeVal EnvKey.startOffsetVal
    . Score.eventEnviron
