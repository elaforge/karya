-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Note calls that transform other note calls.  They rely on track slicing
-- via 'Sub.sub_events'.
module Derive.C.Prelude.Parent (
    library
    -- testing
    , interpolateSubs
) where
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe

import qualified Util.Lists as Lists
import qualified Util.Num as Num

import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Call.Ly as Ly
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.SubT as SubT
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Eval as Eval
import qualified Derive.Flags as Flags
import qualified Derive.Library as Library
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Lilypond.Constants as Constants
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import qualified Ui.ScoreTime as ScoreTime

import           Global
import           Types


library :: Library.Library
library = Library.generators
    [ ("ap", cAp)
    , ("t", cTuplet)
    , ("tup", cTuplet) -- longer name in case 't' is shadowed
    , ("`arp-up`", cRealArpeggio ToRight)
    , ("`arp-down`", cRealArpeggio ToLeft)
    , ("`arp-rnd`", cRealArpeggio Random)
    , ("interpolate", cInterpolate)
    , ("e-interpolate", cEventInterpolate)
    , ("cycle", cCycle)
    , ("cycle-t", cCycleT)
    ]

cAp :: Derive.Generator Derive.Note
cAp = Derive.generator Module.prelude "ap" Tags.subs
    "Derive sub events with no changes.  This is used to apply a transformer\
    \ to sub events."
    $ Sig.call0 $ Sub.derive . concat <=< Sub.subEvents

-- * tuplet

cTuplet :: Derive.Generator Derive.Note
cTuplet = Derive.generator Module.prelude "tuplet" mempty
    "A generalized tuplet. The notes within its scope are stretched so that\
    \ their collective duration is the same as the tuplet's duration.\
    \\nThis doesn't work so well for zero duration notes. The last note\
    \ winds up at the end of the tuplet, which is not very useful. But zero\
    \ duration is common for percussion, so there's a hack to cover this case:\
    \ if there are >1 equidistant zero duration sub events, the distance\
    \ between them is considered their implicit duration.\
    \\nIf there are multiple note tracks, they get the stretch of the longest\
    \ one. This is so the timing will come out right if some of the notes are\
    \ chords."
    $ Sig.callSub (Sig.many "subtrack" "Subtrack to stretch.") $
        \subtracks args -> Ly.whenLilypond
            (lilyTuplet (Args.range args) (map SubT._events subtracks))
            (tuplet (Args.range args) (map SubT._events subtracks))

tuplet :: (ScoreTime, ScoreTime) -> [[SubT.Event]] -> Derive.NoteDeriver
tuplet (start, end) tracks =
    case tupletNoteEnd (map (map toStartDur) tracks) of
        Nothing -> pure mempty
        Just noteEnd ->
            mconcat $ map (Sub.fit (start, noteEnd) (start, end)) tracks
    where toStartDur e = (SubT._start e, SubT._duration e)

-- | Get the end of the notes inside the tuplet.  If it has >1 note, and they
-- are all zero dur, and notes are equidistant, assume the last one has the
-- same dur.
tupletNoteEnd :: (Ord a, Num a) => [[(a, a)]] -> Maybe a
tupletNoteEnd = Lists.maximum . mapMaybe lastEnd
    where
    lastEnd events = inferDuration events <|> Lists.maximum (map endOf events)
    inferDuration events = case zipWith (-) (drop 1 starts) starts of
        d : ds | all ((==0) . durOf) events && all (==d) ds ->
            Just $ startOf (last events) + d
        _ -> Nothing
        where starts = map startOf events
    startOf = fst
    durOf = snd
    endOf (s, d) = s + d

-- | Emit a special 'tuplet_event' and derive the sub events with no time
-- changes.
lilyTuplet :: (ScoreTime, ScoreTime) -> [[SubT.Event]] -> Derive.NoteDeriver
lilyTuplet (start, end) trackNotes = do
    trackNotes <- case filter (not . null) trackNotes of
        [] -> Derive.throw "no sub events"
        notes -> pure notes
    trackEvents <- mapM Sub.derive trackNotes
    let notes = map (filter (not . Ly.isCode0) . Stream.eventsOf) trackEvents
    notesEnd <- case filter (not . null) notes of
        [] -> Derive.throw "no sub events"
        tracks -> Derive.require "can't figure out tuplet duration" $
            tupletNoteEnd (map (map toStartDur) tracks)
    realStart <- Derive.real start
    realEnd <- Derive.real end
    tuplet <- Derive.place start (end-start) $
        tupletEvent (notesEnd - realStart) (realEnd - realStart)
    pure $ mconcat (tuplet : trackEvents)
    where toStartDur e = (Score.eventStart e, Score.eventDuration e)

-- | Emit the special event that tells lilypond to gather the following events
-- into a tuplet.
tupletEvent :: RealTime -> RealTime -> Derive.NoteDeriver
tupletEvent scoreDur realDur =
    Call.addFlags Flags.lyCode $
        Derive.withEnviron (Constants.setTuplet scoreDur realDur) $
        Call.note

-- * arpeggio

-- | Direction in which to arpeggiate.  This is a general arpeggiation that
-- just makes each track slightly delayed with regard to its neighbor.
--
-- Since I can't know the pitch of things (and a 'SubT.Event' may not have
-- a single pitch), the arpeggiation is by track position, not pitch.
data Arpeggio = ToRight | ToLeft | Random deriving (Show)

cRealArpeggio :: Arpeggio -> Derive.Generator Derive.Note
cRealArpeggio arp = Derive.generator Module.prelude "arp" Tags.subs
    ("Arpeggiate the transformed notes. This shifts each note's start time\
    \ by a different amount, increasing to the right for `arp-up`,\
    \ to the left for `arp-down`, and randomly for `arp-rnd`.\
    \ Since it transforms score and not events, it doesn't know the\
    \ pitches of the sub notes (they may not have a single pitch) so\
    \ it's not actually \"up\" or \"down\"."
    ) $ Sig.call ((,)
    <$> Sig.defaulted "time" (0.1 :: Double)
        "This much RealTime between each note."
    <*> Sig.defaulted "random" (0.5 :: Double)
        "Each note can vary randomly by `+- time/2 * random`."
    ) $ \(time, random) args -> lilyCode args $
        arpeggio arp (RealTime.seconds time) random =<< Sub.subEvents args
    where
    lilyCode = Ly.notesWith $ Ly.addFirst (Ly.prepend, prefix)
        . Ly.addFirst (Ly.append Constants.First, suffix)
    prefix = case arp of
        ToRight -> "\\arpeggioArrowUp"
        ToLeft -> "\\arpeggioArrowDown"
        Random -> "\\arpeggioNormal"
    suffix = "\\arpeggio"

-- | Shift each track of notes by a successive amount.
arpeggio :: Arpeggio -> RealTime -> Double -> [[SubT.Event]]
    -> Derive.NoteDeriver
arpeggio arp time random tracks = do
    delayTracks <- jitter . zip (Lists.range_ 0 time) =<< sort tracks
    events <- fmap concat $ forM delayTracks $ \(delay, track) ->
        forM track $ \(SubT.EventT start dur d) -> do
            delay <- Call.scoreDuration start delay
            pure $ SubT.EventT (start+delay) (dur-delay) d
    Sub.derive events
    where
    jitter tracks
        | random == 0 = pure tracks
        | otherwise = do
            rs <- Call.randomsIn (-random) random
            pure $ zipWith nudge rs tracks
    nudge r (delay, notes)
        | delay == 0 = (delay, notes)
        | otherwise = (delay + time/2 * RealTime.seconds r, notes)
    sort = case arp of
        ToRight -> pure
        ToLeft -> pure . reverse
        Random -> Call.shuffle

-- | This is the old version that shifts each note as a postproc.  This means
-- it can arpeggiate by pitch since it knows the pitches at that point, but
-- also means it won't place events that consist of multiple notes correctly.
--
-- It's also buggy for events after the start since it will make their
-- duration negative.
arpeggioByNote :: Arpeggio -> RealTime -> Derive.NoteDeriver
    -> Derive.NoteDeriver
arpeggioByNote arp time deriver = do
    (events, logs) <- Stream.partition <$> deriver
    let sort = case arp of
            ToRight -> pure . Lists.reverseSortOn Score.initialNn
            ToLeft -> pure . Lists.sortOn Score.initialNn
            Random -> Call.shuffle
    arpeggiated <- zipWith (Score.moveStart 0) (Lists.range_ 0 time)
        <$> sort events
    pure $ Stream.mergeLogs logs $ Stream.fromSortedEvents arpeggiated

-- * interpolate

cInterpolate :: Derive.Generator Derive.Note
cInterpolate = Derive.generator Module.prelude "interpolate" Tags.subs
    "Interpolate between multiple sub-tracks, each of which must have the\
    \ same number of events. This interpolates rhythm only. To interpolate\
    \ pitch and controls, it would need to work at the score event level,\
    \ rather than ui events."
    $ Sig.call (
        Sig.defaultedEnv "at" Sig.Both (Typecheck.Normalized 0)
            "interpolate position"
    ) $ \at args -> do
        tracks <- filter (not . null) <$> Sub.subEvents args
        unless (allEqual (map length tracks)) $
            Derive.throw $ "sub tracks should have the same number of events: "
                <> pretty (map length tracks)
        toReal <- Derive.realFunction
        Sub.derive $ interpolateTracks (at . toReal) (Lists.rotate tracks)

interpolateTracks :: (ScoreTime -> Signal.Y) -> [[SubT.Event]] -> [SubT.Event]
interpolateTracks at = mapMaybe interpolate1
    where
    interpolate1 events = interpolateSubs (at start) events
        where
        start = Num.sum (map SubT._start events) / fromIntegral (length events)

interpolateSubs :: Double -> [SubT.EventT a] -> Maybe (SubT.EventT a)
interpolateSubs at events = case drop i events of
    [] -> Lists.last events
    [event] -> Just event
    e1 : e2 : _ -> Just $ SubT.EventT
        { _start = interpolate (SubT._start e1) (SubT._start e2)
        , _duration = interpolate (SubT._duration e1) (SubT._duration e2)
        , _note = SubT._note e1
        }
    where
    (i, frac) = properFraction (at * fromIntegral (length events - 1))
    interpolate x y = Num.scale x y (ScoreTime.fromDouble frac)

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = all (==x) xs

-- UI should be: call with two args, like 'interp b1 b2'

cEventInterpolate :: Derive.Generator Derive.Note
cEventInterpolate = Derive.generator Module.prelude "e-interpolate" Tags.subs
    "Interpolate rhythms of the transformed sequence."
    $ Sig.call ((,,)
    <$> Sig.defaulted "notes" (Nothing :: Maybe Sig.Dummy) "source deriver"
    <*> Sig.defaulted "model" (Nothing :: Maybe Sig.Dummy) "rhythm model"
    <*> Sig.defaultedEnv "at" Sig.Both (Typecheck.Normalized 0)
        "interpolate position"
    ) $ \(mbNotes, mbModel, at) args -> do
        (model, notes) <- resolveDerivers2 args
            ("model", mbModel) ("notes", mbNotes)
        let startDur e = (Score.eventStart e, Score.eventDuration e)
        (modelStarts, modelLogs) <-
            first (map startDur) . Stream.partition <$> model
        Stream.mergeLogs modelLogs
            . Post.apply (interpolateEvents at modelStarts) <$> notes

interpolateEvents :: (RealTime -> Signal.Y) -> [(RealTime, RealTime)]
    -> [Score.Event] -> [Score.Event]
interpolateEvents at ms = map interpolate . Lists.zipPaddedFst ms
    where
    interpolate (Just (toStart, toDur), event) =
        Score.place
            (Num.scale (Score.eventStart event) toStart frac)
            (Num.scale (Score.eventDuration event) toDur frac)
            event
        where
        frac = RealTime.seconds $ at (Score.eventStart event)
    interpolate (Nothing, event) = event

resolveDerivers2 :: Derive.NoteArgs
    -> (Text, Maybe DeriveT.Quoted) -> (Text, Maybe DeriveT.Quoted)
    -> Derive.Deriver (Derive.NoteDeriver, Derive.NoteDeriver)
resolveDerivers2 args sym1 sym2 = resolveDerivers args [sym1, sym2] >>= \case
    [d1, d2] -> pure (d1, d2)
    ds -> Derive.throw $ "expected 2, got " <> showt (length ds)

-- TODO this is awkward... maybe it should be built into Derive.Sig.
resolveDerivers :: Derive.NoteArgs -> [(Text, Maybe DeriveT.Quoted)]
    -> Derive.Deriver [Derive.NoteDeriver]
resolveDerivers args syms
    | all (Maybe.isJust . snd) syms = pure $ map eval $ mapMaybe snd syms
    | otherwise = do
        tracks <- Sub.subEvents args
        go syms tracks
    where
    eval = Eval.evalQuoted (Args.context args)
    go ((_, Just sym) : syms) tracks = (eval sym :) <$> go syms tracks
    go ((_, Nothing) : syms) (track : tracks) =
        (Sub.derive track :) <$> go syms tracks
    go [] [] = pure []
    go ((argName, Nothing) : _) [] =
        Derive.throw $ "arg not given, expected a track: " <> argName
    go [] tracks
        | null (filter (not . null) tracks) = pure []
        | otherwise = Derive.throw $ "extra tracks: " <> showt (length tracks)

-- * cycle

cCycle :: Derive.Generator Derive.Note
cCycle = Derive.generator Module.prelude "cycle" Tags.subs
    "Apply transformers in a cycle to the sub events."
    $ Sig.call (
        Sig.many1 "transformer" "Transformers to apply."
    ) $ \transformers args -> do
        tracks <- Sub.subEvents args
        mconcatMap (Sub.derive . cycleCall (Args.context args) transformers)
            tracks

cycleCall :: Derive.Context Score.Event -> NonEmpty DeriveT.Quoted
    -> [SubT.Event] -> [SubT.Event]
cycleCall ctx transformers =
    zipWith apply (cycle (NonEmpty.toList transformers))
    where apply quoted = fmap $ Eval.evalQuotedTransformers ctx quoted

cCycleT :: Derive.Generator Derive.Note
cCycleT = Derive.generator Module.prelude "cycle-t" Tags.subs
    "Apply transformers in a cycle to the sub events. This is like 'cycle',\
    \ except that it also gets a duration for each transformer, and cycles in\
    \ the given rhythm, rather than for each event."
    $ Sig.call (
        Sig.many1Pairs "transformer" "Transformers to apply, and the ScoreTime\
            \ duration for each transformer."
    ) $ \transformers args -> do
        tracks <- Sub.subEvents args
        let ctx = Args.context args
        mconcatMap (Sub.derive . cycleT ctx (Args.start args) transformers)
            tracks

cycleT :: Derive.Context Score.Event -> ScoreTime
    -> NonEmpty (DeriveT.Quoted, Double) -> [SubT.Event] -> [SubT.Event]
cycleT ctx start transformers =
    go (zip (cycle ts) (tail (scanl (+) start (cycle durs))))
    where
    go [] _ = []
    go _ [] = []
    go ts@((quoted, until) : restTs) (event : events)
        | SubT._start event >= until = go restTs (event : events)
        | otherwise = fmap (Eval.evalQuotedTransformers ctx quoted) event
            : go ts events
    (ts, durs) = second (map ScoreTime.fromDouble)
        (unzip (NonEmpty.toList transformers))
