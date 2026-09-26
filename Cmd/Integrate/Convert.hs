-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
-- | The 'convert' function and support.
module Cmd.Integrate.Convert (
    Track(..), Tracks
    , convert
#ifdef TESTING
    , module Cmd.Integrate.Convert
#endif
) where
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Texts as Texts

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Perf as Perf
import qualified Derive.Call as Call
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Expr as Expr
import qualified Derive.PSignal as PSignal
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Stack as Stack
import qualified Derive.Stream as Stream

import qualified Instrument.Common as Common
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import qualified Ui.Event as Event
import qualified Ui.Ui as Ui

import           Global
import           Types


-- | Include flags as a comment in generated events, for debugging.  It
-- clutters the output though.  TODO: need a better way
debug :: Bool
debug = False

type Error = Text
type Title = Text

-- | A simplified description of a UI track, as collected by
-- "Derive.Call.Integrate".
data Track = Track
    { trackTitle :: !Title
    , trackEvents :: ![Event.Event]
    } deriving (Eq, Ord, Show)

instance Pretty Track where
    format (Track title events) = Pretty.record "Track"
        [ ("title", Pretty.format title)
        , ("events", Pretty.format events)
        ]

-- | (note track, control tracks)
type Tracks = [(Track, [Track])]
type Config = (GetCallMap, Pitch.ScaleId)
type GetCallMap = ScoreT.Instrument -> Common.CallMap

-- | Convert 'Score.Event's to 'Tracks'.  This involves splitting overlapping
-- events into tracks, and trying to map low level notation back to high level.
convert :: Cmd.M m => BlockId -> Stream.Stream Score.Event -> m Tracks
convert sourceBlock stream = do
    lookupInst <- Cmd.getLookupInstrument
    let getCallMap = maybe mempty (Common.commonCallMap . Cmd.instCommon)
            . lookupInst
    defaultScaleId <- Perf.defaultScaleId
    tracknums <- Map.fromList <$> Ui.tracknumsOf sourceBlock
    let (events, logs) = Stream.partition stream
        (errs, tracks) = integrate (getCallMap, defaultScaleId)
            tracknums events
    mapM_ (Log.write . Log.addPrefix "integrate") logs
    -- If something failed to derive I shouldn't integrate that into the block.
    when (any ((>=Log.Warn) . Log.msgPriority) logs) $
        Cmd.throw "aborting integrate due to warnings"
    unless (null errs) $
        Cmd.throw $ "integrating events: " <> Text.intercalate "; " errs
    pure tracks

-- | Convert derived score events back into UI events.
integrate :: Config -> Map TrackId TrackNum -> [Score.Event]
    -> ([Error], Tracks)
integrate config tracknums =
    Either.partitionEithers . map (integrateTrack config)
    . allocateTracks tracknums

-- | Allocate the events to separate tracks.
allocateTracks :: Map TrackId TrackNum -> [Score.Event]
    -> [(TrackKey, [Score.Event])]
allocateTracks tracknums = concatMap overlap . Lists.keyedGroupSort groupKey
    where
    overlap (key, events) = map ((,) key) (splitOverlapping events)
    -- Sort by tracknum so an integrated block's tracks come out in the same
    -- order as the original.
    groupKey :: Score.Event -> TrackKey
    groupKey event =
        ( tracknumOf =<< trackOf event
        , Score.eventInstrument event
        , PSignal.sigScaleId (Score.eventPitch event)
        , eventVoice event
        , eventHand event
        )
    tracknumOf tid = Map.lookup tid tracknums

-- | Split events into separate lists of non-overlapping events.
splitOverlapping :: [Score.Event] -> [[Score.Event]]
splitOverlapping [] = []
splitOverlapping events = track : splitOverlapping rest
    where
    -- Go through the track and collect non-overlapping events, then do it
    -- recursively until there are none left.
    (track, rest) = Either.partitionEithers (strip events)
    strip [] = []
    strip (event:events) = Left event : map Right overlapping ++ strip rest
        where (overlapping, rest) = span (overlaps event) events

overlaps :: Score.Event -> Score.Event -> Bool
overlaps e1 e2 = Score.eventStart e2 < Score.eventEnd e1
    || Score.eventStart e1 == Score.eventStart e2

eventVoice :: Score.Event -> Maybe Voice
eventVoice = Env.maybeVal EnvKey.voice . Score.eventEnviron

eventHand :: Score.Event -> Maybe Call.Hand
eventHand = Env.maybeVal EnvKey.hand . Score.eventEnviron

trackOf :: Score.Event -> Maybe TrackId
trackOf = Lists.head . mapMaybe Stack.trackOf . Stack.innermost
    . Score.eventStack

-- | This determines how tracks are split when integration recreates track
-- structure.
type TrackKey =
    ( Maybe TrackNum, ScoreT.Instrument, Pitch.ScaleId
    , Maybe Voice, Maybe Call.Hand
    )
type Voice = Int

integrateTrack :: Config -> (TrackKey, [Score.Event])
    -> Either Error (Track, [Track])
integrateTrack (getCallMap, defaultScaleId)
        ((_, inst, scaleId, voice, hand), events) = do
    pitchTrack <- if noPitchSignals events || noScale
        then pure []
        else case pitchEvents sid $ events of
            (track, []) -> pure [track]
            (_, errs) -> Left $ Text.intercalate "; " errs
    pure
        ( noteEvents inst (voice, hand) (getCallMap inst) events
        , pitchTrack ++ controlEvents events
        )
    where
    -- Instruments like mridangam '(natural)' call use this for ambient pitch.
    noScale = scaleId == PSignal.pscaleScaleId PSignal.noScale
    sid = if scaleId == defaultScaleId then Pitch.emptyScale else scaleId

-- ** note

noteEvents :: ScoreT.Instrument -> (Maybe Voice, Maybe Call.Hand)
    -> Common.CallMap -> [Score.Event] -> Track
noteEvents inst (voice, hand) callMap events =
    makeTrack noteTitle (map (noteEvent callMap) events)
    where
    noteTitle = Text.intercalate " | " $ filter (/="")
        [ ParseTitle.instrumentToTitle inst
        , addEnv EnvKey.voice voice
        , addEnv EnvKey.hand hand
        ]
    addEnv key = maybe "" (((key <> "=")<>) . ShowVal.showVal)

noteEvent :: Common.CallMap -> Score.Event -> Event.Event
noteEvent callMap event =
    uiEvent (Score.eventStack event)
        (RealTime.toScore (Score.eventStart event))
        (RealTime.toScore (Score.eventDuration event))
        (noteCall callMap event)

noteCall :: Common.CallMap -> Score.Event -> Text
noteCall callMap event = Texts.join2 " -- " text comment
    where
    text
        | Score.eventIntegrate event /= "" = Score.eventIntegrate event
        | Just sym <- Map.lookup attrs callMap = Expr.unsym sym
        | attrs /= mempty = ShowVal.showVal attrs
        | otherwise = ""
        where attrs = Score.eventAttributes event
    -- Append flags to help with debugging.  The presence of a flag
    -- probably means some postproc step wasn't applied.
    comment
        | debug && flags /= mempty = pretty flags
        | otherwise = ""
        where flags = Score.eventFlags event


-- ** pitch

-- | Unlike 'control_events', this only drops dups that occur within the same
-- event.  This is because it's more normal to think of each note as
-- establishing a new pitch, even if it's the same as the last one.
pitchEvents :: Pitch.ScaleId -> [Score.Event] -> (Track, [Error])
pitchEvents scaleId events =
    (makeTrack pitchTitle (tidyPitches uiEvents), concat errs)
    where
    pitchTitle = ParseTitle.scaleToTitle scaleId
    (uiEvents, errs) = unzip $ map pitchSignalEvents events
    tidyPitches = clipToZero . clipConcat . map dropDups

noPitchSignals :: [Score.Event] -> Bool
noPitchSignals = all (PSignal.null . Score.eventPitch)

-- | Convert an event's pitch signal to symbolic note names.  This uses
-- 'PSignal.pitch_note', which handles a constant transposition, but not
-- continuous pitch changes (it's not even clear how to spell those).  I could
-- try to convert back from NoteNumbers, but I still have the problem of how
-- to convert the curve back to high level pitches.
pitchSignalEvents :: Score.Event -> ([Event.Event], [Error])
pitchSignalEvents event = (uiEvents, pitchErrs)
    where
    start = Score.eventStart event
    (xs, ys) = unzip $ PSignal.toPairs $ PSignal.clipBefore start $
        Score.eventPitch event
    pitches = zip3 xs ys
        (map (PSignal.pitchNote . Score.applyControls event start) ys)
    pitchErrs =
        [ pretty x <> ": converting " <> pretty p <> " " <> pretty err
        | (x, p, Left err) <- pitches
        ]
    uiEvents =
        [ uiEvent (Score.eventStack event) (RealTime.toScore x) 0
            (Pitch.noteText note)
        | (x, _, Right note) <- pitches
        ]

-- ** control

controlEvents :: [Score.Event] -> [Track]
controlEvents events =
    filter (not . emptyTrack) $ map (controlTrack events) controls
    where
    controls = List.sort $ Lists.unique $ concatMap
        (map typedControl . filter wanted . Map.toList . Score.eventControls)
        events
    -- The integrate calls always include these because they affect the
    -- pitches.  'pitch_signal_events' will have already applied them though,
    -- so we don't need to have them again.
    -- TODO: technically they should be from pscale_transposers, but that's
    -- so much work to collect, let's just assume the standards.
    wanted = (`Set.notMember` Controls.integrateKeep) . fst
    typedControl (control, sig) = ScoreT.Typed (ScoreT.typeOf sig) control

controlTrack :: [Score.Event] -> ScoreT.Typed ScoreT.Control -> Track
controlTrack events control =
    makeTrack (ParseTitle.controlToTitle control) uiEvents
    where
    uiEvents = dropDyn $ tidyControls $ map (signalEvents c) events
    -- Don't emit a dyn track if it's just the default.
    dropDyn events = case Map.lookup c Derive.initialControlVals of
        Just val | all ((==t) . Event.text) events -> []
            where t = ShowVal.showHexVal val
        _ -> events
    tidyControls = clipToZero . dropDups . clipConcat
    c = ScoreT.valOf control

signalEvents :: ScoreT.Control -> Score.Event -> [Event.Event]
signalEvents control event = case Score.eventControl control event of
    Nothing -> []
    Just sig -> map (uncurry mk) $ Signal.toPairs $
        Signal.clipBefore start (ScoreT.valOf sig)
    where
    -- Suppose ambient dyn is .75, but then post integrate it is set to .6.
    -- Since the dyn track multiplies by default, this would wind up doubly
    -- applying the .75, for .75*.6.  So the integrate call saves its ambient
    -- dyn so we can invert it here.
    invert
        | control == Controls.dynamic = fromMaybe 1 $
            Env.maybeVal (ScoreT.controlName Controls.dynamicIntegrate) $
            Score.eventEnviron event
        | otherwise = 1
    start = Score.eventStart event
    mk x y = uiEvent (Score.eventStack event) (RealTime.toScore x) 0
        (ShowVal.showHexVal (y / invert))

-- * util

uiEvent :: Stack.Stack -> ScoreTime -> ScoreTime -> Text -> Event.Event
uiEvent stack pos dur text =
    Event.stack_ #= Just (Event.Stack stack pos) $ Event.event pos dur text

-- | Concatenate the events, dropping ones that are out of order.  The
-- durations are not modified, so they still might overlap in duration, but the
-- start times will be increasing.
clipConcat :: [[Event.Event]] -> [Event.Event]
clipConcat = Lists.dropWith outOfOrder . concat
    where outOfOrder e1 e2 = Event.start e2 <= Event.start e1

-- | Drop subsequent events with the same text, since those are redundant for
-- controls.
dropDups :: [Event.Event] -> [Event.Event]
dropDups = Lists.dropDups Event.text

-- | Drop events before 0, keeping at least one at 0.  Controls can wind up
-- with samples before 0 (e.g. after using 'Derive.Score.move'), but events
-- can't start before 0.
clipToZero :: [Event.Event] -> [Event.Event]
clipToZero (e1 : rest@(e2 : _))
    | Event.start e1 <= 0 && Event.start e2 <= 0 = clipToZero rest
    | otherwise = (Event.start_ %= max 0 $ e1) : rest
clipToZero [e] = [Event.start_ %= max 0 $ e]
clipToZero [] = []

makeTrack :: Title -> [Event.Event] -> Track
makeTrack title events = Track title (Lists.sortOn Event.start events)

emptyTrack :: Track -> Bool
emptyTrack (Track _ []) = True
emptyTrack _ = False
