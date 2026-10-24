-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{- | A TimeStep is an abstract description of a ScoreTime interval.

    It's used to advance a cursor, snap a selection, set a note duration, etc.
-}
module Cmd.TimeStep (
    -- * TimeStep
    TimeStep, timeStep, eventStep, fromList, toList
    , modifyRank
    , matchMeter
    , eventEdge
    , rank
    , Step(..), Tracks(..)
    , MarklistMatch(..)
    , Direction(..)
    , showTimeStep, parseTimeStep, showDirection
    , parseRank

    -- * step
    , snap
    , stepFrom, rewind, advance, direction
    , durationAt
    , getPointsFrom

#ifdef TESTING
    , ascendingPoints, descendingPoints
#endif
) where
import qualified Data.List.Ordered as Ordered
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Lists as Lists
import qualified Util.P as P
import qualified Util.ParseM as ParseM

import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Meter.Mark as Mark
import qualified Ui.Meter.Meter as Meter
import qualified Ui.Ruler as Ruler
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.Ui as Ui

import           Global
import           Types


-- | A TimeStep is the union of a set of Steps.
newtype TimeStep = TimeStep [Step]
    deriving (Eq, Show)

timeStep :: Step -> TimeStep
timeStep = TimeStep . (:[])

-- | Step to start and end of events.
eventStep :: TimeStep
eventStep = fromList [EventStart CurrentTrack, EventEnd CurrentTrack]

fromList :: [Step] -> TimeStep
fromList = TimeStep

toList :: TimeStep -> [Step]
toList (TimeStep steps) = steps

modifyRank :: (Meter.Rank -> Meter.Rank) -> TimeStep -> TimeStep
modifyRank f = fromList . map modify . toList
    where
    modify (AbsoluteMark m r) = AbsoluteMark m (f r)
    modify (RelativeMark m r) = RelativeMark m (f r)
    modify step = step

-- | Match on the meter marklist, which is the usual thing to do.
matchMeter :: MarklistMatch
matchMeter = NamedMarklists [Ruler.meterName]

eventEdge :: TimeStep
eventEdge =
    TimeStep [EventStart CurrentTrack, EventEnd CurrentTrack, BlockEdge]

rank :: Meter.Rank -> TimeStep
rank = timeStep . AbsoluteMark matchMeter

-- | The possible matchers for a TimeStep.
data Step
     -- | Step a certain amount of time.  It's measured relative to the current
     -- selection, rather than absolute from the beginning of the block.
    = Duration ScoreTime
    -- | Until the next mark that matches.
    | AbsoluteMark MarklistMatch Meter.Rank
    -- | Until next matching mark + offset from previous mark.
    | RelativeMark MarklistMatch Meter.Rank
    -- | Until the end or beginning of the block.
    | BlockEdge
    -- | Until event edges.  EventStart is after EventEnd if the duration is
    -- negative.
    | EventStart Tracks
    | EventEnd Tracks
    deriving (Eq, Show)

-- | Events of which tracks the event time step should use.
data Tracks = CurrentTrack | AllTracks | TrackNums [TrackNum]
    deriving (Eq, Show)

data MarklistMatch = AllMarklists | NamedMarklists [Ruler.Name]
    deriving (Eq, Show)

-- | Another way to express a 'step_from' of 1 or -1.
data Direction = Advance | Rewind deriving (Eq, Show)

-- | Convert a TimeStep to a compact and yet somehow still somewhat readable
-- representation.
showTimeStep :: TimeStep -> Text
showTimeStep (TimeStep steps)
    -- Abbreviate to keep the status line shorter.
    | TimeStep steps == eventEdge = "E"
    | otherwise = Text.intercalate ";" (map showStep steps)
    where
    -- The keywords and symbols are chosen carefully to allow unambiguous
    -- parsing.
    showStep step = case step of
        Duration pos -> "d:" <> pretty pos
        RelativeMark mlists rank ->
            "r:" <> showMarklists mlists <> Meter.rankName rank
        BlockEdge -> "END"
        EventStart tracks -> "start" <> showTracks tracks
        EventEnd tracks -> "end" <> showTracks tracks
        AbsoluteMark mlists rank ->
            showMarklists mlists <> Meter.rankName rank
    showMarklists AllMarklists = ""
    showMarklists (NamedMarklists mlists) = Text.intercalate "," mlists <> "|"
    showTracks CurrentTrack = ""
    showTracks AllTracks = "s"
    showTracks (TrackNums tracks) =
        "s:" <> Text.intercalate "," (map showt tracks)

-- | Parse that curiously somewhat readable representation back to a TimeStep.
parseTimeStep :: Text -> Either Text TimeStep
parseTimeStep = ParseM.parse pTimeStep
    where
    pTimeStep = TimeStep <$> P.sepBy pStep (P.char ';')
    -- P.choice must backtrack because AbsoluteMark parses can overlap.
    pStep = P.choice $ map P.try
        [ str "d:" *> (Duration <$>
            (ScoreTime.fromDouble
                <$> (ParseM.pFloat <* P.char ScoreTime.suffix)))
        , str "r:" *> (RelativeMark <$> pMarklists <*> parseRank)
        , str "END" *> pure BlockEdge
        , str "start" *> (EventStart <$> pTracks)
        , str "end" *> (EventEnd <$> pTracks)
        , AbsoluteMark <$> pMarklists <*> parseRank
        ]
    pMarklists =
        P.try ((NamedMarklists <$> P.sepBy pName (P.char ',')) <* P.char '|')
        <|> pure AllMarklists
    pName = txt <$> P.some (P.lowerChar <|> P.char '-')
    pTracks = P.option CurrentTrack $
        P.try (TrackNums <$> (str "s:" *> pTracknums))
        <|> P.char 's' *> pure AllTracks
    pTracknums = P.sepBy ParseM.pNat (P.char ',')
    str = P.string

parseRank :: ParseM.Parser Meter.Rank
parseRank = P.choice $ map P.try
    [P.string name *> pure rank | (rank, name) <- Meter.rankNames]

showDirection :: Direction -> Text
showDirection Advance = "+"
showDirection Rewind = "-"

-- * snap

-- | Given a pos, the point on a timestep at or previous to that pos.  If
-- there was no snap point, the pos is return unchanged.
snap :: Ui.M m => TimeStep -> BlockId -> TrackNum
    -> Maybe TrackTime -- ^ Last sel pos, needed to snap relative steps like
    -- 'Duration' and 'RelativeMark'.
    -> TrackTime -> m TrackTime
snap tstep blockId tracknum prevPos pos =
    -- I only need to step from prev_pos if there's a RelativeMark in the tstep.
    -- Otherwise, I can be a bit more efficient and snap pos directly.
    fromMaybe pos <$> snapFrom (if any isRelative (toList tstep)
        then fromMaybe pos prevPos else pos)
    where
    snapFrom p
        | pos > p = -- Advance p until one before pos.
            Lists.head . Lists.dropBefore id pos <$>
                getPointsFrom Advance blockId tracknum p tstep
        | otherwise = -- Rewind p until before pos.
            Lists.head . dropWhile (>pos) <$>
                getPointsFrom Rewind blockId tracknum p tstep
    isRelative (Duration {}) = True
    isRelative (RelativeMark {}) = True
    isRelative _ = False

-- * step

rewind :: Ui.M m => TimeStep -> BlockId -> TrackNum -> TrackTime
    -> m (Maybe TrackTime)
rewind = stepFrom (-1)

advance :: Ui.M m => TimeStep -> BlockId -> TrackNum -> TrackTime
    -> m (Maybe TrackTime)
advance = stepFrom 1

direction :: Direction -> Int
direction Advance = 1
direction Rewind = -1

durationAt :: Ui.M m => BlockId -> TrackNum -> TrackTime -> TimeStep
    -> m (Maybe ScoreTime)
durationAt blockId tracknum pos tstep = do
    pre <- getPointsFrom Rewind blockId tracknum pos tstep
    post <- getPointsFrom Advance blockId tracknum pos tstep
    pure $ case (pre, dropWhile (==pos) post) of
        (t0 : _, t1 : _) -> Just (t1 - t0)
        _ -> Nothing

-- | Step in the given direction from the given position, or Nothing if
-- there is no step from that point.  This may return a point past the end of
-- the ruler, or before 0, so if the caller wants to step the selection it
-- should make sure to limit the value.  The reason is that this is also used
-- to get e.g. the duration of a whole note at a given point, and that should
-- work even if the given point is near the end of the ruler.
stepFrom :: Ui.M m => Int -> TimeStep -> BlockId -> TrackNum
    -> TrackTime -> m (Maybe TrackTime)
stepFrom steps tstep blockId tracknum start = extract <$>
    getPointsFrom (if steps >= 0 then Advance else Rewind) blockId tracknum
        start tstep
    where
    extract = Lists.head
        . if steps == 0 then id else drop (abs steps - 1) . dropWhile (==start)

getPointsFrom :: Ui.M m => Direction -> BlockId -> TrackNum -> TrackTime
    -> TimeStep -> m [TrackTime]
getPointsFrom dir blockId tracknum start tstep =
    mergePoints dir <$> mapM (get blockId tracknum start) (toList tstep)
    where
    get = case dir of
        Advance -> ascendingPoints
        Rewind -> descendingPoints

-- | Step points ascending from the given time.  Includes the start
-- point.
ascendingPoints :: Ui.M m => BlockId -> TrackNum -> TrackTime -> Step
    -> m [TrackTime]
ascendingPoints blockId tracknum start step =
    dropWhile (<start) <$> case step of
        Duration t -> do
            end <- Ui.blockRulerEnd blockId
            pure $ Lists.range start end t
        AbsoluteMark match rank ->
            getMarks Advance False match rank start <$>
                getRuler blockId tracknum
        RelativeMark match rank ->
            shift . getMarks Advance True match rank start <$>
                getRuler blockId tracknum
        BlockEdge -> do
            end <- Ui.blockRulerEnd blockId
            pure [0, end]
        EventStart tracks ->
            trackEvents Advance True blockId tracknum start tracks
        EventEnd tracks ->
            trackEvents Advance False blockId tracknum start tracks
    where
    shift [] = []
    shift (p:ps)
        | p == start = p : ps
        | otherwise = map (+ (start-p)) (p:ps)

-- | Step points descending from the given time.  Includes the start
-- point.
descendingPoints :: Ui.M m => BlockId -> TrackNum -> TrackTime -> Step
    -> m [TrackTime]
descendingPoints blockId tracknum start step =
    dropWhile (>start) <$> case step of
        Duration t -> pure $ Lists.range start 0 (-t)
        AbsoluteMark match rank ->
            getMarks Rewind True match rank start <$>
                getRuler blockId tracknum
        RelativeMark match rank ->
            shift . getMarks Rewind True match rank start <$>
                getRuler blockId tracknum
        BlockEdge -> do
            end <- Ui.blockRulerEnd blockId
            pure [end, 0]
        EventStart tracks ->
            trackEvents Rewind True blockId tracknum start tracks
        EventEnd tracks ->
            trackEvents Rewind False blockId tracknum start tracks
    where
    shift [] = []
    shift (p:ps)
        | p == start = p : ps
        | otherwise = map (+ (start-p)) (p:ps)

trackEvents :: Ui.M m => Direction -> Bool
    -> BlockId -> TrackNum -> TrackTime -> Tracks -> m [TrackTime]
trackEvents dir eventStart blockId tracknum start = \case
    AllTracks -> do
        trackIds <- Ui.trackIdsOf blockId
        mergePoints dir <$> mapM getTimes trackIds
    CurrentTrack -> getTimes =<< Ui.getEventTrackAt blockId tracknum
    TrackNums tracknums -> do
        trackIds <- mapM (Ui.getEventTrackAt blockId) tracknums
        mergePoints dir <$> mapM getTimes trackIds
    where
    eventTime = if eventStart then Event.start else Event.end
    getTimes = fmap (map eventTime . getEvents) . Ui.getEvents
    getEvents = case dir of
        Advance -> if eventStart then Events.atAfter start
            else snd . Events.splitAtBefore start
        Rewind -> atBefore start
    atBefore p events = case post of
            e : _ | Event.start e == p -> e : pre
            _ -> pre
        where (pre, post) = Events.splitLists p events

mergePoints :: Direction -> [[TrackTime]] -> [TrackTime]
mergePoints Advance = Lists.dropDups id . Lists.mergeLists id
mergePoints Rewind = Lists.dropDups id . mergeDesc

mergeDesc :: [[TrackTime]] -> [TrackTime]
mergeDesc = foldr (Ordered.mergeBy (flip compare)) []

-- | Get all marks from the marklists that match the proper names and
-- merge their marks into one list.
getMarks :: Direction -> Bool -> MarklistMatch -> Meter.Rank -> TrackTime
    -> Ruler.Marklists -> [TrackTime]
getMarks dir minus1 match rank start marklists =
    mergePoints dir $ map extract matching
    where
    extract = case dir of
        Advance
            | minus1 -> ascending1
            | otherwise -> withRank . Mark.ascending start
        Rewind
            | minus1 -> descending1
            | otherwise -> withRank . Mark.descending start
    matching = map snd $ case match of
        AllMarklists -> Map.elems marklists
        NamedMarklists names -> mapMaybe (flip Map.lookup marklists) names
    ascending1 mlist
        | Just p <- Lists.head marks, p == start = marks
        | otherwise = maybe marks (:marks) $
            Lists.head (withRank $ Mark.descending start mlist)
        where marks = withRank $ Mark.ascending start mlist
    descending1 mlist = maybe marks (:marks) $
        Lists.head (withRank $ Mark.ascending start mlist)
        where marks = withRank $ Mark.descending start mlist
    withRank = map fst . filter ((<=rank) . Mark.markRank . snd)

getRuler :: Ui.M m => BlockId -> TrackNum -> m Ruler.Marklists
getRuler blockId tracknum = do
    rulerId <- fromMaybe Ui.noRuler <$> Ui.rulerTrackAt blockId tracknum
    Ruler.rulerMarklists <$> Ui.getRuler rulerId
