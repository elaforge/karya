{-# LANGUAGE CPP #-}
{- | A TimeStep is an abstract description of a ScoreTime interval.

    It's used to advance a cursor, snap a selection, set a note duration, etc.
-}
module Cmd.TimeStep (
    -- * TimeStep
    TimeStep(..), time_step, event_step, to_list
    , match_meter
    , Step(..), Tracks(..)
    , MarklistMatch(..)
    , Direction(..)
    , show_time_step, parse_time_step, show_direction
    , parse_rank

    -- * step
    , snap
    , step_from, rewind, advance, direction

#ifdef TESTING
    , get_points_from, ascending_points, descending_points
    , find_before_equal
#endif
) where
import qualified Data.List.Ordered as Ordered
import qualified Data.Map as Map
import qualified Text.Parsec as P

import Util.Control
import qualified Util.Parse as Parse
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Ruler as Ruler
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.State as State
import qualified Ui.Track as Track

import qualified Cmd.Meter as Meter
import Types


-- | A TimeStep is the union of a set of Steps.
newtype TimeStep = TimeStep [Step]
    deriving (Eq, Show, Read)

time_step :: Step -> TimeStep
time_step = TimeStep . (:[])

event_step :: TimeStep
event_step = TimeStep [EventStart CurrentTrack, EventEnd CurrentTrack]

to_list :: TimeStep -> [Step]
to_list (TimeStep steps) = steps

-- | Match on the meter marklist, which is the usual thing to do.
match_meter :: MarklistMatch
match_meter = NamedMarklists [Meter.meter]

-- | The possible matchers for a TimeStep.
data Step =
     -- | Step a certain amount of time.  It's measured relative to the current
     -- selection, rather than absolute from the beginning of the block.
    Duration ScoreTime
    -- | Until the next mark that matches.
    | AbsoluteMark MarklistMatch Ruler.Rank
    -- | Until next matching mark + offset from previous mark.
    | RelativeMark MarklistMatch Ruler.Rank
    -- | Until the end or beginning of the block.
    | BlockEdge
    -- | Until event edges.  EventStart is after EventEnd if the duration is
    -- negative.
    | EventStart Tracks
    | EventEnd Tracks
    deriving (Eq, Show, Read)

-- | Events of which tracks the event time step should use.
data Tracks = CurrentTrack | AllTracks | TrackNums [TrackNum]
    deriving (Eq, Show, Read)

data MarklistMatch = AllMarklists | NamedMarklists [Ruler.Name]
    deriving (Eq, Show, Read)

-- | Another way to express a 'step_from' of 1 or -1.
data Direction = Advance | Rewind deriving (Eq, Show)

-- | Convert a TimeStep to a compact and yet somehow still somewhat readable
-- representation.
show_time_step :: TimeStep -> String
show_time_step (TimeStep steps) = Seq.join ";" (map show_step steps)
    where
    -- The keywords and symbols are chosen carefully to allow unambiguous
    -- parsing.
    show_step step = case step of
        Duration pos -> "d:" ++ Pretty.pretty pos
        RelativeMark mlists rank ->
            "r:" ++ show_marklists mlists ++ show_rank rank
        BlockEdge -> "END"
        EventStart tracks -> "start" ++ show_tracks tracks
        EventEnd tracks -> "end" ++ show_tracks tracks
        AbsoluteMark mlists rank -> show_marklists mlists ++ show_rank rank
    show_marklists AllMarklists = ""
    show_marklists (NamedMarklists mlists) =
        Seq.join "," (map untxt mlists) ++ "|"
    show_tracks CurrentTrack = ""
    show_tracks AllTracks = "s"
    show_tracks (TrackNums tracks) = "s:" ++ Seq.join "," (map show tracks)

-- | Parse that curiously somewhat readable representation back to a TimeStep.
parse_time_step :: Text -> Either String TimeStep
parse_time_step = Parse.parse p_time_step
    where
    p_time_step = TimeStep <$> P.sepBy p_step (P.char ';')
    -- P.choice must backtrack because AbsoluteMark parses can overlap.
    p_step = P.choice $ map P.try
        [ str "d:" *> (Duration <$>
            (ScoreTime.double <$> (Parse.p_float <* P.char ScoreTime.suffix)))
        , str "r:" *> (RelativeMark <$> p_marklists <*> parse_rank)
        , str "END" *> return BlockEdge
        , str "start" *> (EventStart <$> p_tracks)
        , str "end" *> (EventEnd <$> p_tracks)
        , AbsoluteMark <$> p_marklists <*> parse_rank
        ]
    p_marklists =
        P.try ((NamedMarklists <$> P.sepBy p_name (P.char ',')) <* P.char '|')
        <|> return AllMarklists
    p_name = txt <$> P.many1 (P.lower <|> P.char '-')
    p_tracks = P.option CurrentTrack $
        P.try (TrackNums <$> (str "s:" *> p_tracknums))
        <|> P.char 's' *> return AllTracks
    p_tracknums = P.sepBy Parse.p_nat (P.char ',')
    str = P.string

show_rank :: Ruler.Rank -> String
show_rank rank = fromMaybe ('R' : show rank) $ lookup rank Meter.rank_names

parse_rank :: Parse.Parser st Ruler.Rank
parse_rank = P.choice $ map P.try
    [P.string name *> return rank | (rank, name) <- Meter.rank_names]
    ++ [P.char 'R' *> Parse.p_nat]

show_direction :: Direction -> String
show_direction Advance = "+"
show_direction Rewind = "-"

-- * snap

-- | Given a pos, the point on a timestep at or previous to that pos.  If
-- there was no snap point, the pos is return unchanged.
snap :: (State.M m) => TimeStep -> BlockId -> TrackNum
    -> Maybe TrackTime -- ^ Last sel pos, needed to snap relative steps like
    -- 'Duration' and 'RelativeMark'.
    -> TrackTime -> m TrackTime
snap tstep block_id tracknum prev_pos pos =
    -- I only need to step from prev_pos if there's a RelativeMark in the tstep.
    -- Otherwise, I can be a bit more efficient and snap pos directly.
    fromMaybe pos <$> snap_from (if any is_relative (to_list tstep)
        then fromMaybe pos prev_pos else pos)
    where
    snap_from p
        | pos > p = -- Advance p until one before pos.
            find_before_equal pos <$>
                get_points_from Advance block_id tracknum p tstep
        | otherwise = -- Rewind p until before pos.
            Seq.head . dropWhile (>pos) <$>
                get_points_from Rewind block_id tracknum p tstep
    is_relative (Duration {}) = True
    is_relative (RelativeMark {}) = True
    is_relative _ = False

-- | Find the first element from a list before or equal to the given element.
find_before_equal :: (Ord a) => a -> [a] -> Maybe a
find_before_equal _ [] = Nothing
find_before_equal p (x:xs)
    | p < x = Nothing
    | otherwise = case xs of
        next : _ | p >= next -> find_before_equal p xs
        _ -> Just x

-- * step

rewind :: (State.M m) => TimeStep -> BlockId -> TrackNum -> TrackTime
    -> m (Maybe TrackTime)
rewind = step_from (-1)

advance :: (State.M m) => TimeStep -> BlockId -> TrackNum -> TrackTime
    -> m (Maybe TrackTime)
advance = step_from 1

direction :: Direction -> Int
direction Advance = 1
direction Rewind = -1

-- | Step in the given direction from the given position, or Nothing if
-- there is no step from that point.  This may return a point past the end of
-- the ruler, or before 0, so if the caller wants to step the selection it
-- should make sure to limit the value.  The reason is that this is also used
-- to get e.g. the duration of a whole note at a given point, and that should
-- work even if the given point is near the end of the ruler.
step_from :: (State.M m) => Int -> TimeStep -> BlockId -> TrackNum
    -> TrackTime -> m (Maybe TrackTime)
step_from steps tstep block_id tracknum start = extract <$>
    get_points_from (if steps >= 0 then Advance else Rewind) block_id tracknum
        start tstep
    where
    extract = Seq.head
        . if steps == 0 then id else drop (abs steps - 1) . dropWhile (==start)

get_points_from :: (State.M m) => Direction -> BlockId -> TrackNum -> TrackTime
    -> TimeStep -> m [TrackTime]
get_points_from dir block_id tracknum start tstep =
    merge_points dir <$> mapM (get block_id tracknum start) (to_list tstep)
    where
    get = case dir of
        Advance -> ascending_points
        Rewind -> descending_points

-- | Step points ascending from the given time.  Includes the start
-- point.
ascending_points :: (State.M m) => BlockId -> TrackNum -> TrackTime -> Step
    -> m [TrackTime]
ascending_points block_id tracknum start step =
    dropWhile (<start) <$> case step of
        Duration t -> do
            end <- State.block_ruler_end block_id
            return $ Seq.range start end t
        AbsoluteMark match rank ->
            get_marks Advance False match rank start <$>
                get_ruler block_id tracknum
        RelativeMark match rank ->
            shift . get_marks Advance True match rank start <$>
                get_ruler block_id tracknum
        BlockEdge -> do
            end <- State.block_ruler_end block_id
            return [0, end]
        EventStart tracks ->
            track_events Advance True block_id tracknum start tracks
        EventEnd tracks ->
            track_events Advance False block_id tracknum start tracks
    where
    shift [] = []
    shift (p:ps)
        | p == start = p : ps
        | otherwise = map (+ (start-p)) (p:ps)

-- | Step points descending from the given time.  Includes the start
-- point.
descending_points :: (State.M m) => BlockId -> TrackNum -> TrackTime -> Step
    -> m [TrackTime]
descending_points block_id tracknum start step =
    dropWhile (>start) <$> case step of
        Duration t -> return $ Seq.range start 0 (-t)
        AbsoluteMark match rank ->
            get_marks Rewind True match rank start <$>
                get_ruler block_id tracknum
        RelativeMark match rank ->
            shift . get_marks Rewind True match rank start <$>
                get_ruler block_id tracknum
        BlockEdge -> do
            end <- State.block_ruler_end block_id
            return [end, 0]
        EventStart tracks ->
            track_events Rewind True block_id tracknum start tracks
        EventEnd tracks ->
            track_events Rewind False block_id tracknum start tracks
    where
    shift [] = []
    shift (p:ps)
        | p == start = p : ps
        | otherwise = map (+ (start-p)) (p:ps)

track_events :: (State.M m) => Direction -> Bool
    -> BlockId -> TrackNum -> TrackTime -> Tracks -> m [TrackTime]
track_events dir event_start block_id tracknum start tracks = case tracks of
    AllTracks -> do
        track_ids <- State.track_ids_of block_id
        merge_points dir <$> mapM get_times track_ids
    CurrentTrack -> do
        track_id <- State.get_event_track_at block_id tracknum
        get_times track_id
    TrackNums tracknums -> do
        track_ids <- mapM (State.get_event_track_at block_id) tracknums
        merge_points dir <$> mapM get_times track_ids
    where
    event_time = if event_start then Event.start else Event.end
    get_times = fmap (map event_time . get_events . Track.track_events)
        . State.get_track
    get_events = case dir of
        Advance -> if event_start then Events.at_after start
            else snd . Events.split_at_before start
        Rewind -> at_before start
    at_before p events = case post of
            e : _ | Event.start e == p -> e : pre
            _ -> pre
        where (pre, post) = Events.split p events

merge_points :: Direction -> [[TrackTime]] -> [TrackTime]
merge_points Advance = Seq.drop_dups id . Seq.merge_lists id
merge_points Rewind = Seq.drop_dups id . merge_desc

merge_desc :: [[TrackTime]] -> [TrackTime]
merge_desc = foldr (Ordered.mergeBy (flip compare)) []

-- | Get all marks from the marklists that match the proper names and
-- merge their marks into one list.
get_marks :: Direction -> Bool -> MarklistMatch -> Ruler.Rank -> TrackTime
    -> Ruler.Marklists -> [TrackTime]
get_marks dir minus1 match rank start marklists =
    merge_points dir $ map extract matching
    where
    extract = case dir of
        Advance
            | minus1 -> ascending1
            | otherwise -> with_rank . Ruler.ascending start
        Rewind
            | minus1 -> descending1
            | otherwise -> with_rank . Ruler.descending start
    matching = case match of
        AllMarklists -> Map.elems marklists
        NamedMarklists names -> mapMaybe (flip Map.lookup marklists) names
    ascending1 mlist
        | Just p <- Seq.head marks, p == start = marks
        | otherwise = maybe marks (:marks) $
            Seq.head (with_rank $ Ruler.descending start mlist)
        where marks = with_rank $ Ruler.ascending start mlist
    descending1 mlist = maybe marks (:marks) $
        Seq.head (with_rank $ Ruler.ascending start mlist)
        where marks = with_rank $ Ruler.descending start mlist
    with_rank = map fst . filter ((<=rank) . Ruler.mark_rank . snd)

get_ruler :: (State.M m) => BlockId -> TrackNum -> m Ruler.Marklists
get_ruler block_id tracknum = do
    ruler_id <- fromMaybe State.no_ruler <$>
        State.ruler_track_at block_id tracknum
    Ruler.ruler_marklists <$> State.get_ruler ruler_id
