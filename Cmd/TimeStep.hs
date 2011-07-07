{- | A TimeStep is an abstract description of a ScoreTime interval.

    It's used to advance a cursor, snap a selection, set a note duration, etc.
-}
module Cmd.TimeStep (
    -- * TimeStep
    TimeStep, Skip, time_step, step, merge, to_list
    , Step(..)
    , MarklistMatch(..), Rank
    , Direction(..)
    , show_step

    -- * step
    , snap
    , step_from, rewind, advance, direction

    -- * for testing
    , step_from_points, all_points
) where
import qualified Data.Fixed as Fixed
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import Ui
import qualified Ui.Block as Block
import qualified Ui.Events as Events
import qualified Ui.Ruler as Ruler
import qualified Ui.State as State
import qualified Ui.Track as Track


-- | A list of Steps and skip factors.  The TimeStep is the given steps
-- merged.
--
-- Use the 'time_step' and 'merge' constructors to preserve the invariant of
-- a non-empty list and non-negative skip.
newtype TimeStep = TimeStep [(Step, Skip)]
    deriving (Eq, Show, Read)

-- | Natural number.  Skip this many Steps.  So a skip of 0 will match at
-- every point the Step matches, and a skip of 1 will match every other point.
type Skip = Int -- positive

time_step :: Skip -> Step -> TimeStep
time_step skip step = TimeStep [(step, abs skip)]

step :: Step -> TimeStep
step = time_step 0

merge :: Skip -> Step -> TimeStep -> TimeStep
merge skip step (TimeStep steps) = TimeStep ((step, abs skip) : steps)

to_list :: TimeStep -> [(Step, Skip)]
to_list (TimeStep steps) = steps

-- | The possible matchers for a TimeStep.
--
-- TODO
-- - snap to named marks (aka cue points)
data Step =
     -- | Absolute time step.
    Absolute ScoreTime
    -- | Until the next mark that matches.
    | AbsoluteMark MarklistMatch Rank
    -- | Until next matching mark + offset from previous mark.
    | RelativeMark MarklistMatch Rank
    -- | Until the end or beginning of the block.
    | BlockEnd
    -- | Until event edges.  EventStart is after EventEnd if the duration is
    -- negative.
    | EventStart
    | EventEnd
    deriving (Eq, Show, Read)

data MarklistMatch = AllMarklists | NamedMarklists [Ruler.MarklistName]
    deriving (Eq, Show, Read)

-- | Match the given rank.
type Rank = Int

-- | Another way to express a 'step_from' of 1 or -1.
data Direction = Advance | Rewind deriving (Eq, Show)

show_step :: Maybe Direction -> TimeStep -> String
show_step direction (TimeStep steps) = dir_s ++ Seq.join ";" (map show1 steps)
    where
    show1 (step, skip) = show_skip skip ++ case step of
            Absolute pos -> "abs:" ++ Pretty.pretty pos
            AbsoluteMark mlists match ->
                "mark:" ++ show_marklists mlists ++ show_match match
            RelativeMark mlists match ->
                "rel:" ++ show_marklists mlists ++ show_match match
            BlockEnd -> "end"
            EventStart -> "start"
            EventEnd -> "end"
    show_skip skip = if skip > 0 then show (skip+1) ++ "*" else ""
    dir_s = case direction of
        Just Advance -> "+"
        Just Rewind -> "-"
        Nothing -> ""
    show_match rank = "r" ++ show rank
    show_marklists AllMarklists = ""
    show_marklists (NamedMarklists mlists) = Seq.join "," mlists ++ "/"


-- * snap

-- | Given a pos, the point on a timestep at or previous to that pos.  If
-- there was no snap point, the pos is return unchanged.
--
-- To snap RelativeMark I need the last sel pos.
snap :: (State.M m) => TimeStep -> BlockId -> TrackNum
    -> Maybe ScoreTime -> ScoreTime -> m ScoreTime
snap step block_id tracknum prev_pos pos = do
    -- 'pos' is the pos to snap, prev_pos is what we want to snap to
    maybe_points <- get_points step block_id tracknum
        (Maybe.fromMaybe pos prev_pos)
    -- 'step_from 0' also achieves this, but doing it directly might be more
    -- efficient.
    return $ Maybe.fromMaybe pos $ find_before_equal pos =<< maybe_points

-- * step

-- | Step in the given direction from the given position, or Nothing if
-- the step is out of range.
step_from :: (State.M m) => Int -> TimeStep -> BlockId -> TrackNum
    -> ScoreTime -> m (Maybe ScoreTime)
step_from n step block_id tracknum pos = do
    maybe_points <- get_points step block_id tracknum pos
    return $ step_from_points n pos =<< maybe_points

rewind :: (State.M m) => TimeStep -> BlockId -> TrackNum
    -> ScoreTime -> m (Maybe ScoreTime)
rewind = step_from (-1)

advance :: (State.M m) => TimeStep -> BlockId -> TrackNum
    -> ScoreTime -> m (Maybe ScoreTime)
advance = step_from 1

-- | TODO remove this along with Direction?
direction :: Direction -> Int
direction Advance = 1
direction Rewind = -1

-- * implementation

step_from_points :: Int -> ScoreTime -> [ScoreTime] -> Maybe ScoreTime
step_from_points n pos points = go =<< find_around pos points
    where
    go (pre, p, post)
        | n < 0 = Seq.at (if p == pos then pre else p:pre) (abs n - 1)
        | n == 0 = Just p
        | otherwise = Seq.at (p:post) n

find_around :: (Ord a) => a -> [a] -> Maybe ([a], a, [a])
find_around pos = List.find close . Seq.zip_around []
    where
    close (_, p, _) | p == pos = True
    close (_, p, next : _) | p < pos && pos < next = True
    close _ = False

get_events :: (State.M m) => BlockId -> TrackNum -> m [Events.PosEvent]
get_events block_id tracknum = do
    maybe_track_id <- State.event_track_at block_id tracknum
    case maybe_track_id of
        Just track_id -> Events.ascending . Track.track_events <$>
            State.get_track track_id
        Nothing -> return []

-- | Get the ruler that applies to the given track.  Search left for the
-- closest ruler that has all the given marklist names.  This includes ruler
-- tracks and the rulers of event tracks.
relevant_ruler :: Block.Block -> TrackNum -> Maybe RulerId
relevant_ruler block tracknum = Seq.at (Block.ruler_ids_of in_order) 0
    where
    in_order = map snd $ dropWhile ((/=tracknum) . fst) $ reverse $
        zip [0..] (Block.block_tracklike_ids block)

-- ** get_points

get_points :: (State.M m) =>
    TimeStep -> BlockId -> TrackNum -> ScoreTime -> m (Maybe [ScoreTime])
get_points step block_id tracknum pos = do
    block <- State.get_block block_id
    events <- get_events block_id tracknum
    case relevant_ruler block tracknum of
        Nothing -> return Nothing
        Just ruler_id -> do
            ruler <- State.get_ruler ruler_id
            return $ Just $
                all_points (Ruler.ruler_marklists ruler) events pos step

-- | The approach is to enumerate all possible matches from the beginning of
-- the block and then pick the right one.  This is inefficient because it
-- involves a linear search, but linear search is pretty fast and I expect
-- rulers to not be more than 1000 elements.  But maybe calculating the
-- lists and generating the garbage is a problem when dragging.
--
-- If it's a problem I can cache it.
all_points :: [(Ruler.MarklistName, Ruler.Marklist)] -> [Events.PosEvent]
    -> ScoreTime -> TimeStep -> [ScoreTime]
all_points marklists events pos (TimeStep steps) = Seq.drop_dups id $
    Seq.merge_asc_lists id $ map (step_points marklists events pos) steps

step_points :: [(Ruler.MarklistName, Ruler.Marklist)] -> [Events.PosEvent]
    -> ScoreTime -> (Step, Skip) -> [ScoreTime]
step_points marklists events pos (step, skip) = stride skip $ case step of
        Absolute incr ->
            -- fmod is in a bizarre place
            Seq.range (Fixed.mod' pos incr) end incr
        AbsoluteMark names matcher -> matches names matcher
        RelativeMark names matcher -> shift (matches names matcher)
        BlockEnd -> [0, end]
        EventStart -> map Events.event_start events
        EventEnd -> map Events.event_end events
    where
    end = Maybe.fromMaybe 0 $
        Seq.maximum (map (Ruler.last_pos . snd) marklists)
    matches names matcher = match_all matcher
        (get_marks marklists names)
    shift points = case find_before_equal pos points of
        Just p | p < pos -> map (+ (pos-p)) points
        _ -> points

match_all :: Rank -> [(ScoreTime, Ruler.Mark)] -> [ScoreTime]
match_all rank = map fst .  filter ((<=rank) . Ruler.mark_rank . snd)

-- | Get all marks from the marklists that match the proper names and
-- merge their marks into one list.
get_marks :: [(Ruler.MarklistName, Ruler.Marklist)] -> MarklistMatch
    -> [(ScoreTime, Ruler.Mark)]
get_marks marklists names =
    Seq.merge_lists fst [Ruler.forward mlist 0 | mlist <- matching]
    where
    matching = case names of
        AllMarklists -> map snd marklists
        NamedMarklists names -> Maybe.mapMaybe (flip lookup marklists) names

-- * seq utils

-- These could be moved to Util.Seq if I they become more generally useful.

-- How awkward!  There must be a more concise way to write these.

-- | Find the first element from a list before or equal to the given element.
find_before_equal :: (Ord a) => a -> [a] -> Maybe a
find_before_equal _ [] = Nothing
find_before_equal p (x:xs)
    | p < x = Nothing
    | otherwise = case xs of
        next : _ | p >= next -> find_before_equal p xs
        _ -> Just x

stride :: Int -> [a] -> [a]
stride n xs
    | n <= 0 = xs
    | otherwise = go xs
    where
    go [] = []
    go (x:xs) = x : go (drop n xs)
