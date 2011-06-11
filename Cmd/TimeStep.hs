{- | A TimeStep is an abstract description of a ScoreTime interval.

    It's used to advance a cursor, snap a selection, set a note duration, etc.
-}
module Cmd.TimeStep where
import qualified Data.Fixed as Fixed
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


-- | A variable time step, used to find out how much to advance
-- the cursor, how long an event should be, etc.
--
-- TODO I'm not real happy about the structure here, it's awkward how absolute
-- and relative mark are split up.  Wait until I add a few more snaps before
-- changing the design
--
-- - snap to named marks (aka cue points)
data TimeStep =
     -- | Absolute time step.
    Absolute ScoreTime
    -- | Until the next mark that matches.
    | AbsoluteMark MarklistMatch MarkMatch
    -- | Until next matching mark + offset from previous mark.
    | RelativeMark MarklistMatch MarkMatch
    -- | Until the end or beginning of the block.
    | BlockEnd
    -- | Until event edges.
    | EventEdge
    | Merge [TimeStep]
    deriving (Show, Read)

data MarklistMatch = AllMarklists | NamedMarklists [Ruler.MarklistName]
    deriving (Show, Read)

-- | Take a rank and a number of that rank to skip: MatchRank rank skips
data MarkMatch = MatchRank Int Int deriving (Show, Read)
-- | Given a marklist view, return the ScoreTime to advance to.
type Matcher = [(ScoreTime, Ruler.Mark)] -> Maybe ScoreTime

data Direction = Advance | Rewind deriving (Eq, Show)


show_step :: Maybe Direction -> TimeStep -> String
show_step direction step = dir_s ++ case step of
    Absolute pos -> "abs:" ++ Pretty.pretty pos
    AbsoluteMark mlists match ->
        "mark:" ++ show_marklists mlists ++ show_match match
    RelativeMark mlists match ->
        "rel:" ++ show_marklists mlists ++ show_match match
    BlockEnd -> "end"
    EventEdge -> "evt"
    Merge ts -> Seq.join ";" (map (show_step Nothing) ts)
    where
    dir_s = case direction of
        Just Advance -> "+"
        Just Rewind -> "-"
        Nothing -> ""
    show_match (MatchRank rank skips) = "r" ++ show rank ++ "+" ++ show skips
    show_marklists AllMarklists = ""
    show_marklists (NamedMarklists mlists) = Seq.join "," mlists ++ "/"


-- * snap

-- | Given a pos, the point on a timestep at or previous to that pos.  If there
-- was no snap point, the pos is return unchanged.
--
-- To snap RelativeMark I need the last sel pos.
snap :: (State.M m) => TimeStep -> BlockId -> TrackNum
    -> Maybe ScoreTime -> ScoreTime -> m ScoreTime
snap step block_id tracknum prev_pos pos = do
    -- RelativeMark needs to be relative to the previous select pos.
    let points_pos = case (step, prev_pos) of
            (RelativeMark {}, Just prev) -> prev
            _ -> pos
    maybe_points <- get_points step block_id tracknum points_pos
    return $ Maybe.fromMaybe pos $ find_before_equal pos =<< maybe_points

-- | Step in the given direction from the given position, or Nothing if
-- the step is out of range.
step_from :: (State.M m) => TimeStep -> Direction -> BlockId
    -> TrackNum -> ScoreTime -> m (Maybe ScoreTime)
step_from step direction =
    (if direction == Advance then advance else rewind) step

rewind :: (State.M m) => TimeStep -> BlockId -> TrackNum
    -> ScoreTime -> m (Maybe ScoreTime)
rewind step block_id tracknum pos = do
    maybe_points <- get_points step block_id tracknum pos
    return $ find_before pos =<< maybe_points

advance :: (State.M m) => TimeStep -> BlockId -> TrackNum
    -> ScoreTime -> m (Maybe ScoreTime)
advance step block_id tracknum pos = do
    maybe_points <- get_points step block_id tracknum pos
    return $ find_after =<< maybe_points
    where find_after xs = Seq.head (dropWhile (<=pos) xs)

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

-- | The approach is to enumerate all possible matches from the beginning of
-- the block and then pick the right one.  This is inefficient because it
-- involves a linear search, but linear search is pretty fast and I expect
-- rulers to not be more than 1000 elements.  But maybe calculating the
-- lists and generating the garbage is a problem when dragging.
--
-- If it's a problem I can cache it.
all_points :: [(Ruler.MarklistName, Ruler.Marklist)] -> [Events.PosEvent]
    -> ScoreTime -> TimeStep -> [ScoreTime]
all_points marklists events pos step = Seq.drop_dups id $ case step of
        Absolute incr ->
            -- fmod is in a bizarre place
            Seq.range (Fixed.mod' pos incr) end incr
        AbsoluteMark names matcher -> matches names matcher
        RelativeMark names matcher -> shift (matches names matcher)
        BlockEnd -> [0, end]
        EventEdge -> event_points events
        Merge ts -> Seq.merge_asc_lists id $
            map (all_points marklists events pos) ts
    where
    end = Maybe.fromMaybe 0 $
        Seq.maximum (map (Ruler.last_pos . snd) marklists)
    matches names matcher = match_all matcher
        (get_marks marklists names)
    shift points = case find_before_equal pos points of
        Just p | p < pos -> map (+ (pos-p)) points
        _ -> points
    event_points [] = []
    event_points (e:es) =
        Events.event_min e : Events.event_max e : event_points es

match_all :: MarkMatch -> [(ScoreTime, Ruler.Mark)] -> [ScoreTime]
match_all (MatchRank rank skips) marks = stride (skips+1) $ map fst matches
    where matches = filter ((<=rank) . Ruler.mark_rank . snd) marks

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


match :: MarkMatch -> Matcher
match (MatchRank rank skips) = match_rank rank skips

-- | Get the pos of the next mark <= the given rank.
match_rank :: Int -> Int -> Matcher
match_rank rank skips marks = case drop skips matches of
    [] -> Nothing
    (pos, _) : _ -> Just pos
    where matches = filter ((<=rank) . Ruler.mark_rank . snd) marks

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

-- | Find the first element from a list strictly before the given element.
find_before :: (Ord a) => a -> [a] -> Maybe a
find_before _ [] = Nothing
find_before p (x:xs)
    | p <= x = Nothing
    | otherwise = case xs of
        next : _ | p > next -> find_before p xs
        _ -> Just x

stride :: Int -> [a] -> [a]
stride _ [] = []
stride n xs@(x:_) = x : stride n (drop n xs)
