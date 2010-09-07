{- | A TimeStep is an abstract description of a ScoreTime interval.

    It's used to advance a cursor, snap a selection, set a note duration, etc.
-}
module Cmd.TimeStep where
import qualified Data.Maybe as Maybe
import qualified Data.Fixed as Fixed

import qualified Util.Seq as Seq

import Ui
import qualified Ui.Block as Block
import qualified Ui.Ruler as Ruler
import qualified Ui.State as State


-- | A variable time step, used to find out how much to advance
-- the cursor, how long an event should be, etc.
--
-- TODO I'm not real happy about the structure here, it's awkward how absolute
-- and relative mark are split up.  Wait until I add a few more snaps before
-- changing the design
--
-- - snap to event begin/end
--
-- - snap to named marks (aka cue points)
data TimeStep =
     -- | Absolute time step.
    Absolute ScoreTime
    -- | Until the next mark that matches.
    | AbsoluteMark MarklistMatch MarkMatch
    -- | Until next matching mark + offset from previous mark.
    | RelativeMark MarklistMatch MarkMatch
    deriving (Show, Read)

data MarklistMatch = AllMarklists | NamedMarklists [Ruler.MarklistName]
    deriving (Show, Read)

-- | Take a rank and a number of that rank to skip: MatchRank rank skips
data MarkMatch = MatchRank Int Int deriving (Show, Read)
-- | Given a marklist view, return the ScoreTime to advance to.
type Matcher = [(ScoreTime, Ruler.Mark)] -> Maybe ScoreTime

data Direction = Advance | Rewind deriving (Eq, Show)


-- | Given a pos, the point on a timestep at or previous to that pos.
--
-- To snap RelativeMark I need the last sel pos.
snap :: (State.UiStateMonad m) => TimeStep -> BlockId -> TrackNum
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
step_from :: (State.UiStateMonad m) => TimeStep -> Direction -> BlockId
    -> TrackNum -> ScoreTime -> m (Maybe ScoreTime)
step_from step direction =
    (if direction == Advance then advance else rewind) step

rewind :: (State.UiStateMonad m) => TimeStep -> BlockId -> TrackNum
    -> ScoreTime -> m (Maybe ScoreTime)
rewind step block_id tracknum pos = do
    maybe_points <- get_points step block_id tracknum pos
    return $ find_before pos =<< maybe_points

advance :: (State.UiStateMonad m) => TimeStep -> BlockId -> TrackNum
    -> ScoreTime -> m (Maybe ScoreTime)
advance step block_id tracknum pos = do
    maybe_points <- get_points step block_id tracknum pos
    return $ find_after =<< maybe_points
    where find_after xs = Seq.mhead Nothing Just (drop 1 (dropWhile (<pos) xs))

get_points :: (State.UiStateMonad m) =>
    TimeStep -> BlockId -> TrackNum -> ScoreTime -> m (Maybe [ScoreTime])
get_points step block_id tracknum pos = do
    block <- State.get_block block_id
    case relevant_ruler block tracknum of
        Nothing -> return Nothing
        Just ruler_id -> do
            ruler <- State.get_ruler ruler_id
            return $ Just $ all_points step (Ruler.ruler_marklists ruler) pos

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
all_points :: TimeStep -> [(Ruler.MarklistName, Ruler.Marklist)]
    -> ScoreTime -> [ScoreTime]
all_points step marklists pos = case step of
        Absolute incr ->
            -- fmod is in a bizarre place
            Seq.range (Fixed.mod' pos incr) end incr
        AbsoluteMark names matcher -> matches names matcher
        RelativeMark names matcher -> shift (matches names matcher)
    where
    end = Seq.maximum 0 (map (Ruler.last_pos . snd) marklists)
    matches names matcher = match_all matcher
        (get_marks marklists names)
    shift points = case find_before_equal pos points of
        Just p | p < pos -> map (+ (pos-p)) points
        _ -> points

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
        NamedMarklists names -> Seq.map_maybe (flip lookup marklists) names


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
