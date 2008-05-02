{- | A TimeStep is an abstract description of a TrackPos interval.

It's used to advance a cursor, snap a selection, set a note duration, etc.
-}
module Cmd.TimeStep where
import Data.Function
import qualified Data.Maybe as Maybe

import qualified Util.Seq as Seq
import Ui.Types
import qualified Ui.Ruler as Ruler

import Cmd.Types


-- | A variable time step, used to find out how much to advance
-- the cursor, how long an event should be, etc.
data TimeStep
     -- | Absolute time step.
    = Absolute TrackPos
    -- | Until the next mark that matches.
    | UntilMark Marklists MarkMatch
    -- | Until next matching mark + offset from previous mark.
    | MarkDistance Marklists MarkMatch
    deriving (Show)

data Marklists = AllMarklists | NamedMarklists [Ruler.MarklistName]
    deriving (Show)


-- | Advance the given pos according to step on the ruler.
advance :: TimeStep -> [(Ruler.MarklistName, Ruler.Marklist)] -> TrackPos
    -> Maybe TrackPos
advance step marklists start_pos = case step of
    Absolute pos -> Just (start_pos + pos)
    UntilMark names matcher -> match matcher
        (relevant_marks marklists names Advance start_pos)
    MarkDistance names matcher -> do
        prev_pos <- match matcher
            (relevant_marks marklists names Rewind start_pos)
        next_pos <- match matcher
            (relevant_marks marklists names Advance start_pos)
        return (next_pos + (start_pos - prev_pos))

-- | Just like 'advance', but get a previous pos.
rewind :: TimeStep -> [(Ruler.MarklistName, Ruler.Marklist)] -> TrackPos
    -> Maybe TrackPos
rewind step marklists start_pos = case step of
    Absolute pos -> Just (start_pos - pos)
    UntilMark names matcher -> match matcher
        (relevant_marks marklists names Rewind start_pos)
    MarkDistance names matcher -> do
        prev_pos <- match matcher
            (relevant_marks marklists names Advance start_pos)
        prev_prev_pos <- match matcher
            (relevant_marks marklists names Rewind prev_pos)
        return (prev_prev_pos + (start_pos - prev_pos))

-- | Given a marklist view, return the TrackPos to advance to.
data MarkMatch = MatchRank Int
    deriving (Show)
type Matcher = [(TrackPos, Ruler.Mark)] -> Maybe TrackPos

-- Extract @names@ from alist @marklists@, use @to_marks@ to extract marks
-- from @start_pos@, and return the merged result.
relevant_marks marklists names direction start_pos =
    let mlists = case names of
            AllMarklists -> map snd marklists
            NamedMarklists names ->
                Maybe.catMaybes (map (flip lookup marklists) names)
        (cmp, marks) = case direction of
            Advance -> (compare,
                map (flip Ruler.forward_from start_pos) mlists)
            Rewind -> (Seq.reverse_compare,
                map (flip Ruler.backward start_pos) mlists)
    -- Sort on TrackPos.  foldr is important to preserve laziness.
    in foldr (Seq.merge_by (cmp `on` fst)) [] marks


match :: MarkMatch -> Matcher
match (MatchRank rank) = match_rank rank

-- | Get the pos of the next mark <= the given rank.
match_rank :: Int -> Matcher
match_rank rank marks
    | null matches = Nothing
    | otherwise = Just (fst (head matches))
    where
    matches = filter ((<=rank) . Ruler.mark_rank . snd) marks
