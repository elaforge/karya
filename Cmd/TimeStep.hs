{- | A TimeStep is an abstract description of a TrackPos interval.

It's used to advance a cursor, snap a selection, set a note duration, etc.
-}
module Cmd.TimeStep where
import Ui.Types
import qualified Ui.Ruler as Ruler

-- | A variable time step, used to find out how much to advance
-- the cursor, how long an event should be, etc.
data TimeStep
    = Absolute TrackPos -- ^ absolute time step
    | UntilMark MarkMatch -- ^ until the next mark that matches
    | MarkDistance MarkMatch -- ^ until next mark + offset from previous mark
    deriving (Show)


-- TODO advance and rewind should merge the marklists

-- | Advance the given pos according to step on the ruler.
advance :: TimeStep -> [Ruler.Marklist] -> TrackPos -> Maybe TrackPos
advance step (marklist:_) start_pos = case step of
    Absolute pos -> Just (start_pos + pos)
    UntilMark matcher -> match matcher (Ruler.forward_from marklist start_pos)
    MarkDistance matcher -> do
        prev_pos <- match matcher (Ruler.backward marklist start_pos)
        next_pos <- match matcher (Ruler.forward_from marklist start_pos)
        return (next_pos + (start_pos - prev_pos))

-- | Just like 'advance', but get a previous pos.
rewind :: TimeStep -> [Ruler.Marklist] -> TrackPos -> Maybe TrackPos
rewind step (marklist:_) start_pos = case step of
    Absolute pos -> Just (start_pos - pos)
    UntilMark matcher -> match matcher (Ruler.backward marklist start_pos)
    MarkDistance matcher -> do
        prev_pos <- match matcher (Ruler.backward marklist start_pos)
        prev_prev_pos <- match matcher (Ruler.forward_from marklist prev_pos)
        return (prev_prev_pos + (start_pos - prev_pos))

-- | Given a marklist view, return the TrackPos to advance to.
-- TODO: should have a symbolic representation for this so I can print it out?

data MarkMatch = MatchRank Int
    deriving (Show)
type Matcher = [(TrackPos, Ruler.Mark)] -> Maybe TrackPos

match :: MarkMatch -> Matcher
match (MatchRank rank) = match_rank rank

-- | Get the pos of the next mark of the given rank.
match_rank :: Int -> Matcher
match_rank rank marks
    | null matches = Nothing
    | otherwise = Just (fst (head matches))
    where
    matches = filter ((==rank) . Ruler.mark_rank . snd) marks
