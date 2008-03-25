module Msg.TimeStep where
import Ui.Types
import qualified Ui.Ruler as Ruler

-- | A variable time step, used to find out how much to advance
-- the cursor, how long an event should be, etc.
data TimeStep
    = Absolute TrackPos -- ^ absolute time step
    | UntilMark MarkMatch -- ^ until the next mark that matches
    | MarkDistance MarkMatch -- ^ until next mark + offset from previous mark


-- | Advance the given pos according to step on the ruler.
advance :: TimeStep -> Ruler.Marklist -> TrackPos -> Maybe TrackPos
advance step marklist start_pos = case step of
    Absolute pos -> Just (start_pos + pos)
    UntilMark matcher -> matcher (Ruler.forward_from marklist start_pos)
    MarkDistance matcher -> do
        prev_pos <- matcher (Ruler.backward_from marklist start_pos)
        next_pos <- matcher (Ruler.forward_from marklist start_pos)
        return (next_pos + (start_pos - prev_pos))

-- | Just like 'advance', but get a previous pos.
rewind :: TimeStep -> Ruler.Marklist -> TrackPos -> Maybe TrackPos
rewind step marklist start_pos = case step of
    Absolute pos -> Just (start_pos - pos)
    UntilMark matcher -> matcher
        (Ruler.backward_from marklist start_pos)
    MarkDistance matcher -> do
        prev_pos <- matcher (Ruler.backward_from marklist start_pos)
        prev_prev_pos <- matcher (Ruler.forward_from marklist prev_pos)
        return (prev_prev_pos + (start_pos - prev_pos))

-- | Given a marklist view, return the TrackPos to advance to.
type MarkMatch = [(TrackPos, Ruler.Mark)] -> Maybe TrackPos

-- | Get the pos of the next mark of the given rank.
match_rank :: Int -> MarkMatch
match_rank rank marks
    | null matches = Nothing
    | otherwise = Just (fst (head matches))
    where
    matches = filter ((==rank) . Ruler.mark_rank . snd) marks
