-- | Pattern based derivation in the Javanese sekaran tradition.
--
-- TODO This is merely a proof of concept.  Sekaran could be implemented a lot
-- of ways, but I'll have to wait to see which ones are most compositionally
-- useful.  It would definitely be more natural, though, if the notation marked
-- the seleh instead of the first note.  But that would require either
-- a preproc pass or deriving the notes backwards.
--
-- TODO This is also incorrect, because at the time it's evaluated the controls
-- are already evaluated.  Revisit this later.
module Derive.Call.Sekar where
-- import Control.Monad
-- import qualified Data.Char as Char
-- import Ui
-- import qualified Ui.Event as Event
-- import qualified Ui.Track as Track
-- import Util.Control
-- import qualified Util.Seq as Seq
-- 
-- import qualified Derive.Call as Call
import qualified Derive.Derive as Derive
-- import Derive.Operator
-- import qualified Derive.TrackLang as TrackLang
-- import Derive.TrackLang (optional, required, control)
-- 
-- import qualified Perform.PitchSignal as PitchSignal
-- import qualified Perform.Signal as Signal


note_calls :: Derive.CallMap
note_calls = Derive.make_calls
    [ -- ("sekar", c_sekar)
    ]


{-
-- | (event, time until next event)
type Pattern = [(Derive.EventDeriver, ScoreTime)]

-- | Plain sekaran derivation.
--
-- [pattern /String/] Apply this pattern to the following notes.
--      TODO really this should be the *previous* notes, but then I have to
--      derive bottom to top, right?
c_sekar :: Derive.Call
c_sekar = Derive.generator $ \args prev event next -> TrackLang.call1_error args
    (required "pattern") $ \pattern_s -> do
        let events = (0, Event.set_string "" event) : next
        (pattern, nevents, total_dur) <- to_pattern pattern_s prev events
        return (sekar pattern +~ tracem "d2" total_dur, nevents + 1)

-- | Derive the pattern into 0--1.
sekar :: Pattern -> Derive.EventDeriver
sekar pattern = Derive.d_merge_list
    [deriver +~ tracem "d1" dur +@ pos | (pos, dur, deriver) <- place pattern]

place :: Pattern -> [(ScoreTime, ScoreTime, Derive.EventDeriver)]
place pattern = [(pos / total_dur, dur / total_dur , event)
        | (pos, (event, dur)) <- zip starts pattern]
    where
    durs = map snd pattern
    total_dur = sum durs
    starts = scanl (+) 0 durs

-- | Lowercase letters represent a deriver starting from @a@, uppercase ones
-- represent a rest of the duration of that deriver.  E.g. @abAb@ 
to_pattern :: String -> [Track.PosEvent] -> [Track.PosEvent]
    -> TrackLang.WithError (Pattern, Int, ScoreTime)
to_pattern cs prev events
    | null cs = Left "null pattern"
    | otherwise = do
        ints <- mapM to_int cs
        pattern <- mapM (at (zip derivers event_durs)) ints
        let max_int = maximum ints -- I know it's not [] becaue of guard above.
        last_evt <- at events max_int
        return (pattern, max_int, Track.event_max last_evt)
    where
    to_int c
        | not ('a' <= c && c <= 'z') =
            Left $ "pattern character out of a-z range: " ++ show c
        | otherwise = return $ Char.ord c - Char.ord 'a'
    to_char i = Char.chr (i + Char.ord 'a')
    -- The deriver dur is the time until the next event begins, i.e. it
    -- includes the rest after the event if there is one.
    event_durs :: [ScoreTime]
    event_durs = do
        ((cur_pos, cur_evt), next) <- Seq.zip_next events
        return $ case next of
            Nothing -> Event.event_duration cur_evt
            Just (next_pos, _) -> next_pos - cur_pos
    -- Derive the events in their original places so they pick up the controls
    -- in the right spot.
    derivers = Call.subeval_events prev events
    at xs i = case Seq.at xs i of
        Nothing -> Left $ "pattern character has no event: " ++ show (to_char i)
        Just val -> return val

-- Alternately, take pitches at intergral multiples of the first duration.  If
-- there is no note overlapping, take it as a rest.  This derives a straight
-- rhythm, more similar to a real javanese pattern.

FROM Derive.Call:

-- | Eval ignoring consumed.
subeval_events :: [Track.PosEvent] -> [Track.PosEvent] -> [Derive.EventDeriver]
subeval_events prev next = map go (zip_neighbors prev next)
    where
    with_stack pos evt = Derive.with_stack_pos pos (Event.event_duration evt)
    go (prev, cur@(pos, event), next) = with_stack pos event $ do
        (deriver, _) <- eval_event prev cur next
        deriver

zip_neighbors :: [a] -> [a] -> [([a], a, [a])]
zip_neighbors prev xs = undefined

-- | Derive a single deriver's worth of events.  Most calls will only consume
-- a single event, but some may handle a series of events.
eval_event :: [Track.PosEvent] -- ^ previous events, in reverse order
    -> Track.PosEvent -- ^ cur event
    -> [Track.PosEvent] -- ^ following events
    -> Derive.Deriver (Derive.EventDeriver, Int)
eval_event prev cur@(_, event) next
    | Event.event_string event == "--" = skip_event
    | otherwise = case TrackLang.parse (Event.event_string event) of
        Left err -> Derive.warn err >> skip_event
        Right expr -> eval_generator "note" expr prev cur next

eval_one_event :: Event.Event -> Derive.EventDeriver
eval_one_event event = do
    (deriver, _) <- eval_event [] (0, event) []
    deriver
-}
