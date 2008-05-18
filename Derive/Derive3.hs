{-# OPTIONS_GHC -XGeneralizedNewtypeDeriving #-}
module Derive.Derive3 where
import qualified Control.Exception as Exception
current_event_stack get
    Log.warn_stack event_stack msg

-- ** environment

with_env :: (Monad m) =>
    Score.Controller -> Signal.Signal -> DeriveT m t -> DeriveT m t
with_env cont signal op = do
    old_env <- fmap state_env get
    modify $ \st -> st { state_env = Map.insert cont signal old_env }
    result <- op
    modify $ \st -> st { state_env = old_env }
    return result

-- * basic derivers

-- ** track

-- | Get events from a track, applying the enviroment's controllers to it.
track :: (Monad m) => Track.TrackId -> DeriveT m [Score.Event]
track track_id = do
    track <- get_track track_id
    cmap <- fmap state_env get
    return $ (map (Score.from_track_event cmap track_id) . Track.event_list
        . Track.track_events) track

get_block block_id = get >>= lookup_id block_id . State.state_blocks . state_ui
get_track track_id = get >>= lookup_id track_id . State.state_tracks . state_ui

-- | Lookup @map!key@, throwing if it doesn't exist.
lookup_id key map = case Map.lookup key map of
    Nothing -> throw $ "unknown " ++ show key
    Just val -> return val

-- ** merge

merge tracks = Score.Score (merge_events (map Score.score_events tracks))

merge_events :: [[Score.Event]] -> [Score.Event]
merge_events = foldr (Seq.merge_by (compare `on` Score.event_start)) []

-- * utils

-- | General purpose iterator over events.
--
-- It's like 'map_state_m' but sets the current event stack before operating on
-- each event, so that Derive.warn can use it.  In addition, EventErrors are
-- caught and turned into warnings.  Events that threw aren't included in the
-- output.
map_events state f event_of xs =
    fmap Maybe.catMaybes (Util.Control.map_state_m state apply xs)
    where
    apply st x = do
        set_event_stack (Score.event_stack (event_of x))
        val <- catch_event (f st x)
        return $ case val of
            Nothing -> (st, Nothing)
            Just val -> (st, Just val)
