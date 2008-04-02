module Ui.Sync where
import qualified Data.Map as Map

import qualified Ui.State as State
import qualified Ui.Update as Update

{-
create and delete views: create_view, insert/remove track
redraw in areas where events where added / removed
although for the moment redrawing everywhere is fine
-}

-- | Sync with the ui by applying the given updates to it.
sync :: Initialize.Send -> State.State -> [Update.Update]
    -> IO [(State.ViewId, Int)]
sync send state updates = undefined

update (Update.CreateView view_id) = do
    view <- get_view view_id
    block <- get_block (State.view_block view)
    ruler <- get_ruler (State.view_ruler view)
    viewp <- send $ BlockImpl.create_view block
        (State.view_rect rect) ruler
        (State.view_config view)

-- These should go in the SyncM monad:
-- read state, exception
-- but this should also be reusable in the Cmd/Handler monad:
-- read/write state, exception, midi thru, log

get_view view_id = do
    st <- get_state
    case Map.lookup view_id (State.state_views st) of
        Nothing -> throw $ "unknown view_id " ++ show view_id
        Just (view, _) -> return view

get_block block_id = do
    st <- get_state
    case Map.lookup block_id (State.state_blocks st) of
        Nothing -> throw $ "unknown block_id " ++ show block_id
        Just block -> return block

get_ruler ruler_id = do
    st <- get_state
    case Map.lookup ruler_id (State.state_rulers st) of
        Nothing -> throw $ "unknown ruler_id " ++ show ruler_id
        Just ruler -> return ruler

type GetMarks = Ptr TrackPos -> Ptr TrackPos -> Ptr Int -> IO (Ptr TrackPos)

get_marks :: State.Marklist -> GetMarks
get_marks marklist startp endp np = do
    start <- peek startp
    end <- peek endp
    let marks = State.marklist_find marklist start end
    len n = length marks
    poke np n
    if n > 0
        then newArray marks
        else return nullPtr

foreign import ccall "wrapper"
    cb_get_marks :: GetMarks -> IO (FunPtr GetMarks)

foreign export ccall "ruler_find" export_ruler_find :: RulerId -> TrackPos
    -> RulerIterator
