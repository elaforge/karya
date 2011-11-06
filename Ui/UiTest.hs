module Ui.UiTest where
import Control.Monad
import qualified Control.Monad.Identity as Identity
import qualified Data.Map as Map

import Util.Control
import qualified Util.Rect as Rect
import qualified Util.Seq as Seq

import Ui
import qualified Ui.Block as Block
import qualified Ui.Color as Color
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.Skeleton as Skeleton
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Types as Types

import qualified Cmd.MakeRuler as MakeRuler
import qualified Cmd.Serialize as Serialize
import qualified Cmd.Simple as Simple
import qualified Cmd.TimeStep as TimeStep

import qualified Derive.Schema as Schema
import qualified App.Config as Config


-- | (10, 50) seems to be the smallest x,y OS X will accept.  Apparently
-- fltk's sizes don't take the menu bar into account, which is about 44 pixels
-- high, so a y of 44 is the minimum.
default_rect :: Rect.Rect
default_rect = Rect.xywh 10 50 200 200
default_divider :: Block.Divider
default_divider = Block.Divider Color.blue

-- state

test_ns = "test"
mkid = Id.id test_ns
bid = Types.BlockId . mkid
vid = Types.ViewId . mkid
tid = Types.TrackId . mkid
rid = Types.RulerId . mkid

default_zoom = Config.zoom

default_view :: Block.View
default_view = Block.view default_block_id default_rect default_zoom

-- | Save the state to disk, so I can load it into the app and see it.
save :: State.State -> FilePath -> IO ()
save ui_state fname = do
    save_state <- Serialize.save_state ui_state
    Serialize.serialize fname save_state

-- * monadic mk- functions

-- | (block_name, tracks)
type BlockSpec = (String, [TrackSpec])
-- | (track_title, events)
type TrackSpec = (String, [(Double, Double, String)])

-- | Often tests work with a single block, or a single view.  To make them
-- less verbose, there is a default block and view so functions can omit the
-- parameter if convenient.
default_block_id :: BlockId
default_block_id = bid default_block_name

default_block_name :: String
default_block_name = "b1"

default_view_id :: ViewId
default_view_id = mk_vid default_block_id

default_ruler_id :: RulerId
default_ruler_id = rid "r1"

-- | Return the val and state, throwing an IO error on an exception.  Intended
-- for tests that don't expect to fail here.
run :: State.State -> State.StateId a -> (a, State.State)
run state m = case result of
        Left err -> error $ "state error: " ++ show err
        Right (val, state', _) -> (val, state')
    where result = Identity.runIdentity (State.run state m)

exec :: State.State -> State.StateId a -> State.State
exec state m = case State.exec state m of
    Left err -> error $ "state error: " ++ show err
    Right state' -> state'

eval :: State.State -> State.StateId a -> a
eval state m = case State.eval state m of
    Left err -> error $ "state error: " ++ show err
    Right val -> val

run_mkblock :: [TrackSpec] -> ([TrackId], State.State)
run_mkblock track_specs =
    run State.empty (mkblock (default_block_name, track_specs))

run_mkview :: [TrackSpec] -> ([TrackId], State.State)
run_mkview track_specs =
    run State.empty (mkblock_view (default_block_name, track_specs))

mkblocks :: (State.M m) => [BlockSpec] -> m [BlockId]
mkblocks blocks = do
    mapM_ mkblock blocks
    return $ map (bid . fst) blocks

mkviews :: (State.M m) => [BlockSpec] -> m [ViewId]
mkviews blocks = mapM mkview =<< mkblocks blocks

mkblock :: (State.M m) => BlockSpec -> m [TrackId]
mkblock block = do
    maybe_rid <- State.lookup_ruler default_ruler_id
    ruler_id <- maybe
        (State.create_ruler (Id.unpack_id default_ruler_id) default_ruler)
        (const (return default_ruler_id)) maybe_rid
    mkblock_ruler ruler_id block

mkblocks_skel :: (State.M m) => [(BlockSpec, [Skeleton.Edge])] -> m ()
mkblocks_skel blocks = forM_ blocks $ \(block, skel) ->
    mkblock block <* State.set_skeleton (bid (fst block)) (Skeleton.make skel)

-- | Like 'mkblock', but uses the provided ruler instead of creating its
-- own.  Important if you are creating multiple blocks and don't want
-- a separate ruler for each.
mkblock_ruler :: (State.M m) => RulerId -> BlockSpec -> m [TrackId]
mkblock_ruler ruler_id (block_name, tracks) = do
    let block_id = bid block_name
    State.set_namespace test_ns
    tids <- forM (zip [0..] tracks) $ \(i, track) ->
        State.create_track (Id.unpack_id (mk_tid_block block_id i))
            (make_track track)
    State.create_block (Id.unpack_id block_id) $ make_block "b1 title"
        ((Block.RId ruler_id, 20)
            : [(Block.TId tid ruler_id, 40) | tid <- tids])
    State.set_skeleton block_id =<< parse_skeleton block_id
    return tids

parse_skeleton :: (State.M m) => BlockId -> m Skeleton.Skeleton
parse_skeleton block_id = do
    tracks <- State.get_track_info block_id
    return $ Schema.default_parser tracks

mkview :: (State.M m) => BlockId -> m ViewId
mkview block_id = do
    view_id <- State.create_view (Id.unpack_id (mk_vid block_id)) $
        Block.view block_id default_rect default_zoom
    State.set_track_size view_id (400, 800)
    return view_id

mkblock_view :: (State.M m) => BlockSpec -> m [TrackId]
mkblock_view block_spec = mkblock block_spec <* mkview (bid (fst block_spec))

mk_vid :: BlockId -> ViewId
mk_vid block_id = Types.ViewId $ Id.id ns ("v." ++ block_name)
    where (ns, block_name) = Id.un_id (Id.unpack_id block_id)

mk_vid_name :: String -> ViewId
mk_vid_name = mk_vid . bid

-- | Make a TrackId as mkblock does.  This is so tests can independently come
-- up with the track IDs mkblock created just by knowing their tracknum.
mk_tid :: TrackNum -> TrackId
mk_tid = mk_tid_block default_block_id

mk_tid_block :: BlockId -> TrackNum -> TrackId
mk_tid_block block_id i =
    Types.TrackId $ Id.id ns (block_name ++ ".t" ++ show i)
    where (ns, block_name) = Id.un_id (Id.unpack_id block_id)

mk_tid_name :: String -> TrackNum -> TrackId
mk_tid_name = mk_tid_block . bid

-- ** from dump

from_dump :: Simple.Block -> (BlockId, State.State)
from_dump dump = run State.empty (Simple.make_block Block.default_config dump)

-- * state to spec

-- | These can be used from 'Cmd.Lang.LState.save_test' to dump state in
-- a form that can be pasted into a test, trimmed down by hand, and passed to
-- 'mkblocks_skel'.  This way problems that show up in the app can be pasted
-- into a test.
to_spec :: State.State -> [(BlockSpec, [Skeleton.Edge])]
to_spec state = map (block_to_spec state) (Map.keys (State.state_blocks state))

block_to_spec :: State.State -> BlockId -> (BlockSpec, [Skeleton.Edge])
block_to_spec state block_id = ((block_name, map dump_track tracks), skel)
    where
    (id_str, _, tracks, skel) = eval state (Simple.dump_block block_id)
    block_name = snd (Id.un_id (Id.read_id id_str))
    dump_track (_, title, events) = (title, events)

-- * view

select :: (State.M m) => ViewId -> Types.Selection -> m ()
select view_id sel =
    State.set_selection view_id Config.insert_selnum (Just sel)

select_point :: (State.M m) => ViewId -> TrackNum -> ScoreTime -> m ()
select_point view_id tracknum pos =
    select view_id (Types.point_selection tracknum pos)

-- * extract from ustate

extract_tracks :: State.State -> [(String, [Simple.Event])]
extract_tracks ustate = map (\(_, title, events) -> (title, events)) tracks
    where (_, _, tracks, _) = eval ustate (Simple.dump_block default_block_id)

dump_block :: State.State -> BlockId -> Simple.Block
dump_block ustate block_id = eval ustate (Simple.dump_block block_id)

dump_blocks :: State.State -> [Simple.Block]
dump_blocks ustate =
    map (dump_block ustate) (Map.keys (State.state_blocks ustate))

-- * pure make_- functions

make_block :: String -> [(Block.TracklikeId, Types.Width)] -> Block.Block
make_block title tracks = Block.block Block.default_config
    title (map (uncurry Block.track) tracks) Config.schema

-- ** track

event_track_1 = make_track ("1", [(0, 16, "hi"), (30, 32, "there")])
event_track_2 = make_track ("2", [(16, 10, "ho"), (30, 32, "eyo")])

make_track :: TrackSpec -> Track.Track
make_track (title, triplets) = Track.modify_events
    (Events.insert_events (map make_event triplets)) (empty_track title)
empty_track title = Track.track title []

-- ** event

make_event :: (Double, Double, String) -> Events.PosEvent
make_event (pos, dur, text) =
    (realToFrac pos, Event.event text (realToFrac dur))

-- ** ruler

default_ruler = mkruler 16 1
no_ruler = mkruler 0 0
ruler_until pos = ruler [("until", Ruler.marklist [(pos, Ruler.null_mark)])]

-- | TimeStep to step by 1 ScoreTime on the default ruler.
step1 :: TimeStep.TimeStep
step1 = steps 0

steps :: Int -> TimeStep.TimeStep
steps n = TimeStep.time_step n (TimeStep.AbsoluteMark TimeStep.AllMarklists 3)

-- | Create a ruler with a 4/4 "meter" marklist with the given number of marks
-- at the given distance.  Marks are rank [1, 2, 2, ...].
mkruler :: Int -> ScoreTime -> Ruler.Ruler
mkruler marks dist = ruler [marklist marks dist]

ruler mlists =
    Ruler.Ruler (Map.fromList mlists) ruler_bg True False False False
ruler_bg = Color.rgb 1 0.85 0.5
marklist n dist = (MakeRuler.meter_marklist,
    Ruler.marklist (take n $ zip (Seq.range_ 0 dist) m44))
m44 = concatMap (\n -> [major n, minor, minor, minor]) [0..]
major n = Ruler.Mark 1 3 (Color.rgba 0.45 0.27 0 0.35) (show n) 0 0
minor = Ruler.Mark 2 2 (Color.rgba 1 0.39 0.2 0.35) "" 0 0

mark name = Ruler.Mark 0 3 (Color.rgba 0.4 0 0.4 0.4) name 0 0

-- Convert a ruler config for an overlay ruler.
overlay_ruler ruler = ruler
    { Ruler.ruler_show_names = False
    , Ruler.ruler_use_alpha = True
    , Ruler.ruler_full_width = True
    }


-- * extract

block_structure :: State.State -> [(BlockId, [TrackId])]
block_structure state = [(block_id, Block.block_track_ids block)
    | (block_id, block) <- Map.assocs (State.state_blocks state)]

simplify :: State.State -> [Simple.Block]
simplify state = eval state $ mapM Simple.dump_block block_ids
    where
    block_ids = Map.keys (State.state_blocks state)
