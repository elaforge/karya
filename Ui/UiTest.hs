module Ui.UiTest where
import qualified Data.Map as Map

import Control.Monad
import qualified Control.Monad.Identity as Identity
import qualified System.IO as IO
import qualified Util.Rect as Rect

import qualified Ui.Color as Color

import Ui
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Types as Types

import qualified Derive.Schema as Schema

import qualified Cmd.Simple as Simple

import qualified App.Config as Config


pause = putStr "? " >> IO.hFlush IO.stdout >> getLine >> return ()

-- (10, 50) seems to be the smallest x,y os x will accept.  Apparently
-- fltk's sizes don't take the menu bar into account, which is about 44 pixels
-- high, so a y of 44 is the minimum.
default_rect = Rect.xywh 10 50 200 200

default_divider = Block.Divider Color.blue

-- state

test_ns = "test"
mkid = Id.id test_ns
bid = Types.BlockId . mkid
vid = Types.ViewId . mkid
tid = Types.TrackId . mkid
rid = Types.RulerId . mkid

default_zoom = Config.zoom

-- * mkstate

default_block_id = bid default_block_name
default_block_name = "b1"
default_view_id = vid "v1"

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

run_mkstate :: [TrackSpec] -> ([TrackId], State.State)
run_mkstate track_specs =
    run State.empty (mkstate default_block_name track_specs)

run_mkview :: [TrackSpec] -> ([TrackId], State.State)
run_mkview track_specs =
    run State.empty (mkstate_view default_block_name track_specs)

mkstate :: (State.M m) => String -> [TrackSpec] -> m [TrackId]
mkstate block_name tracks = mkstate_id (bid block_name) tracks

mkstate_id :: (State.M m) => BlockId -> [TrackSpec] -> m [TrackId]
mkstate_id block_id tracks = do
    let (ns, block_name) = Id.un_id (Id.unpack_id block_id)
    ruler_id <- State.create_ruler (Id.id ns (block_name ++ ".r0"))
        default_ruler
    mkstate_id_ruler block_id ruler_id tracks

-- | Like 'mkstate_id', but uses the provided ruler instead of creating its
-- own.  Important if you are creating multiple blocks and don't want
-- a separate ruler for each.
mkstate_id_ruler :: (State.M m) => BlockId -> RulerId
    -> [TrackSpec] -> m [TrackId]
mkstate_id_ruler block_id ruler_id tracks = do
    State.set_namespace test_ns
    tids <- forM (zip [0..] tracks) $ \(i, track) ->
        State.create_track (Id.unpack_id (mk_tid_block block_id i))
            (mktrack track)
    State.create_block (Id.unpack_id block_id) $ mkblock "b1 title"
        ((Block.RId ruler_id, 20)
            : [(Block.TId tid ruler_id, 40) | tid <- tids])
    State.set_skeleton block_id =<< parse_skeleton block_id
    return tids

parse_skeleton block_id = do
    tracks <- State.get_track_info block_id
    return $ Schema.default_parser tracks

mkview :: (State.M m) => m ViewId
mkview = do
    view_id <- State.create_view (Id.unpack_id default_view_id) $
        Block.view default_block_id default_rect default_zoom
    State.set_track_size view_id (400, 800)
    return view_id

mkstate_view block_id tracks = do
    r <- mkstate block_id tracks
    mkview
    return r

-- | Make a TrackId as mkstate does.  This is so tests can independently come
-- up with the track IDs mkstate created just by knowing their tracknum.
mk_tid :: TrackNum -> TrackId
mk_tid = mk_tid_block default_block_id

mk_tid_block :: BlockId -> TrackNum -> TrackId
mk_tid_block block_id i =
    Types.TrackId $ Id.id ns (block_name ++ ".t" ++ show i)
    where (ns, block_name) = Id.un_id (Id.unpack_id block_id)

mk_tid_name :: String -> TrackNum -> TrackId
mk_tid_name block_name = mk_tid_block (bid block_name)

-- * view

select :: (State.M m) => ViewId -> Types.Selection -> m ()
select view_id sel = State.set_selection view_id Config.insert_selnum (Just sel)

select_point :: (State.M m) => ViewId -> TrackNum -> ScoreTime -> m ()
select_point view_id tracknum pos =
    select view_id (Types.point_selection tracknum pos)

-- * extract from ustate

extract_tracks :: State.State -> [(String, [Simple.Event])]
extract_tracks ustate = map (\(_, title, events) -> (title, events)) tracks
    where (_, _, tracks) = eval ustate (Simple.dump_block default_block_id)

dump_block :: State.State -> BlockId -> Simple.Block
dump_block ustate block_id = eval ustate (Simple.dump_block block_id)

dump_blocks :: State.State -> [Simple.Block]
dump_blocks ustate =
    map (dump_block ustate) (Map.keys (State.state_blocks ustate))

-- * block

mkblock :: String -> [(Block.TracklikeId, Types.Width)] -> Block.Block
mkblock title tracks = Block.block Block.default_config
    title (map (uncurry Block.track) tracks) Config.schema

-- * track

type TrackSpec = (String, [(Double, Double, String)])

event_track_1 = mktrack ("1", [(0, 16, "hi"), (30, 32, "there")])
event_track_2 = mktrack ("2", [(16, 10, "ho"), (30, 32, "eyo")])

mktrack :: TrackSpec -> Track.Track
mktrack (title, triplets) = Track.modify_events
    (Track.insert_events (map mkevent triplets)) (empty_track title)
empty_track title = Track.track title [] Config.track_bg Config.render_config

-- * event

mkevent :: (Double, Double, String) -> Track.PosEvent
mkevent (pos, dur, text) = (realToFrac pos, Event.event text (realToFrac dur))

-- * ruler

default_ruler = mkruler 256 1
no_ruler = mkruler 0 0
ruler_until pos = ruler [Ruler.marklist "until" [(pos, Ruler.null_mark)]]

mkruler marks dist = Ruler.Ruler [marklist marks dist] ruler_bg
    True False False False
ruler mlists = Ruler.Ruler mlists ruler_bg True False False False
ruler_bg = Color.rgb 1 0.85 0.5
marklist n dist = Ruler.marklist "meter" (take n $ zip [0, dist ..] m44)
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
