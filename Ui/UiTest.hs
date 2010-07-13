module Ui.UiTest where

import Control.Monad
import qualified Control.Monad.Identity as Identity
import qualified System.IO as IO

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
default_rect = Types.Rect 10 50 200 200

default_divider = Block.Divider Color.blue

-- state

test_ns = "test"
mkid = Id.id test_ns
bid = Types.BlockId . mkid
vid = Types.ViewId . mkid
tid = Types.TrackId . mkid

default_zoom = Config.zoom

-- TODO convert to use mkstate
initial_state :: State.UiStateMonad m => m ()
initial_state = do
    let cont text = Event.event text (ScoreTime 0)

    ruler <- State.create_ruler (mkid "r1") (ruler [marklist 64 10])
    overlay <- State.create_ruler (mkid "r1.overlay")
        =<< fmap overlay_ruler (State.get_ruler ruler)

    t0 <- State.create_track (mkid "b1.tempo") (empty_track "tempo")
    State.insert_events t0
        [(ScoreTime 0, cont ".05"), (ScoreTime 50, cont "i.1")]
            -- (ScoreTime 100, cont "i.01")]
    t1 <- State.create_track (mkid "b1.t1") (empty_track ">fm8/bass")
    let notes = ["6a", "6b", "7c", "7d", "7e", "7f"]
        -- note_events = zip (notes ++ tail (reverse notes)) [0, 10..]
        note_events = zip notes [0, 10..]
    State.insert_events t1 [(ScoreTime p, Event.event e (ScoreTime 10))
        | (e, p) <- note_events]
    t2 <- State.create_track (mkid "b1.t2") (empty_track "velocity")
    State.insert_events t2 [(ScoreTime 0, cont "1")]
    b1 <- State.create_block (mkid "b1") $
        mkblock "hi b1" [(Block.RId ruler, 20), (Block.TId t0 overlay, 40),
            (Block.TId t1 overlay, 40), (Block.TId t2 overlay, 40)]
    State.create_view (mkid "v1") (Block.view b1 default_rect default_zoom)
    return ()

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

exec state m = case State.exec state m of
    Left err -> error $ "state error: " ++ show err
    Right state' -> state'

eval :: State.State -> State.StateId a -> a
eval state m = case State.eval state m of
    Left err -> error $ "state error: " ++ show err
    Right val -> val

run_mkstate track_specs = run State.empty (mkstate "b1" track_specs)
run_mkview track_specs = run State.empty (mkstate_view "b1" track_specs)

mkstate :: (State.UiStateMonad m) => String -> [TrackSpec] -> m [TrackId]
mkstate block_name tracks = mkstate_id (bid block_name) tracks

mkstate_id :: (State.UiStateMonad m) => BlockId -> [TrackSpec] -> m [TrackId]
mkstate_id block_id tracks = do
    State.set_project test_ns
    let (ns, block_name) = Id.un_id (Id.unpack_id block_id)
        mkid = Id.id ns
    tids <- forM (zip [0..] tracks) $ \(i, track) -> do
        State.create_track (mkid (block_name ++ ".t" ++ show i)) (mktrack track)

    ruler <- State.create_ruler (mkid (block_name ++ ".r0")) default_ruler
    State.create_block (mkid block_name) $
        mkblock "b1 title"
            ((Block.RId ruler, 20) : [(Block.TId tid ruler, 40) | tid <- tids])
    State.set_skeleton block_id =<< parse_skeleton block_id
    return tids

parse_skeleton block_id = do
    tracks <- State.get_track_info block_id
    return $ Schema.default_parser tracks

mkview :: (State.UiStateMonad m) => m ViewId
mkview = State.create_view (Id.unpack_id default_view_id) $
    Block.view default_block_id default_rect default_zoom

mkstate_view block_id tracks = do
    r <- mkstate block_id tracks
    mkview
    return r

-- * extract from ustate

extract_tracks :: State.State -> [(String, [Simple.Event])]
extract_tracks ustate = map (\(_, title, events) -> (title, events)) tracks
    where (_, _, tracks) = eval ustate (Simple.dump_block default_block_id)

-- * block

mkblock :: String -> [(Block.TracklikeId, Types.Width)] -> Block.Block
mkblock title tracks = Block.block Block.default_config
    title (map (uncurry Block.block_track) tracks) Config.schema

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

default_ruler = mkruler 10 10
no_ruler = mkruler 0 0
ruler_until pos = ruler [Ruler.marklist "until" [(pos, Ruler.null_mark)]]

mkruler marks dist = Ruler.Ruler [marklist marks dist] ruler_bg
    True False False False
ruler mlists = Ruler.Ruler mlists ruler_bg True False False False
ruler_bg = Color.rgb 1 0.85 0.5
marklist n dist =
    Ruler.marklist "meter" (take n $ zip (map ScoreTime [0, dist ..]) m44)
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
