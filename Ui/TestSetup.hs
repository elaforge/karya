module Ui.TestSetup where

import Control.Monad
import qualified System.IO as IO

import qualified Ui.Color as Color

import Ui.Types
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Font as Font
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.State as State
import qualified Ui.Track as Track

import qualified App.Config as Config


pause = putStr "? " >> IO.hFlush IO.stdout >> getLine >> return ()

-- (10, 50) seems to be the smallest x,y os x will accept.  Apparently
-- fltk's sizes don't take the menu bar into account, which is about 44 pixels
-- high, so a y of 44 is the minimum.
default_rect = Block.Rect (10, 50) (200, 200)

default_block_config = Block.Config
    { Block.config_selection_colors =
        let sel = Color.alpha 0.3 . Color.lighten 0.8 in
            [sel Color.blue, sel Color.green, sel Color.red, sel Color.purple,
                sel Color.yellow]
    , Block.config_bg_color = Color.gray8
    , Block.config_track_box_color = Color.rgb 0.25 1 1
    , Block.config_sb_box_color = Color.rgb 0.25 1 1
    }

default_view_config = Block.ViewConfig
    { Block.vconfig_zoom_speed = 1
    , Block.vconfig_block_title_height = 20
    , Block.vconfig_track_title_height = 20
    , Block.vconfig_sb_size = 12
    , Block.vconfig_status_size = 16
    }

default_divider = Block.Divider Color.blue

-- state

mkid = Id.id "test"
default_zoom = Config.zoom

-- TODO convert to use mkstate
initial_state :: State.UiStateMonad m => m ()
initial_state = do
    let cont text = Config.event text (TrackPos 0)

    ruler <- State.create_ruler (mkid "r1") (ruler [marklist 64 10])
    overlay <- State.create_ruler (mkid "r1.overlay")
        =<< fmap overlay_ruler (State.get_ruler ruler)

    t0 <- State.create_track (mkid "b1.tempo") (empty_track "tempo")
    State.insert_events t0
        [(TrackPos 0, cont ".05"), (TrackPos 50, cont "i.1")]
            -- (TrackPos 100, cont "i.01")]
    t1 <- State.create_track (mkid "b1.t1") (empty_track ">fm8/bass")
    let notes = ["6a-", "6b-", "7c-", "7d-", "7e-", "7f-"]
        -- note_events = zip (notes ++ tail (reverse notes)) [0, 10..]
        note_events = zip notes [0, 10..]
    State.insert_events t1 [(TrackPos p, Config.event e (TrackPos 10))
        | (e, p) <- note_events]
    t2 <- State.create_track (mkid "b1.t2") (empty_track "velocity")
    State.insert_events t2 [(TrackPos 0, cont "1")]
    b1 <- State.create_block (mkid "b1") $ Block.block "hi b1"
        Config.block_config
        [(Block.RId ruler, 20), (Block.TId t0 overlay, 40),
            (Block.TId t1 overlay, 40), (Block.TId t2 overlay, 40)]
        Config.schema
    v1 <- State.create_view (mkid "v1")
        (Block.view b1 default_rect default_zoom Config.view_config)
    return ()

mkstate :: [TrackSpec] -> ([Track.TrackId], State.State)
mkstate tracks = State.run_state State.empty $ do
    ruler <- State.create_ruler (mkid "r1") no_ruler
    tids <- forM (zip [0..] tracks) $ \(i, track) -> do
        State.create_track (mkid ("b1." ++ show i)) (mktrack track)
    State.create_block (mkid "b1") $
        Block.block "b1 title" default_block_config
            ((Block.RId ruler, 20) : [(Block.TId tid ruler, 40) | tid <- tids])
            (Block.SchemaId (mkid "no schema"))
    return tids

-- track

type TrackSpec = (String, [(Double, Double, String)])

event_track_1 = mktrack ("1", [(0, 16, "hi"), (30, 32, "there")])
event_track_2 = mktrack ("2", [(16, 10, "ho"), (30, 32, "eyo")])

mktrack :: TrackSpec -> Track.Track
mktrack (title, triplets) = Track.modify_events (empty_track title)
    (Track.insert_events (map mkevent triplets))
empty_track title = Track.track title [] Config.track_bg Config.render_config

-- event

mkevent (pos, dur, text) =
    (TrackPos pos, event text (TrackPos dur))
event text dur =
    Event.Event text dur (Color.rgb 0.9 0.9 0.7) default_style False
default_style = Font.TextStyle Font.Helvetica [] 9 Color.black

-- ruler

default_ruler = mkruler 20 10
no_ruler = mkruler 0 0

mkruler marks dist = Ruler.Ruler [marklist marks dist] ruler_bg True False False
ruler mlists = Ruler.Ruler mlists ruler_bg True False False
ruler_bg = Color.rgb 1 0.85 0.5
marklist n dist =
    Ruler.marklist "meter" (take n $ zip (map TrackPos [0, dist ..]) m44)
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
