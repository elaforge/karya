module Ui.TestSetup where

import qualified System.IO as IO

import qualified Ui.Color as Color

import Ui.Types
import qualified Ui.Font as Font

import qualified Ui.Block as Block
import qualified Ui.Event as Event
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
    , Block.vconfig_ruler_size = 18
    , Block.vconfig_status_size = 16
    }

default_divider = Block.Divider Color.blue

-- state

initial_state :: State.UiStateMonad m => m ()
initial_state = do
    let cont text = Config.event text (TrackPos 0)

    ruler <- State.create_ruler "r1" (ruler [marklist 64 10])
    overlay <- State.create_ruler "r1.overlay"
        =<< fmap overlay_ruler (State.get_ruler ruler)

    t0 <- State.create_track "b1.tempo" (empty_track "tempo")
    State.insert_events t0
        [(TrackPos 0, cont ".01"), (TrackPos 50, cont "i.1")]
            -- (TrackPos 100, cont "i.01")]
    t1 <- State.create_track "b1.t1" (empty_track ">fm8/bass")
    let notes = ["6a-", "6b-", "7c-", "7d-", "7e-", "7f-"]
        -- note_events = zip (notes ++ tail (reverse notes)) [0, 10..]
        note_events = zip notes [0, 10..]
    State.insert_events t1 [(TrackPos p, Config.event e (TrackPos 10))
        | (e, p) <- note_events]
    t2 <- State.create_track "b1.t2" (empty_track "velocity")
    State.insert_events t2 [(TrackPos 0, cont "1")]
    b1 <- State.create_block "b1" $ Block.block "hi b1"
        Config.block_config (Block.RId ruler)
        [(Block.TId t0 overlay, 40), (Block.TId t1 overlay, 40),
            (Block.TId t2 overlay, 40)]
        Config.schema
    v1 <- State.create_view "v1"
        (Block.view b1 default_rect Config.view_config)
    return ()
    -- State.set_selection v1 0 (Block.point_selection 0 (TrackPos 0))
    -- _v2 <- State.create_view "v2"
    --     (Block.view b1 (Block.Rect (500, 30) (200, 200))
    --         view_config)

-- track

empty_track title = Track.track title [] Color.white
event_track_1 = Track.modify_events (empty_track "1") (Track.insert_events
    [eventpos 0 "hi" 16, eventpos 30 "there" 32])
event_track_2 = Track.modify_events (empty_track "2") (Track.insert_events
    [eventpos 16 "ho" 10, eventpos 30 "eyo" 32])
eventpos pos name dur = (TrackPos pos, event name (TrackPos dur))

-- event

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
