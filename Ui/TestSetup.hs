module Ui.TestSetup where

import qualified System.IO as IO

import qualified Ui.Color as Color

import Ui.Types
import qualified Ui.Block as Block
import qualified Ui.Ruler as Ruler
import qualified Ui.Track as Track
import qualified Ui.Event as Event


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

-- track

empty_track = Track.track "track1" [] Color.white
event_track_1 = Track.modify_events empty_track (Track.insert_events
    [event 0 "hi" 16, event 30 "there" 32])
event_track_2 = Track.modify_events empty_track (Track.insert_events
    [event 16 "ho" 10, event 30 "eyo" 32])
event pos name dur = (TrackPos pos, Event.event name (TrackPos dur))

-- ruler

default_ruler = mkruler 20 10
no_ruler = mkruler 0 0

mkruler marks dist = Ruler.Ruler [marklist marks dist] ruler_bg True False False
ruler_bg = Color.rgb 1 0.85 0.5
marklist n dist = Ruler.marklist (take n $ zip (map TrackPos [0, dist ..]) m44)
m44 = concatMap (\n -> [major n, minor, minor, minor]) [0..]
major n = Ruler.Mark 1 3 (Color.rgba 0.45 0.27 0 0.35) (show n) 0 0
minor = Ruler.Mark 2 2 (Color.rgba 1 0.39 0.2 0.35) "" 0 0

-- Convert a ruler config for an overlay ruler.
overlay_ruler ruler = ruler
    { Ruler.ruler_show_names = False
    , Ruler.ruler_use_alpha = True
    , Ruler.ruler_full_width = True
    }
