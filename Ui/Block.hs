{-# OPTIONS_GHC -XBangPatterns #-}
module Ui.Block (
    -- * Block model
    Config(..), Block -- no constructors for Block
    , create
    -- ** Model modification
    , get_config, set_config
    , get_title, set_title, get_attrs, set_attrs

    -- ** Track management
    , TrackNum, Width, SelNum, Tracklike(..)
    , tracks, track_at, insert_track, remove_track

    -- * Block view
    , View, Rect(..), ViewConfig(..), Zoom(..), Selection(..)
    , create_view, view_block

    -- ** View modification
    , get_size, set_size
    , get_view_config, set_view_config
    , get_zoom, set_zoom
    , get_track_scroll, set_track_scroll
    , get_selection, set_selection
    , get_track_width, set_track_width
) where

{-
Fully evaluate arguments and ship them to send_action.
-}

import Ui.Ui (send_action)
import qualified Ui.BlockImpl as B
import Ui.BlockImpl (Block, Config(..), Tracklike(..)
    , TrackNum, SelNum, Width
    , View, Rect(..), ViewConfig(..), Zoom(..), Selection(..)
    , view_block
    )

create = B.create
get_title = send_action . B.get_title
set_title block s = send_action (B.set_title block s)

-- No serialization needed for these.
get_attrs = B.get_attrs
set_attrs = B.set_attrs

get_config = B.get_config
set_config block config = send_action (B.set_config block config)

-- | How many tracks does 'block' have?
tracks :: Block -> IO Int
tracks block = send_action (B.tracks block)

-- | Return track for the track index.
track_at :: Block -> TrackNum -> IO (Tracklike, Width)
track_at block at = send_action (B.track_at block at)

insert_track block at track width =
    send_action (B.insert_track block at track width)
remove_track block at = send_action (B.remove_track block at)

-- * views

create_view rect block ruler config =
    send_action (B.create_view rect block ruler config)

-- | Get the current size of the view window.
get_size :: View -> IO Rect
get_size view = send_action (B.get_size view)

set_size view rect = send_action (B.set_size view rect)

get_zoom view = send_action (B.get_zoom view)
set_zoom view zoom = send_action (B.set_zoom view zoom)

get_track_scroll :: View -> IO Int
get_track_scroll view = send_action (B.get_track_scroll view)
set_track_scroll :: View -> Int -> IO ()
set_track_scroll view offset = send_action (B.set_track_scroll view offset)

-- | Get the selection on the given view.
get_selection :: View -> Int -> IO Selection
get_selection view selnum = send_action (B.get_selection view selnum)
set_selection view selnum sel
    = send_action (B.set_selection view selnum sel)

get_track_width view at = send_action (B.get_track_width view at)
set_track_width view at width =
    send_action (B.set_track_width view at width)

get_view_config view = send_action (B.get_view_config view)
set_view_config view config = send_action
    (B.set_view_config view config)
