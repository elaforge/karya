{-# OPTIONS_GHC -XBangPatterns #-}
module Ui.Block (
    -- * Block model
    Config(..), Block -- no constructors for Block
    , create
    -- ** Model modification
    , get_config, set_config
    , get_title, set_title, get_attrs, set_attrs

    -- ** Track management
    , Tracklike(..)
    , tracks, track_at, insert_track, remove_track

    -- * Block view
    , ViewConfig(..), View, view_block
    , Zoom(..), Selection(..)
    , create_view

    -- ** View modification
    , resize
    , get_view_config, set_view_config
    , get_zoom, set_zoom
    , get_track_scroll, set_track_scroll
    , get_selection, set_selection
    , get_track_width, set_track_width
) where

{-
Fully evaluate arguments and ship them to send_action.
-}

import qualified Control.Concurrent.MVar as MVar
import System.IO.Unsafe

import Ui.Ui (send_action)
import qualified Ui.BlockImpl as B
import Ui.BlockImpl (Block, Config(..), Tracklike(..)
    , View, view_block, ViewConfig(..), Zoom(..), Selection(..)
    )

force = id

view_list :: MVar.MVar [View]
view_list = unsafePerformIO (MVar.newMVar [])

create = B.create
get_title = send_action . B.get_title
set_title block s = send_action (B.set_title block (force s))

-- No serialization needed for these.
get_attrs = B.get_attrs
set_attrs = B.set_attrs

get_config = B.get_config
set_config block config = send_action (B.set_config block config)

-- | How many tracks does 'block' have?
tracks :: Block -> IO Int
tracks block = send_action (B.tracks block)

-- | Return track for the track index.
track_at :: Block -> Int -> IO Tracklike
track_at block at = send_action (B.track_at block at)

insert_track !block !at !track !width =
    send_action (B.insert_track block at track width)
remove_track !block !at = send_action (B.remove_track block at)

-- * views

create_view (!x, !y) (!w, !h) !block !ruler !config = do
    view <- send_action (B.create_view (x, y) (w, h) block ruler config)
    MVar.modifyMVar_ view_list (return . (view:))
    return view

resize !view (!x, !y) (!w, !h) = send_action (B.resize view (x, y) (w, h))

get_zoom !view = send_action (B.get_zoom view)
set_zoom !view !zoom = send_action (B.set_zoom view (force zoom))

get_track_scroll !view = send_action (B.get_track_scroll view)
set_track_scroll !view !offset = send_action (B.set_track_scroll view offset)

get_selection !view !selnum = send_action (B.get_selection view selnum)
set_selection !view !selnum !sel
    = send_action (B.set_selection view selnum (force sel))

get_track_width !view !at = send_action (B.get_track_width view at)
set_track_width !view !at !width =
    send_action (B.set_track_width view at width)

get_view_config !view = send_action (B.get_view_config view)
set_view_config !view !config = send_action
    (B.set_view_config view (force config))
