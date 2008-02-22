{-# OPTIONS_GHC -XBangPatterns #-}
module Interface.Block (
    BlockModelConfig(..), Block -- no constructors for Block
    , create
    -- , select_colors
    , get_title, set_title, get_attrs, set_attrs

    -- * Track management
    , Tracklike(..)
    , insert_track, remove_track

    , BlockViewConfig(..), BlockView
    , Zoom(..), Selection(..)
    , create_view
    , redraw, resize, get_zoom, set_zoom
    , get_selection, set_selection, get_view_config, set_view_config
) where

{-
Fully evaluate arguments and ship them to send_action.
-}

import qualified Control.Concurrent.MVar as MVar
import System.IO.Unsafe

import Interface.Ui (send_action)
import qualified Interface.BlockImpl as B
import Interface.BlockImpl (Block, BlockModelConfig(..), Tracklike(..)
    , BlockView, BlockViewConfig(..), Zoom(..), Selection(..)
    )

force = id

view_list :: MVar.MVar [BlockView]
view_list = unsafePerformIO MVar.newEmptyMVar

create = B.create
get_title = send_action . B.get_title
set_title block s = send_action (B.set_title block (force s))

-- No serialization needed for these.
get_attrs = B.get_attrs
set_attrs = B.set_attrs

insert_track !block !at !track !width =
    send_action (B.insert_track block at track width)
remove_track !block !at = send_action (B.remove_track block at)

-- * views

create_view (!x, !y) (!w, !h) !block !ruler !config = do
    view <- send_action (B.create_view (x, y) (w, h) block ruler config)
    MVar.modifyMVar_ view_list (return . (view:))
    return view

redraw !view = send_action (redraw view)
resize !view (!x, !y) (!w, !h) = send_action (B.resize view (x, y) (w, h))

get_zoom !view = send_action (B.get_zoom view)
set_zoom !view !zoom = send_action (B.set_zoom view (force zoom))

get_selection !view = send_action (B.get_selection view)
set_selection !view !sel = send_action (B.set_selection view (force sel))

get_view_config !view = send_action (B.get_view_config view)
set_view_config !view !config = send_action
    (B.set_view_config view (force config))
