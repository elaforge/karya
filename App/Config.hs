{- | Global static app defaults.
-}
module App.Config where
import qualified Control.Exception as Exception
import qualified Network
import qualified System.Directory as Directory

import qualified Ui.Font as Font
import qualified Ui.Color as Color
import qualified Ui.Block as Block
import qualified Ui.Ruler as Ruler

import qualified Ui.Event as Event


-- * lang

-- | Port to listen on for language requests.
lang_port = Network.UnixSocket "seq_language"
initialize_lang_port =
    Exception.handle (\exc -> print exc) $ Directory.removeFile "seq_language"

-- | This string coming from the lang socket indicates that the message is
-- complete and the server should process it and send a response.  It's
-- necessary because I can't exactly use EOF for this if I want to send
-- a response.
message_complete_token :: String
message_complete_token = "\n\NUL"

-- * selnums

-- | SelNum of the insertion point.
insert_selnum :: Block.SelNum
insert_selnum = 0

-- | SelNum of the play position indicator.
play_position_selnum :: Block.SelNum
play_position_selnum = 4
play_position_color :: Color.Color
play_position_color = make_selection_color Color.purple


-- * colors

box_color = Color.rgb 0.25 1 1
edit_color = Color.rgb 1 0.5 0.5
play_color = Color.rgb 0 0.6 0
warning_color = Color.rgb 1 0.2 0.2

-- * defaults

-- | Default size of new views.
view_size :: (Int, Int)
view_size = (300, 300)

schema = Block.SchemaId "default"

event text dur = Event.Event text dur (Color.rgb 0.9 0.9 0.7) style
    False
style = Font.TextStyle Font.Helvetica [] 9 Color.black

make_selection_color = Color.alpha 0.3 . Color.lighten 0.8

track_bg = Color.white
ruler_bg = Color.rgb 1 0.85 0.5

block_config = Block.Config
    { Block.config_selection_colors =
        let sel = make_selection_color in
            [sel Color.blue, sel Color.green, sel Color.red, sel Color.yellow,
                play_position_color]
    , Block.config_bg_color = Color.gray8
    , Block.config_track_box_color = box_color
    , Block.config_sb_box_color = box_color
    }

view_config = Block.ViewConfig
    { Block.vconfig_zoom_speed = 1
    , Block.vconfig_block_title_height = 20
    , Block.vconfig_track_title_height = 20
    , Block.vconfig_sb_size = 12
    , Block.vconfig_ruler_size = 18
    , Block.vconfig_status_size = 16
    }
