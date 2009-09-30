{- | Global static app defaults.
-}
module App.Config where
import qualified Network
import qualified System.Directory as Directory
import System.FilePath ((</>))

import qualified Util.File as File

import qualified Ui.Id as Id
import qualified Ui.Color as Color
import qualified Ui.Block as Block
import qualified Ui.Track as Track
import qualified Ui.Types as Types
import qualified Ui.UiMsg as UiMsg

import qualified Perform.Midi.Controller as Controller


-- * paths

-- | All paths should be relative to this one.
-- I may later change this to an env var, a flag, or just leave it hardcoded.
get_app_dir :: IO FilePath
get_app_dir = return "."

-- | All code and data local to an installation (i.e. specific to a particular
-- configuration) should go here.
local_dir :: FilePath
local_dir = "Local"

-- | Store instrument db code and data.
instrument_dir :: FilePath
instrument_dir = local_dir </> "Instrument"

-- | Local CmdL code goes here.
lang_dir :: FilePath
lang_dir = local_dir </> "Lang"

-- | Where to look for the instrument db cache.
instrument_db_cache :: FilePath
instrument_db_cache = instrument_dir </> "inst.db"

-- * lang

-- | Port to listen on for language requests.
lang_port = Network.UnixSocket "seq_language"
initialize_lang_port = File.ignore_enoent $ Directory.removeFile "seq_language"

-- | This string coming from the lang socket indicates that the message is
-- complete and the server should process it and send a response.  It's
-- necessary because I can't exactly use EOF for this if I want to send
-- a response.
message_complete_token :: String
message_complete_token = "\n\NUL"

-- * input

-- | This is the primary mouse button, used to set the insertion selection.
mouse_select :: UiMsg.MouseButton
mouse_select = 1

-- * selnums

-- | SelNum of the insertion selection.
insert_selnum :: Types.SelNum
insert_selnum = 0

-- | SelNum of the play position indicator.
play_position_selnum :: Types.SelNum
play_position_selnum = 4
play_position_color :: Color.Color
play_position_color = make_selection_color Color.purple


-- * colors

box_color = Color.rgb 0.7 0.7 0.7
raw_edit_color = Color.rgb 0.25 0.25 1
val_edit_color = Color.rgb 1 0.5 0.5
-- Similar to val color because you toggle between val and method.
method_edit_color = Color.rgb 0.6 0 0

play_color = Color.rgb 0 0.6 0
warning_color = Color.rgb 1 0.2 0.2
abbreviation_color = Color.rgb 0 0 1

-- * defaults

-- | New project scale id.
project_scale_id :: String
project_scale_id = "twelve"

-- | Controllers start out with this range.
controller_pb_range :: Controller.PbRange
controller_pb_range = (-2, 2)

-- | Default size of new views.
view_size :: (Int, Int)
view_size = (300, 300)

-- | Create new views at this zoom.
zoom :: Types.Zoom
zoom = Types.Zoom 0 46

-- | Default width for the block ruler track.
ruler_width :: Types.Width
ruler_width = 18

-- | Default width for new tracks.
track_width :: Types.Width
track_width = 30

render_config = Track.RenderConfig Track.NoRender render_color
render_color = Color.rgba 0.65 0.65 0.8 0.5

-- | SchemaId of the default hardcoded schema.  This should probably go in
-- Schema, but being here modules can get it without depending on Schema.
schema :: Types.SchemaId
schema = Types.SchemaId (Id.global "default")

-- | The default namespace for the clipboard.
clip_namespace :: Id.Namespace
clip_namespace = "clip"

-- | The copied block will be BlockId (Id.id clip_namespace clip_block_name).
clip_block_name :: String
clip_block_name = "clip"

make_selection_color = Color.alpha 0.3 . Color.lighten 0.8

-- ** colors

track_bg = Color.white
ruler_bg = Color.rgb 1 0.85 0.5
muted_track_bg = Color.gray6
solo_track_bg = Color.rgb 1 0.75 0.75

block_config = Block.Config
    { Block.config_selection_colors =
        let sel = make_selection_color in
            [sel Color.blue, sel Color.green, sel Color.red, sel Color.yellow,
                play_position_color]
    , Block.config_bg_color = Color.gray8
    , Block.config_track_box = (box_color, ' ')
    , Block.config_sb_box = (box_color, ' ')
    }

view_config = Block.ViewConfig
    { Block.vconfig_block_title_height = 20
    , Block.vconfig_track_title_height = 20
    , Block.vconfig_skel_height = 16
    , Block.vconfig_sb_size = 12
    , Block.vconfig_status_size = 16
    }
