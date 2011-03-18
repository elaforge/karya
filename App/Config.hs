{- | Global static app defaults.
-}
module App.Config where
import qualified Network
import qualified System.Directory as Directory
import System.FilePath ((</>))

import qualified Util.File as File

import qualified Ui.Id as Id
import qualified Ui.Color as Color
import qualified Ui.Track as Track
import qualified Ui.Types as Types
import qualified Ui.UiMsg as UiMsg

import qualified Perform.Midi.Control as Control


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
instrument_dir = "inst_db"

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

error_selnum :: Types.SelNum
error_selnum = 3
error_selnum_color :: Color.Color
error_selnum_color = make_selection_color Color.red

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
busy_color = Color.rgb 0 1 1

mute_color = Color.gray6
solo_color = Color.rgb 1 0.75 0.75

track_bg = Color.white
ruler_bg = Color.rgb 1 0.85 0.5

-- * defaults

-- | Default scale id for new projects.
default_scale_id :: String
default_scale_id = "twelve"

-- | Default range for instruments that don't say otherwise.
control_pb_range :: Control.PbRange
control_pb_range = (-2, 2)

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

make_selection_color :: Color.Color -> Color.Color
make_selection_color = Color.alpha 0.3 . Color.brightness 1.25

-- * hardcoded configs

bconfig_selection_colors =
    [sel Color.blue, sel Color.green, sel Color.yellow, error_selnum_color,
        play_position_color]
    where sel = make_selection_color
bconfig_bg_color = Color.gray8
bconfig_track_box = (box_color, ' ')
bconfig_sb_box = (box_color, ' ')

vconfig_block_title_height, vconfig_track_title_height, vconfig_skel_height,
    vconfig_sb_size, vconfig_status_size :: Int
vconfig_block_title_height = 20
vconfig_track_title_height = 20
vconfig_skel_height = 16
vconfig_sb_size = 12
vconfig_status_size = 16
