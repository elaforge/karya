{- | Global static app defaults.
-}
module App.Config where
import qualified Network
import qualified System.Directory as Directory
import System.FilePath ((</>))
import qualified System.Info

import qualified Util.File as File
import qualified Ui.Color as Color
import qualified Ui.Id as Id
import qualified Ui.Style as Style
import qualified Ui.Types as Types

import qualified Perform.Midi.Control as Control


#include "fltk/config.h"

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

-- | Directory for instruments with slow patch loading to save their caches.
instrument_cache_dir :: FilePath
instrument_cache_dir = "db"

-- * status view

-- The block status bar is not very big, so it's important to control what
-- goes in there.

-- ** per-view

-- | Selection start and range.
status_selection :: String
status_selection = "s"

-- | Zoom and scroll of the visible area.
status_zoom :: String
status_zoom = "z"

-- ** global

-- | Base octave of the kbd note entry.
status_octave :: String
status_octave = "8"

-- | Current time step.
status_step :: String
status_step = "t"

-- | Text of the last note, even if it didn't create an event.  Useful to know
-- what a key would enter.
status_note :: String
status_note = "n"

-- | Track 'Cmd.state_note_text', which is the previously entered note track
-- text.  This is useful e.g. to set an attribute and maintain that for
-- several notes in a row.
status_note_text :: String
status_note_text = "note"

-- | Various record flags.  Most are reflected in the color of the edit box,
-- but the secondary ones go here.
status_record :: String
status_record = "r"


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
mouse_select :: Types.MouseButton
mouse_select = 1

-- * selnums

-- | SelNum of the insertion selection.
insert_selnum :: Types.SelNum
error_selnum :: Types.SelNum
step_play_selnum :: Types.SelNum
-- | SelNum of the play position indicator.
play_position_selnum :: Types.SelNum

[ (insert_selnum, _)
    -- Unused.  Secondary select?
    , _
    -- Temporary insert point,  to indicate insert position when it's not the
    -- insert_selnum.
    , (temporary_insert_selnum, _)
    -- Highlight errors.
    , (error_selnum, _)
    -- Display current play position, managed by updater thread.
    , (play_position_selnum, play_selection_color)
    -- Display current step play position.
    , (step_play_selnum, _)
    ] = zip [0..] bconfig_selection_colors

bconfig_selection_colors :: [Color.Color]
bconfig_selection_colors = take max_selections $ map make_selection_color
    [Color.blue, Color.green, Color.yellow, Color.red, Color.purple,
        Color.turquoise]

-- * colors

box_color, raw_edit_color, val_edit_color, method_edit_color :: Color.Color
box_color = Color.rgb 0.7 0.7 0.7
raw_edit_color = Color.rgb 0.25 0.25 1
val_edit_color = Color.rgb 1 0.5 0.5
-- Similar to val color because you toggle between val and method.
method_edit_color = Color.rgb 0.6 0 0

-- | Colors indicate if 'Cmd.state_advance' is set.
advance_color, no_advance_color :: Color.Color
advance_color = play_color
no_advance_color = val_edit_color

play_color, warning_color :: Color.Color
play_color = Color.rgb 0 0.6 0
warning_color = Color.rgb 1 0.2 0.2

abbreviation_color :: Color.Color
abbreviation_color = Color.rgba_word (#const Config::abbreviation_color_word)

busy_color :: Color.Color
busy_color = Color.rgb 0 1 1

mute_color, solo_color :: Color.Color
mute_color = Color.gray6
solo_color = Color.rgb 1 0.75 0.75

track_bg, ruler_bg :: Color.Color
track_bg = Color.white
ruler_bg = Color.rgb 1 0.85 0.5

-- * defaults

-- | Default scale id for new projects.  It's a string so I don't have import
-- anything.
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

-- | Default width for new tracks.  This should just fit common control track
-- contents, e.g. @e (4f#)@.
track_width :: Types.Width
track_width = 37

-- | Width of the divider that represents a collapsed track.
collapsed_width :: Types.Width
collapsed_width = 3

render_color :: Color.Color
render_color = Color.rgba 0.65 0.65 0.8 0.5

-- | The default namespace for the clipboard.
clip_namespace :: Id.Namespace
clip_namespace = Id.unsafe_namespace "clip"

-- | The copied block will be BlockId (Id.id clip_namespace clip_block_name).
clip_block_name :: String
clip_block_name = "clip"

make_selection_color :: Color.Color -> Color.Color
make_selection_color = Color.alpha 0.3 . Color.brightness 1.25

-- * hardcoded configs

-- These are for 'Ui.Block.Config' and 'Ui.Block.ViewConfig', using them
-- directly would cause a circular import.

bconfig_bg_color :: Color.Color
bconfig_bg_color = Color.gray8

-- | Default contents of track and sb boxes.
bconfig_box :: (Color.Color, Char)
bconfig_box = (box_color, ' ')

-- | Maximum number of selections supported by the GUI.
max_selections :: Int
max_selections = #const Config::max_selections

-- | This is the number of pixels taken up by the various gizmos in the window
-- track beyond the main track view.  Only correct when the window is first
-- created, since the skel_height may be dragged around.
view_time_padding :: Int
view_time_padding =
    #const Config::View::track_title_height
    + #const Config::View::skel_height
    + #const Config::View::status_size
    + #const Config::View::sb_size

block_title_height :: Int
block_title_height = #const Config::View::block_title_height

view_track_padding :: Int
view_track_padding = #const Config::View::sb_size


-- * fonts

emmentaler :: String
emmentaler = case System.Info.os of
    "darwin" -> "Emmentaler 11"
    _ -> " Emmentaler"


-- * event style

-- | Like Symbols, these are sent to the UI layer at startup and then remain
-- static.
styles :: [Style.Style]
styles =
    [ plain 0.9 0.9 0.7 -- default_style, is also note style
    , plain 0.8 0.9 0.8 -- control_style
    , plain 0.9 0.8 0.9 -- pitch_style
    , plain 1.0 1.0 0.65 -- declaration_style
    , plain 1.0 0.8 0.8
    ]
    where
    plain r g b = Style.Style Style.Helvetica [] 12 Color.black
        (Color.rgb r g b)

-- | Normal events.
default_style :: Style.StyleId
control_style :: Style.StyleId
pitch_style :: Style.StyleId
-- | Events that affect further derivation and don't output any notes
-- themselves, e.g. @x = y@.
declaration_style :: Style.StyleId
-- | Events that can't be parsed.
parse_error_style :: Style.StyleId

default_style : control_style : pitch_style : declaration_style
    : parse_error_style : _ = map Style.StyleId [0..]
