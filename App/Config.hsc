-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Global static app defaults.
-}
module App.Config where
import qualified Data.Array.IArray as IArray
import qualified Data.Bits as Bits
import qualified Network
import qualified System.Directory as Directory
import System.FilePath ((</>))
import qualified System.Info
import Util.Control

import qualified Util.Array as Array
import qualified Util.File as File
import qualified Util.Thread as Thread

import qualified Ui.Color as Color
import qualified Ui.Id as Id
import qualified Ui.Style as Style
import qualified Ui.Types as Types

import qualified Perform.Midi.Control as Control


#include "fltk/config.h"


-- | These go into the git save files, because git insists on having them.
name, email :: String
name = "Evan Laforge"
email = "qdunkan@gmail.com"

data Platform = Mac | Linux deriving (Show, Eq)

platform :: Platform
platform = case System.Info.os of
    "darwin" -> Mac
    _ -> Linux
    -- That's all there is, right?

-- * paths

-- | Paths which are intended to be relative to the app dir get this type,
-- so it's harder to accidentally use them directly.
newtype RelativePath = RelativePath FilePath deriving (Show)

make_path :: FilePath -> RelativePath -> FilePath
make_path app_dir (RelativePath path) = app_dir </> path

-- | All paths should be relative to this one.
-- I may later change this to an env var, a flag, or just leave it hardcoded.
get_app_dir :: IO FilePath
get_app_dir = return "."

-- | All code and data local to an installation (i.e. specific to a particular
-- configuration) should go here.
local_dir :: RelativePath
local_dir = RelativePath "Local"

-- | Store instrument db code and data.
instrument_dir :: RelativePath
instrument_dir = RelativePath "inst_db"

-- | Local CmdL code goes here.
repl_dir :: RelativePath
repl_dir = RelativePath $ (\(RelativePath p) -> p) local_dir </> "Repl"

-- | Directory for instruments with slow patch loading to save their caches.
instrument_cache_dir :: RelativePath
instrument_cache_dir = RelativePath "db"

log_dir :: RelativePath
log_dir = RelativePath "log"

-- | 'Ui.State.state_project_dir' is in this directory, by default.
save_dir :: RelativePath
save_dir = RelativePath "save"

-- * status view

-- The block status bar is not very big, so it's important to control what
-- goes in there.  The format is (sort_key, text).

-- ** per-view

-- | Selection start and range.  This goes at the end because it changes width
-- a lot.
status_selection :: (Int, Text)
status_selection = (8, "s")

-- | Zoom and scroll of the visible area.
status_zoom :: (Int, Text)
status_zoom = (9, "z")

-- ** per block

-- | Base octave of the kbd note entry.
status_octave :: (Int, Text)
status_octave = (0, "8")

-- | Current time step.
status_step :: (Int, Text)
status_step = (1, "t")

status_note_duration :: (Int, Text)
status_note_duration = (2, "d")

-- | Various record flags.  Most are reflected in the color of the edit box,
-- but the secondary ones go here.
status_record :: (Int, Text)
status_record = (4, "r")

-- | Text of the last note, even if it didn't create an event.  Useful to know
-- what a key would enter.
status_note :: (Int, Text)
status_note = (5, "n")

-- | Track 'Cmd.state_note_text', which is the previously entered note track
-- text.  This is useful e.g. to set an attribute and maintain that for
-- several notes in a row.
status_note_text :: (Int, Text)
status_note_text = (5, "note")

-- | Show the source block for blocks integrated from another block.
status_integrate_source :: (Int, Text)
status_integrate_source = (8, "src")


-- * repl

-- | Port to listen on for repl requests.
repl_port :: Network.PortID
repl_port = Network.UnixSocket "seq-repl"
initialize_repl_port :: IO ()
initialize_repl_port =
    void $ File.ignoreEnoent $ Directory.removeFile "seq-repl"

-- | This string coming from the repl socket indicates that the message is
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
    -- Display current play position, managed by play monitor thread.
    , (play_position_selnum, play_selection_color)
    -- Display current step play position.
    , (step_play_selnum, _)
    ] = zip [0..] selection_colors

selection_colors :: [Color.Color]
selection_colors = map to_sel $ take max_selections
    [Color.blue, Color.green, Color.yellow, Color.red, Color.purple,
        Color.turquoise]
    where to_sel = Color.alpha 0.3 . Color.brightness 1.25

lookup_selection_color :: Types.SelNum -> Color.Color
lookup_selection_color selnum
    | Array.in_bounds selnum a = a IArray.! selnum
    | otherwise = Color.black
    where a = Array.from_list selection_colors

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

-- | Set when playing.
play_color :: Color.Color
play_color = Color.rgb 0 0.6 0

warning_color :: Color.Color
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

-- | Default color for the block status line background.
status_default :: Color.Color
status_default = Color.white

-- | Color for the status of the root block.
status_root :: Color.Color
status_root = Color.rgb 1 1 0.8

-- | Color for the status of a block integrated from another block.
status_integrate_destination :: Color.Color
status_integrate_destination = Color.rgb 1 0.85 0.85

-- * defaults

-- | The background derive threads will wait this many seconds before starting
-- up, to avoid working too hard during an edit.
default_derive_wait :: Thread.Seconds
default_derive_wait = 1

-- | Keep this many past history entries for undo.  Beyond this entries will
-- have to be loaded from disk.
default_keep_history :: Int
default_keep_history = 20

-- | Default scale id for new projects.  It's a string so I don't have import
-- anything.
default_scale_id :: Text
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
track_width = 40

-- | Width of the divider that represents a collapsed track.
collapsed_width :: Types.Width
collapsed_width = 3

-- | Default color for the track signal, as rendered in the track UI.
render_color :: Color.Color
render_color = Color.rgba 0.65 0.65 0.8 0.5

-- | The default namespace for the clipboard.  Copies go to block + tracks in
-- this namespace.
clip_namespace :: Id.Namespace
clip_namespace = Id.unsafe_namespace "clip"

-- | The copied block will be BlockId (Id.id clip_namespace clip_block_name).
clip_block_name :: String
clip_block_name = "clip"

-- * hardcoded configs

-- These are for 'Ui.Block.Config' and 'Ui.Block.ViewConfig', using them
-- directly would cause a circular import.

-- | Default contents of track and sb boxes.
bconfig_box :: (Color.Color, Char)
bconfig_box = (box_color, ' ')

-- | Maximum number of selections supported by the GUI.
max_selections :: Int
max_selections = #const Config::max_selections

-- ** fiddly pixel bits

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

-- | How many pixels the window bar consumes above the window.  This is
-- needed because to place windows I need an accurate idea of their
-- dimensions.  I can wait to get the resized msg back from fltk, but that's
-- too late if if I want to change the size of a window and do something (e.g.
-- zoom) based on the new size.
--
-- TODO I should actually be getting this from fltk but it's easier to
-- hardcode for now.
window_decoration_h :: Int
#ifdef __APPLE__
window_decoration_h = 22
#else
window_decoration_h = 0
#endif

-- * fonts

emmentaler :: String
emmentaler = case platform of
    Mac -> "Emmentaler 11"
    Linux -> " Emmentaler"
    -- I have no idea why the lilypond font names are inconsistent.


-- * event style

-- | Like Symbols, these are sent to the UI layer at startup and then remain
-- static.
styles :: [Style.Style]
styles =
    [style { Style.style_face = face } | style <- plain_styles, face <- faces]
    where faces = [[], [Style.Bold], [Style.Italic], [Style.Bold, Style.Italic]]

set_face :: Bool -> Style.FontFace -> Style.StyleId -> Style.StyleId
set_face set face (Style.StyleId style) = Style.StyleId $ n * 4 + case face of
        Style.Bold -> set_bit c 0
        Style.Italic -> set_bit c 1
    where
    set_bit = if set then Bits.setBit else Bits.clearBit
    (n, c) = style `divMod` 4

event_style :: Style -> Style.StyleId -> Style.StyleId
event_style style (Style.StyleId code) =
    Style.StyleId $ fromIntegral (fromEnum style) * 4 + code `mod` 4

data Style = Default | Control | Pitch | NoteTransformer | Error
    deriving (Enum, Show)

default_style :: Style.StyleId
default_style = Style.StyleId 0

plain_styles :: [Style.Style]
plain_styles =
    [ plain note_color, plain control_color, plain pitch_color
    , plain note_transformer_color, plain parse_error_color
    ]
    where plain = Style.Style Style.Helvetica [] 12 Color.black

-- | Events on note tracks.
note_color :: Color.Color
note_color = Color.rgb 0.9 0.9 0.7

-- | Events on control tracks.
control_color :: Color.Color
control_color = Color.rgb 0.7 0.8 0.7

-- | Events on pitch tracks
pitch_color :: Color.Color
pitch_color = Color.rgb 0.7 0.8 0.9

-- | NoteTransformer style, events on note tracks which are parents of other
-- note tracks.
note_transformer_color :: Color.Color
note_transformer_color = Color.rgb 1.0 1.0 0.65

-- | Parse errors.
parse_error_color :: Color.Color
parse_error_color = Color.rgb 1.0 0.8 0.8


-- | Indicates that this event was integrated from somewhere else.
integrated_style :: Style.StyleId -> Style.StyleId
integrated_style = set_face True Style.Italic

-- | Indicates that this integrated event has not yet been modified.
unmodified_style :: Style.StyleId -> Style.StyleId
unmodified_style = set_face True Style.Bold

-- | Indicates that this integrated event was modified after integration.
modified_style :: Style.StyleId -> Style.StyleId
modified_style = set_face False Style.Bold
