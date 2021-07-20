-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Global static app defaults.
module App.Config where
import qualified App.Path as Path
import App.Path ((</>))
import qualified Data.Array.IArray as IArray
import qualified Data.Bits as Bits
import qualified Data.Map.Strict as Map
import qualified System.Info

import qualified Util.Array as Array
import qualified Util.Thread as Thread
import qualified Util.Network as Network

import qualified Ui.Color as Color
import qualified Ui.Id as Id
import qualified Ui.Style as Style
import qualified Ui.Types as Types
import qualified Ui.Zoom as Zoom

import Global


#include "fltk/config.h"


-- | This is 'Ui.Sel.Num', but duplicated here to avoid the import.
type SelNum = Int

data Platform = Mac | Linux deriving (Show, Eq)

platform :: Platform
platform = case System.Info.os of
    "darwin" -> Mac
    _ -> Linux
    -- That's all there is, right?

-- * paths

-- | All local data is relative to this dir.
data_dir :: Path.Relative
data_dir = "data"

-- ** data, shared between repos

-- | Saved scores are expected to be relative to this directory.  I use
-- a symlink to have a path relative to the app dir, but point to a global
-- directory.
save_dir :: Path.Relative
save_dir = data_dir </> "save"

-- | These directories are searched for ky files containing local definitions.
-- The directory of the saved score is prepended to the list.
ky_paths :: [Path.Relative]
ky_paths = [local_dir </> "ky"]

-- | Store instrument db code and data.
instrument_dir :: Path.Relative
instrument_dir = data_dir </> "inst_db"

im_dir :: Path.Relative
im_dir = data_dir </> "im"

sc_dir :: Path.Relative
sc_dir = data_dir </> "sc"

-- | Directory for instruments with slow patch loading to save their caches.
instrument_cache_dir :: Path.Relative
instrument_cache_dir = instrument_dir </> "db"

-- ** data, local to a repo

-- | All code and data local to an installation (i.e. specific to a particular
-- configuration) should go here.
local_dir :: Path.Relative
local_dir = "Local"

log_dir :: Path.Relative
log_dir = "log"


-- * status view

-- ** per-view

-- | The block status bar is not very wide, so it's important to control what
-- goes in there, and more importantly, which order.  Items with a high key
-- are more likely to be cut off.
type SortKey = Int

status_chord :: (SortKey, Text)
status_chord = (7, "chord")

-- | Control value, if this is a control track.
status_control :: (SortKey, Text)
status_control = (6, "c")

-- | Selection start and range.
status_selection :: (SortKey, Text)
status_selection = (8, "s")

-- | Selected TrackId.
status_track_id :: (SortKey, Text)
status_track_id = (12, "tid")

-- | Zoom and scroll of the visible area.
status_zoom :: (SortKey, Text)
status_zoom = (10, "z")

-- ** per block

-- | Base octave of the kbd note entry.
status_octave :: (SortKey, Text)
status_octave = (0, "8")

-- | Current time step.
status_step :: (SortKey, Text)
status_step = (1, "t")

-- | Various record flags.  Most are reflected in the color of the edit box,
-- but the secondary ones go here.
status_record :: (SortKey, Text)
status_record = (4, "r")

-- | Text of the last note, even if it didn't create an event.  Useful to know
-- what a key would enter.
status_note :: (SortKey, Text)
status_note = (5, "n")

-- | Track 'Cmd.state_note_text', which is the previously entered note track
-- text.  This is useful e.g. to set an attribute and maintain that for
-- several notes in a row.
status_note_text :: (SortKey, Text)
status_note_text = (5, "note")

-- | Show the source block for blocks integrated from another block.
status_integrate_source :: (SortKey, Text)
status_integrate_source = (8, "src")


-- * repl

-- | Unix socket to listen on for repl requests.
repl_socket :: Network.Addr
repl_socket = Network.Unix repl_socket_name

repl_socket_name :: FilePath
repl_socket_name = "seq-repl"

-- * input

-- | This is the primary mouse button, used to set the insertion selection.
mouse_select :: Types.MouseButton
mouse_select = 1

-- * selnums

-- | The insertion selection is what just about all Cmds should look at to
-- know where to apply any score changes they are supposed to make.  Generally
-- if a Cmd doesn't have specific BlockId, TrackId, and ScoreTime arguments,
-- it probably defaults to the insert_selnum.
insert_selnum :: SelNum
-- | Temporary insert point,  to indicate insert position when it's not the
-- insert_selnum.
temporary_insert_selnum :: SelNum
-- | Highlight errors.  I use 'insert_selnum' for this nowadays, so it's
-- probably unused.
error_selnum :: SelNum
-- | Display current "Cmd.StepPlay" position.
step_play_selnum :: SelNum
-- | Display current play position, managed by play monitor thread.
play_position_selnum :: SelNum
-- | Display 'Color.Highlight's.  This has many possible colors.
highlight_selnum :: SelNum
-- | Show im progress, as communicated from external im synths.  This also has
-- multiple colors.
im_progress_selnum :: SelNum

-- | The most number of SelNums that will be used.  The only reason this needs
-- a constant is so 'Ui.Diff.refresh_selections' can emit updates for all
-- selections.
max_selnums :: SelNum
max_selnums = 8

-- This must be the same length as 'max_selnums'.
[ (insert_selnum, _)
    -- Unused.  Maybe use this for secondary select someday?
    , _
    , (temporary_insert_selnum, _)
    , (error_selnum, _)
    , (play_position_selnum, play_selection_color)
    , (step_play_selnum, _)
    , (highlight_selnum, _)
    , (im_progress_selnum, _)
    ] = zip [0..] selection_colors

-- | Colors that come from a SelNum.
selection_colors :: [Color.Color]
selection_colors = map to_sel $ take max_selnums $
    [ Color.blue, Color.green, Color.yellow, Color.red, Color.purple
    , Color.turquoise
    -- This makes a visible artifact rather than crashing with an array index
    -- bounds error.
    ] ++ repeat Color.black
    where to_sel = Color.alpha 0.3 . Color.brightness 1.25

lookup_selection_color :: SelNum -> Color.Color
lookup_selection_color selnum
    | Array.in_bounds selnum a = a IArray.! selnum
    | otherwise = Color.black
    where a = Array.from_list selection_colors

-- * colors

highlight_colors :: Map Color.Highlight Color.Color
highlight_colors = Map.fromList
    [ (Color.Notice, Color.alpha 0.15 Color.green)
    , (Color.Warning, Color.alpha 0.1 Color.red)
    , (Color.Error, Color.alpha 0.25 Color.red)
    ]

box_color, val_edit_color, method_edit_color :: Color.Color
box_color = Color.rgb 0.7 0.7 0.7
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

abbreviation_color :: Color.Color
abbreviation_color = Color.rgba_word (#const Config::abbreviation_color_word)

busy_color :: Color.Color
busy_color = Color.rgb 0 1 1

mute_color, solo_color :: Color.Color
mute_color = Color.gray6
solo_color = Color.rgb 1 0.75 0.75

im_pending_color, im_working_color :: Color.Color
im_pending_color = Color.rgba 0 0 0 0.15
im_working_color = Color.alpha 0.15 busy_color
    -- The status box uses this color when im is rendering, so use it for the
    -- progress display too.

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

-- | Like 'status_integrate_destination', but for score integration.
status_score_integrate_destination :: Color.Color
status_score_integrate_destination = Color.rgb 0.85 1 0.85

-- | Color of the skeleton lines.
skeleton :: Color.Color
skeleton = Color.black

-- | Color of lines indicating an integrate dependency.
integrate_skeleton :: Color.Color
integrate_skeleton = Color.red

-- | Color of lines indicating a score integrate dependency.
score_integrate_skeleton :: Color.Color
score_integrate_skeleton = Color.green

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
-- Perform.Pitch.
default_scale_id :: Text
default_scale_id = "twelve"

-- | Default range for input devices (e.g. MIDI keyboards).
read_device_pb_range :: (Int, Int)
read_device_pb_range = (-2, 2)

-- | Default size of new views.
view_size :: (Int, Int)
view_size = (300, 300)

-- | Create new views at this zoom.
zoom :: Zoom.Zoom
zoom = Zoom.Zoom { offset = 0, factor = 46 }

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
track_signal_color :: Color.Color
track_signal_color = Color.rgba 0.65 0.65 0.8 0.5

-- | The default namespace for the clipboard.  Copies go to block + tracks in
-- this namespace.
clip_namespace :: Id.Namespace
clip_namespace = Id.namespace "clip"

-- | The copied block will be BlockId (Id.id clip_namespace clip_block_name).
clip_block_name :: Text
clip_block_name = "clip"

-- * hardcoded configs

-- These are for 'Ui.Block.Config' and 'Ui.Block.ViewConfig', using them
-- directly would cause a circular import.

-- | Default contents of track and sb boxes.
bconfig_box :: (Color.Color, Char)
bconfig_box = (box_color, ' ')

-- ** fiddly pixel bits

-- | This is the number of pixels taken up by the various gizmos in the window
-- track beyond the main track view.  It's only correct when the window is
-- first created, since various widgets can be dragged around.
view_top_padding :: Int
view_top_padding =
    -- This is intentionally missing block_title_height, because it may or may
    -- not be present.
    #const Config::Block::skel_height
    + #const Config::Block::track_title_height
    + 2 -- tweak for track bevel, same as in Block::get_padding()

block_title_height :: Int
block_title_height = #const Config::Block::block_title_height

track_title_height :: Int
track_title_height = #const Config::Block::track_title_height

view_bottom_padding :: Int
view_bottom_padding =
    #const Config::Block::sb_size
    + #const Config::Block::status_size
    + 2 -- tweak for track bevel, same as in Block::get_padding()

view_left_padding :: Int
view_left_padding = #const Config::Block::sb_size

-- | How many pixels the window bar consumes above the window.  This is
-- needed because to place windows I need an accurate idea of their
-- dimensions.  I can wait to get the resized msg back from fltk, but that's
-- too late if if I want to change the size of a window and do something (e.g.
-- zoom) based on the new size.
--
-- TODO I should actually be getting this from fltk but it's easier to
-- hardcode for now.  But I need a window, since it comes from
-- Fl_Window::decorated_h() - Fl_Window::h().  However I can't assume that
-- linux will have 0.
window_decoration_h :: Int
window_decoration_h = case platform of
    Linux -> 0
    Mac -> 22

-- * fonts

bravura :: String
bravura = "Bravura"


-- * event style

-- | The comment for tracklang is --, but if an event starts with this comment,
-- the whole event is ignored.  A normal comment would mean the event exists,
-- but has no expression.
event_comment :: Text
event_comment = "--|"

-- | Like Symbols, these are sent to the UI layer at startup and then remain
-- static.
styles :: [Style.Style]
styles =
    [ style { Style.style_face = face }
    | style <- plain_styles, face <- style_faces
    ]

style_faces :: [[Style.FontFace]]
style_faces = [[], [Style.Bold], [Style.Italic], [Style.Bold, Style.Italic]]

-- | Tweak the StyleId to set or enset the given 'Style.FontFace'.
set_face :: Bool -> Style.FontFace -> Style.StyleId -> Style.StyleId
set_face set face (Style.StyleId style) =
    Style.StyleId $ n * faces + case face of
        Style.Bold -> set_bit c 0
        Style.Italic -> set_bit c 1
    where
    set_bit = if set then Bits.setBit else Bits.clearBit
    (n, c) = style `divMod` faces
    faces = fromIntegral (length style_faces)

event_style :: Style -> Style.StyleId -> Style.StyleId
event_style style (Style.StyleId code) =
    Style.StyleId $ fromIntegral (fromEnum style) * faces + code `mod` faces
    where faces = fromIntegral (length style_faces)

-- | These should line up with the 'plain_styles' list below.
data Style =
    Note | NoteBlockCall | Control | Pitch | NoteParent | Error | Commented
    deriving (Enum, Show)

default_style :: Style.StyleId
default_style = Style.StyleId 0

plain_styles :: [Style.Style]
plain_styles =
    [ plain note_color
    , plain block_call_color
    , plain control_color
    , plain pitch_color
    , plain note_parent_color
    , plain parse_error_color
    , commented
    ]
    where
    commented = (plain Color.gray8) { Style.style_text_color = Color.gray4 }
    plain event_color = Style.Style
        { style_font = Style.Helvetica
        , style_face = []
        , style_size = 12
        , style_text_color = Color.black
        , style_event_color = event_color
        }

-- | Events on note tracks.
note_color :: Color.Color
note_color = Color.rgb 0.9 0.9 0.7

-- | Events on note tracks which have a block call.  I color these specially
-- because they're very common, and thus it's common for them to accidentally
-- conflict with other note generators.
block_call_color :: Color.Color
block_call_color = note_parent_color
    -- The theory is that this color isn't in use since I only use it in parent
    -- calls, and those aren't block calls.

-- | Events on control tracks.
control_color :: Color.Color
control_color = Color.rgb 0.7 0.8 0.7

-- | Events on pitch tracks
pitch_color :: Color.Color
pitch_color = Color.rgb 0.7 0.8 0.9

-- | A note track with tracks below it.
note_parent_color :: Color.Color
note_parent_color = Color.rgb 1.0 1.0 0.65

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
