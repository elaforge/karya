-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Global static app defaults.
module App.Config where
import qualified Data.Array.IArray as IArray
import qualified Data.Bits as Bits
import qualified Data.Map.Strict as Map
import qualified Data.String as String
import qualified Network
import qualified System.Info
import qualified System.FilePath as FilePath

import qualified Util.Array as Array
import qualified Util.Thread as Thread

import qualified Ui.Color as Color
import qualified Ui.Id as Id
import qualified Ui.Sel as Sel
import qualified Ui.Style as Style
import qualified Ui.Types as Types
import qualified Ui.Zoom as Zoom

import Global


#include "fltk/config.h"


data Platform = Mac | Linux deriving (Show, Eq)

platform :: Platform
platform = case System.Info.os of
    "darwin" -> Mac
    _ -> Linux
    -- That's all there is, right?

-- * paths

-- | Paths which are intended to be relative to the app dir get this type,
-- so it's harder to accidentally use them directly.
newtype RelativePath = RelativePath FilePath deriving (Show, String.IsString)

(</>) :: RelativePath -> RelativePath -> RelativePath
RelativePath a </> RelativePath b = RelativePath (a FilePath.</> b)

make_path :: FilePath -> RelativePath -> FilePath
make_path app_dir (RelativePath path) = app_dir FilePath.</> path

-- | All paths should be relative to this one.
-- I may later change this to an env var, a flag, or just leave it hardcoded.
get_app_dir :: IO FilePath
get_app_dir = return "."

-- | All code and data local to an installation (i.e. specific to a particular
-- configuration) should go here.
local_dir :: RelativePath
local_dir = "Local"

-- | These directories are searched for ky files containing local definitions.
-- The directory of the saved score is prepended to the list.
ky_paths :: [RelativePath]
ky_paths = [local_dir </> "ky"]

-- | Store instrument db code and data.
instrument_dir :: RelativePath
instrument_dir = "inst_db"

-- | Directory for instruments with slow patch loading to save their caches.
-- This should be below 'instrument_dir'.
instrument_cache_dir :: RelativePath
instrument_cache_dir = "db"

log_dir :: RelativePath
log_dir = "log"

-- | Saved scores are expected to be relative to this directory.
save_dir :: RelativePath
save_dir = "save"


-- * status view

-- ** per-view

-- The block status bar is not very wide, so it's important to control what
-- goes in there, and more importantly, which order.  Items with a high key
-- are more likely to be cut off.
type SortKey = Int

status_chord :: (SortKey, Text)
status_chord = (7, "chord")

-- | Control value, if this is a control track.
status_control :: (SortKey, Text)
status_control = (6, "c")

-- | Selection start and range.  This goes at the end because it changes width
-- a lot.
status_selection :: (SortKey, Text)
status_selection = (8, "s")

-- | Selection start in RealTime.
status_realtime :: (SortKey, Text)
status_realtime = (9, "sec")

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

status_note_duration :: (SortKey, Text)
status_note_duration = (2, "d")

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
repl_port :: FilePath
repl_port = "seq-repl"

-- | PortID version of 'repl_port'.  The clients use filenames so they rely on
-- this being a unix socket.
repl_socket :: Network.PortID
repl_socket = Network.UnixSocket repl_port

-- * input

-- | This is the primary mouse button, used to set the insertion selection.
mouse_select :: Types.MouseButton
mouse_select = 1

-- * selnums

-- | The insertion selection is what just about all Cmds should look at to
-- know where to apply any score changes they are supposed to make.  Generally
-- if a Cmd doesn't have specific BlockId, TrackId, and ScoreTime arguments,
-- it probably defaults to the insert_selnum.
insert_selnum :: Sel.Num
-- | Temporary insert point,  to indicate insert position when it's not the
-- insert_selnum.
temporary_insert_selnum :: Sel.Num
-- | Highlight errors, possibly reported internally but probably sent by
-- logview when you click on a stack trace.
error_selnum :: Sel.Num
-- | Display current "Cmd.StepPlay" position.
step_play_selnum :: Sel.Num
-- | Display current play position, managed by play monitor thread.
play_position_selnum :: Sel.Num
-- | Display 'Color.Highlight's.  This has many possible colors.
highlight_selnum :: Sel.Num

-- | The most number of Sel.Nums that will be used.  The only reason this needs
-- a constant is so 'Ui.Diff.refresh_selections' can emit updates for all
-- selections.
max_selnums :: Sel.Num
max_selnums = 7

-- This must be the same length as 'max_selnums'.
[ (insert_selnum, _)
    -- Unused.  Secondary select?
    , _
    , (temporary_insert_selnum, _)
    , (error_selnum, _)
    , (play_position_selnum, play_selection_color)
    , (step_play_selnum, _)
    , (highlight_selnum, _)
    ] = zip [0..] selection_colors

-- | Colors that come from a Sel.Num.
selection_colors :: [Color.Color]
selection_colors = map to_sel $ take max_selnums $
    [ Color.blue, Color.green, Color.yellow, Color.red, Color.purple
    , Color.turquoise
    -- This makes a visible artifact rather than crashing with an array index
    -- bounds error.
    ] ++ repeat Color.black
    where to_sel = Color.alpha 0.3 . Color.brightness 1.25

lookup_selection_color :: Sel.Num -> Color.Color
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
-- anything.
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
-- track beyond the main track view.  Only correct when the window is first
-- created, since the skel_height may be dragged around.
view_time_padding :: Int
view_time_padding =
    #const Config::Block::track_title_height
    + #const Config::Block::skel_height
    + #const Config::Block::status_size
    + #const Config::Block::sb_size
    + #const Config::Block::extra_time_padding

block_title_height :: Int
block_title_height = #const Config::Block::block_title_height

view_track_padding :: Int
view_track_padding = #const Config::Block::sb_size

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

bravura :: String
bravura = "Bravura"


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

data Style = Default | Control | Pitch | Parent | Error
    deriving (Enum, Show)

default_style :: Style.StyleId
default_style = Style.StyleId 0

plain_styles :: [Style.Style]
plain_styles =
    [ plain note_color, plain control_color, plain pitch_color
    , plain parent_color, plain parse_error_color
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

-- | A note track with tracks below it.
parent_color :: Color.Color
parent_color = Color.rgb 1.0 1.0 0.65

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
