-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Global static app defaults.
--
-- This should minimize imports, since everyone should be able to import this
-- without incurring dependencies.
module App.Config where
import qualified Data.Array.IArray as IArray
import qualified Data.Bits as Bits
import qualified Data.Map.Strict as Map
import qualified Data.Time as Time
import qualified System.Info

import qualified App.Path as Path
import           App.Path ((</>))
import qualified Util.Array as Array

import qualified Ui.Color as Color
import qualified Ui.Style as Style
import qualified Ui.Types as Types
import qualified Ui.Zoom as Zoom

import           Global


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
dataDir :: Path.Relative
dataDir = "data"

-- ** data, shared between repos

-- | Saved scores are expected to be relative to this directory.  I use
-- a symlink to have a path relative to the app dir, but point to a global
-- directory.
saveDir :: Path.Relative
saveDir = dataDir </> "save"

-- | These directories are searched for ky files containing local definitions.
-- The directory of the saved score is prepended to the list.
kyPaths :: [Path.Relative]
kyPaths = [localDir </> "ky"]

-- | Store instrument db code and data.
instrumentDir :: Path.Relative
instrumentDir = dataDir </> "inst_db"

imDir :: Path.Relative
imDir = dataDir </> "im"

scDir :: Path.Relative
scDir = dataDir </> "sc"

-- | Directory for instruments with slow patch loading to save their caches.
instrumentCacheDir :: Path.Relative
instrumentCacheDir = instrumentDir </> "db"

-- ** data, local to a repo

-- | All code and data local to an installation (i.e. specific to a particular
-- configuration) should go here.
localDir :: Path.Relative
localDir = "Local"

logDir :: Path.Relative
logDir = "log"


-- * status view

-- ** per-view

-- | The block status bar is not very wide, so it's important to control what
-- goes in there, and more importantly, which order.  Items with a high key
-- are more likely to be cut off.
type SortKey = Int

statusChord :: (SortKey, Text)
statusChord = (7, "chord")

-- | Control value, if this is a control track.
statusControl :: (SortKey, Text)
statusControl = (6, "c")

-- | Selection start and range.
statusSelection :: (SortKey, Text)
statusSelection = (8, "s")

-- | Selected TrackId.
statusTrackId :: (SortKey, Text)
statusTrackId = (12, "tid")

-- | Zoom and scroll of the visible area.
statusZoom :: (SortKey, Text)
statusZoom = (10, "z")

-- ** per block

-- | Base octave of the kbd note entry.
statusOctave :: (SortKey, Text)
statusOctave = (0, "8")

-- | Current time step.
statusStep :: (SortKey, Text)
statusStep = (1, "t")

-- | Various record flags.  Most are reflected in the color of the edit box,
-- but the secondary ones go here.
statusRecord :: (SortKey, Text)
statusRecord = (4, "r")

-- | Text of the last note, even if it didn't create an event.  Useful to know
-- what a key would enter.
statusNote :: (SortKey, Text)
statusNote = (5, "n")

-- | Track 'Cmd.state_note_text', which is the previously entered note track
-- text.  This is useful e.g. to set an attribute and maintain that for
-- several notes in a row.
statusNoteText :: (SortKey, Text)
statusNoteText = (5, "note")

-- | Show the source block for blocks integrated from another block.
statusIntegrateSource :: (SortKey, Text)
statusIntegrateSource = (8, "src")


-- * repl

-- | Unix socket to listen on for repl requests.
replSocketName :: FilePath
replSocketName = "seq-repl"

-- * input

-- | This is the primary mouse button, used to set the insertion selection.
mouseSelect :: Types.MouseButton
mouseSelect = 1

-- * selnums

-- | The insertion selection is what just about all Cmds should look at to
-- know where to apply any score changes they are supposed to make.  Generally
-- if a Cmd doesn't have specific BlockId, TrackId, and ScoreTime arguments,
-- it probably defaults to the insert_selnum.
insertSelnum :: SelNum
-- | Temporary insert point,  to indicate insert position when it's not the
-- insert_selnum.
temporaryInsertSelnum :: SelNum
-- | Highlight errors.  I use 'insert_selnum' for this nowadays, so it's
-- probably unused.
errorSelnum :: SelNum
-- | Display current "Cmd.StepPlay" position.
stepPlaySelnum :: SelNum
-- | Display current play position, managed by play monitor thread.
playPositionSelnum :: SelNum
-- | Display 'Color.Highlight's.  This has many possible colors.
highlightSelnum :: SelNum
-- | Show im progress, as communicated from external im synths.  This also has
-- multiple colors.
imProgressSelnum :: SelNum

-- | The most number of SelNums that will be used.  The only reason this needs
-- a constant is so 'Ui.Diff.refresh_selections' can emit updates for all
-- selections.
maxSelnums :: SelNum
maxSelnums = 8

-- This must be the same length as 'max_selnums'.
[ (insertSelnum, _)
    -- Unused.  Maybe use this for secondary select someday?
    , _
    , (temporaryInsertSelnum, _)
    , (errorSelnum, _)
    , (playPositionSelnum, playSelectionColor)
    , (stepPlaySelnum, _)
    , (highlightSelnum, _)
    , (imProgressSelnum, _)
    ] = zip [0..] selectionColors

-- | Colors that come from a SelNum.
selectionColors :: [Color.Color]
selectionColors = map toSel $ take maxSelnums $
    [ Color.blue, Color.green, Color.yellow, Color.red, Color.purple
    , Color.turquoise
    -- This makes a visible artifact rather than crashing with an array index
    -- bounds error.
    ] ++ repeat Color.black
    where toSel = Color.alpha 0.3 . Color.brightness 1.25

lookupSelectionColor :: SelNum -> Color.Color
lookupSelectionColor selnum
    | Array.inBounds selnum a = a IArray.! selnum
    | otherwise = Color.black
    where a = Array.fromList selectionColors

-- * colors

highlightColors :: Map Color.Highlight Color.Color
highlightColors = Map.fromList
    [ (Color.Notice, Color.alpha 0.15 Color.green)
    , (Color.Warning, Color.alpha 0.1 Color.red)
    , (Color.Error, Color.alpha 0.25 Color.red)
    ]

boxColor, valEditColor, methodEditColor :: Color.Color
boxColor = Color.rgb 0.7 0.7 0.7
valEditColor = Color.rgb 1 0.5 0.5
-- Similar to val color because you toggle between val and method.
methodEditColor = Color.rgb 0.6 0 0

-- | Colors indicate if 'Cmd.state_advance' is set.
advanceColor, noAdvanceColor :: Color.Color
advanceColor = playColor
noAdvanceColor = valEditColor

-- | Set when playing.
playColor :: Color.Color
playColor = Color.rgb 0 0.6 0

abbreviationColor :: Color.Color
abbreviationColor = Color.rgbaWord (#const Config::abbreviation_color_word)

busyColor :: Color.Color
busyColor = Color.rgb 0 1 1

muteColor, soloColor :: Color.Color
muteColor = Color.gray6
soloColor = Color.rgb 1 0.75 0.75

imPendingColor, imWorkingColor :: Color.Color
imPendingColor = Color.rgba 0 0 0 0.15
imWorkingColor = Color.alpha 0.15 busyColor
    -- The status box uses this color when im is rendering, so use it for the
    -- progress display too.

trackBg, rulerBg :: Color.Color
trackBg = Color.white
rulerBg = Color.rgb 1 0.85 0.5

-- | Default color for the block status line background.
statusDefault :: Color.Color
statusDefault = Color.white

-- | Color for the status of the root block.
statusRoot :: Color.Color
statusRoot = Color.rgb 1 1 0.8

-- | Color for the status of a block integrated from another block.
statusIntegrateDestination :: Color.Color
statusIntegrateDestination = Color.rgb 1 0.85 0.85

-- | Like 'status_integrate_destination', but for score integration.
statusScoreIntegrateDestination :: Color.Color
statusScoreIntegrateDestination = Color.rgb 0.85 1 0.85

-- | Color of the skeleton lines.
skeleton :: Color.Color
skeleton = Color.black

-- | Color of lines indicating an integrate dependency.
integrateSkeleton :: Color.Color
integrateSkeleton = Color.red

-- | Color of lines indicating a score integrate dependency.
scoreIntegrateSkeleton :: Color.Color
scoreIntegrateSkeleton = Color.green

-- * defaults

-- | The background derive threads will wait this many seconds before starting
-- up, to avoid working too hard during an edit.
defaultDeriveWait :: Time.NominalDiffTime
defaultDeriveWait = 1

-- | Keep this many past history entries for undo.  Beyond this entries will
-- have to be loaded from disk.
defaultKeepHistory :: Int
defaultKeepHistory = 20

-- | Default scale id for new projects.  It's a string so I don't have import
-- Perform.Pitch.
defaultScaleId :: Text
defaultScaleId = "twelve"

-- | Default range for input devices (e.g. MIDI keyboards).
readDevicePbRange :: (Int, Int)
readDevicePbRange = (-2, 2)

-- | Default size of new views.
viewSize :: (Int, Int)
viewSize = (300, 300)

-- | Create new views at this zoom.
zoom :: Zoom.Zoom
zoom = Zoom.Zoom { offset = 0, factor = 46 }

-- | Default width for the block ruler track.
rulerWidth :: Types.Width
rulerWidth = 18

-- | Default width for new tracks.  This should just fit common control track
-- contents, e.g. @e (4f#)@.
trackWidth :: Types.Width
trackWidth = 40

-- | Width of the divider that represents a collapsed track.
collapsedWidth :: Types.Width
collapsedWidth = 3

-- | Default color for the track signal, as rendered in the track UI.
trackSignalColor :: Color.Color
trackSignalColor = Color.rgba 0.65 0.65 0.8 0.5

-- | The default namespace for the clipboard.  Copies go to block + tracks in
-- this namespace.
clipNamespace :: Text
clipNamespace = "clip"

-- | The copied block will be BlockId (Id.id clip_namespace clip_block_name).
clipBlockName :: Text
clipBlockName = "clip"

-- * hardcoded configs

-- These are for 'Ui.Block.Config' and 'Ui.Block.ViewConfig', using them
-- directly would cause a circular import.

-- | Default contents of track and sb boxes.
bconfigBox :: (Color.Color, Char)
bconfigBox = (boxColor, ' ')

-- ** fiddly pixel bits

-- | This is the number of pixels taken up by the various gizmos in the window
-- track beyond the main track view.  It's only correct when the window is
-- first created, since various widgets can be dragged around.
viewTopPadding :: Int
viewTopPadding =
    -- This is intentionally missing block_title_height, because it may or may
    -- not be present.
    #const Config::Block::skel_height
    + #const Config::Block::track_title_height
    + 2 -- tweak for track bevel, same as in Block::get_padding()

blockTitleHeight :: Int
blockTitleHeight = #const Config::Block::block_title_height

trackTitleHeight :: Int
trackTitleHeight = #const Config::Block::track_title_height

viewBottomPadding :: Int
viewBottomPadding =
    #const Config::Block::sb_size
    + #const Config::Block::status_size
    + 2 -- tweak for track bevel, same as in Block::get_padding()

viewLeftPadding :: Int
viewLeftPadding = #const Config::Block::sb_size

-- | How many pixels the window bar consumes above the window.  This is
-- needed because to place windows I need an accurate idea of their
-- dimensions.  I can wait to get the resized msg back from fltk, but that's
-- too late if if I want to change the size of a window and do something (e.g.
-- zoom) based on the new size.
--
-- TODO I should actually be getting this from fltk but it's easier to
-- hardcode for now.  But I need a window, since it comes from
-- Fl_Window::decorated_h() - Fl_Window::h().  However I can't assume that
-- linux will have 0.  Oh and this doesn't work on linux with fvwm anyway,
-- because fltk can't figure out the decoration size.
windowDecorationH :: Int
windowDecorationH = case platform of
    Linux -> 0
    Mac -> 22

-- * fonts

bravura :: String
bravura = "Bravura"

-- | This is enough pixels to display one line of text in the normal font and
-- size, and not seem cramped.  It corresponds to a bit more than fl_height()
-- of the 'plain_styles' below.
--
-- TODO it could be derived to adjust when the default style changes.
-- Currently it's derived via the traditional process of zooming and squinting.
eventTextHeight :: Int
eventTextHeight = 15

-- * event style

-- | Like Symbols, these are sent to the UI layer at startup and then remain
-- static.
styles :: [Style.Style]
styles =
    [ style { Style.styleFace = face }
    | style <- plainStyles, face <- styleFaces
    ]

styleFaces :: [[Style.FontFace]]
styleFaces = [[], [Style.Bold], [Style.Italic], [Style.Bold, Style.Italic]]

-- | Tweak the StyleId to set or enset the given 'Style.FontFace'.
setFace :: Bool -> Style.FontFace -> Style.StyleId -> Style.StyleId
setFace set face (Style.StyleId style) =
    Style.StyleId $ n * faces + case face of
        Style.Bold -> setBit c 0
        Style.Italic -> setBit c 1
    where
    setBit = if set then Bits.setBit else Bits.clearBit
    (n, c) = style `divMod` faces
    faces = fromIntegral (length styleFaces)

eventStyle :: Style -> Style.StyleId -> Style.StyleId
eventStyle style (Style.StyleId code) =
    Style.StyleId $ fromIntegral (fromEnum style) * faces + code `mod` faces
    where faces = fromIntegral (length styleFaces)

-- | These should line up with the 'plain_styles' list below.
data Style =
    Note | NoteBlockCall | Control | Pitch | NoteParent | Error | Commented
    deriving (Enum, Show)

defaultStyle :: Style.StyleId
defaultStyle = Style.StyleId 0

plainStyles :: [Style.Style]
plainStyles =
    [ plain noteColor
    , plain blockCallColor
    , plain controlColor
    , plain pitchColor
    , plain noteParentColor
    , plain parseErrorColor
    , commented
    ]
    where
    commented = (plain Color.gray8) { Style.styleTextColor = Color.gray4 }
    plain eventColor = Style.Style
        { styleFont = Style.Helvetica
        , styleFace = []
        , styleSize = 12
        , styleTextColor = Color.black
        , styleEventColor = eventColor
        }

-- | Events on note tracks.
noteColor :: Color.Color
noteColor = Color.rgb 0.9 0.9 0.7

-- | Events on note tracks which have a block call.  I color these specially
-- because they're very common, and thus it's common for them to accidentally
-- conflict with other note generators.
blockCallColor :: Color.Color
blockCallColor = noteParentColor
    -- The theory is that this color isn't in use since I only use it in parent
    -- calls, and those aren't block calls.

-- | Events on control tracks.
controlColor :: Color.Color
controlColor = Color.rgb 0.7 0.8 0.7

-- | Events on pitch tracks
pitchColor :: Color.Color
pitchColor = Color.rgb 0.7 0.8 0.9

-- | A note track with tracks below it.
noteParentColor :: Color.Color
noteParentColor = Color.rgb 1.0 1.0 0.65

-- | Parse errors.
parseErrorColor :: Color.Color
parseErrorColor = Color.rgb 1.0 0.8 0.8

-- | Indicates that this event was integrated from somewhere else.
integratedStyle :: Style.StyleId -> Style.StyleId
integratedStyle = setFace True Style.Italic

-- | Indicates that this integrated event has not yet been modified.
unmodifiedStyle :: Style.StyleId -> Style.StyleId
unmodifiedStyle = setFace True Style.Bold

-- | Indicates that this integrated event was modified after integration.
modifiedStyle :: Style.StyleId -> Style.StyleId
modifiedStyle = setFace False Style.Bold
