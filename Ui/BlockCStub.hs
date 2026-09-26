-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
-- | This is a stubbed out version of BlockC.  The real BlockC will use this
-- when TESTING is defined, since they don't work from ghci if they have
-- a C dependency.
--
-- It's just a big copy-paste from BlockC, but should be ok since that changes
-- very rarely.
module Ui.BlockCStub where
import qualified Util.Rect as Rect
import qualified Ui.Block as Block
import qualified Ui.Color as Color
import qualified Ui.Events as Events
import Ui.Fltk (Fltk, fltk)
import qualified Ui.Sel as Sel
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Track as Track
import qualified Ui.Types as Types
import qualified Ui.Zoom as Zoom

import Global
import Types


-- * query

getScreens :: IO [Rect.Rect]
getScreens = pure [Rect.xywh 0 0 800 600]

-- * view creation

createView :: ViewId -> Text -> Rect.Rect -> Block.Config -> Fltk ()
createView viewId windowTitle rect blockConfig = fltk $ pure ()

destroyView :: ViewId -> Fltk ()
destroyView viewId = fltk $ pure ()

getViewStatus :: ViewId -> Fltk (Rect.Rect, Zoom.Zoom, Int, Int)
getViewStatus viewId = fltk $ errorIO "unimplemented"

setSize :: ViewId -> Rect.Rect -> Fltk ()
setSize viewId rect = fltk $ pure ()

setZoom :: ViewId -> Zoom.Zoom -> Fltk ()
setZoom viewId zoom = fltk $ pure ()

setTrackScroll :: ViewId -> Types.Width -> Fltk ()
setTrackScroll viewId offset = fltk $ pure ()

setSelection :: ViewId -> Sel.Num -> [TrackNum] -> [Selection] -> Fltk ()
setSelection viewId selnum tracknums sels = fltk $ pure ()

bringToFront :: ViewId -> Fltk ()
bringToFront viewId = fltk $ pure ()

-- * Block operations

setConfig :: ViewId -> Block.Config -> Fltk ()
setConfig viewId config = fltk $ pure ()

setSkeleton :: ViewId -> Skeleton.Skeleton
    -> [(Color.Color, [(TrackNum, TrackNum)])] -> Fltk ()
setSkeleton viewId skel integrateEdges = fltk $ pure ()

setTitle :: ViewId -> Text -> Fltk ()
setTitle viewId title = fltk $ pure ()

setStatus :: ViewId -> Text -> Color.Color -> Fltk ()
setStatus viewId status color = fltk $ pure ()

setDisplayTrack :: ViewId -> TrackNum -> Block.DisplayTrack -> Fltk ()
setDisplayTrack viewId tracknum dtrack = fltk $ pure ()

floatingOpen :: ViewId -> TrackNum -> ScoreTime -> Text -> (Int, Int)
    -> Fltk ()
floatingOpen viewId tracknum pos text (selStart, selEnd) = fltk $ pure ()

floatingInsert :: [ViewId] -> Text -> Fltk ()
floatingInsert viewIds text = fltk $ pure ()

tracks :: ViewId -> Fltk TrackNum
tracks viewId = fltk $ pure 0

insertTrack :: ViewId -> TrackNum -> Block.Tracklike -> [Events.Events]
    -> Track.SetStyle -> Types.Width -> Fltk ()
insertTrack viewId tracknum tracklike merged setStyle width =
    fltk $ pure ()

removeTrack :: ViewId -> TrackNum -> Fltk ()
removeTrack viewId tracknum = fltk $ pure ()

updateTrack :: Bool -- ^ True if the ruler has changed and should be copied
    -- over.  It's a bit of a hack to be a separate flag, but rulers are
    -- updated rarely and copied over entirely for efficiency.
    -> ViewId -> TrackNum -> Block.Tracklike
    -> [Events.Events] -> Track.SetStyle -> ScoreTime -> ScoreTime -> Fltk ()
updateTrack updateRuler viewId tracknum tracklike merged setStyle start
        end = fltk $ pure ()

-- | Like 'update_track' except update everywhere.
updateEntireTrack :: Bool -> ViewId -> TrackNum -> Block.Tracklike
    -> [Events.Events] -> Track.SetStyle -> Fltk ()
updateEntireTrack updateRuler viewId tracknum tracklike merged setStyle =
    -- -1 is special cased in c++.
    updateTrack updateRuler viewId tracknum tracklike merged setStyle
        (-1) (-1)

-- | Unlike other Fltk functions, this doesn't throw if the ViewId is not
-- found.  That's because it's called asynchronously when derivation is
-- complete.
setTrackSignal :: ViewId -> TrackNum -> Track.TrackSignal -> Fltk ()
setTrackSignal viewId tracknum tsig = fltk $ pure ()

setWaveform :: ViewId -> TrackNum -> Track.WaveformChunk -> Fltk ()
setWaveform _ _ _ = fltk $ pure ()

clearWaveforms :: ViewId -> Fltk ()
clearWaveforms _ = fltk $ pure ()

gcWaveforms :: Fltk ()
gcWaveforms = fltk $ pure ()

setTrackTitle :: ViewId -> TrackNum -> Text -> Fltk ()
setTrackTitle viewId tracknum title = fltk $ pure ()

setTrackTitleFocus :: ViewId -> TrackNum -> Fltk ()
setTrackTitleFocus viewId tracknum = fltk $ pure ()

setBlockTitleFocus :: ViewId -> Fltk ()
setBlockTitleFocus viewId = fltk $ pure ()


-- ** debugging

printDebug :: ViewId -> Fltk ()
printDebug viewId = pure ()

dump :: IO [(ViewId, String)]
dump = pure []

-- ** selection

-- TODO copy and pasted with "Ui.BlockC".  Put them in Ui.BlockCTypes if there
-- are ever any more.

-- | This is the low level version of 'Sel.Selection'.  It only applies to
-- a single track, and has an explicit color.
data Selection = Selection
    { selColor :: !Color.Color
    , selStart :: !TrackTime
    , selCur :: !TrackTime
    , selOrientation :: !SelectionOrientation
    }
    deriving (Eq, Ord, Show)

data SelectionOrientation = None | Positive | Negative | Both
    deriving (Show, Eq, Ord, Enum)
