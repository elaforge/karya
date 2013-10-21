-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ui.Ruler (
    -- * Ruler
    Ruler(..), Marklists, Name, ruler, no_ruler
    , lookup_marklist, get_marklist, set_marklist, remove_marklist
    , modify_marklist, modify_marklists, map_marklists
    , time_end
    -- * Marklist
    , Marklist, MarklistVector, PosMark
    , marklist, marklist_from_vector, marklist_vec
    , marklist_fptr -- This should only be used from Ui.RulerC.
    , ascending, descending
    , marklist_end, marklist_start
    -- ** modification
    , insert_mark
    -- * Mark
    , Mark(..), null_mark, Rank

    -- * for RulerC's eyes only
    , MarklistPtr(..)
) where
import qualified Control.Concurrent.MVar as MVar
import qualified Control.DeepSeq as DeepSeq
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Foreign
import qualified System.IO.Unsafe as Unsafe

import Util.Control
import qualified Util.Pretty as Pretty
import Util.Pretty
import qualified Util.TimeVector as TimeVector

import qualified Ui.Color as Color
import qualified Perform.RealTime as RealTime
import qualified App.Config as Config
import Types


-- * Ruler

-- | A Ruler contains all the data to display a ruler.
data Ruler = Ruler {
    ruler_marklists :: !Marklists
    , ruler_bg :: !Color.Color
    -- | Show the names if this is on an event track.  Ruler tracks always show
    -- the names.
    , ruler_show_names :: !Bool
    -- | Align bottoms of marks to beats, instead of the top.  Looks good used
    -- with negative duration events (arrival beats).
    , ruler_align_to_bottom :: !Bool
    } deriving (Eq, Show)

-- | Each ruler can have multiple named marklists.  This means a ruler can
-- have multiple simultaneous meters, or a separate list of ad-hoc cue points.
-- All marks are flattened before display, and are drawn in the sort order of
-- their Names.
type Marklists = Map.Map Name Marklist
type Name = Text

instance Pretty.Pretty Ruler where
    format (Ruler mlists bg show_names align_to_bottom) =
        Pretty.record_title "Ruler"
            [ ("marklists", Pretty.format mlists)
            , ("bg", Pretty.format bg)
            , ("show_names", Pretty.format show_names)
            , ("align_to_bottom", Pretty.format align_to_bottom)
            ]

instance DeepSeq.NFData Ruler where rnf (Ruler mlists _ _ _) = mlists `seq` ()

-- | Constructor for "plain" rulers.
ruler :: [(Name, Marklist)] -> Ruler
ruler marklists = Ruler
    { ruler_marklists = Map.fromList marklists
    , ruler_bg = Config.ruler_bg
    , ruler_show_names = False
    , ruler_align_to_bottom = False
    }

-- | Empty ruler.
no_ruler :: Ruler
no_ruler = ruler []

lookup_marklist :: Name -> Ruler -> Maybe Marklist
lookup_marklist name = Map.lookup name . ruler_marklists

get_marklist :: Name -> Ruler -> Marklist
get_marklist name = fromMaybe empty_marklist . lookup_marklist name

set_marklist :: Name -> Marklist -> Ruler -> Ruler
set_marklist name mlist = modify_marklists (Map.insert name mlist)

remove_marklist :: Name -> Ruler -> Ruler
remove_marklist = modify_marklists . Map.delete

-- | If the marklist isn't set, modify will be given an empty one.
modify_marklist :: Name -> (Marklist -> Marklist) -> Ruler -> Ruler
modify_marklist name modify ruler =
    set_marklist name (modify (get_marklist name ruler)) ruler

modify_marklists :: (Map.Map Name Marklist -> Map.Map Name Marklist)
    -> Ruler -> Ruler
modify_marklists modify ruler =
    ruler { ruler_marklists = modify (ruler_marklists ruler) }

-- | Transform all the marklists in a ruler.
map_marklists :: (Marklist -> Marklist) -> Ruler -> Ruler
map_marklists f ruler =
    ruler { ruler_marklists = Map.map f (ruler_marklists ruler) }

-- | Get the position of the last mark of the ruler.
time_end :: Ruler -> ScoreTime
time_end = maximum . (0 :) . map marklist_end . Map.elems . ruler_marklists

-- * marklist

data Marklist = Marklist
    { marklist_vec :: !MarklistVector
    -- | This is a cache for the C-marshalled version of the marklist.
    -- It will be allocated if the Marklist is passed to C, and is managed with
    -- its own reference count.
    --
    -- I think this should be safe as long as 'marklist' is the only
    -- constructor.
    , marklist_fptr :: !MarklistPtr
    }

type MarklistVector = TimeVector.Boxed Mark

instance Show Marklist where
    show m = "Ruler.marklist " ++ show (ascending 0 m)
instance Pretty.Pretty Marklist where
    pretty m = "((" <> show (TimeVector.length (marklist_vec m)) <> " marks))"

-- | This should be opaque, but it needs to be exported for RulerC.  Don't look
-- inside if you're not RulerC, OK?
--
-- The Left value is actually not used, but prevents the unsafePerformIO from
-- being floated out of the lambda.
--
-- I used to just copy the ruler each time, but it was actually pretty big.
-- Rulers can have lots of marks, there are many rulers per block since each
-- track has one.  But many tracks share the same ruler, and they change
-- rarely.  All of these differences from 'Events.Events' push for passing
-- by pointer rather than copying over the whole thing each time (as with the
-- signal), or passing a callback that fetches the required range (as with
-- events).
--
-- TODO I could share the memory by making MarklistVector into Vector.Storable
-- PosMark.  It's otherwise equivalant though, and the number of distinct
-- rulers is probably small, so the memory savings doesn't seem that
-- compelling.
newtype MarklistPtr = MarklistPtr
    (MVar.MVar (Either MarklistVector (Foreign.ForeignPtr Marklist)))
type PosMark = (ScoreTime, Mark)

instance Eq Marklist where
    m1 == m2 = marklist_vec m1 == marklist_vec m2

{-# NOINLINE marklist #-}
marklist :: [PosMark] -> Marklist
marklist = marklist_from_vector . TimeVector.signal . map (first RealTime.score)

marklist_from_vector :: MarklistVector -> Marklist
marklist_from_vector vec = Marklist vec $ MarklistPtr $ Unsafe.unsafePerformIO $
        MVar.newMVar (Left vec)

empty_marklist :: Marklist
empty_marklist = marklist mempty

-- | Marks starting at the first mark >= the given pos, to the end.
ascending :: ScoreTime -> Marklist -> [PosMark]
ascending pos mlist = map unsample $
    TimeVector.ascending (RealTime.score pos) (marklist_vec mlist)

-- | Marks starting at the first mark below the given pos, to the beginning.
descending :: ScoreTime -> Marklist -> [PosMark]
descending pos mlist = map unsample $
    TimeVector.descending (RealTime.score pos) (marklist_vec mlist)

unsample :: TimeVector.Sample Mark -> PosMark
unsample s = (RealTime.to_score (TimeVector.sx s), TimeVector.sy s)

-- | Get the position of the last mark.
marklist_end :: Marklist -> ScoreTime
marklist_end = maybe 0 (RealTime.to_score . TimeVector.sx)
    . TimeVector.last . marklist_vec

marklist_start :: Marklist -> ScoreTime
marklist_start = maybe 0 (RealTime.to_score . TimeVector.sx)
    . TimeVector.head . marklist_vec

-- ** modification

insert_mark :: ScoreTime -> Mark -> Marklist -> Marklist
insert_mark pos mark = marklist . List.insertBy cmp (pos, mark) . ascending 0
    where cmp a b = compare (fst a) (fst b)

-- * mark

data Mark = Mark {
    -- | An arbitrary low integer.  This is the only part of the mark that
    -- matters to the code, the rest is purely visual.  By convention, the
    -- most prominent divisions start at rank 0 and go up from there.
    mark_rank :: !Rank
    -- | Width in pixels.
    , mark_width :: !Int
    , mark_color :: !Color.Color
    -- | A bit of text displayed with the mark.
    , mark_name :: !Text
    -- | The text is only displayed when the zoom factor exceeds this value.
    , mark_name_zoom_level :: !Double
    -- | The mark itself is only displayed when the zoom factor exeeds this
    -- value.
    , mark_zoom_level :: !Double
    } deriving (Eq, Show, Read)

null_mark :: Mark
null_mark = Mark 0 0 Color.black "" 0 0

type Rank = Int

instance DeepSeq.NFData Mark where
    rnf (Mark rank width color name name_zoom zoom) = rank `seq` width
        `seq` color `seq` name `seq` name_zoom `seq` zoom `seq` ()

instance Pretty Mark where
    pretty m = "<mark: " ++ show (mark_rank m) ++ name ++ ">"
        where
        name = if Text.null (mark_name m) then "" else ' ' : untxt (mark_name m)
