-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Ui.Meter.Mark (
    Marklist, MarklistVector
    , marklist, marklist_from_vector
    , empty
    , to_list
    , ascending, descending
    , end, start
    , insert_mark
    , Mark(..)
    , Rank
    , Label

    -- * for RulerC's eyes only
    , MarklistPtr(..)
    , marklist_vec, marklist_fptr
) where
import qualified Control.Concurrent.MVar as MVar
import qualified Control.DeepSeq as DeepSeq
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Foreign
import qualified System.IO.Unsafe as Unsafe

import qualified Util.TimeVector as TimeVector
import qualified Perform.RealTime as RealTime
import qualified Ui.Color as Color

import           Global
import           Types


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

instance Eq Marklist where
    m1 == m2 = marklist_vec m1 == marklist_vec m2
instance Show Marklist where
    show m = "Ruler.marklist " ++ show (ascending 0 m)
instance Pretty Marklist where
    pretty m = "((" <> showt (TimeVector.length (marklist_vec m)) <> " marks))"

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

marklist :: [PosMark] -> Marklist
marklist = marklist_from_vector . TimeVector.from_pairs
    . map (first RealTime.from_score)

{-# NOINLINE marklist_from_vector #-} -- due to evil unsafePerformIO
marklist_from_vector :: MarklistVector -> Marklist
marklist_from_vector vec = Marklist
    { marklist_vec = vec
    , marklist_fptr = MarklistPtr $ Unsafe.unsafePerformIO $
        MVar.newMVar (Left vec)
    }

empty :: Marklist
empty = marklist mempty

to_list :: Marklist -> [PosMark]
to_list = map unsample . TimeVector.toList . marklist_vec

-- | Marks starting at the first mark >= the given pos, to the end.
ascending :: ScoreTime -> Marklist -> [PosMark]
ascending pos mlist = map unsample $
    TimeVector.ascending (RealTime.from_score pos) (marklist_vec mlist)

-- | Marks starting at the first mark below the given pos, to the beginning.
descending :: ScoreTime -> Marklist -> [PosMark]
descending pos mlist = map unsample $
    TimeVector.descending (RealTime.from_score pos) (marklist_vec mlist)

unsample :: TimeVector.Sample Mark -> PosMark
unsample s = (RealTime.to_score (TimeVector.sx s), TimeVector.sy s)

-- | Get the position of the last mark.
end :: Marklist -> ScoreTime
end = maybe 0 (RealTime.to_score . TimeVector.sx)
    . TimeVector.last . marklist_vec

start :: Marklist -> ScoreTime
start = maybe 0 (RealTime.to_score . TimeVector.sx)
    . TimeVector.head . marklist_vec

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
    -- | A bit of text displayed with the mark.  This can use backtick symbols.
    , mark_name :: !Label
    -- | The text is only displayed when the zoom factor exceeds this value.
    , mark_name_zoom_level :: !Double
    -- | The mark itself is only displayed when the zoom factor exeeds this
    -- value.
    , mark_zoom_level :: !Double
    } deriving (Eq, Show, Read)

type Rank = Int
type Label = Text

instance DeepSeq.NFData Mark where
    rnf (Mark rank width color name name_zoom zoom) = rank `seq` width
        `seq` color `seq` name `seq` name_zoom `seq` zoom `seq` ()

instance Pretty Mark where
    pretty m = "(mark " <> showt (mark_rank m) <> name <> ")"
        where name = if Text.null (mark_name m) then "" else " " <> mark_name m
