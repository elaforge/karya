{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ui.Ruler where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Array.IArray as IArray
import Data.Array.IArray ((!))

import Util.Pretty
import qualified Util.Array as Array

import Ui
import qualified Ui.Color as Color


-- TODO I should just use a ScoreTimeMap like tracks do.  Then I can share some
-- of the utilities.

data Ruler = Ruler {
    -- | It's handy for marklists to have symbolic names, but their order is
    -- also important.
    ruler_marklists :: [NameMarklist]
    , ruler_bg :: Color
    , ruler_show_names :: Bool
    , ruler_use_alpha :: Bool
    -- | Align bottoms of marks to beats, instead of the top.  Looks good used
    -- with negative duration events (arrival beats).
    , ruler_align_to_bottom :: Bool
    , ruler_full_width :: Bool
    } deriving (Eq, Show, Read)
ruler = Ruler

instance DeepSeq.NFData Ruler where
    rnf (Ruler mlists bg names alpha align full) = DeepSeq.rnf mlists
        `seq` bg `seq` names `seq` alpha `seq` align `seq` full `seq` ()

-- | Get the position of the last mark of the ruler.
time_end :: Ruler -> ScoreTime
time_end = maximum . (ScoreTime 0 :) . map (last_pos . snd) . ruler_marklists

-- | Empty ruler.
no_ruler :: Ruler
no_ruler = ruler [] Color.black False False False False

-- | Transform all the marklists in a ruler.
map_marklists :: (NameMarklist -> NameMarklist) -> Ruler -> Ruler
map_marklists f ruler =
    ruler { ruler_marklists = map f (ruler_marklists ruler) }

clip :: ScoreTime -> Ruler -> Ruler
clip pos = map_marklists (clip_marklist pos)

-- * marklist

newtype Marklist = Marklist (IArray.Array Int PosMark)
    deriving (DeepSeq.NFData, Eq, Show, Read)
-- If I used a Map here instead of an Array I could reuse functions from
-- EventList.  On the other hand, there aren't that many to reuse and arrays
-- are compact, so I'll let it be.

type NameMarklist = (MarklistName, Marklist)
type MarklistName = String
type PosMark = (ScoreTime, Mark)

-- | Construct a Marklist.
marklist :: MarklistName -> [PosMark] -> NameMarklist
marklist name posmarks =
    (name, Marklist $ IArray.listArray (0, length posmarks-1) posmarks)

-- | Clip the marklist before the given pos.
--
-- Unlike most functions that take ranges, the output will *include* the end
-- pos.  This is because it's convenient for rulers to have a mark at the end.
clip_marklist :: ScoreTime -> NameMarklist -> NameMarklist
clip_marklist pos (name, mlist) =
    marklist name (takeWhile ((<=pos) . fst) (forward mlist (ScoreTime 0)))

-- | Get the position of the last mark.
last_pos :: Marklist -> ScoreTime
last_pos (Marklist marray)
    | i == -1 = ScoreTime 0
    | otherwise = fst (marray ! i)
    where i = snd (IArray.bounds marray)

data Mark = Mark {
    mark_rank :: Int
    , mark_width :: Int
    , mark_color :: Color
    , mark_name :: String
    , mark_name_zoom_level :: Double
    , mark_zoom_level :: Double
    } deriving (Eq, Show, Read)

null_mark :: Mark
null_mark = Mark 0 0 Color.black "" 0 0

instance DeepSeq.NFData Mark where
    rnf (Mark rank width color name name_zoom zoom) = rank `seq` width
        `seq` color `seq` name `seq` name_zoom `seq` zoom `seq` ()

instance Pretty Mark where
    pretty m = "<mark: " ++ show (mark_rank m) ++ name ++ ">"
        where name = if null (mark_name m) then "" else " " ++ mark_name m

at :: Marklist -> ScoreTime -> ([PosMark], [PosMark])
at (Marklist a) pos = (map (a!) [i-1, i-2..low], map (a!) [i..high])
    where
    i = Array.bsearch_on fst a pos
    (low, high) = IArray.bounds a

-- | Marks starting at the first mark >= the given pos, to the end.
forward :: Marklist -> ScoreTime -> [PosMark]
forward (Marklist a) 0 = IArray.elems a
forward marklist pos = snd (at marklist pos)

-- | Like 'forward', but don't include a mark equal to @pos@.
forward_from :: Marklist -> ScoreTime -> [PosMark]
forward_from marklist pos
    | not (null marks) && fst (head marks) == pos = tail marks
    | otherwise = marks
    where marks = forward marklist pos

-- | Marks starting at the first mark <= the given pos, to the beginning.
backward :: Marklist -> ScoreTime -> [PosMark]
backward marklist pos = fst (at marklist pos)
