{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ui.Ruler where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Array.IArray as IArray
import Data.Array.IArray ((!))
import qualified Data.List as List
import qualified Data.Monoid as Monoid

import qualified Util.Array as Array
import Util.Control
import Util.Pretty
import qualified Util.Seq as Seq

import Ui
import qualified Ui.Color as Color


data Ruler = Ruler {
    -- | It's handy for marklists to have symbolic names, but their order is
    -- also important.
    ruler_marklists :: [NameMarklist]
    , ruler_bg :: Color.Color
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

-- | Empty ruler.
no_ruler :: Ruler
no_ruler = ruler [] Color.black False False False False

get_marklist :: MarklistName -> Ruler -> Maybe Marklist
get_marklist name = lookup name . ruler_marklists

set_marklist :: NameMarklist -> Ruler -> Ruler
set_marklist (name, mlist) ruler = ruler { ruler_marklists =
    Seq.replace_with ((==name) . fst) (name, mlist) (ruler_marklists ruler) }

remove_marklist :: MarklistName -> Ruler -> Ruler
remove_marklist name ruler = ruler
    { ruler_marklists = filter ((/=name) . fst) (ruler_marklists ruler) }

-- | Transform all the marklists in a ruler.
map_marklists :: (NameMarklist -> NameMarklist) -> Ruler -> Ruler
map_marklists f ruler =
    ruler { ruler_marklists = map f (ruler_marklists ruler) }

clip :: ScoreTime -> Ruler -> Ruler
clip pos = map_marklists (clip_marklist pos)

-- | Get the position of the last mark of the ruler.
time_end :: Ruler -> ScoreTime
time_end = maximum . (0 :) . map (last_pos . snd) . ruler_marklists

-- * marklist

newtype Marklist = Marklist (Array.Array PosMark)
    deriving (DeepSeq.NFData, Eq, Show, Read)

mapm :: (Array.Array PosMark -> Array.Array PosMark) -> Marklist -> Marklist
mapm f (Marklist m) = Marklist (f m)

type NameMarklist = (MarklistName, Marklist)
type MarklistName = String
type PosMark = (ScoreTime, Mark)

-- | Construct a Marklist.
marklist :: MarklistName -> [PosMark] -> NameMarklist
marklist name posmarks = (name, Marklist $ Array.from_list posmarks)

marks_of :: Marklist -> [PosMark]
marks_of (Marklist a) = IArray.elems a

instance Monoid.Monoid Marklist where
    mempty = Marklist Array.empty
    -- Unfortunately I don't think IArray has an efficient way to do this.
    mappend m1@(Marklist a1) m2@(Marklist a2)
        | m2 == mempty = m1
        | otherwise = Marklist $
            Array.from_list $ clip (IArray.elems a1) ++ IArray.elems a2
        where
        clip = takeWhile ((<start) . fst)
        start = first_pos m2
    mconcat = List.foldl' Monoid.mappend Monoid.mempty

place_marklists :: [(ScoreTime, ScoreTime, Marklist)] -> Marklist
place_marklists = mconcat . map (\(p, s, m) -> place p s m)

shift :: ScoreTime -> Marklist -> Marklist
shift offset = mapm $ IArray.amap (first (+offset))

place :: ScoreTime -> ScoreTime -> Marklist -> Marklist
place start dur m = mapm (IArray.amap (first ((+start) . (*factor)))) m
    where
    factor = if mdur == 0 then 1 else dur / mdur
    mdur = last_pos m

-- | Clip the marklist before the given pos.
--
-- Unlike most functions that take ranges, the output will *include* the end
-- pos.  This is because it's convenient for rulers to have a mark at the end.
clip_marklist :: ScoreTime -> NameMarklist -> NameMarklist
clip_marklist pos (name, mlist) =
    marklist name (takeWhile ((<=pos) . fst) (forward mlist 0))

-- | Get the position of the last mark.
last_pos :: Marklist -> ScoreTime
last_pos (Marklist a)
    | i == -1 = 0
    | otherwise = fst (a ! i)
    where i = snd (IArray.bounds a)

first_pos :: Marklist -> ScoreTime
first_pos (Marklist a)
    | Array.null a = 0
    | otherwise = fst (a ! 0)

data Mark = Mark {
    mark_rank :: Int
    , mark_width :: Int
    , mark_color :: Color.Color
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
