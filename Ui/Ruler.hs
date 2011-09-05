{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ui.Ruler where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Array.IArray as IArray
import Data.Array.IArray ((!))
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid

import qualified Util.Array as Array
import Util.Control
import Util.Pretty

import Ui
import qualified Ui.Color as Color


data Ruler = Ruler {
    -- | The marklists are drawn in order, and the draw order is determined
    -- by the order of the string key.
    ruler_marklists :: Marklists
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
no_ruler = ruler Map.empty Color.black False False False False

get_marklist :: Name -> Ruler -> Maybe Marklist
get_marklist name = Map.lookup name . ruler_marklists

set_marklist :: Name -> Marklist -> Ruler -> Ruler
set_marklist name mlist = modify_marklists (Map.insert name mlist)

remove_marklist :: Name -> Ruler -> Ruler
remove_marklist = modify_marklists . Map.delete

-- | If the marklist isn't set, modify will be given an empty one.
modify_marklist :: Name -> (Marklist -> Marklist) -> Ruler -> Ruler
modify_marklist name modify ruler = set_marklist name new ruler
    where new = maybe (modify mempty) modify (get_marklist name ruler)

-- | Transform all the marklists in a ruler.
map_marklists :: (Marklist -> Marklist) -> Ruler -> Ruler
map_marklists f ruler =
    ruler { ruler_marklists = Map.map f (ruler_marklists ruler) }

modify_marklists :: (Map.Map Name Marklist -> Map.Map Name Marklist)
    -> Ruler -> Ruler
modify_marklists modify ruler =
    ruler { ruler_marklists = modify (ruler_marklists ruler) }

clip :: ScoreTime -> Ruler -> Ruler
clip pos = map_marklists (clip_marklist pos)

-- | Get the position of the last mark of the ruler.
time_end :: Ruler -> ScoreTime
time_end = maximum . (0 :) . map last_pos . Map.elems . ruler_marklists

-- * marklist

newtype Marklist = Marklist (Array.Array PosMark)
    deriving (DeepSeq.NFData, Eq, Show, Read)

mapm :: (Array.Array PosMark -> Array.Array PosMark) -> Marklist -> Marklist
mapm f (Marklist m) = Marklist (f m)

type Marklists = Map.Map Name Marklist
type Name = String
type PosMark = (ScoreTime, Mark)

-- | Construct a Marklist.
marklist :: [PosMark] -> Marklist
marklist = Marklist . Array.from_list

marks_of :: Marklist -> [PosMark]
marks_of (Marklist a) = IArray.elems a

instance Monoid.Monoid Marklist where
    mempty = Marklist Array.empty
    -- Unfortunately I don't think IArray has an efficient way to do this.
    mappend m1@(Marklist a1) m2@(Marklist a2)
        | m2 == mempty = m1
        | otherwise = marklist $ clip (IArray.elems a1) ++ IArray.elems a2
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

insert_mark :: PosMark -> Marklist -> Marklist
insert_mark mark (Marklist a) = marklist $
    List.insertBy (\x y -> compare (fst x) (fst y)) mark (IArray.elems a)

-- | Clip the marklist before the given pos.
--
-- Unlike most functions that take ranges, the output will *include* the end
-- pos.  This is because it's convenient for rulers to have a mark at the end.
clip_marklist :: ScoreTime -> Marklist -> Marklist
clip_marklist pos mlist =
    marklist (takeWhile ((<=pos) . fst) (forward mlist 0))

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
