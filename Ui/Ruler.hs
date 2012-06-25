{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ui.Ruler where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid

import Util.Control
import qualified Util.Map as Map
import qualified Util.Pretty as Pretty
import Util.Pretty

import qualified Ui.Color as Color
import Types


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

instance Pretty.Pretty Ruler where
    pretty ruler = "((Ruler "
        ++ Pretty.pretty (Map.map marklist_length (ruler_marklists ruler))
        ++ "))"

instance DeepSeq.NFData Ruler where
    rnf (Ruler mlists bg names alpha align full) = DeepSeq.rnf mlists
        `seq` bg `seq` names `seq` alpha `seq` align `seq` full `seq` ()

-- | Empty ruler.
no_ruler :: Ruler
no_ruler = ruler Map.empty Color.black False False False False

lookup_marklist :: Name -> Ruler -> Maybe Marklist
lookup_marklist name = Map.lookup name . ruler_marklists

set_marklist :: Name -> Marklist -> Ruler -> Ruler
set_marklist name mlist = modify_marklists (Map.insert name mlist)

remove_marklist :: Name -> Ruler -> Ruler
remove_marklist = modify_marklists . Map.delete

-- | If the marklist isn't set, modify will be given an empty one.
modify_marklist :: Name -> (Marklist -> Marklist) -> Ruler -> Ruler
modify_marklist name modify ruler = set_marklist name new ruler
    where new = maybe (modify mempty) modify (lookup_marklist name ruler)

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

newtype Marklist = Marklist (Map.Map ScoreTime Mark)
    deriving (DeepSeq.NFData, Eq, Show, Read)

mapm :: (Map.Map ScoreTime Mark -> Map.Map ScoreTime Mark)
    -> Marklist -> Marklist
mapm f (Marklist m) = Marklist (f m)

marklist_length :: Marklist -> Int
marklist_length (Marklist mlist) = Map.size mlist

type Marklists = Map.Map Name Marklist
type Name = String
type PosMark = (ScoreTime, Mark)

-- | Construct a Marklist.
marklist :: [PosMark] -> Marklist
marklist = Marklist . Map.fromList

marks_of :: Marklist -> [PosMark]
marks_of (Marklist m) = Map.toList m

instance Monoid.Monoid Marklist where
    mempty = Marklist mempty
    mappend m1@(Marklist a1) m2@(Marklist a2)
        | m2 == mempty = m1
        | otherwise = Marklist $ Map.union (fst (Map.split start a1)) a2
        where start = first_pos m2
    mconcat = List.foldl' Monoid.mappend Monoid.mempty

place_marklists :: [(ScoreTime, ScoreTime, Marklist)] -> Marklist
place_marklists = mconcat . map (\(p, s, m) -> place p s m)

shift :: ScoreTime -> Marklist -> Marklist
shift offset = mapm $ Map.mapKeys (+offset)

place :: ScoreTime -> ScoreTime -> Marklist -> Marklist
place start dur m = mapm (Map.mapKeys ((+start) . (*factor))) m
    where
    factor = if mdur == 0 then 1 else dur / mdur
    mdur = last_pos m

insert_mark :: ScoreTime -> Mark -> Marklist -> Marklist
insert_mark pos mark = mapm $ Map.insert pos mark

-- | Clip the marklist before the given pos.
--
-- Unlike most functions that take ranges, the output will *include* the end
-- pos.  This is because it's convenient for rulers to have a mark at the end.
clip_marklist :: ScoreTime -> Marklist -> Marklist
clip_marklist pos (Marklist m) =
    Marklist $ maybe pre (\v -> Map.insert pos v pre) at
    where (pre, at, _) = Map.splitLookup pos m

-- | Get the position of the last mark.
last_pos :: Marklist -> ScoreTime
last_pos (Marklist m) = maybe 0 fst (Map.max m)

first_pos :: Marklist -> ScoreTime
first_pos (Marklist m) = maybe 0 fst (Map.min m)

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

split :: Marklist -> ScoreTime -> ([PosMark], [PosMark])
split (Marklist m) pos = (Map.toDescList pre, Map.toAscList post)
    where (pre, post) = Map.split2 pos m

-- | Marks starting at the first mark >= the given pos, to the end.
ascending :: Marklist -> ScoreTime -> [PosMark]
ascending (Marklist m) 0 = Map.toAscList m
ascending marklist pos = snd (split marklist pos)

-- | Marks starting at the first mark <= the given pos, to the beginning.
descending :: Marklist -> ScoreTime -> [PosMark]
descending marklist pos = fst (split marklist pos)
