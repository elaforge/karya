{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ui.Ruler (
    -- * Ruler
    Ruler(..), Marklists, Name, no_ruler
    , lookup_marklist, get_marklist, set_marklist, remove_marklist
    , modify_marklist, modify_marklists, map_marklists
    , time_end
    -- * Marklist
    , Marklist, PosMark, marklist, marklist_map
    , marklist_fptr -- This should only be used from Ui.RulerC.
    , split, ascending, descending
    , marklist_end, first_pos
    -- ** modification
    , shift, insert_mark
    -- * Mark
    , Mark(..), null_mark, Rank
) where
import qualified Control.Concurrent.MVar as MVar
import qualified Control.DeepSeq as DeepSeq
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid

import qualified Foreign
import qualified System.IO.Unsafe as Unsafe

import Util.Control
import qualified Util.Map as Map
import qualified Util.Pretty as Pretty
import Util.Pretty

import qualified Ui.Color as Color
import Types


-- * Ruler

data Ruler = Ruler {
    -- | The marklists are drawn in order, and the draw order is determined
    -- by the order of the string key.
    ruler_marklists :: !Marklists
    , ruler_bg :: !Color.Color
    -- | Show the names if this is on an event track.  Ruler tracks always show
    -- the names.
    , ruler_show_names :: !Bool
    -- | Align bottoms of marks to beats, instead of the top.  Looks good used
    -- with negative duration events (arrival beats).
    , ruler_align_to_bottom :: !Bool
    } deriving (Eq, Show)

type Marklists = Map.Map Name Marklist
type Name = String

instance Pretty.Pretty Ruler where
    pretty ruler = "((Ruler "
        ++ Pretty.pretty
            (Map.map (Map.size . marklist_map) (ruler_marklists ruler))
        ++ "))"

instance DeepSeq.NFData Ruler where
    rnf (Ruler mlists _ _ _) = DeepSeq.rnf mlists

-- | Empty ruler.
no_ruler :: Ruler
no_ruler = Ruler
    { ruler_marklists = Map.empty
    , ruler_bg = Color.black
    , ruler_show_names = False
    , ruler_align_to_bottom = False
    }

lookup_marklist :: Name -> Ruler -> Maybe Marklist
lookup_marklist name = Map.lookup name . ruler_marklists

get_marklist :: Name -> Ruler -> Marklist
get_marklist name = fromMaybe mempty . lookup_marklist name

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
    { marklist_map :: !(Map.Map ScoreTime Mark)
    -- | This is a cache for the C-marshalled version of the marklist.
    -- It will be allocated if the Marklist is passed to C, and is managed with
    -- its own reference count.
    --
    -- I think this should be safe as long as 'marklist' is the only
    -- constructor.
    , marklist_fptr :: !(MVar.MVar (Maybe (Foreign.ForeignPtr Marklist)))
    }

type PosMark = (ScoreTime, Mark)

instance Show Marklist where
    showsPrec _ mlist =
        ("Ruler.marklist ("++) . shows (marklist_map mlist) . (")"++)
instance Eq Marklist where
    m1 == m2 = marklist_map m1 == marklist_map m2
instance DeepSeq.NFData Marklist where
    rnf mlist = DeepSeq.rnf (marklist_map mlist)

marklist :: Map.Map ScoreTime Mark -> Marklist
marklist marks = Marklist marks (Unsafe.unsafePerformIO (MVar.newMVar Nothing))

split :: Marklist -> ScoreTime -> ([PosMark], [PosMark])
split mlist pos = (Map.toDescList pre, Map.toAscList post)
    where (pre, post) = Map.split2 pos (marklist_map mlist)

-- | Marks starting at the first mark >= the given pos, to the end.
ascending :: Marklist -> ScoreTime -> [PosMark]
ascending mlist 0 = Map.toAscList (marklist_map mlist)
ascending marklist pos = snd (split marklist pos)

-- | Marks starting at the first mark <= the given pos, to the beginning.
descending :: Marklist -> ScoreTime -> [PosMark]
descending marklist pos = fst (split marklist pos)

-- | Get the position of the last mark.
marklist_end :: Marklist -> ScoreTime
marklist_end = maybe 0 fst . Map.max . marklist_map

first_pos :: Marklist -> ScoreTime
first_pos mlist = maybe 0 fst (Map.min (marklist_map mlist))

-- ** modification

mapm :: (Map.Map ScoreTime Mark -> Map.Map ScoreTime Mark)
    -> Marklist -> Marklist
mapm f = marklist . f . marklist_map

instance Monoid.Monoid Marklist where
    mempty = marklist mempty
    mappend m1@(Marklist a1 _) m2@(Marklist a2 _)
        | m2 == mempty = m1
        | otherwise = marklist $ Map.union (fst (Map.split start a1)) a2
        where start = first_pos m2
    mconcat = List.foldl' Monoid.mappend Monoid.mempty

shift :: ScoreTime -> Marklist -> Marklist
shift offset = mapm $ Map.mapKeys (+offset)

insert_mark :: ScoreTime -> Mark -> Marklist -> Marklist
insert_mark pos mark = mapm $ Map.insert pos mark

-- * mark

data Mark = Mark {
    mark_rank :: !Rank
    , mark_width :: !Int
    , mark_color :: !Color.Color
    , mark_name :: !String
    , mark_name_zoom_level :: !Double
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
        where name = if null (mark_name m) then "" else ' ' : mark_name m
