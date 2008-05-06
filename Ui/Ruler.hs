module Ui.Ruler where
import qualified Data.Array.IArray as IArray
import Data.Array.IArray ((!))

import qualified Util.Data

import Ui.Types
import qualified Ui.Color as Color


data Ruler = Ruler {
    -- | It's handy for marklists to have symbolic names, but their order is
    -- also important.
    ruler_marklists :: [(MarklistName, Marklist)]
    , ruler_bg :: Color
    , ruler_show_names :: Bool
    , ruler_use_alpha :: Bool
    , ruler_full_width :: Bool
    } deriving (Eq, Show, Read)

newtype RulerId = RulerId String deriving (Eq, Ord, Show, Read)
type MarklistName = String

data Marklist = Marklist (IArray.Array Int (TrackPos, Mark))
    deriving (Eq, Show, Read)
-- If I used a Map here instead of an Array I could reuse functions from
-- EventList.  On the other hand, there aren't that many to reuse and arrays
-- are compact, so I'll let it be.

-- | Construct a Marklist.
marklist :: String -> [(TrackPos, Mark)] -> (MarklistName, Marklist)
marklist name posmarks =
    (name, Marklist $ IArray.listArray (0, length posmarks-1) posmarks)

data Mark = Mark
    { mark_rank :: Int
    , mark_width :: Int
    , mark_color :: Color
    , mark_name :: String
    , mark_name_zoom_level :: Double
    , mark_zoom_level :: Double
    } deriving (Eq, Show, Read)
null_mark = Mark 0 0 Color.black "" 0 0


at :: Marklist -> TrackPos -> ([(TrackPos, Mark)], [(TrackPos, Mark)])
at (Marklist a) pos = (map (a!) [i-1, i-2..low], map (a!) [i..high])
    where
    i = Util.Data.bsearch_on fst a pos
    (low, high) = IArray.bounds a

-- | Marks starting at the first mark >= the given pos, to the end.
forward :: Marklist -> TrackPos -> [(TrackPos, Mark)]
forward marklist pos = snd (at marklist pos)

-- | Like 'forward', but don't include a mark equal to @pos@.
forward_from marklist pos
    | not (null marks) && fst (head marks) == pos = tail marks
    | otherwise = marks
    where marks = forward marklist pos

-- | Marks starting at the first mark <= the given pos, to the beginning.
backward :: Marklist -> TrackPos -> [(TrackPos, Mark)]
backward marklist pos = fst (at marklist pos)
