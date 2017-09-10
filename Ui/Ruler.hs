-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ui.Ruler (
    -- * Ruler
    Ruler(..), Marklists, Name
    , MeterConfig(..), default_config
    , ruler, meter_ruler, meter
    , no_ruler
    , get_meter, lookup_meter, set_meter
    , lookup_marklist, get_marklist, set_marklist, remove_marklist
    , modify_marklist, modify_marklists
    , time_end
    -- ** bounds
    , bounds, set_bounds, get_bounds, bounds_of
    -- * Marklist
    , Marklist, MarklistVector, PosMark
    , marklist, empty_marklist, marklist_from_vector, marklist_vec
    , marklist_fptr -- This should only be used from Ui.RulerC.
    , to_list, ascending, descending
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

import qualified Util.Pretty as Pretty
import qualified Util.TimeVector as TimeVector
import qualified Ui.Color as Color
import qualified Perform.RealTime as RealTime
import qualified App.Config as Config
import Global
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
type Marklists = Map Name (Maybe MeterConfig, Marklist)
type Name = Text

-- | Configuration specific to the 'meter' marklist.
data MeterConfig = MeterConfig {
    -- | The type of meter that this marklist represents.  This is looked up in
    -- a table of meter types to figure out how to do transformations on the
    -- meter, since different meters follow different rules.
    config_name :: !Text
    , config_start_measure :: !Int
    } deriving (Eq, Show)

default_config :: MeterConfig
default_config = MeterConfig
    { config_name = "meter"
    , config_start_measure = 1
    }

instance Pretty Ruler where
    format (Ruler mlists bg show_names align_to_bottom) = Pretty.record "Ruler"
        [ ("marklists", Pretty.format mlists)
        , ("bg", Pretty.format bg)
        , ("show_names", Pretty.format show_names)
        , ("align_to_bottom", Pretty.format align_to_bottom)
        ]

instance Pretty MeterConfig where
    format (MeterConfig name start_measure) = Pretty.record "MeterConfig"
        [ ("name", Pretty.format name)
        , ("start_measure", Pretty.format start_measure)
        ]

instance DeepSeq.NFData Ruler where rnf (Ruler mlists _ _ _) = mlists `seq` ()

-- | Constructor for "plain" rulers.
ruler :: [(Name, (Maybe MeterConfig, Marklist))] -> Ruler
ruler marklists = Ruler
    { ruler_marklists = Map.fromList marklists
    , ruler_bg = Config.ruler_bg
    , ruler_show_names = False
    , ruler_align_to_bottom = False
    }

-- | Create a ruler with just a 'meter' marklist.
meter_ruler :: MeterConfig -> Marklist -> Ruler
meter_ruler config marklist = ruler [(meter, (Just config, marklist))]

-- | The meter marklist by convention has marks corresponding to the meter of
-- the piece.  Other commands may use this to find out where beats are.
meter :: Name
meter = "meter"

-- | Empty ruler.
no_ruler :: Ruler
no_ruler = ruler []

get_meter :: Ruler -> (MeterConfig, Marklist)
get_meter = first (fromMaybe default_config)
    . fromMaybe (Nothing, empty_marklist) . lookup_meter

lookup_meter :: Ruler -> Maybe (Maybe MeterConfig, Marklist)
lookup_meter = Map.lookup meter . ruler_marklists

set_meter :: MeterConfig -> Marklist -> Ruler -> Ruler
set_meter config mlist =
    modify_marklists $ Map.insert meter (Just config, mlist)

get_marklist :: Name -> Ruler -> Marklist
get_marklist name = fromMaybe empty_marklist . lookup_marklist name

lookup_marklist :: Name -> Ruler -> Maybe Marklist
lookup_marklist name = fmap snd . Map.lookup name . ruler_marklists

set_marklist :: Name -> Marklist -> Ruler -> Ruler
set_marklist name mlist = modify_marklists (Map.insert name (Nothing, mlist))

remove_marklist :: Name -> Ruler -> Ruler
remove_marklist = modify_marklists . Map.delete

-- | If the marklist isn't set, modify will be given an empty one.
modify_marklist :: Name -> (Marklist -> Marklist) -> Ruler -> Ruler
modify_marklist name modify ruler =
    set_marklist name (modify (get_marklist name ruler)) ruler

modify_marklists :: (Map Name (Maybe MeterConfig, Marklist)
        -> Map Name (Maybe MeterConfig, Marklist))
    -> Ruler -> Ruler
modify_marklists modify ruler =
    ruler { ruler_marklists = modify (ruler_marklists ruler) }

-- | Get the position of the last mark of the ruler.
time_end :: Ruler -> ScoreTime
time_end = maximum . (0 :) . map (marklist_end . snd) . Map.elems
    . ruler_marklists

-- ** bounds

-- | Marks on this marklist given the logical block bounds.  If there is one
-- mark, it denotes the logical block end.  Two morks mean the first one is the
-- logical block start.  The mark text doesn't matter, but @s@ and @e@ are
-- customary.
bounds :: Name
bounds = "bounds"

set_bounds :: Maybe ScoreTime -> Maybe ScoreTime -> Ruler -> Ruler
set_bounds start end = set_marklist bounds $ marklist $ case (start, end) of
    -- Ensure that start <= end.
    (Just s, Just e) -> [(min s e, start_mark), (max s e, end_mark)]
    (Just s, Nothing) -> [(s, start_mark)]
    (Nothing, Just e) -> [(e, end_mark)]
    (Nothing, Nothing) -> []

start_mark, end_mark :: Mark
start_mark = Mark 0 2 (Color.rgb 0 0.75 0) "s" 0 0
end_mark = Mark 0 2 (Color.rgb 0 0.75 0) "e" 0 0

get_bounds :: Ruler -> (Maybe ScoreTime, Maybe ScoreTime)
get_bounds ruler = case lookup_marklist bounds ruler of
    Nothing -> (Nothing, Nothing)
    Just mlist -> case to_list mlist of
        [] -> (Nothing, Nothing)
        [(p, m)]
            | mark_name m == mark_name end_mark -> (Nothing, Just p)
            | otherwise -> (Just p, Nothing)
        _ -> (Just (marklist_start mlist), Just (marklist_end mlist))

-- | Get block bounds as defined by the ruler.  This uses explicit 'bounds' if
-- there are any, otherwise it uses the 'meter'.  Otherwise, the start time can
-- default to 0, but the end time defaults to Nothing so the caller can use the
-- end of the last event.
bounds_of :: Ruler -> (ScoreTime, Maybe ScoreTime)
bounds_of ruler = case get_bounds ruler of
    (Nothing, Nothing) -> (0, meter_end)
    (start, end) -> (fromMaybe 0 start, end <|> meter_end)
    where meter_end = marklist_end . snd <$> lookup_meter ruler

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
marklist = marklist_from_vector . TimeVector.signal . map (first RealTime.score)

{-# NOINLINE marklist_from_vector #-} -- due to evil unsafePerformIO
marklist_from_vector :: MarklistVector -> Marklist
marklist_from_vector vec = Marklist vec $ MarklistPtr $ Unsafe.unsafePerformIO $
        MVar.newMVar (Left vec)

empty_marklist :: Marklist
empty_marklist = marklist mempty

to_list :: Marklist -> [PosMark]
to_list = map unsample . TimeVector.toList . marklist_vec

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
    -- | A bit of text displayed with the mark.  This can use backtick symbols.
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
    pretty m = "(mark " <> showt (mark_rank m) <> name <> ")"
        where name = if Text.null (mark_name m) then "" else " " <> mark_name m
