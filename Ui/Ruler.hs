-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ui.Ruler (
    -- * Ruler
    Ruler(..), Marklists, Name
    , meter_ruler, meter_name
    , empty_ruler
    , get_meter, lookup_meter, set_meter, modify_meter
    , lookup_marklist, get_marklist, set_marklist, remove_marklist
    , modify_marklist, modify_marklists
    , time_end
    -- ** bounds
    , bounds_name, set_bounds, get_bounds, bounds_of
) where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Map as Map

import qualified Util.Pretty as Pretty
import qualified App.Config as Config
import qualified Ui.Color as Color
import qualified Ui.Meter.Make as Make
import qualified Ui.Meter.Mark as Mark
import           Ui.Meter.Mark (Marklist)
import qualified Ui.Meter.Meter as Meter

import           Global
import           Types


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
--
-- However, multiple meters are liable to be cluttered, and multiple cue lists
-- also seem gratuitous.
type Marklists = Map Name (Maybe Meter.Meter, Marklist)
type Name = Text

instance Pretty Ruler where
    format (Ruler mlists bg show_names align_to_bottom) = Pretty.record "Ruler"
        [ ("marklists", Pretty.format mlists)
        , ("bg", Pretty.format bg)
        , ("show_names", Pretty.format show_names)
        , ("align_to_bottom", Pretty.format align_to_bottom)
        ]

instance DeepSeq.NFData Ruler where rnf (Ruler mlists _ _ _) = mlists `seq` ()


-- * ruler

empty_ruler :: Ruler
empty_ruler = Ruler
    { ruler_marklists = mempty
    , ruler_bg = Config.ruler_bg
    , ruler_show_names = False
    , ruler_align_to_bottom = False
    }

-- | Create a ruler with just a 'meter' marklist.
meter_ruler :: Meter.Meter -> Ruler
meter_ruler meter = set_meter meter empty_ruler

-- | The meter marklist by convention has marks corresponding to the meter of
-- the piece.  Other commands may use this to find out where beats are.
meter_name :: Name
meter_name = "meter"

get_meter :: Ruler -> Meter.Meter
get_meter = fromMaybe Meter.empty_meter . lookup_meter

lookup_meter :: Ruler -> Maybe Meter.Meter
lookup_meter = maybe Nothing fst . Map.lookup meter_name . ruler_marklists

set_meter :: Meter.Meter -> Ruler -> Ruler
set_meter meter = modify_marklists $
    Map.insert meter_name (Just meter, Make.make_marklist meter)

modify_meter :: (Meter.Meter -> Meter.Meter) -> Ruler -> Ruler
modify_meter modify ruler = set_meter (modify (get_meter ruler)) ruler

get_marklist :: Name -> Ruler -> Marklist
get_marklist name = fromMaybe Mark.empty . lookup_marklist name

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

modify_marklists
    :: (Map Name (Maybe Meter.Meter, Marklist)
        -> Map Name (Maybe Meter.Meter, Marklist))
    -> Ruler -> Ruler
modify_marklists modify ruler =
    ruler { ruler_marklists = modify (ruler_marklists ruler) }

-- | Get the position of the last mark of the ruler.
time_end :: Ruler -> ScoreTime
time_end = maximum . (0 :) . map (Mark.end . snd) . Map.elems . ruler_marklists

-- ** bounds

-- | Marks on this marklist given the logical block bounds.  If there is one
-- mark, it denotes the logical block end.  Two morks mean the first one is the
-- logical block start.  The mark text doesn't matter, but @s@ and @e@ are
-- customary.
bounds_name :: Name
bounds_name = "bounds"

set_bounds :: Maybe ScoreTime -> Maybe ScoreTime -> Ruler -> Ruler
set_bounds start end =
    set_marklist bounds_name $ Mark.marklist $ case (start, end) of
        -- Ensure that start <= end.
        (Just s, Just e) -> [(min s e, start_mark), (max s e, end_mark)]
        (Just s, Nothing) -> [(s, start_mark)]
        (Nothing, Just e) -> [(e, end_mark)]
        (Nothing, Nothing) -> []

start_mark, end_mark :: Mark.Mark
start_mark = Mark.Mark 0 2 (Color.rgb 0 0.75 0) "s" 0 0
end_mark = Mark.Mark 0 2 (Color.rgb 0 0.75 0) "e" 0 0

get_bounds :: Ruler -> (Maybe ScoreTime, Maybe ScoreTime)
get_bounds ruler = case lookup_marklist bounds_name ruler of
    Nothing -> (Nothing, Nothing)
    Just mlist -> case Mark.to_list mlist of
        [] -> (Nothing, Nothing)
        [(p, m)]
            | Mark.mark_name m == Mark.mark_name end_mark -> (Nothing, Just p)
            | otherwise -> (Just p, Nothing)
        _ -> (Just (Mark.start mlist), Just (Mark.end mlist))

-- | Get block bounds as defined by the ruler.  This uses explicit
-- 'bounds_name' if there are any, otherwise it uses the 'meter'.  Otherwise,
-- the start time can default to 0, but the end time defaults to Nothing so the
-- caller can use the end of the last event.
bounds_of :: Ruler -> (ScoreTime, Maybe ScoreTime)
bounds_of ruler = case get_bounds ruler of
    (Nothing, Nothing) -> (0, mb_end)
    (start, end) -> (fromMaybe 0 start, end <|> mb_end)
    where mb_end = Meter.meter_end <$> lookup_meter ruler
