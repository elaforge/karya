-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ui.Ruler (
    -- * Ruler
    Ruler(..), Marklists, Name
    , meterRuler, meterName
    , empty
    , getMeter, lookupMeter, setMeter, modifyMeter
    , lookupMarklist, getMarklist, setMarklist, removeMarklist
    , modifyMarklist, modifyMarklists
    , timeEnd
    -- ** bounds
    , boundsName, setBounds, getBounds, boundsOf
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
data Ruler = Ruler
    { rulerMarklists :: !Marklists
    , rulerBg :: !Color.Color
    -- | Show the names if this is on an event track.  Ruler tracks always show
    -- the names.
    , rulerShowNames :: !Bool
    -- | Align bottoms of marks to beats, instead of the top.  Looks good used
    -- with negative duration events (arrival beats).
    , rulerAlignToBottom :: !Bool
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
    format (Ruler mlists bg showNames alignToBottom) = Pretty.record "Ruler"
        [ ("marklists", Pretty.format mlists)
        , ("bg", Pretty.format bg)
        , ("show_names", Pretty.format showNames)
        , ("align_to_bottom", Pretty.format alignToBottom)
        ]

instance DeepSeq.NFData Ruler where rnf (Ruler mlists _ _ _) = mlists `seq` ()


-- * ruler

empty :: Ruler
empty = Ruler
    { rulerMarklists = mempty
    , rulerBg = Config.rulerBg
    , rulerShowNames = False
    , rulerAlignToBottom = False
    }

-- | Create a ruler with just a 'meter' marklist.
meterRuler :: Meter.Meter -> Ruler
meterRuler meter = setMeter meter empty

-- | The meter marklist by convention has marks corresponding to the meter of
-- the piece.  Other commands may use this to find out where beats are.
meterName :: Name
meterName = "meter"

getMeter :: Ruler -> Meter.Meter
getMeter = fromMaybe Meter.emptyMeter . lookupMeter

lookupMeter :: Ruler -> Maybe Meter.Meter
lookupMeter = maybe Nothing fst . Map.lookup meterName . rulerMarklists

setMeter :: Meter.Meter -> Ruler -> Ruler
setMeter meter = modifyMarklists $
    Map.insert meterName (Just meter, Make.makeMarklist meter)

modifyMeter :: (Meter.Meter -> Meter.Meter) -> Ruler -> Ruler
modifyMeter modify ruler = setMeter (modify (getMeter ruler)) ruler

getMarklist :: Name -> Ruler -> Marklist
getMarklist name = fromMaybe Mark.empty . lookupMarklist name

lookupMarklist :: Name -> Ruler -> Maybe Marklist
lookupMarklist name = fmap snd . Map.lookup name . rulerMarklists

setMarklist :: Name -> Marklist -> Ruler -> Ruler
setMarklist name mlist = modifyMarklists (Map.insert name (Nothing, mlist))

removeMarklist :: Name -> Ruler -> Ruler
removeMarklist = modifyMarklists . Map.delete

-- | If the marklist isn't set, modify will be given an empty one.
modifyMarklist :: Name -> (Marklist -> Marklist) -> Ruler -> Ruler
modifyMarklist name modify ruler =
    setMarklist name (modify (getMarklist name ruler)) ruler

modifyMarklists
    :: (Map Name (Maybe Meter.Meter, Marklist)
        -> Map Name (Maybe Meter.Meter, Marklist))
    -> Ruler -> Ruler
modifyMarklists modify ruler =
    ruler { rulerMarklists = modify (rulerMarklists ruler) }

-- | Get the position of the last mark of the ruler.
timeEnd :: Ruler -> ScoreTime
timeEnd = maximum . (0 :) . map (Mark.end . snd) . Map.elems . rulerMarklists

-- ** bounds

-- | Marks on this marklist given the logical block bounds.  If there is one
-- mark, it denotes the logical block end.  Two morks mean the first one is the
-- logical block start.  The mark text doesn't matter, but @s@ and @e@ are
-- customary.
boundsName :: Name
boundsName = "bounds"

setBounds :: Maybe ScoreTime -> Maybe ScoreTime -> Ruler -> Ruler
setBounds start end =
    setMarklist boundsName $ Mark.marklist $ case (start, end) of
        -- Ensure that start <= end.
        (Just s, Just e) -> [(min s e, startMark), (max s e, endMark)]
        (Just s, Nothing) -> [(s, startMark)]
        (Nothing, Just e) -> [(e, endMark)]
        (Nothing, Nothing) -> []

startMark, endMark :: Mark.Mark
startMark = Mark.Mark minBound 2 (Color.rgb 0 0.75 0) "s" 0 0
endMark = Mark.Mark minBound 2 (Color.rgb 0 0.75 0) "e" 0 0

getBounds :: Ruler -> (Maybe ScoreTime, Maybe ScoreTime)
getBounds ruler = case lookupMarklist boundsName ruler of
    Nothing -> (Nothing, Nothing)
    Just mlist -> case Mark.toList mlist of
        [] -> (Nothing, Nothing)
        [(p, m)]
            | Mark.markName m == Mark.markName endMark -> (Nothing, Just p)
            | otherwise -> (Just p, Nothing)
        _ -> (Just (Mark.start mlist), Just (Mark.end mlist))

-- | Get block bounds as defined by the ruler.  This uses explicit
-- 'bounds_name' if there are any, otherwise it uses the 'meter'.  Otherwise,
-- the start time can default to 0, but the end time defaults to Nothing so the
-- caller can use the end of the last event.
boundsOf :: Ruler -> (ScoreTime, Maybe ScoreTime)
boundsOf ruler = case getBounds ruler of
    (Nothing, Nothing) -> (0, meterEnd)
    (start, end) -> (fromMaybe 0 start, end <|> meterEnd)
    where
    meterEnd = Meter.meterEnd <$> lookupMeter ruler
        -- Fall back on marklist end, old scores won't have a Meter.Meter.
        <|> Mark.end <$> lookupMarklist meterName ruler
