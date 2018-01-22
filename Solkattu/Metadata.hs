-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions to deal with 'Korvai.Metadata'.  The type itself has to be
-- defined in "Solkattu.Korvai" to avoid a circular import.
module Solkattu.Metadata (
    -- * query
    get, getLocation, setLocation, showLocation, getModuleVariable
    -- * add
    , comment, date, source, similarTo, tSimilarTo
    , recording
    , korvaiT, koraippu, mohra, sarvalaghu, tirmanam
    , sequenceT, faran, exercise, trikalam
) where
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.CallStack as CallStack
import qualified Solkattu.Korvai as Korvai
import Solkattu.Korvai (Korvai)
import Global


-- * query

get :: Text -> Korvai -> [Text]
get tag = Map.findWithDefault [] tag . untags . Korvai._tags
    . Korvai.korvaiMetadata
    where untags (Korvai.Tags tags) = tags

getLocation :: Korvai -> Korvai.Location
getLocation = Korvai._location . Korvai.korvaiMetadata

showLocation :: Korvai.Location -> Text
showLocation (module_, line, name) =
    name <> " (" <> module_ <> ":" <> showt line <> ")"

setLocation :: Korvai.Location -> Korvai -> Korvai
setLocation loc korvai = korvai
    { Korvai.korvaiMetadata = (Korvai.korvaiMetadata korvai)
        { Korvai._location = loc
        }
    }

getModuleVariable :: Korvai -> Text
getModuleVariable korvai = last (Text.splitOn "." module_) <> "." <> name
    where (module_, _, name) = getLocation korvai

-- * add

-- | Attach a generic comment.
comment :: Text -> Korvai -> Korvai
comment = withTag "comment"

date :: CallStack.Stack => Int -> Int -> Int -> Korvai -> Korvai
date y m d = Korvai.withMetadata $ mempty { Korvai._date = Just date }
    where !date = Korvai.date y m d

-- | Where or from who I learned it.
source :: Text -> Korvai -> Korvai
source = withTag "source"

-- | This could be considered a variant of the other.  Takes "Module"
-- "variableName", since the location is added later in "Solkattu.All".
-- The link is verified in Db_test.
similarTo :: Text -> Text -> Korvai -> Korvai
similarTo module_ variableName =
    withTag tSimilarTo (module_ <> "." <> variableName)

tSimilarTo :: Text
tSimilarTo = "similar_to"

-- | (hour, minute, second)
type Time = (Int, Int, Int)

-- | A recording where the clip is played.
recording :: CallStack.Stack => Text -- ^ URL to the recording or video
    -> Maybe (Time, Time)
    -- ^ start and end time of the clip within the recording
    -> Korvai -> Korvai
recording url maybeRange = withTag "recording" (showRecording url maybeRange)

showRecording :: CallStack.Stack => Text -> Maybe (Time, Time) -> Text
showRecording url maybeRange = Text.unwords $
    url : case maybeRange of
        Nothing -> []
        Just (start, end) -> [showTime start, showTime end]

showTime :: CallStack.Stack => Time -> Text
showTime (h, m, s)
    | any (<0) [h, m, s] || any (>=60) [m, s] =
        errorStack $ "invalid time: " <> showt (h, m, s)
    | otherwise = mconcat $ concat
        [ [showt h <> "h" | h > 0]
        , [showt m <> "m" | m > 0]
        , [showt s <> "s" | s > 0]
        ]

-- ** types

korvaiT :: Korvai -> Korvai
korvaiT = withType "korvai"

koraippu :: Korvai -> Korvai
koraippu = withType "koraippu"

mohra :: Korvai -> Korvai
mohra = withType "mohra"

sarvalaghu :: Korvai -> Korvai
sarvalaghu = withType "sarvalaghu"

tirmanam :: Korvai -> Korvai
tirmanam = withType "tirmanam"

-- | A development sequence, possibly leading to a korvai.
sequenceT :: Korvai -> Korvai
sequenceT = withType "sequence"

faran :: Korvai -> Korvai
faran = withType "faran"

exercise :: Korvai -> Korvai
exercise = withType "exercise"

trikalam :: Korvai -> Korvai
trikalam = withType "trikalam"

withType :: Text -> Korvai -> Korvai
withType = withTag "type"

withTag :: Text -> Text -> Korvai -> Korvai
withTag k v = Korvai.withMetadata $
    mempty { Korvai._tags = Korvai.Tags (Map.singleton k [v]) }
