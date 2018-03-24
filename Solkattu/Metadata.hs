-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions to deal with 'Korvai.Metadata'.  The type itself has to be
-- defined in "Solkattu.Korvai" to avoid a circular import.
module Solkattu.Metadata where
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.CallStack as CallStack
import qualified Util.Parse as Parse
import qualified Util.Regex as Regex

import qualified Solkattu.Korvai as Korvai
import Solkattu.Korvai (Korvai)
import qualified Solkattu.Solkattu as Solkattu
import qualified Solkattu.Tags as Tags

import Global


-- * query

get :: Text -> Korvai -> [Text]
get tag = Map.findWithDefault [] tag . untags . Korvai._tags
    . Korvai.korvaiMetadata
    where untags (Tags.Tags tags) = tags

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

-- * time

-- | (hour, minute, second)
type Time = (Int, Int, Int)

showRecording :: CallStack.Stack => Text -> Maybe (Time, Time) -> Text
showRecording url maybeRange = Text.unwords $
    url : case maybeRange of
        Nothing -> []
        Just (start, end) -> [showTime start, showTime end]

parseRecording :: Text -> Maybe (Text, Maybe (Time, Time))
parseRecording s = case Text.words s of
    url : range -> (url,) <$> parseRange range
    _ -> Nothing
    where
    parseRange [] = Just Nothing
    parseRange [start, end] = do
        start <- parseTime start
        end <- parseTime end
        return $ Just (start, end)
    parseRange _ = Nothing

parseTime :: Text -> Maybe Time
parseTime s = case Regex.groups time s of
    (_, groups) : _ -> Just (parse h, parse m, parse s)
        where h : m : s : _ = groups ++ repeat ""
    _ -> Nothing
    where
    Right time = Regex.compile "(\\d+h)?(\\d+m)?(\\d+s)?"
    parse :: Text -> Int
    parse = fromMaybe 0 . Parse.parse_maybe Parse.p_nat . Text.dropEnd 1

showTime :: CallStack.Stack => Time -> Text
showTime (h, m, s)
    | any (<0) [h, m, s] || any (>=60) [m, s] =
        Solkattu.throw $ "invalid time: " <> showt (h, m, s)
    | all (==0) [h, m, s] = "0s"
    | otherwise = mconcat $ concat
        [ [showt h <> "h" | h > 0]
        , [showt m <> "m" | m > 0]
        , [showt s <> "s" | s > 0]
        ]
