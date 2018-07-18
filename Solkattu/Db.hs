-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Collect korvais into a searchable form.
module Solkattu.Db (
    module Solkattu.Db
    , module Solkattu.Dsl
) where
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Time.Calendar as Calendar

import System.FilePath ((</>))

import qualified Util.CallStack as CallStack
import qualified Util.Doc as Doc
import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Util.SourceControl as SourceControl

import qualified Solkattu.All as All -- generated
import Solkattu.Dsl (index, realize, realizep, realizeM, realizeK1, realizeR)
import qualified Solkattu.Format.Format as Format
import qualified Solkattu.Format.Html as Html
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Metadata as Metadata
import qualified Solkattu.Tags as Tags

import Global


korvais :: [Korvai.Korvai]
korvais = All.korvais

-- * predicates

aroundDate :: Calendar.Day -> Integer -> Korvai.Korvai -> Bool
aroundDate date days =
    maybe False inRange . Korvai._date . Korvai.korvaiMetadata
    where
    inRange = Num.inRange (Calendar.addDays (-days) date)
        (Calendar.addDays days date)

ofType :: Text -> Korvai.Korvai -> Bool
ofType type_ = (type_ `elem`) . Metadata.korvaiTag "type"

variableName :: Text -> Korvai.Korvai -> Bool
variableName name = (name `Text.isInfixOf`) . Metadata.getModuleVariable

hasInstrument :: Text -> Korvai.Korvai -> Bool
hasInstrument inst = (inst `elem`) . Metadata.korvaiTag "instrument"

tagHas :: Text -> Text -> Korvai.Korvai -> Bool
tagHas tag val korvai =
    any (val `Text.isInfixOf`) $
        Metadata.korvaiTag tag korvai ++ Metadata.sectionTag tag korvai

date :: CallStack.Stack => Int -> Int -> Int -> Calendar.Day
date = Metadata.makeDate

-- * search

searchp :: (Korvai.Korvai -> Bool) -> IO ()
searchp = liftIO . Text.IO.putStrLn . search

search :: (Korvai.Korvai -> Bool) -> Text
search predicate = Text.stripEnd $
    Text.unlines $ map format $ filter (predicate . snd) $ zip [0..] korvais

format :: (Int, Korvai.Korvai) -> Text
format (i, korvai) =
    showt i <> ": " <> Metadata.showLocation (Metadata.getLocation korvai)
        <> "\n" <> tagsText
    where
    tagsText = Text.unlines $ map ("    "<>) $ map (Text.intercalate "; ") $
        Seq.chunked 3 $ map (\(k, v) -> k <> ": " <> Text.unwords v) $
        Map.toAscList tags
    Tags.Tags tags = Korvai._tags $ Korvai.korvaiMetadata korvai


-- * write

write :: IO ()
write = writeHtml "../data/solkattu" False

-- | Write all Korvais as HTML into the given directory.
writeHtml :: FilePath -> Bool -> IO ()
writeHtml dir realizePatterns = do
    mapM_ write1 All.korvais
    Text.IO.writeFile (dir </> "index.html") $
        Doc.un_html $ Html.indexHtml ((<>".html") . korvaiFname) All.korvais
    where
    write1 korvai = Html.writeAll (dir </> korvaiFname korvai <> ".html")
        realizePatterns korvai

korvaiFname :: Korvai.Korvai -> FilePath
korvaiFname korvai = untxt $ mod <> "." <> variableName
    where
    (mod, _, variableName) = Korvai._location $ Korvai.korvaiMetadata korvai

-- ** writeText

textDir :: FilePath
textDir = "../data/solkattu-text"

writeText :: IO ()
writeText = writeTextTo textDir False

-- | The usual textDir is a git repo, so I can see what effect changes have, in
-- the same manner as App.VerifyPerformance.
writeTextTo :: FilePath -> Bool -> IO ()
writeTextTo dir realizePatterns = do
    mapM_ write1 All.korvais
    patch <- either (errorIO . txt) return =<< SourceControl.current "."
    Text.IO.writeFile (dir </> "commit") (SourceControl._hash patch <> "\n")
    where
    write1 korvai = Format.writeAll (dir </> korvaiFname korvai <> ".txt")
        realizePatterns korvai
