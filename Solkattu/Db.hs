-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RankNTypes #-}
-- | Collect korvais into a searchable form.
module Solkattu.Db (
    module Solkattu.Db
    , module Solkattu.SolkattuGlobal
) where
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Time.Calendar as Calendar

import qualified System.Directory as Directory
import System.FilePath ((</>))

import qualified Util.CallStack as CallStack
import qualified Util.Doc as Doc
import qualified Util.File as File
import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Util.SourceControl as SourceControl

import qualified Solkattu.All as All -- generated
import qualified Solkattu.Format.Format as Format
import qualified Solkattu.Format.Html as Html
import qualified Solkattu.Format.Terminal as Terminal
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Metadata as Metadata
import Solkattu.SolkattuGlobal
       (index, realize, realizep, realizeM, realizeK1, realizeR)
import qualified Solkattu.Tags as Tags

import Global


korvais :: [Korvai.Korvai]
korvais = All.korvais

-- * predicates

-- | The number of date groups starting from the most recent.
recentDates :: Int -> FilterP
recentDates groups = concat . Seq.rtake groups
    . Seq.group_sort (Korvai._date . Korvai.korvaiMetadata . snd)

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
searchp = Text.IO.putStrLn . search id

-- | TODO this is awkward.  I just want to lift 'snd' into [x] -> [x] the same
-- way I can trivially lift it into x -> Bool.  But I don't think I can write
-- ([a] -> [a]) -> ([(x, a)] -> [(x, a)]), because [a]->[a] is not guaranteed
-- to be a filter.  Also it's just for 'recentDates', but search needs a whole
-- new argument for it, and it seems too annoying to lift all predicates to the
-- list-to-list form.
type FilterP = forall x. [(x, Korvai.Korvai)] -> [(x, Korvai.Korvai)]

search :: FilterP -> (Korvai.Korvai -> Bool) -> Text
search filterP predicate = Text.stripEnd $
    Text.unlines $ map format $ filter (predicate . snd) $ filterP $
    zip [0..] korvais

searchIndices :: FilterP -> (Korvai.Korvai -> Bool) -> [Int]
searchIndices filterP predicate =
    map fst $ filter (predicate . snd) $ filterP $ zip [0..] korvais

format :: (Int, Korvai.Korvai) -> Text
format (i, korvai) = mconcat
    [ showt i
    , ": "
    , Metadata.showLocation (Metadata.getLocation korvai)
    , " -- " <> maybe "no date" showt date
    , "\n"
    , tagsText
    ]
    where
    tagsText = Text.unlines $ map ("    "<>) $ map (Text.intercalate "; ") $
        Seq.chunked 3 $ map (\(k, v) -> k <> ": " <> Text.unwords v) $
        Map.toAscList tags
    Tags.Tags tags = Korvai._tags $ Korvai.korvaiMetadata korvai
    date = Korvai._date (Korvai.korvaiMetadata korvai)


-- * write

writeHtml :: IO ()
writeHtml = writeHtmlTo "../data/solkattu"

-- | Write all Korvais as HTML into the given directory.
writeHtmlTo :: FilePath -> IO ()
writeHtmlTo dir = do
    clearDir dir
    writeWithStatus write1 All.korvais
    Text.IO.writeFile (dir </> "index.html") $
        Doc.un_html $ Html.indexHtml ((<>".html") . korvaiFname) All.korvais
    writeCommit dir
    where
    write1 korvai = Html.writeAll (dir </> korvaiFname korvai <> ".html") korvai

korvaiFname :: Korvai.Korvai -> FilePath
korvaiFname korvai = untxt $ mod <> "." <> variableName
    where
    (mod, _, variableName) = Korvai._location $ Korvai.korvaiMetadata korvai

-- ** writeText

textDir :: FilePath
textDir = "../data/solkattu-text"

writeText :: IO ()
writeText = writeTextTo textDir Format.defaultAbstraction

-- | The usual textDir is a git repo, so I can see what effect changes have, in
-- the same manner as App.VerifyPerformance.
writeTextTo :: FilePath -> Format.Abstraction -> IO ()
writeTextTo dir abstraction = do
    clearDir dir
    writeWithStatus write1 All.korvais
    writeCommit dir
    where
    write1 korvai =
        Terminal.writeAll (dir </> korvaiFname korvai <> ".txt")
            abstraction korvai

writeWithStatus :: (Korvai.Korvai -> IO ()) -> [Korvai.Korvai] -> IO ()
writeWithStatus write korvais = do
    mapM_ one (zip [1..] korvais)
    putChar '\n'
    where
    one (i, korvai) = do
        Text.IO.putStr $ "\ESC[K" <> num i <> "/" <> showt (length korvais)
            <> ": " <> txt (korvaiFname korvai) <> "\r"
        write korvai
    num = Text.justifyLeft 3 ' ' . showt

writeCommit :: FilePath -> IO ()
writeCommit dir = do
    patch <- either (errorIO . txt) return =<< SourceControl.current "."
    Text.IO.writeFile (dir </> "commit") (SourceControl._hash patch <> "\n")

clearDir :: FilePath -> IO ()
clearDir = mapM_ Directory.removeFile <=< File.list
