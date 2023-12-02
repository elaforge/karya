-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RankNTypes #-}
-- | Collect korvais into a searchable form.
module Solkattu.Db where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Monoid as Monoid
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Time.Calendar as Calendar

import           GHC.Stack (HasCallStack)
import qualified System.Directory as Directory
import           System.FilePath ((</>))

import qualified Util.Files as Files
import qualified Util.Html
import qualified Util.Lists as Lists
import qualified Util.Num as Num
import qualified Util.SourceControl as SourceControl

import qualified Solkattu.All as All -- generated
import qualified Solkattu.Format.Format as Format
import qualified Solkattu.Format.Html as Html
import qualified Solkattu.Format.Terminal as Terminal
import qualified Solkattu.Instrument.Mridangam as Instrument.Mridangam
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Metadata as Metadata
import qualified Solkattu.Realize as Realize
import qualified Solkattu.S as S
import qualified Solkattu.Solkattu as Solkattu
import qualified Solkattu.Tags as Tags

import           Global


scores :: [(Int, Korvai.Score)]
scores = zip [0..] (List.sortOn key All.scores)
    where
    key score = (Korvai._date m, Korvai._location m)
        where m = Korvai.scoreMetadata score

realizeKon :: Int -> IO ()
realizeKon i = do
    let score = get i
    Text.IO.putStr $ format (i, score)
    Korvai.realizeScore (Terminal.printKonnakol Terminal.konnakolConfig) score

realizeM :: Int -> IO ()
realizeM i = do
    let score = get i
    Text.IO.putStr $ format (i, score)
    Korvai.realizeScore (Terminal.printInstrument Korvai.IMridangam mempty)
        score

get :: Int -> Korvai.Score
get = snd . (scores !!)

-- * predicates

-- | The number of date groups starting from the most recent.
recentDates :: Int -> Select
recentDates groups = concat . Lists.takeEnd groups
    . Lists.groupSort (Korvai._date . Korvai.scoreMetadata . snd)

aroundDate :: Calendar.Day -> Integer -> Korvai.Korvai -> Bool
aroundDate date days =
    maybe False inRange . Korvai._date . Korvai.korvaiMetadata
    where
    inRange = Num.inRange (Calendar.addDays (-days) date)
        (Calendar.addDays days date)

-- | Make a date for 'aroundDate'.
date :: HasCallStack => Int -> Int -> Int -> Calendar.Day
date = Metadata.makeDate

ofType :: Text -> Korvai.Score -> Bool
ofType type_ = (type_ `elem`) . Metadata.scoreTag "type"

nameLike :: Text -> Korvai.Score -> Bool
nameLike name = (name `Text.isInfixOf`) . qualifiedName

hasInstrument :: Text -> Korvai.Score -> Bool
hasInstrument inst = (inst `elem`) . Metadata.scoreTag "instrument"

tagHas :: Text -> Text -> Korvai.Score -> Bool
tagHas tag val score =
    any (val `Text.isInfixOf`) $
        Metadata.scoreTag tag score ++ Metadata.sectionTag tag score

-- | "na na nadin" - like grep, but skips whitespace.  But, it doesn't
-- highlight the matches like grep can.
sollus :: Text -> Korvai.Score -> Bool
sollus str = scoreHas Korvai.IKonnakol strokes
    where strokes = Maybe.catMaybes $ Solkattu.check $ Solkattu.parseSollus str

-- | Search for mridangam strokes, e.g. "n n nd".  Like 'sollus.
strokesM :: String -> Korvai.Score -> Bool
strokesM str = scoreHas Korvai.IMridangam strokes
    where
    strokes = Maybe.catMaybes $ Solkattu.check $
        Instrument.Mridangam.fromString str

scoreHas :: Eq a => Korvai.Instrument a -> [a] -> Korvai.Score -> Bool
scoreHas instrument strokes =
    any (korvaiHas instrument strokes) . Korvai.scoreKorvais

korvaiHas :: Eq a => Korvai.Instrument a -> [a] -> Korvai.Korvai -> Bool
korvaiHas instrument strokes =
    any (strokes `List.isInfixOf`) . korvaiStrokes instrument

korvaiStrokes :: Korvai.Instrument a -> Korvai.Korvai -> [[a]]
korvaiStrokes instrument =
    maybe [] (map section) . Korvai.getSections instrument
        . Korvai.korvaiSections
    where
    section = mapMaybe (fmap Realize._stroke . Solkattu.solluOf) . S.notes
        . S.toList . Korvai.sectionSequence

-- * search

-- | Search for and print korvais.
searchp :: [Korvai.Score -> Bool] -> IO ()
searchp = Text.IO.putStrLn . formats . search

-- | Select scores to search.  Filter can only look at one score at a time,
-- this can select a group of them.
type Select = forall i. [(i, Korvai.Score)] -> [(i, Korvai.Score)]

search :: [Korvai.Score -> Bool] -> [(Int, Korvai.Score)]
search = searchAll id

searchAll :: Select -> [Korvai.Score -> Bool] -> [(Int, Korvai.Score)]
searchAll select predicates = filter (predicate . snd) $ select scores
    where predicate = Monoid.getAll . mconcatMap (Monoid.All .) predicates

formats :: [(Int, Korvai.Score)] -> Text
formats = Text.stripEnd . Text.unlines . map format

format :: (Int, Korvai.Score) -> Text
format (i, score) = mconcat
    [ showt i, ": "
    , qualifiedName score, " -- " <> maybe "no date" showt date, "\n"
    , tagsText
    ]
    where
    tagsText = Text.unlines $ map ("    "<>) $ map (Text.intercalate "; ") $
        Lists.chunked 3 $ map (\(k, v) -> k <> ": " <> Text.unwords v) $
        Map.toAscList tags
    Tags.Tags tags = Korvai._tags meta
    date = Korvai._date meta
    meta = Korvai.scoreMetadata score


-- * write

writeAll :: IO ()
writeAll = writeText >> writeHtml

writeHtml :: IO ()
writeHtml = writeHtmlTo "data/solkattu-html"

writeHtml1 :: Korvai.Score -> IO ()
writeHtml1 score = do
    let fname = scoreFname score <> ".html"
    putStrLn $ "write " <> fname
    Html.writeAll fname score

-- | Write all Korvais as HTML into the given directory.
writeHtmlTo :: FilePath -> IO ()
writeHtmlTo dir = do
    clearDir dir
    writeWithStatus write1 All.scores
    Text.IO.writeFile (dir </> "index.html") $
        Util.Html.un_html $ Html.indexHtml ((<>".html") . scoreFname) All.scores
    writeCommit dir
    where
    write1 score = Html.writeAll (dir </> scoreFname score <> ".html") score

scoreFname :: Korvai.Score -> FilePath
scoreFname = untxt . qualifiedName

qualifiedName :: Korvai.Score -> Text
qualifiedName score = mod <> "." <> name
    where (mod, _, name) = Korvai._location $ Korvai.scoreMetadata score


-- ** writeText

-- | Write to solkattu-text for grepping and diffing, and solkattu-color for
-- catting.
writeText :: IO ()
writeText = writeTextTo "data/solkattu-text" "data/solkattu-color"
    Format.defaultAbstraction

-- | The usual text dir is a git repo, so I can see what effect changes have,
-- in the same manner as App.VerifyPerformance.
writeTextTo :: FilePath -> FilePath -> Format.Abstraction -> IO ()
writeTextTo dir colorDir abstraction = do
    clearDir dir
    clearDir colorDir
    writeWithStatus write1 All.scores
    writeCommit dir
    writeCommit colorDir
    where
    write1 score = do
        Files.writeLines (colorDir </> scoreFname score <> ".txt") lines
        Files.writeLines (dir </> scoreFname score <> ".txt")
            (map stripColors lines)
        where lines = Terminal.renderAll abstraction score

writeText1 :: Korvai.Score -> IO ()
writeText1 score =
    Files.writeLines (dir </> scoreFname score <> ".txt")
        (map stripColors lines)
    where
    lines = Terminal.renderAll Format.defaultAbstraction score
    dir = "data/solkattu-text"

stripColors :: Text -> Text
stripColors = Text.stripEnd . mconcat
    . Lists.mapTail (Text.drop 1 . Text.dropWhile (/='m'))
    . Text.splitOn "\ESC["

writeWithStatus :: (Korvai.Score -> IO ()) -> [Korvai.Score] -> IO ()
writeWithStatus write scores = do
    mapM_ one (zip [1..] scores)
    putChar '\n'
    where
    one (i, korvai) = do
        Text.IO.putStr $ "\ESC[K" <> num i <> "/" <> showt (length scores)
            <> ": " <> txt (scoreFname korvai) <> "\r"
        write korvai
    num = Text.justifyLeft 3 ' ' . showt

writeCommit :: FilePath -> IO ()
writeCommit dir = do
    patch <- either (errorIO . txt) return =<< SourceControl.current "."
    Text.IO.writeFile (dir </> "commit") (SourceControl._hash patch <> "\n")

clearDir :: FilePath -> IO ()
clearDir = mapM_ Directory.removeFile <=< Files.list
