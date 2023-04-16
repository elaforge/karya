-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveGeneric #-}
-- | Utilities to randomly select things to practice, and save what
-- I practiced, for a flashcard-esque system.
module Solkattu.Practice where
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Time as Time

import qualified GHC.Generics as Generics
import qualified System.Random as Random

import qualified Util.Lists as Lists
import qualified Util.Texts as Texts
import qualified Solkattu.Db as Db
import qualified Solkattu.Format.Format as Format
import qualified Solkattu.Format.Terminal as Terminal
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Metadata as Metadata

import           Global


-- | The number of date groups starting from the most recent.
recentDates :: Int -> IO ()
recentDates n =
    Text.IO.putStrLn $ Db.formats $ Db.searchAll (Db.recentDates n) []

searchName :: Text -> IO ()
searchName name = Db.searchp [Db.nameLike name]

types :: [Text]
types = ["exercise", "korvai"]

-- | Pick a random korvai with any of the given types.
randomTypes :: [Text] -> IO ()
randomTypes types = do
    forM_ types $ \typ -> do
        score <- pick $ filter (Db.ofType typ . snd) Db.scores
        Text.IO.putStrLn $ typ <> ":"
        Text.IO.putStrLn $ maybe "Nothing" Db.format score

realize, realizep :: Int -> IO ()
realize i = do
    let score = get i
    Text.IO.putStr $ Db.format (i, score)
    realizeM mempty score
realizep i = do
    let score = get i
    Text.IO.putStr $ Db.format (i, score)
    realizeM Format.defaultAbstraction score

realizeM :: Format.Abstraction -> Korvai.Score -> IO ()
realizeM abstraction = Korvai.realizeScore $
    Terminal.printInstrument Korvai.IMridangam abstraction

realizeKon :: Int -> IO ()
realizeKon i =
    Korvai.realizeScore (Terminal.printKonnakol Terminal.konnakolConfig) (get i)

-- | Mark these korvais as practiced.  Using the index is awkward because it's
-- the same type as BPM.
practiced :: Int -> BPM -> IO ()
practiced index bpm = practicedName name bpm
    where name = Db.qualifiedName $ snd $ Db.scores !! index

practicedName :: Text -> BPM -> IO ()
practicedName name bpm = do
    now <- Time.getCurrentTime
    case Db.search [Db.nameLike name] of
        [(_, score)] -> savePracticed
            [Practiced (Db.qualifiedName score) now (Just bpm)]
        [] -> Text.IO.putStrLn $ "no score named like " <> showt name
        scores -> Text.IO.putStrLn $ "multiple scores that match:\n"
            <> Db.formats scores

type BPM = Int

get :: Int -> Korvai.Score
get = snd . (Db.scores !!)

-- * practiced db

practicedDb :: FilePath
practicedDb = "data/practiced.json"

pick :: [a] -> IO (Maybe a)
pick [] = return Nothing
pick ks = do
    i <- Random.randomRIO (0, length ks - 1)
    return $ Just $ ks !! i

data Practiced = Practiced {
    name :: Text
    , date :: Time.UTCTime
    , bpm :: Maybe BPM
    } deriving (Eq, Show, Generics.Generic)
instance Aeson.ToJSON Practiced
instance Aeson.FromJSON Practiced

savePracticed :: [Practiced] -> IO ()
savePracticed = Text.IO.appendFile practicedDb . Text.unlines
    . map (Texts.toText . Aeson.encode) . Lists.sortOn date

loadPracticed :: IO [Practiced]
loadPracticed = do
    lines <- Text.lines <$> Text.IO.readFile practicedDb
    case sequence (map (Aeson.eitherDecode . Texts.toLazyByteString) lines) of
        Left err -> errorIO $ txt err
        Right ps -> return ps

-- | Display practice record.
display :: IO ()
display = do
    tz <- Time.getCurrentTimeZone
    by_date <- Lists.keyedGroupSort (localDay tz . date) <$> loadPracticed
    mapM_ (Text.IO.putStr . pretty) by_date
    where
    pretty (day, ps) = Text.unlines $ (prettyDay day <> ":")
        : map (("  "<>) . prettyP) ps
    prettyP p = name p <> maybe "" (("("<>) . (<>")") . showt) (bpm p)
    prettyDay day = showt day <> " " <> showt (Time.dayOfWeek day)

localDay :: Time.TimeZone -> Time.UTCTime -> Time.Day
localDay tz = Time.localDay . Time.utcToLocalTime tz
