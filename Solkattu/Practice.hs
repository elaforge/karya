-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveGeneric #-}
-- | Utilities to randomly select things to practice, and save what
-- I practiced, for a flashcard-esque system.
module Solkattu.Practice where
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Text.IO as Text.IO
import qualified Data.Time as Time

import qualified GHC.Generics as Generics
import qualified System.Random as Random

import qualified Util.File as File
import qualified Solkattu.Db as Db
import qualified Solkattu.Format.Format as Format
import qualified Solkattu.Format.Terminal as Terminal
import qualified Solkattu.Korvai as Korvai

import           Global


-- | The number of date groups starting from the most recent.
recentDates :: Int -> IO ()
recentDates n =
    Text.IO.putStrLn $ Db.formats $ Db.searchAll (Db.recentDates n) []

types :: [Text]
types = ["exercise", "korvai"]

-- | Pick a random korvai with any of the given types.
randomTypes :: [Text] -> IO ()
randomTypes types = do
    forM_ types $ \typ -> do
        korvai <- pick $ filter (Db.ofType typ . snd) Db.korvais
        Text.IO.putStrLn $ typ <> ":"
        Text.IO.putStrLn $ maybe "Nothing" Db.format korvai

realize, realizep :: Int -> IO ()
realize i = realizeM mempty (get i)
realizep i = realizeM Format.defaultAbstraction (get i)

realizeM :: Format.Abstraction -> Korvai.Korvai -> IO ()
realizeM = Terminal.printInstrument Korvai.mridangam

realizeKon :: Int -> IO ()
realizeKon i = Terminal.printKonnakol Terminal.konnakolConfig (get i)

-- | Mark these korvais as practiced.
practiced :: Int -> Maybe BPM -> IO ()
practiced index bpm = do
    now <- Time.getCurrentTime
    let korvaiName = txt $ Db.korvaiFname $ snd $ Db.korvais !! index
    pmap <- either (errorIO . txt) return =<< loadPracticed
    let practiced = Practiced now bpm
    savePracticed $ Map.alter (Just . maybe [practiced] (practiced:))
        korvaiName pmap

type PracticedMap = Map Text [Practiced]
data Practiced = Practiced {
    _date :: Time.UTCTime
    , _bpm :: Maybe BPM
    } deriving (Eq, Show, Generics.Generic)

type BPM = Int

instance Aeson.ToJSON Practiced
instance Aeson.FromJSON Practiced

get :: Int -> Korvai.Korvai
get = snd . (Db.korvais !!)

practicedDb :: FilePath
practicedDb = "data/practiced"

savePracticed :: PracticedMap -> IO ()
savePracticed = Aeson.encodeFile practicedDb

loadPracticed :: IO (Either String PracticedMap)
loadPracticed = fmap (fromMaybe (Right Map.empty)) $ File.ignoreEnoent $
    Aeson.eitherDecodeFileStrict practicedDb

pick :: [a] -> IO (Maybe a)
pick [] = return Nothing
pick ks = do
    i <- Random.randomRIO (0, length ks - 1)
    return $ Just $ ks !! i
