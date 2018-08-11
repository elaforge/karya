-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities to randomly select things to practice, and save what
-- I practiced, for a flashcard-esque system.
module Solkattu.Practice where
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Time as Time

import qualified System.Random as Random

import qualified Solkattu.Db as Db
import qualified Solkattu.Format.Terminal as Terminal
import Solkattu.Format.Format (Abstraction(..))
import qualified Solkattu.Korvai as Korvai

import Global


-- | The number of date groups starting from the most recent.
recentDates :: Int -> IO ()
recentDates n = Text.IO.putStrLn $ Db.search (Db.recentDates n) (const True)

types :: [Text]
types = ["exercise", "korvai"]

randomTypes :: [Text] -> IO ()
randomTypes types = do
    let korvais = zip [0..] Db.korvais
    forM_ types $ \typ -> do
        korvai <- pick $ filter (Db.ofType typ . snd) korvais
        Text.IO.putStrLn $ typ <> ":"
        Text.IO.putStrLn $ maybe "Nothing" Db.format korvai

realize, realizep :: Int -> IO ()
realize i = realizeM None (Db.korvais !! i)
realizep i = realizeM Patterns (Db.korvais !! i)

realizeM :: Abstraction -> Korvai.Korvai -> IO ()
realizeM = Terminal.printInstrument Korvai.mridangam

realizeKon :: Int -> IO ()
realizeKon i = Terminal.printKonnakol 100 Patterns (Db.korvais !! i)

-- | Mark these korvais as practiced.
practiced :: [Int] -> IO ()
practiced idxs = do
    now <- Time.getCurrentTime
    let korvais = map (txt . Db.korvaiFname . (Db.korvais !!)) idxs
    p <- either (errorIO . txt) return =<< loadPracticed
    savePracticed $ foldr (incrementPracticed now) p korvais

type Practiced = Map Text [Time.UTCTime]

incrementPracticed :: Time.UTCTime -> Text -> Practiced -> Practiced
incrementPracticed date = Map.alter (Just . maybe [date] (date:))

practicedDb :: FilePath
practicedDb = "../data/practiced"

savePracticed :: Practiced -> IO ()
savePracticed = Aeson.encodeFile practicedDb

loadPracticed :: IO (Either String Practiced)
loadPracticed = Aeson.eitherDecodeFileStrict practicedDb

parse :: Text -> Either Text Practiced
parse = fmap Map.fromList . mapM line . Text.lines
    where
    line = undefined
    -- line s = case Text.words s of
    --     [name, date] | Right n <- Parse.parse Parse.p_nat times ->
    --         Right (name, n)
    --     _ -> Left $ "can't parse line: " <> showt s

unparse :: Practiced -> Text
unparse = Text.unlines . map (\(name, times) -> name <> " " <> showt times)
    . Map.toList

dateFormat = Time.iso8601DateFormat (Just "%H:%M:%S")

pick :: [a] -> IO (Maybe a)
pick [] = return Nothing
pick ks = do
    i <- Random.randomRIO (0, length ks - 1)
    return $ Just $ ks !! i
