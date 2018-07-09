module Solkattu.Practice where
import qualified Data.Text.IO as Text.IO
import qualified System.Random as Random

import qualified Solkattu.Db as Db
import qualified Solkattu.Tags as Tags


practice :: IO ()
practice = do
    let korvais = zip [0..] Db.korvais
    exercise <- pick $ filter (Db.ofType Tags.exercise . snd) korvais
    korvai <- pick $ filter (Db.ofType "korvai". snd) korvais
    Text.IO.putStrLn $ Db.format exercise
    Text.IO.putStrLn $ Db.format korvai

-- TODO also practice most recent lessons

-- mark these korvais as practiced
-- practiced :: [Int] -> IO ()
-- practiced

pick :: [a] -> IO a
pick [] = error "can't pick from []"
pick ks = do
    i <- Random.randomRIO (0, length ks - 1)
    return $ ks !! i
