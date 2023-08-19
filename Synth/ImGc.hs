-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Delete audio checkpoints with no symlinks, and those over a certain age.
    This means they correspond to some older score state, so I can delete them
    and loading a new score will still likely hit the cache.

    If I update mtimes on cache hits, the minimum age should keep alive the
    previous few generations, so undo continues to hit them.

    A more sophisticated approach would be to keep an explicit generation
    count, so I could delete by generation, but this way is simpler and
    rerendering isn't a big deal.
-}
module Synth.ImGc (
    Stats(..)
    , showStats
    , gc
    , find
) where
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Time as Time

import qualified Streaming.Prelude as S
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import           System.FilePath ((</>))

import qualified Util.Files as Files
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty

import           Global


data Stats = Stats {
    _instruments :: !(Set Text)
    , _deletedFiles :: !Int
    , _deletedBytes :: !Bytes
    , _remaining :: !Bytes
    } deriving (Show)

type Bytes = Int

instance Semigroup Stats where
    Stats a1 a2 a3 a4 <> Stats b1 b2 b3 b4 =
        Stats (a1<>b1) (a2+b2) (a3+b3) (a4+b4)
instance Monoid Stats where
    mappend = (<>)
    mempty = Stats mempty 0 0 0

showStats :: Stats -> Text
showStats (Stats instruments deletedFiles deletedBytes remaining) =
    Text.intercalate ", "
        [ "GC freed " <> Pretty.bytes 2 deletedBytes
            <> " (" <> showt deletedFiles <> " files)"
        , "remaining " <> Pretty.bytes 2 remaining
        , showt (Set.size instruments) <> " instruments"
        ]

-- | Files younger than this always live.
minimumAge :: Time.NominalDiffTime
minimumAge = 10 * min
    where min = 60

gc :: FilePath -> IO Stats
gc root = do
    now <- Time.getCurrentTime
    foldMapM (check now) $ Files.walk (const True) root
    where
    check now (dir, fnames)
        | FilePath.takeFileName dir /= "checkpoint" = return mempty
        | otherwise = do
            garbage <- Set.fromList <$> findGarbage now dir fnames
            deleted <- fmap Num.sum $
                mapM Directory.getFileSize (Set.toList garbage)
            remaining <- fmap Num.sum $ mapM Directory.getFileSize $
                filter (`Set.notMember` garbage) $ map (dir</>) fnames
            mapM_ Directory.removeFile garbage
            return $ Stats
                { _instruments = Set.singleton $ txt $
                    FilePath.takeFileName $ FilePath.takeDirectory dir
                , _deletedFiles = length garbage
                , _deletedBytes = fromIntegral deleted
                , _remaining = fromIntegral remaining
                }

find :: FilePath -> S.Stream (S.Of (Set FilePath)) IO ()
find root = do
    now <- liftIO Time.getCurrentTime
    S.filter (/=mempty) $ S.mapM (check now) $ Files.walk (const True) root
    where
    check now (dir, fnames)
        | FilePath.takeFileName dir /= "checkpoint" = return mempty
        | otherwise = Set.fromList <$> liftIO (findGarbage now dir fnames)

-- | Any file with a ###.wav symlink is alive, or with the same prefix.
-- So 000.wav -> checkpoint/000.hash.hash.wav, and also
-- 000.hash.hash.state.hash and 000.hash.hash.wav.peaks.
findGarbage :: Time.UTCTime -> FilePath -> [FilePath] -> IO [FilePath]
findGarbage now checkpoint fnames = do
    chunks <- filterM Directory.pathIsSymbolicLink
        =<< Files.list (FilePath.takeDirectory checkpoint)
    alive <- mapM Directory.getSymbolicLinkTarget chunks
    (young, fnames) <- partitionM (isYoung now . (checkpoint</>)) fnames
    -- ["000.FOBBmx5XVFmFDBvy6FaGtw.Pygpsv_oQ01n-YoqmJaGUg", ...]
    let prefixes = map (FilePath.dropExtension . FilePath.takeFileName)
            (alive ++ young)
    let isAlive fn = any (`List.isPrefixOf` fn) prefixes
    return $ map (checkpoint</>) $ filter (not . isAlive) fnames

isYoung :: Time.UTCTime -> FilePath -> IO Bool
isYoung now fname = (<= minimumAge) . (now `Time.diffUTCTime`) <$>
    Directory.getModificationTime fname

foldMapM :: (Monad m, Monoid w) => (a -> m w) -> S.Stream (S.Of a) m r -> m w
foldMapM f = S.foldM_ (\ !accum a -> (accum<>) <$> f a) (return mempty) return
