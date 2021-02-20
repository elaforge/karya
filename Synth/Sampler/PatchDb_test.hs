-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Sampler.PatchDb_test where
import qualified Data.Algorithm.Diff as Diff
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified System.Directory as Directory
import           System.FilePath ((</>))

import qualified Util.File as File
import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.PatchDb as PatchDb
import qualified Synth.Shared.Note as Note

import           Global
import           Util.Test


diff_contents :: Bool
diff_contents = False

test_samples_exist :: Test
test_samples_exist = do
    let root = Patch._rootDir PatchDb.db
    -- Avoid spamming a zillion errors if we just don't have samples here.
    -- CI doesn't check have the samples, so it doesn't check this.
    -- TODO: omit this specific test for CI
    ifM (not <$> Directory.doesDirectoryExist root)
        (putStrLn $ "=== no sample directory: " <> root) $ do
    forM_ allFilenames $ \(name, dir, fnames) -> unless (exclude name) $ do
        pprint (name, dir, length fnames)
        not_equal (length fnames) 0
        forM_ fnames $ \fname ->
            unlessM (Directory.doesFileExist (root </> fname)) $
                failure $ name <> ": " <> txt fname
        when diff_contents $
            diff_dir_contents name dir fnames
    where
    -- This one lets you directly pick samples at "runtime".
    exclude = (=="sample")

-- | If the sample coverage doesn't match, it's easier to debug with a diff.
diff_dir_contents :: Text -> FilePath -> [FilePath] -> IO ()
diff_dir_contents name dir fnames = do
    exist <- filter isSample . map (drop (length root + 1)) <$>
        File.listRecursive (const True) (root </> dir)
    let (extra, notFound) = diff (List.sort exist) fnames
    unless (null notFound) $
        failure $ name <> " references samples that don't exist:\n"
            <> txt (unlines notFound)
    unless (null extra) $
        failure $ name <> " has samples that it doesn't reference:\n"
            <> txt (unlines extra)
    where
    root = Patch._rootDir PatchDb.db
    isSample fname = any (`List.isSuffixOf` fname) [".flac", ".wav"]

diff :: Eq a => [a] -> [a] -> ([a], [a])
diff xs ys = ([x | Diff.First x <- diffs], [x | Diff.Second x <- diffs])
    where diffs = Diff.getDiff xs ys

allFilenames :: [(Note.PatchName, FilePath, [FilePath])]
allFilenames = mapMaybe extract $ Map.toAscList $ Patch._patches PatchDb.db
    where
    extract (name, Patch.DbPatch patch) = Just
        ( name
        , Patch._dir patch
        , map (Patch._dir patch </>) (Set.toList (Patch._allFilenames patch))
        )
    extract (_, Patch.DbDummy _) = Nothing
