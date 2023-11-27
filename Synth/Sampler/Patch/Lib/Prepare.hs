-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Ghci functions for creating sample sets.
module Synth.Sampler.Patch.Lib.Prepare where
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import           System.FilePath ((</>))
import qualified System.Posix.Files as Posix.Files

import qualified Text.Read as Read

import qualified Util.Files as Files
import qualified Util.Lists as Lists

import           Global


{-
    Procedure:
    - Rename samples and takes to short names.
    - Record room tone, apply ReaFir in subtract mode to get a denoise profile.
      TODO: are there better ways to subtract noise?  Audacity has one.
    - Reaper: dynamic split items (d), turn gate threshold down
      shift-up for vertical zoom on waveforms
    - Edit each sample to trim.  Move back attacks a bit since split tends to
      miss them.  Remove bad samples and update varsAt.
    - Fade out ends by select all, F2, set fade out to say 0.5s.
    - Select all, "SWS: create regions for sel items (name by active take)"
    - Verify numbers against permutations count.  Use binary search to find
      mismatches.
    - Select all, then render project regions to $baseDir/$inst/raw
    - Inspect renames, 'relink' renames, inspect output dirs.

    - Normalize?  % normalize --amplitude=-24dbFS *.wav
      pros:
        - Don't need to individually adjust curves for each dyn range.
      cons:
        - Normalize doesn't address perceptual loudness, so may be totally
          wrong (e.g. misled by sharp attack).
        - Natural level difference between keys is lost, becomes increased
          noise.

    ### static dynamics way

    Level adjustment:
    - First adjust variations:
      % run build/opt/sampler-im calibrate-var patch 0 16
      Use this to fill in dynamicTweaks to make variations have consistent dB.
    - Then adjust increasing dyn for a smooth curve:
      % run build/opt/sampler-im calibrate-by patch Dyn +attr sym-pitches+
      Use this to adjust (low, high) dB for dynamic ranges.

    slenthem 21 v4 mf same as ff?
    To fix this, I should calibrate v3 to v4.

    It would be easier to do this with a GUI where I can get immediate
    feedback.  But Reaper UI is clunky because I don't know how to reorder
    the samples.

    ### variable dynamics way

    Having distinct dynamic levels like PP MP MF FF leads to timbre
    discontinuity.  Practically most usage will be MF.  Also unless there are
    variable attack artifacts, variations at the same dynamic are not as useful
    as extra dynamics.

    1. Record many samples of gradually increasing volume.
    2. Intentionally record more at typical levels in the MP MF range.
    3. Tag each with dynamic center.
    4. The chance of selecting each sample is a bounded normal distribution
       from its dynamic.  So for a certain dyn, I get [(fname, weight)] and use
       variation to pick.

    The problem is 3.  How can I consistently select dynamic?
    It should be easier when they are in dyn order.

    I can't use 'relink' for this, I have to export the samples with names
    and generate a declaration for them.  Drum.makeFileList does this.

    Drum.variableDynamic assumes an even spread of dynamics.
-}


baseDir :: FilePath
baseDir = "/Users/elaforge/Music/mix/sample"

-- * check

-- | Show the index numbers for samples.  I use this to check for inaccuracies
-- in the final sample count.  Show two levels of hierarchy, compromise between
-- clutter and detail.
printIndices :: Int -> [FilePath] -> IO ()
printIndices level fnames = do
    mapM_ Text.IO.putStrLn . concatMap fmt1 . group level
        . zip [1 :: Int ..] $ fnames
    print (length fnames)
    where
    fmt1 [] = [] -- unreached
    fmt1 fnames@((n, fname) : _) = fmt n fname
        : map (("    "<>) . uncurry fmt . head)
            (drop 1 (group (level+1) fnames))
    fmt n fname = Text.justifyRight 3 ' ' (showt n) <> " " <> txt fname
    group level = Lists.groupAdjacent (key level . snd)
    key n = take n . Lists.splitBefore (\c -> c == '/' || c == '-')

-- * filesystem

-- | Do the given renames with hard links into a different directory, so it's
-- non-destructive.
relink :: FilePath -> FilePath -> FilePath -> [(FilePath, FilePath)] -> IO ()
relink baseDir fromDir toDir = mapM_ link
    where
    link (old, new) = Posix.Files.createLink (baseDir </> fromDir </> old)
        (baseDir </> toDir </> new)

renameInPlace :: FilePath -> [(FilePath, FilePath)] -> IO ()
renameInPlace dir = mapM_ move
    where
    move (old, new) = Directory.renameFile (dir </> old) (dir </> new)

-- | Generate renames list, matching names against dir contents.
renames :: FilePath -> [FilePath] -> IO [(FilePath, FilePath)]
renames dir groups = do
    samples <- listSampleDir dir
    unless (length groups == length samples) $
        putStrLn $ "expected " <> show (length groups)
            <> " samples, but found " <> show (length samples)
    return $ zip (map FilePath.takeFileName samples) groups

listSampleDir :: FilePath -> IO [FilePath]
listSampleDir dir =
    map snd . Lists.sortOn fst . Lists.keyOnJust sampleNumber
        . filter (".wav" `List.isSuffixOf`) <$> Files.list dir

sampleNumber :: FilePath -> Maybe Int
sampleNumber =
    Read.readMaybe . Lists.takeWhileEnd (/='-') . FilePath.dropExtension
