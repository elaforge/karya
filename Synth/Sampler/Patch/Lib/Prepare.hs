-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Ghci functions for creating sample sets.
module Synth.Sampler.Patch.Lib.Prepare where
import qualified Data.List as List
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import           System.FilePath ((</>))

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
        - Natural level difference between keys is lost, becomes increased
        noise.

    Level adjustment:
    - First adjust variations:
      % run build/opt/sampler-im calibrate-by Var patch +attr sym-pitches+
      Use this to tweak per-sample dB.
    - Then adjust increasing dyn for a smooth curve:
      % run build/opt/sampler-im calibrate-by Dyn patch +attr sym-pitches+
      Use this to adjust (low, high) dB for dynamic ranges.

    slenthem 21 v4 mf same as ff?
    To fix this, I should calibrate v3 to v4.

    It would be easier to do this with a GUI where I can get immediate
    feedback.  But Reaper UI is clunky because I don't know how to reorder
    the samples.
-}


baseDir :: FilePath
baseDir = "/Users/elaforge/Music/mix/sample"

slenthemRelink = relink (baseDir </> "java/slenthem") "raw" "samples"
    =<< renames (baseDir </> "java/slenthem/raw") slenthem

slenthem :: [FilePath]
slenthem = opens ++ mutes
    where
    opens =
        [ "open" </> join [p, dyn, var]
        | p <- pitches
        , dyn <- dynamics
        , var <- varsAt (p, dyn)
        ]
        where
        varsAt = vars . \case
            ("26", "pp") -> 3
            ("26", "mf") -> 5
            ("27", "ff") -> 3
            _ -> 4
    mutes =
        [ "mute" </> join [p, dyn, var]
        | p <- pitches
        , dyn <- dynamics
        , var <- varsAt (p, dyn)
        ]
        where
        varsAt = vars . \case
            ("25", "pp") -> 5
            _ -> 6
    pitches = ["2" <> show p | p <- [1..7]]

-- genderPanerus =
--     pitches = map (\(o, p) -> show o <> show p) $
--         takeWhile (<= (5, 3)) $ dropWhile (< (2, 6))
--             [(o, p) | o <- [2..5], p <- ps]
--         where ps = [1, 2, 3, 5, 6, 7]

dynamics :: [String]
dynamics = ["pp", "mp", "mf", "ff"]

vars :: Int -> [String]
vars n = map (('v':) . show) [1..n]

join :: [String] -> String
join = (<> ".wav") . Lists.join "-"

-- * check

printNumbers :: Int -> [FilePath] -> IO ()
printNumbers level fnames = do
    mapM_ (putStrLn . fmt . head) . Lists.groupAdjacent (key . snd)
        . zip [1..] $ fnames
    print (length fnames)
    where
    fmt (n :: Int, fname) = show n <> " - " <> fname
    key = take level . Lists.splitBefore (\c -> c == '/' || c == '-')

-- * filesystem

-- | Do the given renames with symlinks links into a different directory, so
-- it's non-destructive.
relink :: FilePath -> FilePath -> FilePath -> [(FilePath, FilePath)] -> IO ()
relink baseDir fromDir toDir = mapM_ link
    where
    link (old, new) = Directory.createFileLink (baseDir </> fromDir </> old)
        (baseDir </> toDir </> new)

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
