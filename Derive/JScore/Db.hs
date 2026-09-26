-- Copyright 2024 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE StrictData #-}
-- | Treat Java tscores as a DB of patterns.
module Derive.JScore.Db where
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified System.FilePath as FilePath

import qualified Util.Files as Files
import qualified Util.Lists as Lists
import qualified Util.Logger as Logger
import qualified Util.Num as Num
import qualified Util.Thread as Thread

import qualified Derive.JScore.Check as Check
import qualified Derive.JScore.Format as Format
import qualified Derive.JScore.JScore as JScore
import qualified Derive.JScore.Parse as Parse
import qualified Derive.JScore.T as T

import           Global


type Error = Text

data Entry = Entry
    { eTags :: Tags
    , eMeta :: Maybe Check.Meta
    , eBlock :: Block (Token ())
    } deriving (Show, Eq)

type Block token = T.Block T.Pitch [[token]]
type Token pos = T.Token pos (T.Note T.Pitch ()) T.Rest

type Tags = Map Tag Text
type Tag = Text

printEntries :: [Entry] -> IO ()
printEntries = mapM_ (Text.IO.putStrLn . fmt) . zip [0 :: Int ..]
    where fmt (i, e) = showt i <> ": " <> formatEntry e

formatEntry :: Entry -> Text
formatEntry (Entry { eTags, eMeta, eBlock }) =
    Text.unlines $ formatTags eTags
        : Format.formatTitle eBlock
        : formatBlock fmtPos eMeta eBlock
    where fmtPos = const id

formatBlock :: (pos -> Text -> Text) -> Maybe Check.Meta -> Block (Token pos)
    -> [Text]
formatBlock fmtPos meta block =
    Format.formatBlock fmtPos
        (Check.mIrama <$> meta) (Check.mInstrument <$> meta)
        block

formatGroups :: [Entry] -> [Text]
formatGroups = map formatGroup . Lists.groupSort (T.blockTracks . eBlock)

formatGroup :: [Entry] -> Text
formatGroup entries@(entry0 : _) = Text.unlines $
    formatTags common
    : Text.intercalate " ; " (map (Format.formatTitle . eBlock) entries)
    : formatBlock (const id) (eMeta entry0) (eBlock entry0)
    where
    -- TODO I could show the unique ones, but it takes space
    (common, _tags) = extractCommon $ map eTags entries
formatGroup [] = "<empty>\n"

parseTags :: Text -> Tags
parseTags = Map.fromList . map split . Text.words
    where
    split w = case Text.splitOn "=" w of
        [k, v] -> (k, v)
        _ -> error $ "expected k=v: " <> untxt w

formatTags :: Tags -> Text
formatTags = Text.unwords . map format . Map.toList
    where format (k, v) = k <> "=" <> v

_seleh1 = search T.PelogLima
    "instrument=gender-panerus irama=dadi seleh=1"

search :: T.Laras -> Text -> IO ()
search laras tags = do
    -- entries <- _load1
    (entries, taken) <- Thread.timeAction loadDb
    Text.IO.putStrLn $ "load took " <> Thread.showMetric taken
    mapM_ Text.IO.putStrLn $ formatGroups $
        mapMaybe (entryMatches laras (parseTags tags)) entries

_load1 = either (error . untxt) id <$>
    load "Example/tscore/java/balungan32.tscore"

entryMatches :: T.Laras -> Tags -> Entry -> Maybe Entry
entryMatches laras tags entry
    | Just entry <- convertLaras laras entry, match (eTags entry) =
        Just entry
    | otherwise = Nothing
    where match = (tags `Map.isSubmapOf`)

loadDb :: IO [Entry]
loadDb = do
    (errors, entries) <- loadDir "Example/tscore/java"
    unless (null errors) $
        error $ "errors: " <> unlines (map untxt errors)
    pure entries

type Score = T.Score Check.Block

loadDir :: FilePath -> IO ([Error], [Entry])
loadDir dir = do
    fnames <- filter (".tscore" `List.isSuffixOf`) <$> Files.list dir
    entries <- mapM load fnames
    pure $ second concat $ Either.partitionEithers entries

load :: FilePath -> IO (Either Error [Entry])
load fname = parse fname <$> Text.IO.readFile fname

parse :: FilePath -> Text -> Either Error [Entry]
parse fname source = first ((txt fname <> ": ") <>) $ do
    score <- Parse.parseScore source
    let (checked, errs) = Logger.runId $ Check.formatScore score
    unless (null errs) $
        Left $ Text.unlines $ map (T.showError source) errs
    pure $ scoreEntries fname source checked

scoreEntries :: FilePath -> Text -> Score -> [Entry]
scoreEntries fname source (T.Score toplevels) = go [] (map snd toplevels)
    where
    go metas (T.ToplevelMeta meta : toplevels) = go (meta:metas) toplevels
    go metas (T.BlockDefinition block : toplevels)
        | null (T.blockTracks block) = go metas toplevels
        | otherwise = entry metas block : go metas toplevels
    go _ [] = []
    entry eMetas eBlock = Entry
        { eTags = makeTags (fname <> ":" <> show line) eMetas eBlock
        , eMeta
        -- Strip out T.Pos, I no longer need the original position and it'll
        -- complicate comparing tracks.
        , eBlock = map (map (T.setTokenPos ())) <$> eBlock
        }
        where
        eMeta = Check.makeMeta eMetas
        Just (line, _, _) = T.findPos source  (T.blockPos eBlock)

makeTags :: FilePath -> [T.Meta] -> Check.Block -> Tags
makeTags fname metas block = Map.unions
    [ blockTags block
    , Map.fromListWith (\_ a -> a) (map metaToTag metas)
    , Map.fromList [("file", txt (FilePath.takeFileName fname))]
    ]

metaTags :: Check.Meta -> Tags
metaTags (Check.Meta { mLaras, mIrama, mInstrument }) = Map.fromList
    [ ("laras", Parse.larasEnum mLaras)
    , ("irama", Parse.iramaEnum mIrama)
    , ("instrument", Parse.instrumentEnum mInstrument)
    ]

metaToTag :: T.Meta -> (Tag, Text)
metaToTag = \case
    T.Source a -> ("source", a)
    T.Piece a -> ("piece", a)
    T.Section a -> ("section", a)
    T.Laras a -> ("laras-orig", Parse.larasEnum a)
    T.Irama a -> ("irama", Parse.iramaEnum a)
    T.Instrument a -> ("instrument", Parse.instrumentEnum a)

blockTags :: Block pos -> Tags
blockTags block = Map.fromList $ concat
    [ [ ("seleh", Text.singleton (T.pcChar pc))
      | Just pc <- [T.seleh (T.blockGatra block)]
      ]
    -- TODO it should be multiple name= tags?
    , [ ("name", Text.unwords (T.blockNames block))
      | not (null (T.blockNames block))
      ]
    ]

-- | Try to convert an Entry to a different T.Laras.
convertLaras :: T.Laras -> Entry -> Maybe Entry
convertLaras laras entry = do
    meta <- eMeta entry
    if laras == Check.mLaras meta
        then pure entry
        else do
            trans <- Check.convertLaras (Check.mLaras meta) laras
            pure $ updateTags $ entry
                { eBlock = Format.transformBlock trans (eBlock entry)
                , eMeta = Just $ meta { Check.mLaras = laras }
                }

updateTags :: Entry -> Entry
updateTags entry = entry
    { eTags = maybe mempty metaTags (eMeta entry)
        <> blockTags (eBlock entry)
        <> eTags entry
    }

-- * diff

-- d0 = do
--     es <- _load1
--     Text.IO.putStr $ diff_entries (es !! 54) (es !! 56)
-- d1 = do
--     es <- _load1
--     Text.IO.putStr $ diff_entries (es !! 53) (es !! 56)

d2 = do
    es <- _load1
    mapM_ Text.IO.putStr $ map snd $ takeWhile ((<=20) . fst) $
        findClosest es (es !! 54)

findClosest :: [Entry] -> Entry -> [(Int, Text)]
findClosest entries entry =
    map showDiff $ Lists.sortOn (fst . fst) $
        Lists.keyOnJust (diffEntries entry) entries
    where
    showDiff ((distance, (_tracks1, tracks2)), entry) =
        (distance,) $ Text.unlines
            [ showt distance <> ": " <> formatTags (eTags entry)
            , tracks2
            ]

diffEntries :: Entry -> Entry -> Maybe (Int, (Text, Text))
diffEntries e1 e2 = make <$> diffTracks (tracksOf e1) (tracksOf e2)
    where
    make (tracks1, tracks2) =
        ( distance tracks1
        , fmt tracks1 tracks2
        )
    fmt tracks1 tracks2 =
        ( Text.unlines $ formatBlock fmtPos (eMeta e1)
            ((eBlock e1) { T.blockTracks = tracks1 })
        , Text.unlines $ formatBlock fmtPos (eMeta e2)
            ((eBlock e2) { T.blockTracks = tracks2 })
        )
    tracksOf = T.blockTracks . eBlock
    distance = Num.sum
        . map (Num.sum . map (\t -> if T.tokenPos t then 0 else 1))
    fmtPos True t = t
    fmtPos False t = redColor <> t <> vt100Normal

redColor :: Text
redColor = "\ESC[31m" -- red

vt100Normal :: Text
vt100Normal = "\ESC[m\ESC[m"

diffTracks :: [[Token ()]] -> [[Token ()]]
    -> Maybe ([[Token Bool]], [[Token Bool]])
diffTracks tracks1 tracks2
    | length tracks1 /= length tracks2 = Nothing
    | otherwise = unzip <$> mapM track (zip tracks1 tracks2)
    where
    track (track1, track2)
        | length track1 /= length track2 = Nothing
        | otherwise = Just $ unzip $ zipWith diffTokens track1 track2

diffTokens :: Token () -> Token () -> (Token Bool, Token Bool)
diffTokens t1 t2 = (set t1, set t2)
    where set = T.setTokenPos (t1 == t2)

-- * util

-- | Extract the identical (k, a) pairs from the maps.
extractCommon :: (Ord k, Ord a) => [Map k a] -> (Map k a, [Map k a])
extractCommon ms = (common, map (`Map.withoutKeys` keys) ms)
    where
    keys = Set.fromAscList $ map fst $ Set.toAscList pairs
    pairs = case map (Set.fromList . Map.toList) ms of
        m : ms -> foldl' Set.intersection m ms
        [] -> mempty
    common = Map.fromAscList $ Set.toAscList pairs

-- -- intersection where key and values are equal.
-- -- could do Set (Key, Val), then intersect
-- t0 = extract_common
--     [ Map.fromList [('a', 1), ('b', 2)]
--     , Map.fromList [('a', 1), ('b', 3), ('c', 3)]
--     ]
