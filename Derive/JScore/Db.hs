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
import qualified Derive.JScore.JScore as JScore
import qualified Derive.JScore.Parse as Parse
import qualified Derive.JScore.T as T

import           Global


type Error = Text

data Entry = Entry {
    e_tags :: Tags
    , e_meta :: Maybe JScore.Meta
    , e_block :: Block (Token ())
    } deriving (Show, Eq)

type Block token = T.Block T.Pitch [[token]]
type Token pos = T.Token pos (T.Note T.Pitch ()) T.Rest

type Tags = Map Tag Text
type Tag = Text

print_entries :: [Entry] -> IO ()
print_entries = mapM_ (Text.IO.putStrLn . fmt) . zip [0 :: Int ..]
    where fmt (i, e) = showt i <> ": " <> format_entry e

format_entry :: Entry -> Text
format_entry (Entry { e_tags, e_meta, e_block }) =
    Text.unlines $ format_tags e_tags
        : JScore.format_title e_block
        : format_block fmt_pos e_meta e_block
    where fmt_pos = const id

format_block :: (pos -> Text -> Text) -> Maybe JScore.Meta -> Block (Token pos)
    -> [Text]
format_block fmt_pos meta block =
    JScore.format_block fmt_pos
        (JScore.m_irama <$> meta) (JScore.m_instrument <$> meta)
        block

format_groups :: [Entry] -> [Text]
format_groups = map format_group . Lists.groupSort (T.block_tracks . e_block)

format_group :: [Entry] -> Text
format_group entries@(entry0 : _) = Text.unlines $
    format_tags common
    : Text.intercalate " ; " (map (JScore.format_title . e_block) entries)
    : format_block (const id) (e_meta entry0) (e_block entry0)
    where
    -- TODO I could show the unique ones, but it takes space
    (common, _tags) = extract_common $ map e_tags entries
format_group [] = "<empty>\n"

parse_tags :: Text -> Tags
parse_tags = Map.fromList . map split . Text.words
    where
    split w = case Text.splitOn "=" w of
        [k, v] -> (k, v)
        _ -> error $ "expected k=v: " <> untxt w

format_tags :: Tags -> Text
format_tags = Text.unwords . map format . Map.toList
    where format (k, v) = k <> "=" <> v

_seleh1 = search T.PelogLima
    "instrument=gender-panerus irama=dadi seleh=1"

search :: T.Laras -> Text -> IO ()
search laras tags = do
    -- entries <- _load1
    (entries, taken) <- Thread.timeAction load_db
    Text.IO.putStrLn $ "load took " <> Thread.showMetric taken
    mapM_ Text.IO.putStrLn $ format_groups $
        mapMaybe (entry_matches laras (parse_tags tags)) entries

_load1 = either (error . untxt) id <$>
    load "Example/tscore/java/balungan32.tscore"

entry_matches :: T.Laras -> Tags -> Entry -> Maybe Entry
entry_matches laras tags entry
    | Just entry <- convert_laras laras entry, match (e_tags entry) =
        Just entry
    | otherwise = Nothing
    where match = (tags `Map.isSubmapOf`)

load_db :: IO [Entry]
load_db = do
    (errors, entries) <- load_dir "Example/tscore/java"
    unless (null errors) $
        error $ "errors: " <> unlines (map untxt errors)
    pure entries

type Score = T.Score Check.Block

load_dir :: FilePath -> IO ([Error], [Entry])
load_dir dir = do
    fnames <- filter (".tscore" `List.isSuffixOf`) <$> Files.list dir
    entries <- mapM load fnames
    pure $ second concat $ Either.partitionEithers entries

load :: FilePath -> IO (Either Error [Entry])
load fname = parse fname <$> Text.IO.readFile fname

parse :: FilePath -> Text -> Either Error [Entry]
parse fname source = first ((txt fname <> ": ") <>) $ do
    score <- Parse.parse_score source
    let (checked, errs) = Logger.runId $ Check.format_score score
    unless (null errs) $
        Left $ Text.unlines $ map (T.show_error source) errs
    pure $ score_entries fname source checked

score_entries :: FilePath -> Text -> Score -> [Entry]
score_entries fname source (T.Score toplevels) = go [] (map snd toplevels)
    where
    go metas (T.ToplevelMeta meta : toplevels) = go (meta:metas) toplevels
    go metas (T.BlockDefinition block : toplevels)
        | null (T.block_tracks block) = go metas toplevels
        | otherwise = entry metas block : go metas toplevels
    go _ [] = []
    entry e_metas e_block = Entry
        { e_tags = make_tags (fname <> ":" <> show line) e_metas e_block
        , e_meta
        -- Strip out T.Pos, I no longer need the original position and it'll
        -- complicate comparing tracks.
        , e_block = map (map (T.set_token_pos ())) <$> e_block
        }
        where
        e_meta = JScore.make_meta e_metas
        Just (line, _, _) = T.find_pos source  (T.block_pos e_block)

make_tags :: FilePath -> [T.Meta] -> Check.Block -> Tags
make_tags fname metas block = Map.unions
    [ block_tags block
    , Map.fromListWith (\_ a -> a) (map meta_to_tag metas)
    , Map.fromList [("file", txt (FilePath.takeFileName fname))]
    ]

meta_tags :: JScore.Meta -> Tags
meta_tags (JScore.Meta { m_laras, m_irama, m_instrument }) = Map.fromList
    [ ("laras", Parse.laras_enum m_laras)
    , ("irama", Parse.irama_enum m_irama)
    , ("instrument", Parse.instrument_enum m_instrument)
    ]

meta_to_tag :: T.Meta -> (Tag, Text)
meta_to_tag = \case
    T.Source a -> ("source", a)
    T.Piece a -> ("piece", a)
    T.Section a -> ("section", a)
    T.Laras a -> ("laras-orig", Parse.laras_enum a)
    T.Irama a -> ("irama", Parse.irama_enum a)
    T.Instrument a -> ("instrument", Parse.instrument_enum a)

block_tags :: Block pos -> Tags
block_tags block = Map.fromList $ concat
    [ [ ("seleh", Text.singleton (T.pc_char pc))
      | Just pc <- [T.seleh (T.block_gatra block)]
      ]
    -- TODO it should be multiple name= tags?
    , [ ("name", Text.unwords (T.block_names block))
      | not (null (T.block_names block))
      ]
    ]

-- | Try to convert an Entry to a different T.Laras.
convert_laras :: T.Laras -> Entry -> Maybe Entry
convert_laras laras entry = do
    meta <- e_meta entry
    if laras == JScore.m_laras meta
        then pure entry
        else do
            trans <- JScore.convert_laras (JScore.m_laras meta) laras
            pure $ update_tags $ entry
                { e_block = JScore.transform_block trans (e_block entry)
                , e_meta = Just $ meta { JScore.m_laras = laras }
                }

update_tags :: Entry -> Entry
update_tags entry = entry
    { e_tags = maybe mempty meta_tags (e_meta entry)
        <> block_tags (e_block entry)
        <> e_tags entry
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
        find_closest es (es !! 54)

find_closest :: [Entry] -> Entry -> [(Int, Text)]
find_closest entries entry =
    map show_diff $ Lists.sortOn (fst . fst) $
        Lists.keyOnJust (diff_entries entry) entries
    where
    show_diff ((distance, (_tracks1, tracks2)), entry) =
        (distance,) $ Text.unlines
            [ showt distance <> ": " <> format_tags (e_tags entry)
            , tracks2
            ]

diff_entries :: Entry -> Entry -> Maybe (Int, (Text, Text))
diff_entries e1 e2 = make <$> diff_tracks (tracks_of e1) (tracks_of e2)
    where
    make (tracks1, tracks2) =
        ( distance tracks1
        , fmt tracks1 tracks2
        )
    fmt tracks1 tracks2 =
        ( Text.unlines $ format_block fmt_pos (e_meta e1)
            ((e_block e1) { T.block_tracks = tracks1 })
        , Text.unlines $ format_block fmt_pos (e_meta e2)
            ((e_block e2) { T.block_tracks = tracks2 })
        )
    tracks_of = T.block_tracks . e_block
    distance = Num.sum
        . map (Num.sum . map (\t -> if T.token_pos t then 0 else 1))
    fmt_pos True t = t
    fmt_pos False t = redColor <> t <> vt100Normal

redColor :: Text
redColor = "\ESC[31m" -- red

vt100Normal :: Text
vt100Normal = "\ESC[m\ESC[m"

diff_tracks :: [[Token ()]] -> [[Token ()]]
    -> Maybe ([[Token Bool]], [[Token Bool]])
diff_tracks tracks1 tracks2
    | length tracks1 /= length tracks2 = Nothing
    | otherwise = unzip <$> mapM track (zip tracks1 tracks2)
    where
    track (track1, track2)
        | length track1 /= length track2 = Nothing
        | otherwise = Just $ unzip $ zipWith diff_tokens track1 track2

diff_tokens :: Token () -> Token () -> (Token Bool, Token Bool)
diff_tokens t1 t2 = (set t1, set t2)
    where set = T.set_token_pos (t1 == t2)

-- * util

-- | Extract the identical (k, a) pairs from the maps.
extract_common :: (Ord k, Ord a) => [Map k a] -> (Map k a, [Map k a])
extract_common ms = (common, map (`Map.withoutKeys` keys) ms)
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
