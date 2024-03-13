-- Copyright 2024 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE StrictData #-}
module Derive.TScore.Java.Db where
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified System.FilePath as FilePath

import qualified Util.Files as Files
import qualified Util.Lists as Lists
import qualified Util.Logger as Logger

import qualified Derive.TScore.Java.Check as Check
import qualified Derive.TScore.Java.JScore as JScore
import qualified Derive.TScore.Java.T as T

import           Global


type Error = Text

data Entry = Entry {
    e_tags :: Tags
    , e_meta :: Maybe Meta
    , e_block :: Check.Block
    } deriving (Show, Eq)

type Tags = Map Tag Text
type Tag = Text

data Meta = Meta {
    m_laras :: T.Laras
    , m_irama :: T.Irama
    , m_instrument :: T.Instrument
    } deriving (Show, Eq)

make_meta :: [T.Meta] -> Maybe Meta
make_meta metas = Meta
    <$> Lists.head [a | T.Laras a <- metas]
    <*> Lists.head [a | T.Irama a <- metas]
    <*> Lists.head [a | T.Instrument a <- metas]

format_entry :: Entry -> Text
format_entry (Entry { e_tags, e_meta, e_block }) =
    Text.unlines $ format_tags e_tags
        : JScore.format_block_ (m_irama <$> e_meta) (m_instrument <$> e_meta)
            e_block

t2 = either (error . untxt) id <$> load "Example/tscore/java/balungan32.tscore"

parse_tags :: Text -> Tags
parse_tags = Map.fromList . map split . Text.words
    where
    split w = case Text.splitOn "=" w of
        [k, v] -> (k, v)
        _ -> error $ "expected k=v: " <> untxt w

format_tags :: Tags -> Text
format_tags = Text.unwords . map format . Map.toList
    where format (k, v) = k <> "=" <> v

search :: T.Laras -> Text -> IO ()
search laras tags = do
    -- entries <- t2
    entries <- load_db
    mapM_ (Text.IO.putStrLn . format_entry) $
        mapMaybe (entry_matches laras (parse_tags tags)) entries

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
    score <- JScore.parse_score source
    let (checked, errs) = Logger.runId $ Check.format_score score
    unless (null errs) $
        Left $ Text.unlines $ map (T.show_error source) errs
    pure $ score_entries fname checked

score_entries :: FilePath -> Score -> [Entry]
score_entries fname (T.Score toplevels) = go [] (map snd toplevels)
    where
    go metas (T.ToplevelMeta meta : toplevels) = go (meta:metas) toplevels
    go metas (T.BlockDefinition block : toplevels)
        | null (T.block_tracks block) = go metas toplevels
        | otherwise = entry metas block : go metas toplevels
    go _ [] = []
    entry e_metas e_block = Entry
        { e_tags = make_tags fname e_metas e_block
        , e_meta
        , e_block
        }
        where e_meta = make_meta e_metas

make_tags :: FilePath -> [T.Meta] -> Check.Block -> Tags
make_tags fname metas block = Map.unions
    [ block_tags block
    , Map.fromListWith (\_ a -> a) (map meta_to_tag metas)
    , Map.fromList [("file", txt (FilePath.takeFileName fname))]
    ]

meta_tags :: Meta -> Tags
meta_tags (Meta { m_laras, m_irama, m_instrument }) = Map.fromList
    [ ("laras", JScore.laras_enum m_laras)
    , ("irama", JScore.irama_enum m_irama)
    , ("instrument", JScore.instrument_enum m_instrument)
    ]

meta_to_tag :: T.Meta -> (Tag, Text)
meta_to_tag = \case
    T.Source a -> ("source", a)
    T.Piece a -> ("piece", a)
    T.Section a -> ("section", a)
    T.Laras a -> ("laras-orig", JScore.laras_enum a)
    T.Irama a -> ("irama", JScore.irama_enum a)
    T.Instrument a -> ("instrument", JScore.instrument_enum a)

block_tags :: Check.Block -> Tags
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
    if laras == m_laras meta
        then pure entry
        else do
            trans <- JScore.convert_laras (m_laras meta) laras
            pure $ update_tags $ entry
                { e_block = JScore.transform_block trans (e_block entry)
                , e_meta = Just $ meta { m_laras = laras }

                }

update_tags :: Entry -> Entry
update_tags entry = entry
    { e_tags = maybe mempty meta_tags (e_meta entry)
        <> block_tags (e_block entry)
        <> e_tags entry
    }
