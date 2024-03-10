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
    , e_metas :: [T.Meta]
    , e_block :: Check.Block
    } deriving (Show, Eq)

type Tags = Map Tag Text
type Tag = Text

meta_to_tag :: T.Meta -> (Tag, Text)
meta_to_tag = \case
    T.Source a -> ("source", a)
    T.Piece a -> ("piece", a)
    T.Section a -> ("section", a)
    T.Laras a -> ("laras", JScore.laras_enum a)
    T.Irama a -> ("irama", JScore.irama_enum a)
    T.Instrument a -> ("instrument", JScore.instrument_enum a)

format_entry :: Entry -> Text
format_entry (Entry { e_tags, e_metas, e_block }) =
    Text.unlines $ format_tags e_tags : JScore.format_block_ irama inst e_block
    where
    irama = Lists.head [i | T.Irama i <- e_metas]
    inst = Lists.head [i | T.Instrument i <- e_metas]

t2 = map e_tags <$>
    either (error . untxt) id <$> load "Example/tscore/java/balungan32.tscore"

parse_tags :: Text -> Tags
parse_tags = Map.fromList . map split . Text.words
    where
    split w = case Text.splitOn "=" w of
        [k, v] -> (k, v)
        _ -> error $ "expected k=v: " <> untxt w

format_tags :: Tags -> Text
format_tags = Text.unwords . map format . Map.toList
    where format (k, v) = k <> "=" <> v

search :: Text -> IO ()
search tags = do
    -- entries <- t2
    entries <- load_db
    mapM_ (Text.IO.putStrLn . format_entry) $ find (parse_tags tags) entries

find :: Tags -> [Entry] -> [Entry]
find tags = filter (\entry -> Map.isSubmapOf tags (e_tags entry))

load_db :: IO [Entry]
load_db = do
    (errors, entries) <- load_dir "Example/tscore/java"
    unless (null errors) $
        error $ "errors: " <> unlines (map untxt errors)
    pure entries

type Score = T.Score Check.Block

load :: FilePath -> IO (Either Error [Entry])
load fname = parse <$> Text.IO.readFile fname

load_dir :: FilePath -> IO ([Error], [Entry])
load_dir dir = do
    fnames <- filter (".tscore" `List.isSuffixOf`) <$> Files.list dir
    entries <- mapM load fnames
    pure $ second concat $ Either.partitionEithers entries

parse :: Text -> Either Error [Entry]
parse source = do
    score <- JScore.parse_score source
    let (checked, errs) = Logger.runId $ Check.format_score score
    unless (null errs) $
        Left $ Text.unlines $ map (T.show_error source) errs
    pure $ score_entries checked

score_entries :: Score -> [Entry]
score_entries (T.Score toplevels) = go [] (map snd toplevels)
    where
    go metas (T.ToplevelMeta meta : toplevels) = go (meta:metas) toplevels
    go metas (T.BlockDefinition block : toplevels)
        | null (T.block_tracks block) = go metas toplevels
        | otherwise = entry metas block : go metas toplevels
    go _ [] = []
    entry e_metas e_block = Entry
        { e_tags = make_tags fname e_metas e_block
        , e_metas, e_block
        }

make_tags :: [T.Meta] -> Check.Block -> Tags
make_tags metas block =
    Map.fromListWith (\_ a -> a) $ concat
        [ [ ("seleh", Text.singleton (T.pc_char pc))
          | Just pc <- [T.seleh (T.block_gatra block)]
          ]
        , [ ("name", Text.unwords (T.block_names block))
          | not (null (T.block_names block))
          ]
        , map meta_to_tag metas
        ]
