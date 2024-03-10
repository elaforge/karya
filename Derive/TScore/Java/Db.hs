-- Copyright 2024 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
module Derive.TScore.Java.Db where
import qualified Data.Either as Either
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
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
    -- meta :: Meta
    tags :: Tags
    , metas :: [T.Meta]
    -- , tracks :: [[Check.Token]]
    , block :: Check.Block
    } deriving (Show, Eq)

type Tags = Map Tag Text
type Tag = Text

{-
-- Or just convert everything to [Text]?
-- I want to be able to search and show by these fields.
data Meta = Meta {
    source :: Maybe Text
    , piece :: Maybe Text
    , section :: Maybe Text
    , laras :: Maybe T.Laras
    , irama :: Maybe T.Irama
    , instrument :: Maybe T.Instrument
    , names :: [Text]
    , gatra :: Maybe (T.Gatra (T.Pitch T.Octave))
    } deriving (Show, Eq)

instance Semigroup Meta where
    Meta a1 a2 a3 a4 a5 a6 a7 a8 <> Meta b1 b2 b3 b4 b5 b6 b7 b8 =
        Meta (a1 <|> b1) (a2 <|> b2) (a3 <|> b3) (a4 <|> b4) (a5 <|> b5)
            (a6 <|> b6) (a7 ++ b7) (a8 <|> b8)
instance Monoid Meta where
    mempty = Meta Nothing Nothing Nothing Nothing Nothing Nothing [] Nothing

show_meta :: Meta -> Text
show_meta
    (Meta
        { source, piece, section, laras, irama, instrument, names, gatra }) =
    Text.unwords $ Maybe.catMaybes
        [ source, piece, section, showt <$> laras
        , showt <$> irama, showt <$> instrument
        , Just $ Text.intercalate "," names
        , JScore.format_gatra <$> gatra
        ]

to_meta :: T.Meta -> Meta
to_meta = \case
    T.Source a -> mempty { source = Just a }
    T.Piece a -> mempty { piece = Just a }
    T.Section a -> mempty { section = Just a }
    T.Laras a -> mempty { laras = Just a }
    T.Irama a -> mempty { irama = Just a }
    T.Instrument a -> mempty { instrument = Just a }

convert_metas :: [T.Meta] -> Meta
convert_metas = Foldable.foldMap to_meta
-}

meta_to_tag :: T.Meta -> (Tag, Text)
meta_to_tag = \case
    T.Source a -> ("source", a)
    T.Piece a -> ("piece", a)
    T.Section a -> ("section", a)
    T.Laras a -> ("laras", JScore.laras_enum a)
    T.Irama a -> ("irama", JScore.irama_enum a)
    T.Instrument a -> ("instrument", JScore.instrument_enum a)

format_entry :: Entry -> Text
format_entry (Entry { tags, metas, block }) =
    Text.unlines $ format_tags tags : JScore.format_block_ irama inst block
    where
    irama = Lists.head [i | T.Irama i <- metas]
    inst = Lists.head [i | T.Instrument i <- metas]

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

search :: Text -> IO ()
search tags = do
    -- entries <- t2
    entries <- load_db
    mapM_ (Text.IO.putStrLn . format_entry) $ find (parse_tags tags) entries

find :: Tags -> [Entry] -> [Entry]
find tags = filter (\entry -> Map.isSubmapOf tags entry.tags)

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
    entry metas block = Entry
        { tags = make_tags metas block
        , metas, block
        }
    -- entry metas block = Entry
    --     { meta = (convert_metas metas)
    --         { names = T.block_names block ++ T.block_inferred block
    --         , gatra = Just $ T.block_gatra block
    --         }
    --     }

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
