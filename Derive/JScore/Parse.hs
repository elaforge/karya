-- Copyright 2024 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Derive.JScore.Parse (
    Element
    , parse_score
    , parse_text
    , unparse
    , instrument_enum
    , irama_enum
    , laras_enum
) where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Lists as Lists
import qualified Util.P as P
import qualified Derive.JScore.T as T
import qualified Derive.TScore.Parse as Parse
import           Derive.TScore.Parse (Element, Parser)
import qualified Derive.TScore.T as TScore.T

import           Global


type Error = Text

parse_score :: Text -> Either Error T.ParsedScore
parse_score = second unwrap_score . parse_text

parse_text :: Element a => Text -> Either Error a
parse_text = first txt . Parse.parse_text (Parse.parse Parse.default_config)

unwrap_score :: T.ParsedScore -> T.ParsedScore
unwrap_score = fmap block
    where block b = b { T.block_tracks = unwrap_tracks <$> T.block_tracks b }

-- | We only have max 2 hands, so unwrap >2 into a single line.
unwrap_tracks :: T.Tracks -> T.Tracks
unwrap_tracks (T.Tracks tracks)
    | length tracks > 2 && even (length tracks) =
        T.Tracks [merge t1, merge t2]
    | otherwise = T.Tracks tracks
    where
    (t1, t2) = unzip (pairs tracks)
    pairs (x:y:zs) = (x, y) : pairs zs
    pairs _ = []
    merge (t:ts) = T.Track
        { track_pos = T.track_pos t
        -- Add a barline to unwrapped sections, because I don't end tracks
        -- with a final barline.  Or, I could add one only if there isn't
        -- already one.
        , track_tokens = List.intercalate [T.TBarline T.fake_pos] $
            map T.track_tokens (t:ts)
        }
    merge [] = error "broken invariant length tracks > 2"

unparse :: Parse.Element a => a -> Text
unparse = Parse.unparse Parse.default_config

instance Parse.Element T.ParsedScore where
    parse config = Parse.p_whitespace
        *> (T.Score <$> P.many (Parse.parse config))
    unparse config (T.Score toplevels) =
        Text.unlines $ map (Parse.unparse config) toplevels

instance Parse.Element (T.Toplevel T.ParsedBlock) where
    parse config = Parse.lexeme $
        T.ToplevelMeta <$> Parse.parse config
        <|> T.BlockDefinition <$> Parse.parse config
    unparse config = \case
        T.ToplevelMeta a -> Parse.unparse config a
        T.BlockDefinition a -> Parse.unparse config a

instance Parse.Element T.Meta where
    parse _ = do
        TScore.T.Directive _pos key mb_val <- Parse.p_directive True
        val <- maybe (fail $ "directive with no val: " <> untxt key) return
            mb_val
        let unknown = fail $ "unknown " <> untxt key <> ": " <> untxt val
        case key of
            "source" -> pure $ T.Source val
            "piece" -> pure $ T.Piece val
            "section" -> pure $ T.Section val
            "laras" -> maybe unknown (return . T.Laras) $
                parse_enum laras_enum val
            "instrument" -> maybe unknown (return . T.Instrument) $
                parse_enum instrument_enum val
            "irama" -> maybe unknown (return . T.Irama) $
                parse_enum irama_enum val
            _ -> fail $ "unknown meta: " <> untxt key <> " = " <> untxt val
    unparse _ = \case
        T.Source a -> un "source" a
        T.Piece a -> un "piece" a
        T.Section a -> un "section" a
        T.Laras a -> un "laras" (laras_enum a)
        T.Irama a -> un "irama" (irama_enum a)
        T.Instrument a -> un "instrument" (instrument_enum a)
        where un key val = mconcat ["%", key, " = ", val]

parse_enum :: (Bounded a, Enum a) => (a -> Text) -> Text -> Maybe a
parse_enum unparse t = Map.lookup t m
    where m = Map.fromList $ Lists.keyOn unparse [minBound ..]

laras_enum :: T.Laras -> Text
laras_enum = \case
    T.PelogNem -> "pelog-nem"
    T.PelogLima -> "pelog-lima"
    T.PelogBarang -> "pelog-barang"
    T.SlendroNem -> "slendro-nem"
    T.SlendroSanga -> "slendro-sanga"
    T.SlendroManyura -> "slendro-manyura"

instrument_enum :: T.Instrument -> Text
instrument_enum = \case
    T.GenderBarung -> "gender-barung"
    T.GenderPanerus -> "gender-panerus"
    T.Siter -> "siter"

irama_enum :: T.Irama -> Text
irama_enum = \case
    T.Lancar -> "lancar"
    T.Tanggung -> "tanggung"
    T.Dadi -> "dadi"
    T.Wiled -> "wiled"
    T.Rangkep -> "rangkep"

-- | 1234 name [ tracks ]
instance Parse.Element T.ParsedBlock where
    parse config = do
        block_pos <- Parse.get_pos
        block_gatra <- Parse.lexeme $ Parse.parse config
        -- I want to be able to parse gantung-5 seleh-3, but I can't parse
        -- numbers and use normal whitespace because then "1234\n1234\n"
        -- is ambiguous either a name or another Gatra.  So don't allow
        -- a newline.  It's a bit unsatisfying because it makes newlines
        -- matter.  Alternately, I could do an alternate "inferred tracks"
        -- notation, like say "1234 names *"
        block_names <- P.many $ lexeme_s p_name
        block_tracks <- P.optional $ Parse.parse config
        pure $ T.Block
            { block_gatra, block_names, block_tracks, block_pos
            , block_inferred = []
            }
    unparse config (T.Block { block_gatra, block_names, block_tracks }) =
        Text.unwords $ Parse.unparse config block_gatra : block_names
            ++ maybe [] ((:[]) . Parse.unparse config) block_tracks

p_name :: Parser Text
p_name = Text.cons
    <$> P.satisfy (\c -> 'a' <= c && c <= 'z')
    <*> P.takeWhile1
        (\c -> 'a' <= c && c <= 'z' || '0' <= c && c <= '9' || c == '-')

instance Parse.Element T.Tracks where
    parse config =
        fmap T.Tracks $ Parse.lexeme "[" *> tracks <* Parse.lexeme "]"
        where tracks = P.some (Parse.parse config)
    unparse config (T.Tracks tracks) =
        -- mconcat instead of unwords, and no space on "]", because the last
        -- note's HasSpace should put one on.
        "[ " <> mconcat (map (Parse.unparse config) tracks) <> "]"

instance Parse.Element (T.Track T.ParsedToken) where
    parse config = do
        track_pos <- Parse.get_pos
        Parse.keyword ">"
        track_tokens <- P.many $ Parse.parse config
        return $ T.Track { track_tokens, track_pos }
    unparse config (T.Track { track_tokens }) =
        -- Text.unwords $ ">" : map (Parse.unparse config) track_tokens
        -- Rely on HasSpace to preserving spacing between notes.
        mconcat $ "> " : map (Parse.unparse config) track_tokens

p_has_space :: Parser T.HasSpace
p_has_space = Parse.p_whitespace_ >>= pure . \case
    True -> T.HasSpace
    False -> T.NoSpace

instance Parse.Element T.ParsedToken where
    parse config =
        Parse.lexeme (P.char '|' *> (T.TBarline <$> Parse.get_pos))
        <|> T.TRest <$> Parse.get_pos <*> Parse.parse config
        <|> T.TNote <$> Parse.get_pos <*> Parse.parse config
    unparse _ (T.TBarline _) = "| "
    unparse config (T.TNote _ note) = Parse.unparse config note
    unparse config (T.TRest _ rest) = Parse.unparse config rest

instance Parse.Element T.Rest where
    parse _ = do
        rest_sustain <- P.char '.' *> pure True <|> P.char '_' *> pure False
        -- Parse.lexeme is omitted from the calling parser to support this.
        rest_space <- p_has_space
        pure $ T.Rest { rest_sustain, rest_space }
    unparse _ (T.Rest { rest_sustain, rest_space }) =
        (if rest_sustain then "." else "_") <> case rest_space of
            T.NoSpace -> ""
            T.HasSpace -> " "

instance Parse.Element (T.Note T.ParsedPitch T.HasSpace) where
    parse config = do
        note_pitch <- Parse.parse config
        note_zero_duration <- P.option False (P.char '/' *> pure True)
        -- Parse.lexeme is omitted from the calling parser to support this.
        note_duration <- p_has_space
        return $ T.Note
            { note_pitch
            , note_zero_duration
            , note_duration
            }
    unparse config (T.Note { note_pitch, note_zero_duration, note_duration }) =
        Parse.unparse config note_pitch
        <> if note_zero_duration then "/" else ""
        <> case note_duration of
            T.NoSpace -> ""
            T.HasSpace -> " "

instance Parse.Element T.PitchClass where
    parse _ = maybe mzero pure . T.char_pc
        =<< P.satisfy (\c -> '1' <= c && c <= '9')
    unparse _ p = Text.singleton (T.pc_char p)

instance Parse.Element T.ParsedPitch where
    parse config = do
        oct_hint <- Text.foldl' (\n c -> n + if c == ',' then -1 else 1) 0 <$>
            P.takeWhile (`elem` (",'" :: String))
        p <- Parse.parse config
        pure $ T.ParsedPitch oct_hint p
    unparse config (T.ParsedPitch oct_hint pc) =
        octs <> Parse.unparse config pc
        where
        octs = case compare oct_hint 0 of
            EQ -> ""
            LT -> Text.replicate (- oct_hint) ","
            GT -> Text.replicate oct_hint "'"

instance Parse.Element (T.Gatra T.ParsedPitch) where
    parse config = T.Gatra <$> p <*> p <*> p <*> p
        where p = Parse.parse config
    unparse config (T.Gatra n1 n2 n3 n4) =
        mconcatMap (Parse.unparse config) [n1, n2, n3, n4]

instance Parse.Element (T.Balungan T.ParsedPitch) where
    parse config = T.Balungan
        <$> (P.char '.' *> pure Nothing <|> Just <$> Parse.parse config)
        <*> P.optional (Parse.parse config)
    unparse config (T.Balungan mb_pitch annot) =
        maybe "." (Parse.unparse config) mb_pitch
        <> maybe "" (Parse.unparse config) annot

instance Parse.Element T.BalunganAnnotation where
    parse _ = P.char ')' *> pure T.Gong <|> P.char '^' *> pure T.Kenong
    unparse _ = \case
        T.Gong -> ")"
        T.Kenong -> "^"

-- | Lexeme but no comments or newlines.
lexeme_s :: Parser a -> Parser a
lexeme_s = (<* p_simple_space)

p_simple_space :: Parser ()
p_simple_space = P.skipWhile (\c -> c == ' ' || c == '\t')
