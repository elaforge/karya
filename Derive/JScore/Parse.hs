-- Copyright 2024 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Derive.JScore.Parse (
    Element
    , parseScore
    , parseText
    , unparse
    , instrumentEnum
    , iramaEnum
    , larasEnum
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

parseScore :: Text -> Either Error T.ParsedScore
parseScore = second unwrapScore . parseText

parseText :: Element a => Text -> Either Error a
parseText = first txt . Parse.parseText (Parse.parse Parse.defaultConfig)

unwrapScore :: T.ParsedScore -> T.ParsedScore
unwrapScore = fmap block
    where block b = b { T.blockTracks = unwrapTracks <$> T.blockTracks b }

-- | We only have max 2 hands, so unwrap >2 into a single line.
unwrapTracks :: T.Tracks -> T.Tracks
unwrapTracks (T.Tracks tracks)
    | length tracks > 2 && even (length tracks) =
        T.Tracks [merge t1, merge t2]
    | otherwise = T.Tracks tracks
    where
    (t1, t2) = unzip (pairs tracks)
    pairs (x:y:zs) = (x, y) : pairs zs
    pairs _ = []
    merge (t:ts) = T.Track
        { trackPos = T.trackPos t
        -- Add a barline to unwrapped sections, because I don't end tracks
        -- with a final barline.  Or, I could add one only if there isn't
        -- already one.
        , trackTokens = List.intercalate [T.TBarline T.fakePos] $
            map T.trackTokens (t:ts)
        }
    merge [] = error "broken invariant length tracks > 2"

unparse :: Parse.Element a => a -> Text
unparse = Parse.unparse Parse.defaultConfig

instance Parse.Element T.ParsedScore where
    parse config = Parse.pWhitespace
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
        TScore.T.Directive _pos key mbVal <- Parse.pDirective True
        val <- maybe (fail $ "directive with no val: " <> untxt key) pure
            mbVal
        let unknown = fail $ "unknown " <> untxt key <> ": " <> untxt val
        case key of
            "source" -> pure $ T.Source val
            "piece" -> pure $ T.Piece val
            "section" -> pure $ T.Section val
            "laras" -> maybe unknown (pure . T.Laras) $
                parseEnum larasEnum val
            "instrument" -> maybe unknown (pure . T.Instrument) $
                parseEnum instrumentEnum val
            "irama" -> maybe unknown (pure . T.Irama) $
                parseEnum iramaEnum val
            _ -> fail $ "unknown meta: " <> untxt key <> " = " <> untxt val
    unparse _ = \case
        T.Source a -> un "source" a
        T.Piece a -> un "piece" a
        T.Section a -> un "section" a
        T.Laras a -> un "laras" (larasEnum a)
        T.Irama a -> un "irama" (iramaEnum a)
        T.Instrument a -> un "instrument" (instrumentEnum a)
        where un key val = mconcat ["%", key, " = ", val]

parseEnum :: (Bounded a, Enum a) => (a -> Text) -> Text -> Maybe a
parseEnum unparse t = Map.lookup t m
    where m = Map.fromList $ Lists.keyOn unparse [minBound ..]

larasEnum :: T.Laras -> Text
larasEnum = \case
    T.PelogNem -> "pelog-nem"
    T.PelogLima -> "pelog-lima"
    T.PelogBarang -> "pelog-barang"
    T.SlendroNem -> "slendro-nem"
    T.SlendroSanga -> "slendro-sanga"
    T.SlendroManyura -> "slendro-manyura"

instrumentEnum :: T.Instrument -> Text
instrumentEnum = \case
    T.GenderBarung -> "gender-barung"
    T.GenderPanerus -> "gender-panerus"
    T.Siter -> "siter"

iramaEnum :: T.Irama -> Text
iramaEnum = \case
    T.Lancar -> "lancar"
    T.Tanggung -> "tanggung"
    T.Dadi -> "dadi"
    T.Wiled -> "wiled"
    T.Rangkep -> "rangkep"

-- | 1234 name [ tracks ]
instance Parse.Element T.ParsedBlock where
    parse config = do
        blockPos <- Parse.getPos
        blockGatra <- Parse.lexeme $ Parse.parse config
        -- I want to be able to parse gantung-5 seleh-3, but I can't parse
        -- numbers and use normal whitespace because then "1234\n1234\n"
        -- is ambiguous either a name or another Gatra.  So don't allow
        -- a newline.  It's a bit unsatisfying because it makes newlines
        -- matter.  Alternately, I could do an alternate "inferred tracks"
        -- notation, like say "1234 names *"
        blockNames <- P.many $ lexemeS pName
        blockTracks <- P.optional $ Parse.parse config
        pure $ T.Block
            { blockGatra, blockNames, blockTracks, blockPos
            , blockInferred = []
            }
    unparse config (T.Block { blockGatra, blockNames, blockTracks }) =
        Text.unwords $ Parse.unparse config blockGatra : blockNames
            ++ maybe [] ((:[]) . Parse.unparse config) blockTracks

pName :: Parser Text
pName = Text.cons
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
        trackPos <- Parse.getPos
        Parse.keyword ">"
        trackTokens <- P.many $ Parse.parse config
        pure $ T.Track { trackTokens, trackPos }
    unparse config (T.Track { trackTokens }) =
        -- Text.unwords $ ">" : map (Parse.unparse config) track_tokens
        -- Rely on HasSpace to preserving spacing between notes.
        mconcat $ "> " : map (Parse.unparse config) trackTokens

pHasSpace :: Parser T.HasSpace
pHasSpace = Parse.pWhitespace_ >>= pure . \case
    True -> T.HasSpace
    False -> T.NoSpace

instance Parse.Element T.ParsedToken where
    parse config =
        Parse.lexeme (P.char '|' *> (T.TBarline <$> Parse.getPos))
        <|> T.TRest <$> Parse.getPos <*> Parse.parse config
        <|> T.TNote <$> Parse.getPos <*> Parse.parse config
    unparse _ (T.TBarline _) = "| "
    unparse config (T.TNote _ note) = Parse.unparse config note
    unparse config (T.TRest _ rest) = Parse.unparse config rest

instance Parse.Element T.Rest where
    parse _ = do
        restSustain <- P.char '.' *> pure True <|> P.char '_' *> pure False
        -- Parse.lexeme is omitted from the calling parser to support this.
        restSpace <- pHasSpace
        pure $ T.Rest { restSustain, restSpace }
    unparse _ (T.Rest { restSustain, restSpace }) =
        (if restSustain then "." else "_") <> case restSpace of
            T.NoSpace -> ""
            T.HasSpace -> " "

instance Parse.Element (T.Note T.ParsedPitch T.HasSpace) where
    parse config = do
        notePitch <- Parse.parse config
        noteZeroDuration <- P.option False (P.char '/' *> pure True)
        -- Parse.lexeme is omitted from the calling parser to support this.
        noteDuration <- pHasSpace
        pure $ T.Note
            { notePitch
            , noteZeroDuration
            , noteDuration
            }
    unparse config (T.Note { notePitch, noteZeroDuration, noteDuration }) =
        Parse.unparse config notePitch
        <> if noteZeroDuration then "/" else ""
        <> case noteDuration of
            T.NoSpace -> ""
            T.HasSpace -> " "

instance Parse.Element T.PitchClass where
    parse _ = maybe mzero pure . T.charPc
        =<< P.satisfy (\c -> '1' <= c && c <= '9')
    unparse _ p = Text.singleton (T.pcChar p)

instance Parse.Element T.ParsedPitch where
    parse config = do
        octHint <- Text.foldl' (\n c -> n + if c == ',' then -1 else 1) 0 <$>
            P.takeWhile (`elem` (",'" :: String))
        p <- Parse.parse config
        pure $ T.ParsedPitch octHint p
    unparse config (T.ParsedPitch octHint pc) =
        octs <> Parse.unparse config pc
        where
        octs = case compare octHint 0 of
            EQ -> ""
            LT -> Text.replicate (- octHint) ","
            GT -> Text.replicate octHint "'"

instance Parse.Element (T.Gatra T.ParsedPitch) where
    parse config = T.Gatra <$> p <*> p <*> p <*> p
        where p = Parse.parse config
    unparse config (T.Gatra n1 n2 n3 n4) =
        mconcatMap (Parse.unparse config) [n1, n2, n3, n4]

instance Parse.Element (T.Balungan T.ParsedPitch) where
    parse config = T.Balungan
        <$> (P.char '.' *> pure Nothing <|> Just <$> Parse.parse config)
        <*> P.optional (Parse.parse config)
    unparse config (T.Balungan mbPitch annot) =
        maybe "." (Parse.unparse config) mbPitch
        <> maybe "" (Parse.unparse config) annot

instance Parse.Element T.BalunganAnnotation where
    parse _ = P.char ')' *> pure T.Gong <|> P.char '^' *> pure T.Kenong
    unparse _ = \case
        T.Gong -> ")"
        T.Kenong -> "^"

-- | Lexeme but no comments or newlines.
lexemeS :: Parser a -> Parser a
lexemeS = (<* pSimpleSpace)

pSimpleSpace :: Parser ()
pSimpleSpace = P.skipWhile (\c -> c == ' ' || c == '\t')
