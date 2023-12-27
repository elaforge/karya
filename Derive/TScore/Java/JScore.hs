-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
{-# LANGUAGE StrictData #-}
{- | Similar to "Derive.TScore.Parse", but specialized for javanese notation.

    - Use numbers for pitches instead of letters.  Infer octave based on
    instrument, and ' ,
    - Parse notes without spaces.  Possible because pitch is a single digit.
    - Trailing / for damped notes.
    - Use . for continued pitch, _ for rest.  Usually duration is implicit.
    But inferring gender style from pipilan seems complicated.
    - Can't use . for low if I use it for rest!  Or , and ' like lilypond?
    - Infer left and right hand for instruments.
    - Associate tags (irama, name, balungan, prev seleh, seleh)
    - Switch laras and pathet, e.g. slendro manyura to pelog barang.
    - "|" is the same.  I could use || for gatra if I have multiple gatra things
        (puthut gelut?  or just have two separate calls?)
    - Bar | is optional in TS, but I could make it required to infer rhythm.
    - Biggest difference is to infer durations.  This limits | ability to catch
    errors, but since I only allow 4 and 8, not really.  With explicit dots
    it's easy, parse all notes in the measure, check for 4 or 8.  With implicit
    trailing dots I have to remember spaces and require space separation.

    How to do tempo?  Hard to draw overbars.
    In tanggung, one bar is 2 notes.
    In dadi, one bar is 1 note.
    In wiled, one bar is 1 note.

    TODO it would be nicer if this were simply a subset of normal tscore.
    Not sure if possible.  At the least it should reuse as much as possible
    of Check, and all of TScore.
-}
module Derive.TScore.Java.JScore where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified Util.Lists as Lists
import qualified Util.Logger as Logger
import qualified Util.P as P
import qualified Util.Texts as Texts

import qualified Derive.TScore.Java.Check as Check
import qualified Derive.TScore.Java.T as T
import           Derive.TScore.Java.T (Octave, Pitch(..))
import qualified Derive.TScore.Parse as Parse
import qualified Derive.TScore.T as TScore.T

import           Global


parse_score :: Text -> Either String T.ParsedScore
parse_score = Parse.parse_text (Parse.parse Parse.default_config)

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
            "inst" -> maybe unknown (return . T.Instrument) $
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
        T.Instrument a -> un "inst" (instrument_enum a)
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
        block_gatra <- Parse.lexeme $ Parse.parse config
        -- It's important this doesn't take digits, or it could grab the next
        -- Gatra.
        block_names <- P.many $ Parse.lexeme $ P.takeWhile1 $ \c ->
            'a' <= c && c <= 'z' || c == '-'
        block_tracks <- P.optional $ Parse.parse config
        pure $ T.Block
            { block_gatra, block_names, block_tracks
            , block_inferred = []
            }
    unparse config (T.Block { block_gatra, block_names, block_tracks }) =
        Text.unwords $ Parse.unparse config block_gatra : block_names
            ++ maybe [] ((:[]) . Parse.unparse config) block_tracks

instance Parse.Element T.Tracks where
    parse config =
        fmap T.Tracks $ Parse.lexeme "[" *> tracks <* Parse.lexeme "]"
        where tracks = P.some (Parse.parse config)
    unparse config (T.Tracks tracks) =
        "[ " <> Text.unwords (map (Parse.unparse config) tracks) <> "]"
        -- Intentionally no space on "]", because the last note's HasSpace
        -- should put one on.

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

p_has_space :: Parse.Parser T.HasSpace
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

instance Parse.Element (T.Note (Pitch T.RelativeOctave) T.HasSpace) where
    parse config = do
        note_pos <- Parse.get_pos
        note_pitch <- Parse.parse config
        note_zero_duration <- P.option False (P.char '/' *> pure True)
        -- Parse.lexeme is omitted from the calling parser to support this.
        note_duration <- p_has_space
        return $ T.Note
            { note_pitch
            , note_zero_duration
            , note_duration
            , note_pos
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

instance Parse.Element (Pitch T.RelativeOctave) where
    parse config = do
        oct <- Text.foldl' (\n c -> n + if c == ',' then -1 else 1) 0 <$>
            P.takeWhile (`elem` (",'" :: String))
        p <- Parse.parse config
        pure $ Pitch (T.RelativeOctave oct) p
    unparse config (Pitch (T.RelativeOctave oct) pc) =
        octs <> Parse.unparse config pc
        where
        octs = case compare oct 0 of
            EQ -> ""
            LT -> Text.replicate (- oct) ","
            GT -> Text.replicate oct "'"

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


-- * transform

type Transform = Pitch Octave -> Pitch Octave

-- | Simple pelog lima to pelog barang by changing 1s to 7s.
lima_to_barang :: Transform
lima_to_barang p@(Pitch _ T.P1) = T.add_pc_abs (-1) p
lima_to_barang p = p

-- * format

format_file :: Transform -> FilePath -> IO ()
format_file transform fname = print_score transform =<< Text.IO.readFile fname

print_score :: Transform -> Text -> IO ()
print_score transform source = case parse_score source of
    Left err -> putStrLn err
    Right score
        | null errs -> mapM_ Text.IO.putStrLn lines
        | otherwise -> mapM_ (Text.IO.putStrLn . T.show_error source) errs
        where (lines, errs) = format_score transform score

data FormatState = FormatState {
    state_metas :: [T.Meta]
    , state_prev_is_block :: Bool
    , state_transform :: Transform
    }

-- | Format a T.Score to print.  This is does some Checks but doesn't
-- normalize down to Time, because it's more like normalization than lowering.
format_score :: Transform -> T.ParsedScore -> ([Text], [T.Error])
format_score transform score =
    ( concat $ snd $ List.mapAccumL format_toplevel state (map snd toplevels)
    , errs
    )
    where
    state = FormatState [] False transform
    (T.Score toplevels, errs) = Logger.runId $ Check.format_score score

format_toplevel :: FormatState -> T.Toplevel Check.Block
    -> (FormatState, [Text])
format_toplevel state = \case
    T.ToplevelMeta m ->
        ( state
            { state_metas = m : state_metas state, state_prev_is_block = False }
        , (if state_prev_is_block state then [""] else []) ++ [format_meta m]
        )
    T.BlockDefinition block ->
        ( state { state_prev_is_block = True }
        , format_block state block
        )

map_pitch :: (a -> b) -> [T.Token (T.Note a dur) rest]
    -> [T.Token (T.Note b dur) rest]
map_pitch f = map $ T.map_note (\n -> n { T.note_pitch = f (T.note_pitch n) })

format_block :: FormatState -> Check.Block -> [Text]
format_block state block =
    title : map (("    "<>) . format_tokens bias)
        (map (map_pitch transform) block_tracks)
    where
    metas = state_metas state
    transform = state_transform state
    gatra = fmap transform block_gatra
    title = Texts.join2 " " (format_gatra gatra) (Text.unwords block_names)
        <> if null block_inferred then ""
            else " [" <> Text.unwords block_inferred <> "]"
    T.Block { block_gatra, block_names, block_tracks, block_inferred } = block
    irama = Lists.head [i | T.Irama i <- metas]
    inst = Lists.head [i | T.Instrument i <- metas]
    -- This actually corresponds to parts which are written with every beat
    -- and which ones use overbars.  Or could say that the basic speed for
    -- GenderBarung < Wiled is 4 per bar, while the rest are 8.
    -- TODO maybe there's a more direct way?
    bias
        | inst == Just T.GenderBarung && irama >= Just T.Wiled = Check.BiasEnd
        | otherwise = Check.BiasStart

format_meta :: T.Meta -> Text
format_meta = Text.drop 1 . Parse.unparse Parse.default_config
    -- Input syntax uses a leading %.

format_gatra :: T.Gatra (Pitch Octave) -> Text
format_gatra (T.Gatra n1 n2 n3 n4) =
    mconcatMap format_balungan [n1, n2, n3, n4]

format_balungan :: T.Balungan (Pitch Octave) -> Text
format_balungan (T.Balungan (Just (T.Pitch oct pc)) (Just T.Gong))
    -- The hardcoded circled digit looks better than the combining enclosing
    -- circle.
    | oct == 0 = Text.singleton $ toEnum (circled_digit_one + fromEnum pc)
    where circled_digit_one = 0x2460
format_balungan (T.Balungan pitch annot) = mconcat
    [ maybe "." format_pitch pitch
    , case annot of
        Nothing -> ""
        Just T.Gong -> Text.singleton '\x20dd' -- COMBINING ENCLOSING CIRCLE
        Just T.Kenong -> Text.singleton '\x0302' -- COMBINING CIRCUMFLEX ACCENT
    ]

format_tokens :: Check.Bias -> [Check.Token] -> Text
format_tokens bias = mconcat . go
    where
    go ts = zipWith format_token beats pre ++ case post of
        [] -> []
        bar : post -> format_token True bar : go post
        where
        beats
            | length pre >= 8 = cycle $ case bias of
                Check.BiasStart -> [True, False]
                Check.BiasEnd -> [False, True]
            | otherwise = repeat True
        (pre, post) = break is_barline ts
    is_barline (T.TBarline {}) = True
    is_barline _ = False

format_token :: Bool -> Check.Token -> Text
format_token on_beat = \case
    T.TBarline {} -> " | "
    -- T.TBarline {} -> " " <> vertical_line <> " "
    T.TRest _ (T.Rest { rest_sustain })
        | on_beat -> if rest_sustain then "." else "_"
        | otherwise -> " "
    T.TNote _ n -> format_pitch (T.note_pitch n)
        <> if T.note_zero_duration n then slash else ""
    where
    -- vertical_line = "\x007c" -- VERTICAL LINE
    slash = if use_slash
        then "\x0338" -- COMBINING LONG SOLIDUS OVERLAY
        else "\x0336" -- COMBINING LONG STROKE OVERLAY
    use_slash = True

format_pitch :: Pitch Octave -> Text
format_pitch (Pitch oct pc) = Text.cons (T.pc_char pc) dots
    where
    dots = case oct of
        0 -> ""
        1 -> "\x0307" -- COMBINING DOT ABOVE
        -1 -> "\x0323" -- COMBINING DOT BELOW
        2 -> "\x0308" -- COMBINING DIAERESIS
        -2 -> "\x0324" -- COMBINING DIAERESIS BELOW
        -- These should not show up, no instrument has this range.
        3 -> "\x00b3" -- superscript
        4 -> "\x2074" -- superscript
        -3 -> "\x2083" -- subscript
        -4 -> "\x2084" -- subscript
        _ -> "?"
