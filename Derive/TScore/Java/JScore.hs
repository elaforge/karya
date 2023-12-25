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
import qualified Data.Text as Text

import qualified Util.Logger as Logger
import qualified Util.P as P
import qualified Util.Texts as Texts

import qualified Derive.TScore.Java.Check as Check
import qualified Derive.TScore.Java.T as T
import           Derive.TScore.Java.T (Octave, Pitch(..))
import qualified Derive.TScore.Parse as Parse

import           Global


parse_score :: Text -> Either String T.Score
parse_score = Parse.parse_text (Parse.parse Parse.default_config)

instance Parse.Element T.Score where
    parse config = Parse.p_whitespace
        *> (T.Score <$> P.many (Parse.parse config))
    unparse config (T.Score toplevels) =
        Text.unlines $ map (Parse.unparse config) toplevels

instance Parse.Element T.Toplevel where
    parse config = Parse.lexeme $
        T.ToplevelDirective . un <$> Parse.parse config
        <|> T.BlockDefinition <$> Parse.parse config
        where un (Parse.ToplevelDirective d) = d
    unparse config = \case
        T.ToplevelDirective a ->
            Parse.unparse config (Parse.ToplevelDirective a)
        T.BlockDefinition a -> Parse.unparse config a

-- | 1234 name [ tracks ]
instance Parse.Element T.Block where
    parse config = do
        block_gatra <- Parse.lexeme $ Parse.parse config
        -- It's important this doesn't take digits, or it could grab the next
        -- Gatra.
        block_names <- P.many $ Parse.lexeme $ P.takeWhile1 $ \c ->
            'a' <= c && c <= 'z' || c == '-'
        block_tracks <- P.optional $ Parse.parse config
        pure $ T.Block { block_gatra, block_names, block_tracks }
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

instance Parse.Element T.Track where
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

instance Parse.Element T.Gatra where
    parse config = T.Gatra <$> p <*> p <*> p <*> p
        where p = Parse.parse config
    unparse config (T.Gatra n1 n2 n3 n4) =
        mconcatMap (Parse.unparse config) [n1, n2, n3, n4]

instance Parse.Element T.Balungan where
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

-- | Simple pelog lima to pelog barang by changing 1s to 7s.
lima_to_barang :: [T.Token (T.Note (Pitch Octave) dur) rest]
    -> [T.Token (T.Note (Pitch Octave) dur) rest]
lima_to_barang =
    map (T.map_note (\n -> n { T.note_pitch = replace (T.note_pitch n) }))
    where
    replace p@(Pitch _ T.P1) = T.add_pc (-1) p
    replace p = p

-- * format

-- | Format a T.Score to print.  This is does some Checks but doesn't
-- normalize down to Time, because it's more like normalization than lowering.
format_score :: T.Score -> ([Text], [T.Error])
format_score score = Logger.runId $ do
    T.Score toplevels <- Check.normalize_names score
    concatMapM (format_toplevel . snd) toplevels

format_toplevel :: T.Toplevel -> Check.CheckM [Text]
format_toplevel = \case
    T.ToplevelDirective (T.Directive _ key val) ->
        pure ["", mconcat $ key : maybe [] (\v -> [" = ", v]) val]
    T.BlockDefinition block -> format_block block

format_block :: T.Block -> Check.CheckM [Text]
format_block (T.Block { block_gatra, block_names, block_tracks }) = do
    tracks <- maybe (pure []) format_tracks block_tracks
    pure $ title : map ("    "<>) tracks
    where
    title = Texts.join2 " "
        (Parse.unparse Parse.default_config block_gatra)
        (Text.unwords block_names)

format_directive :: T.Directive -> Text
format_directive (T.Directive _ key mb_val) = key <> maybe "" ("="<>) mb_val

-- | Score-to-score, this takes the parsed score to a somewhat more normalized
-- version.  So not the same as converting to tracklang, because I don't need
-- actual times and durations.  Also I have to 'normalize_hands' to make
-- sure they are at the same "zoom", but it's only since I don't want to
-- convert from times back to notes.
format_tracks :: T.Tracks -> Check.CheckM [Text]
format_tracks (T.Tracks tracks) = case map T.track_tokens tracks of
    [lefts, rights] -> do
        lefts <- process_for_format bias lefts
        rights <- process_for_format bias rights
        (lefts, rights) <- Check.normalize_hands bias lefts rights
        pure [format_tokens lefts, format_tokens rights]
    tracks -> map format_tokens <$> mapM (process_for_format bias) tracks
    where
    bias = Check.BiasStart

process_for_format :: Check.Bias -> [T.ParsedToken]
    -> Check.CheckM [T.Token (T.Note (Pitch Octave) ()) T.Rest]
process_for_format bias = -- fmap lima_to_barang .
    Check.normalize_barlines bias . Check.resolve_pitch
    -- This doesn't do 'resolve_durations', because I want the original rests.

format_tokens :: [T.Token (T.Note (Pitch Octave) dur) T.Rest] -> Text
format_tokens = mconcat . go
    where
    go ts = zipWith format_token beats pre ++ case post of
        [] -> []
        bar : post -> format_token True bar : go post
        where
        beats = if length pre >= 8 then cycle [False, True] else repeat True
        (pre, post) = break is_barline ts
    is_barline (T.TBarline {}) = True
    is_barline _ = False

format_token :: Bool -> T.Token (T.Note (Pitch Octave) dur) T.Rest -> Text
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
