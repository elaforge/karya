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
import qualified Control.Monad.Identity as Identity
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text

import qualified Util.Lists as Lists
import qualified Util.Logger as Logger
import qualified Util.Num as Num
import qualified Util.P as P
import qualified Util.Texts as Texts

import qualified Derive.TScore.Parse as Parse
import qualified Derive.TScore.Java.T as T
import           Derive.TScore.Java.T (Pitch(..), Octave)

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


-- * check

type CheckM a = Logger.Logger T.Error a

warn :: T.Pos -> Text -> CheckM ()
warn pos msg = Logger.log (T.Error pos msg)

{-
    Checks:
    - barlines correct
    - directives understood
    - within instrument range
    - left hand below right hand
-}
{-
check2 :: Text -> Score -> [T.Error]
check2 source (Score toplevels) =
    snd $ Logger.runId $ mapM_ (top . snd) toplevels
    where
    top = \case
        ToplevelDirective {} -> pure ()
        BlockDefinition block ->
            concatMap (check_tokens . track_tokens) tracks
            where Tracks tracks = block_tracks block

check_tokens :: [Token] -> [T.Error]
check_tokens tokens = errs
    where (errs, _) = Either.partitionEithers $ resolve_tokens tokens
-}

data Bias = BiasStart | BiasEnd
    deriving (Show, Eq)

resolve_tokens :: Bias -> [T.ParsedToken]
    -> CheckM [(T.Time, T.Note (Pitch Octave) T.Time)]
resolve_tokens bias =
    fmap (resolve_durations bias) . normalize_barlines bias . resolve_pitch

-- ** check directives

check_score_directive :: T.Directive -> Either T.Error ToplevelTag
check_score_directive (T.Directive pos key Nothing) =
    Left $ T.Error pos $ "directive with no val: " <> key
check_score_directive (T.Directive pos key (Just val)) =
    first (T.Error pos) $ case key of
        "source" -> Right $ Source val
        "piece" -> Right $ Piece val
        "section" -> Right $ Section val
        "laras" -> Laras <$> case val of
           "pelog-nem" -> Right PelogNem
           "pelog-lima" -> Right PelogLima
           "pelog-barang" -> Right PelogBarang
           "slendro" -> Right Slendro -- TODO different slendro pathet
           _ -> Left $ "unknown laras: " <> val
        "irama" -> Irama <$> case val of
           "lancar" -> Right Lancar
           "tanggung" -> Right Tanggung
           "dadi" -> Right Dadi
           "wiled" -> Right Wiled
           "rangkep" -> Right Rangkep
           _ -> Left $ "unknown irama: " <> val
        _ -> Left $ "unknown directive: " <> key <> " = " <> val

data ToplevelTag = Source Text | Piece Text | Section Text | Laras Laras
    | Irama Irama
    deriving (Eq, Show)

data Laras = Slendro | PelogNem | PelogLima | PelogBarang
    deriving (Eq, Show)
-- | Along with Instrument, affects expected number of notes per barline.
data Irama = Lancar | Tanggung | Dadi | Wiled | Rangkep
    deriving (Eq, Ord, Enum, Bounded, Show)
data Instrument = GenderBarung | GenderPanerus | Siter
    deriving (Eq, Show)

-- variations: append 123567 for e.g. gantung 2, cilik, kecil, gede, besar,
-- kempyung / gembyang
standardNames :: [(Text, Text)]
standardNames =
    [ ("ayu-kuning", "ak")
    , ("debyang-debyung", "dd")
    , ("dualolo", "dll")
    , ("duduk", "dd")
    , ("gantung", "gant")
    , ("gelut", "g")
    , ("jarik-kawung", "jk")
    , ("kacaryan", "kc")
    , ("kutuk-kuning", "kk")
    , ("puthut-semedi", "ps")
    , ("puthut", "p") -- 2 part pattern puthut gelut
    , ("tumurun", "tm")
    ]

-- ** resolve_pitch

resolve_pitch
    :: [T.Token (T.Note (Pitch T.RelativeOctave) dur) rest]
    -> [T.Token (T.Note (Pitch Octave) dur) rest]
resolve_pitch = snd . List.mapAccumL resolve (0, Nothing)
    where
    resolve prev = \case
        T.TBarline pos -> (prev, T.TBarline pos)
        T.TRest pos rest -> (prev, T.TRest pos rest)
        T.TNote pos note ->
            ( (oct, Just pc)
            , T.TNote pos $ note { T.note_pitch = pitch }
            )
            where
            pitch@(Pitch oct pc) = infer_octave prev (T.note_pitch note)

-- Initial prev_oct is based on the instrument.
-- Similar to Check.infer_octave, but uses simpler 'Pitch' instead of
-- 'Perform.Pitch.Pitch'.
infer_octave :: (Octave, Maybe T.PitchClass)
    -> Pitch T.RelativeOctave -> Pitch Octave
infer_octave (prev_oct, Nothing) (Pitch (T.RelativeOctave rel_oct) pc) =
    Pitch (prev_oct + rel_oct) pc
infer_octave (prev_oct, Just prev_pc) (Pitch (T.RelativeOctave rel_oct) pc) =
    case compare rel_oct 0 of
        -- If distances are equal, favor downward motion.  But since PitchClass
        -- has an odd number, this never happens.  If I omit P4, it could
        -- though, but then I should also omit P1 or P7.
        EQ
            | prev_pc == pc -> Pitch prev_oct pc
            | otherwise -> Lists.minOn (abs . pitch_diff prev) above below
        GT -> add_oct (rel_oct-1) above
        LT -> add_oct (rel_oct+1) below
    where
    prev = Pitch prev_oct prev_pc
    -- If I'm moving up, then I don't need to increment the octave to be above.
    above = Pitch (if pc > prev_pc then prev_oct else prev_oct+1) pc
    below = Pitch (if pc < prev_pc then prev_oct else prev_oct-1) pc

add_oct :: Octave -> Pitch Octave -> Pitch Octave
add_oct oct (Pitch o pc) = Pitch (oct+o) pc

add_pc :: Int -> Pitch Octave -> Pitch Octave
add_pc steps (Pitch octave pc) = Pitch (octave + oct) pc2
    where (oct, pc2) = toEnumBounded $ fromEnum pc + steps

pitch_diff :: Pitch Octave -> Pitch Octave -> Int
pitch_diff (Pitch oct1 pc1) (Pitch oct2 pc2) =
    per_oct * (oct1 - oct2) + (fromEnum pc1 - fromEnum pc2)
    where
    per_oct = fromEnum (maxBound :: T.PitchClass) + 1

-- ** resolve_durations

resolve_durations :: Bias -> [T.Token (T.Note pitch dur) T.Rest]
    -> [(T.Time, T.Note pitch T.Time)]
resolve_durations bias =
    Maybe.catMaybes . snd . List.mapAccumL resolve_rests 0 . Lists.zipNexts
        . concatMap resolve . Lists.split is_barline
    where
    resolve bar = mapMaybe (add_dur len) bar
        where len = length bar
    add_dur len = \case
        T.TBarline {} -> Nothing
        T.TRest _ rest -> Just (dur, Left rest)
        T.TNote _ note -> Just (dur, Right note)
        where dur = T.Time (1 / fromIntegral len)
    is_barline (T.TBarline {}) = True
    is_barline _ = False
    -- If BiasEnd, each note goes from the *end* of its duration until the
    -- start of the next note, or non-sustaining rest.  This gives them the
    -- end-bias.
    resolve_rests time ((dur, Left _), _) = (time + dur, Nothing)
    resolve_rests time ((dur, Right n), nexts) =
        ( time + dur
        , Just $ case bias of
            BiasEnd -> (time + dur, n { T.note_duration = sustain })
            BiasStart -> (time, n { T.note_duration = dur + sustain })
        )
        where
        -- The final note will have 0 dur, which I can look for and set.
        -- If the final note is a sustain rest, then use its duration, not 0.
        -- TODO Not sure if this is useful, maybe it should be sustain duration
        -- plus the default end dur?  Maybe I should take a default end-note
        -- duration?
        sustain = Num.sum $ case bias of
            BiasEnd -> until_note nexts
            BiasStart -> map fst $ takeWhile is_sustain nexts
    is_sustain (_, Left (T.Rest { rest_sustain })) = rest_sustain
    is_sustain _ = False
    until_note [] = []
    until_note ((dur, n) : ns) = case n of
        Left (T.Rest { rest_sustain }) ->
            dur : if rest_sustain then until_note ns else []
        Right (T.Note {}) -> [dur]

-- | Verify bar durations, infer rests if necessary.  After this, all bars
-- should be a power of 2.
normalize_barlines :: Bias -> [T.Token (T.Note pitch T.HasSpace) T.Rest]
    -> CheckM [T.Token (T.Note pitch ()) T.Rest]
normalize_barlines bias = map_bars $ \case
    [] -> pure []
    bar@(t : _)
        | power_of_2 (length bar) -> pure $ map strip bar
        | power_of_2 (length inferred) -> pure $ map strip inferred
        | otherwise -> do
            warn (T.token_pos t) $
                "bar not a power of 2: " <> showt (length bar)
                <> ", with inferred rests: " <> showt (length inferred)
            pure $ map strip bar
        where
        inferred = infer_rests bias bar
        strip = T.map_note (T.map_duration (const ()))

-- | The T.Tokens given to the function will not contain T.TBarline.
-- I could put it in the type, but it seems too much bother.
map_bars :: Monad m => ([T.Token n1 r1] -> m [T.Token n2 r2])
    -> [T.Token n1 r1] -> m [T.Token n2 r2]
map_bars f tokens = concat . add <$> mapM f (pre : map snd posts)
    where
    add [] = []
    add (t:ts) = t : zipWith (:) bars ts
    bars = map (T.TBarline . fst) posts
    (pre, posts) = Lists.splitWith is_barline tokens
    is_barline (T.TBarline pos) = Just pos
    is_barline _ = Nothing

power_of_2 :: Int -> Bool
power_of_2 n = snd (properFraction (logBase 2 (fromIntegral n))) == 0

-- | If there aren't enough notes in the bar, try inferring a rest before
-- every odd group of notes followed by space.
infer_rests :: Bias -> [T.Token (T.Note pitch T.HasSpace) T.Rest]
    -> [T.Token (T.Note pitch T.HasSpace) T.Rest]
infer_rests bias = concatMap infer . Lists.splitAfter has_space
    where
    infer tokens
        | even (length tokens) = tokens
        | bias == BiasEnd = extra_rest : tokens
        | otherwise = tokens ++ [extra_rest]
    extra_rest = T.TRest T.fake_pos $ T.Rest True T.HasSpace
    has_space = \case
        T.TBarline {} -> True
        T.TNote _ note -> T.note_duration note == T.HasSpace
        T.TRest _ rest -> T.rest_space rest == T.HasSpace

-- | Split on barlines, zipPadded, map across them.
normalize_hands :: Bias -> [T.Token note T.Rest] -> [T.Token note T.Rest]
    -> CheckM ([T.Token note T.Rest], [T.Token note T.Rest])
normalize_hands bias lefts rights =
    fmap (bimap join_bars join_bars . unzip) $ mapM normalize $
    Lists.zipPadded (split_bars lefts) (split_bars rights)
    where
    normalize = \case
        Lists.First (pos, lefts) -> do
            warn pos "left hand with no right hand"
            pure ((pos, lefts), (pos, []))
        Lists.Second (pos, rights) -> do
            warn pos "right hand with no left hand"
            pure ((pos, []), (pos, rights))
        Lists.Both (pos0, lefts) (pos1, rights) ->
            case (log2 (length lefts), log2 (length rights)) of
                (Nothing, _) -> do
                    warn pos0 "not power of 2"
                    pure ((pos0, lefts), (pos1, rights))
                (_, Nothing) -> do
                    warn pos1 "not power of 2"
                    pure ((pos0, lefts), (pos1, rights))
                (Just d1, Just d2)
                    | delta >= 0 ->
                        pure ((pos0, lefts), (pos1, expand pos1 delta rights))
                    | otherwise ->
                        pure ((pos0, expand pos0 delta lefts), (pos1, rights))
                    where delta = d1 - d2
    expand pos delta = case bias of
        BiasEnd -> concatMap (\n -> replicate (delta^2) (rest pos) ++ [n])
        BiasStart -> concatMap (: replicate (delta^2) (rest pos))
    rest pos = T.TRest pos (T.Rest True T.NoSpace)

log2 :: Int -> Maybe Int
log2 n
    | frac == 0 = Just i
    | otherwise = Nothing
    where (i, frac) = properFraction $ logBase 2 (fromIntegral n)

split_bars :: [T.Token note rest] -> [(T.Pos, [T.Token note rest])]
split_bars [] = []
split_bars tokens@(t0 : _) = (T.token_pos t0, group0) : groups
    where
    (group0, groups) = Lists.splitWith is_barline tokens
    is_barline (T.TBarline pos) = Just pos
    is_barline _ = Nothing

join_bars :: [(T.Pos, [T.Token note rest])] -> [T.Token note rest]
join_bars [] = []
join_bars ((_, g0) : gs) = concat $ g0 : map join gs
    where join (pos, tokens) = T.TBarline pos : tokens


-- * transform

-- | Simple pelog lima to pelog barang by changing 1s to 7s.
lima_to_barang :: [T.Token (T.Note (Pitch Octave) dur) rest]
    -> [T.Token (T.Note (Pitch Octave) dur) rest]
lima_to_barang = map (T.map_note (T.map_pitch replace))
    where
    replace p@(Pitch _ T.P1) = add_pc (-1) p
    replace p = p

-- * format

format_score :: T.Score -> ([Text], [T.Error])
format_score (T.Score toplevels) =
    Logger.runId $ concatMapM (format_toplevel . snd) toplevels

format_toplevel :: T.Toplevel -> CheckM [Text]
format_toplevel = \case
    T.ToplevelDirective (T.Directive _ key val) ->
        pure ["", mconcat $ key : maybe [] (\v -> [" = ", v]) val]
    T.BlockDefinition block -> format_block block

format_block :: T.Block -> CheckM [Text]
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
format_tracks :: T.Tracks -> CheckM [Text]
format_tracks (T.Tracks tracks) = case map T.track_tokens tracks of
    [lefts, rights] -> do
        lefts <- process_for_format bias lefts
        rights <- process_for_format bias rights
        (lefts, rights) <- normalize_hands bias lefts rights
        pure [format_tokens lefts, format_tokens rights]
    tracks -> map format_tokens <$> mapM (process_for_format bias) tracks
    where
    bias = BiasStart

process_for_format :: Bias -> [T.ParsedToken]
    -> CheckM [T.Token (T.Note (Pitch Octave) ()) T.Rest]
process_for_format bias = -- fmap lima_to_barang .
    normalize_barlines bias . resolve_pitch
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

-- * util

toEnumBounded :: forall a. (Bounded a, Enum a) => Int -> (Int, a)
toEnumBounded n = (i, toEnum r)
    where (i, r) = n `divMod` (fromEnum (maxBound :: a) + 1)
