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
module Derive.TScore.Java where
import qualified Control.Monad.Identity as Identity
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Text as Text

import qualified Util.EList as EList
import qualified Util.Lists as Lists
import qualified Util.Num as Num
import qualified Util.P as P

import qualified Derive.TScore.Parse as Parse
import qualified Derive.TScore.T as T
import qualified Ui.Id as Id

import           Global


data Pitch oct = Pitch oct PitchClass
    deriving (Eq, Show)
type Octave = Int
newtype RelativeOctave = RelativeOctave Int
    deriving (Eq, Show)

data PitchClass = P1 | P2 | P3 | P4 | P5 | P6 | P7
    deriving (Eq, Ord, Show, Bounded, Enum)

pc_char :: PitchClass -> Char
pc_char = \case
    P1 -> '1'; P2 -> '2'; P3 -> '3'; P4 -> '4'; P5 -> '5'; P6 -> '6'; P7 -> '7'

parse_score :: Text -> Either String Score
parse_score = Parse.parse_text (Parse.parse Parse.default_config)

newtype Score = Score [(T.Pos, Toplevel)]
    deriving (Eq, Show)

instance Parse.Element Score where
    parse config = Parse.p_whitespace
        *> (Score <$> P.many (Parse.parse config))
    unparse config (Score toplevels) =
        Text.unlines $ map (Parse.unparse config) toplevels

data Toplevel = ToplevelDirective T.Directive | BlockDefinition Block
    deriving (Eq, Show)

instance Parse.Element Toplevel where
    parse config = Parse.lexeme $
        ToplevelDirective . un <$> Parse.parse config
        <|> BlockDefinition <$> Parse.parse config
        where un (Parse.ToplevelDirective d) = d
    unparse config = \case
        ToplevelDirective a -> Parse.unparse config (Parse.ToplevelDirective a)
        BlockDefinition a -> Parse.unparse config a

data Block = Block {
    block_name :: Text
    , block_directives :: [T.Directive]
    , block_tracks :: Tracks
    } deriving (Eq, Show)

-- | > name = directive [ tracks ]
instance Parse.Element Block where
    parse config = do
        block_name <- Parse.lexeme $ P.takeWhile1 Id.is_id_char
        Parse.keyword "="
        block_directives <- P.many (Parse.lexeme (Parse.parse config))
        block_tracks <- Parse.parse config
        return $ Block { block_name, block_directives, block_tracks }
    unparse config (Block { block_name, block_directives, block_tracks }) =
        Text.unwords $ filter (not . Text.null) $ concat
            [ [block_name, "="]
            , map (Parse.unparse config) block_directives
            , [Parse.unparse config block_tracks]
            ]

newtype Tracks = Tracks [Track]
    deriving (Eq, Show)

instance Parse.Element Tracks where
    parse config = fmap Tracks $ Parse.lexeme "[" *> tracks <* Parse.lexeme "]"
        where tracks = P.some (Parse.parse config)
    unparse config (Tracks tracks) =
        "[ " <> Text.unwords (map (Parse.unparse config) tracks) <> "]"
        -- Intentionally no space on "]", because the last note's HasSpace
        -- should put one on.

data Track = Track {
    track_tokens :: [Token]
    , track_pos :: T.Pos
    } deriving (Eq, Show)

instance Parse.Element Track where
    parse config = do
        track_pos <- Parse.get_pos
        Parse.keyword ">"
        track_tokens <- P.many $ Parse.parse config
        return $ Track { track_tokens, track_pos }
    unparse config (Track { track_tokens }) =
        -- Text.unwords $ ">" : map (Parse.unparse config) track_tokens
        -- Rely on HasSpace to preserving spacing between notes.
        mconcat $ "> " : map (Parse.unparse config) track_tokens

type Token = T.Token () (Pitch RelativeOctave) HasSpace Rest

-- | Keep track if there was whitespace after notes and rests.
-- I can use this to infer durations.
data HasSpace = HasSpace | NoSpace deriving (Eq, Show)

p_has_space :: Parse.Parser HasSpace
p_has_space = Parse.p_whitespace_ >>= pure . \case
    True -> HasSpace
    False -> NoSpace

instance Parse.Element Token where
    parse config =
        Parse.lexeme (T.TBarline <$> Parse.get_pos <*> Parse.parse config)
        <|> T.TRest <$> Parse.get_pos <*> Parse.parse config
        <|> T.TNote <$> Parse.get_pos <*> Parse.parse config
    unparse config (T.TBarline _ bar) = Parse.unparse config bar <> " "
    unparse config (T.TNote _ note) = Parse.unparse config note
    unparse config (T.TRest _ rest) = Parse.unparse config rest

data Rest = Rest {
    rest_sustain :: Bool
    , rest_space :: HasSpace
    } deriving (Eq, Show)

instance Parse.Element (T.Rest Rest) where
    parse _ = do
        rest_sustain <- P.char '.' *> pure True <|> P.char '_' *> pure False
        -- Parse.lexeme is omitted from the calling parser to support this.
        rest_space <- p_has_space
        pure $ T.Rest $ Rest { rest_sustain, rest_space }
    unparse _ (T.Rest (Rest { rest_sustain, rest_space })) =
        (if rest_sustain then "." else "_") <> case rest_space of
            NoSpace -> ""
            HasSpace -> " "

instance Parse.Element (T.Note () (Pitch RelativeOctave) HasSpace) where
    parse config = do
        note_pos <- Parse.get_pos
        note_pitch <- Parse.parse config
        note_zero_duration <- P.option False (P.char '/' *> pure True)
        -- Parse.lexeme is omitted from the calling parser to support this.
        note_duration <- p_has_space
        return $ T.Note
            { note_call = ()
            , note_pitch
            , note_zero_duration
            , note_duration
            , note_pos
            }
    unparse config (T.Note { note_pitch, note_zero_duration, note_duration }) =
        Parse.unparse config note_pitch
        <> if note_zero_duration then "/" else ""
        <> case note_duration of
            HasSpace -> " "
            NoSpace -> ""

instance Parse.Element PitchClass where
    parse _ = P.satisfy (\c -> '1' <= c && c <= '9') >>= \case
        '1' -> pure P1
        '2' -> pure P2
        '3' -> pure P3
        '4' -> pure P4
        '5' -> pure P5
        '6' -> pure P6
        '7' -> pure P7
        _ -> mzero
    unparse _ p = Text.singleton (pc_char p)

instance Parse.Element (Pitch RelativeOctave) where
    parse config = do
        oct <- Text.foldl' (\n c -> n + if c == ',' then -1 else 1) 0 <$>
            P.takeWhile (`elem` (",'" :: String))
        p <- Parse.parse config
        pure $ Pitch (RelativeOctave oct) p
    unparse config (Pitch (RelativeOctave oct) pc) =
        octs <> Parse.unparse config pc
        where
        octs = case compare oct 0 of
            EQ -> ""
            LT -> Text.replicate (- oct) ","
            GT -> Text.replicate oct "'"

-- * check

type Stream a = [EList.Elt Meta a]
type Meta = T.Error

{-
    Checks:
    - barlines correct
    - directives understood
    - within instrument range
    - left hand below right hand
-}
check :: Text -> Score -> [Text]
check source (Score toplevels) =
    map (T.show_error source) $ concatMap (top . snd) toplevels
    where
    top = \case
        ToplevelDirective {} -> []
        BlockDefinition block ->
            concatMap (check_tokens . track_tokens) tracks
            where Tracks tracks = block_tracks block

check_tokens :: [Token] -> [T.Error]
check_tokens tokens = errs
    where (errs, _) = Either.partitionEithers $ resolve_tokens tokens

resolve_tokens :: [Token]
    -> [Either Meta (T.Time, T.Note () (Pitch Octave) T.Time)]
resolve_tokens = map EList.toEither . resolve_durations . normalize_barlines
    . resolve_pitch

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

-- ** resolve_pitch

resolve_pitch
    :: [T.Token call (Pitch RelativeOctave) dur rdur]
    -> [T.Token call (Pitch Octave) dur rdur]
resolve_pitch = snd . List.mapAccumL resolve (0, Nothing)
    where
    resolve prev = \case
        T.TBarline pos bar -> (prev, T.TBarline pos bar)
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
infer_octave :: (Octave, Maybe PitchClass)
    -- -> (RelativeOctave, PitchClass) -> Pitch Octave
    -> Pitch RelativeOctave -> Pitch Octave
infer_octave (prev_oct, Nothing) (Pitch (RelativeOctave rel_oct) pc) =
    Pitch (prev_oct + rel_oct) pc
infer_octave (prev_oct, Just prev_pc) (Pitch (RelativeOctave rel_oct) pc) =
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

pitch_diff :: Pitch Octave -> Pitch Octave -> Int
pitch_diff (Pitch oct1 pc1) (Pitch oct2 pc2) =
    per_oct * (oct1 - oct2) + (fromEnum pc1 - fromEnum pc2)
    where
    per_oct = fromEnum (maxBound :: PitchClass) + 1

-- ** resolve_durations

resolve_durations :: Stream (T.Token call pitch dur Rest)
    -> Stream (T.Time, T.Note call pitch T.Time)
resolve_durations =
    EList.catMaybes . snd . EList.mapAccumL resolve_rests 0 . EList.zipNexts
    . concatMap resolve . EList.split is_barline
    where
    resolve bar = EList.mapMaybe (add_dur len) bar
        where len = length (EList.elts bar)
    add_dur len = \case
        T.TBarline {} -> Nothing
        T.TRest _ rest -> Just (dur, Left rest)
        T.TNote _ note -> Just (dur, Right note)
        where dur = T.Time (1 / fromIntegral len)
    is_barline (T.TBarline {}) = True
    is_barline _ = False
    resolve_rests time ((dur, Left _), _) = (time + dur, Nothing)
    resolve_rests time ((dur, Right n), nexts) =
        ( time + dur
        , Just (time, n { T.note_duration = dur + sustain })
        )
        where
        sustain = Num.sum $ map fst $ takeWhile is_sustain nexts
        is_sustain (_, Left (T.Rest (Rest { rest_sustain }))) = rest_sustain
        is_sustain _ = False

-- | Verify bar durations, infer rests if necessary.  After this, all bars
-- should be a power of 2.
normalize_barlines :: [T.Token call pitch HasSpace Rest]
    -> Stream (T.Token call pitch () Rest)
normalize_barlines =
    concat
    . Lists.mapTail (EList.Elt barline :) . map resolve_bar . group
    . Lists.splitWith is_barline

    -- join_bars
    -- . map (second resolve_bar)
    -- . split_bars
    where
    barline = T.TBarline T.fake_pos (T.Barline 1)
    is_barline (T.TBarline _ bar) = Just bar
    is_barline _ = Nothing
    -- TODO warn about unsupported Barlines
    group (g0, gs) = g0 : map snd gs

{-
-- Split on barlines, zipPadded, map across them.
normalize_hands :: [T.Token call pitch dur Rest]
    -> [T.Token call pitch dur Rest]
    -> (Stream (T.Token call pitch dur Rest),
        Stream (T.Token call pitch dur Rest))
normalize_hands lefts rights =
    bimap add_barlines add_barlines $
    unzip $ map normalize $
    Lists.zipPadded (split_bars lefts) (split_bars rights)
    where
    normalize = \case
        Lists.First (pos, lefts) ->
            (map EList.Elt lefts, [merror pos "left hand with no right hand"])
        Lists.Second (pos, rights) ->
            ([merror pos "right hand with no left hand"], map EList.Elt rights)
        -- Lists.Both (pos0, lefts) (pos1, rights) ->
        --     case (-) <$> log2 (length lefts) <*> log2 (length rights) of
        --         -- shouldn't happen if normalize_barlines was called
        --         Nothing -> (lefts, rights)
        --         Just delta
        --             | delta >= 0 -> (lefts, expand delta rights)
        --             | otherwise -> (expand delta lefts, rights)
    expand delta = concatMap (: replicate (delta^2) rest)
    rest = T.TRest T.fake_pos (T.Rest (Rest True NoSpace))

    add_barlines = concat . Lists.mapTail (EList.Elt barline :)
    barline = T.TBarline T.fake_pos (T.Barline 1)

log2 :: Int -> Maybe Int
log2 n
    | frac == 0 = Just i
    | otherwise = Nothing
    where (i, frac) = properFraction $ logBase 2 (fromIntegral n)
-}

merror :: T.Pos -> Text -> EList.Elt T.Error a
merror pos msg = EList.Meta $ T.Error pos msg

-- split_bars :: Stream (T.Token call pitch ndur rdur)
--     -> [(T.Pos, Stream (T.Token call pitch ndur rdur))]
-- split_bars = undefined

split_bars :: [T.Token call pitch ndur rdur]
    -> [(T.Pos, [T.Token call pitch ndur rdur])]
split_bars [] = []
split_bars tokens@(t0 : _) = (T.token_pos t0, group0) : groups
    where
    (group0, groups) = Lists.splitWith is_barline tokens
    is_barline (T.TBarline pos _) = Just pos
    is_barline _ = Nothing

-- join_bars :: [(T.Pos, Stream (T.Token call pitch ndur rdur))]
--     -> Stream (T.Token call pitch ndur rdur)
-- join_bars = undefined

join_bars :: [(T.Pos, [T.Token call pitch ndur rdur])]
    -> [T.Token call pitch ndur rdur]
join_bars [] = []
join_bars ((_, g0) : gs) = concat $ g0 : map join gs
    where
    join (pos, tokens) = T.TBarline pos (T.Barline 1) : tokens

-- TODO for format, I want to leave in TBarline
-- But, should be no barline in input.  Don't need dur.
resolve_bar :: [T.Token call pitch HasSpace Rest]
    -> Stream (T.Token call pitch () Rest)
resolve_bar [] = []
resolve_bar group@(t : _)
    | power_of_2 (length group) = map EList.Elt $ map strip group
    | power_of_2 (length inferred) = map EList.Elt $ map strip inferred
    | otherwise = (:[]) $ merror (T.token_pos t) $
        "group not a power of 2: " <> showt (length group)
        <> ", with inferred rests: " <> showt (length inferred)
    where
    inferred = infer_rests group
    strip = Identity.runIdentity . T.map_note_duration (\_ -> pure ())

power_of_2 :: Int -> Bool
power_of_2 n = snd (properFraction (logBase 2 (fromIntegral n))) == 0

-- TODO should be no TBarline, put in the type?
infer_rests :: [T.Token call pitch HasSpace Rest]
    -> [T.Token call pitch HasSpace Rest]
infer_rests = concatMap infer . Lists.splitAfter has_space
    where
    infer tokens
        | even (length tokens) = tokens
        | otherwise = tokens ++ [extra_rest]
    extra_rest = T.TRest T.fake_pos $ T.Rest $ Rest True HasSpace
    has_space = \case
        T.TBarline {} -> True
        T.TNote _ note -> T.note_duration note == HasSpace
        T.TRest _ (T.Rest rest) -> rest_space rest == HasSpace

-- ** check directives

-- variations: append 123567 for e.g. gantung 2, cilik, kecil, gede, besar,
-- kempyung / gembyang
standardNames :: [(Text, Text)]
standardNames =
    [ ("ayu kuning", "ak")
    , ("debyang debyung", "dd")
    , ("dualolo", "dll")
    , ("duduk", "dd")
    , ("gantung", "gant")
    , ("gelut", "g")
    , ("jarik kawung", "jk")
    , ("kacaryan", "kc")
    , ("kutuk kuning", "kk")
    , ("puthut semedi", "ps")
    , ("puthut", "p") -- 2 part pattern puthut gelut
    , ("tumurun", "tm")
    ]


-- * format

format_score :: Score -> [Text]
format_score (Score toplevels) = concatMap (format_toplevel . snd) toplevels

format_toplevel :: Toplevel -> [Text]
format_toplevel = \case
    ToplevelDirective (T.Directive _ key val) ->
        ["", mconcat $ key : maybe [] (\v -> [" = ", v]) val]
    BlockDefinition block -> format_block block

format_block :: Block -> [Text]
format_block block =
    block_name block
        : map ("    " <>) (format_tracks source (block_tracks block))
    where source = "<<saurcy>>"
    -- If I did a 'check' first, these should not happen.

format_tracks :: Text -> Tracks -> [Text]
format_tracks source (Tracks tracks) = map (fmt . track_tokens) tracks
    where
    fmt = mconcat . map (EList.either (T.show_error source) format_token)
        . process_for_format

process_for_format :: [Token]
    -> Stream (T.Token () (Pitch Octave) () Rest)
process_for_format = normalize_barlines . resolve_pitch
    -- This doesn't do 'resolve_durations', because I want the original rests.
    -- TODO normalize between hands so each group is the same length

format_token :: T.Token call (Pitch Octave) dur rdur -> Text
format_token = \case
    T.TBarline {} -> " | "
    -- T.TBarline {} -> " " <> vertical_line <> " "
    T.TRest {} -> "."
    T.TNote _ n -> format_pitch (T.note_pitch n)
        <> if T.note_zero_duration n then slash else ""
    where
    -- vertical_line = "\x007c" -- VERTICAL LINE
    slash = if use_slash
        then "\x0338" -- COMBINING LONG SOLIDUS OVERLAY
        else "\x0336" -- COMBINING LONG STROKE OVERLAY
    use_slash = True

format_pitch :: Pitch Octave -> Text
format_pitch (Pitch oct pc) = Text.cons (pc_char pc) dots
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
