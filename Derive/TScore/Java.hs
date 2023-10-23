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
        "[ " <> Text.unwords (map (Parse.unparse config) tracks) <> " ]"

data Track = Track {
    track_tokens :: [Token]
    , track_pos :: T.Pos
    } deriving (Eq, Show)

instance Parse.Element Track where
    parse config = do
        track_pos <- Parse.get_pos
        Parse.keyword ">"
        track_tokens <- P.many $ Parse.lexeme $ Parse.parse config
        return $ Track { track_tokens, track_pos }
    unparse config (Track { track_tokens }) =
        Text.unwords $ ">" : map (Parse.unparse config) track_tokens
        -- If I do this, barlines also get no spaces.
        -- mconcat $ "> " : map (Parse.unparse config) track_tokens

type Token = T.Token () (Pitch RelativeOctave) () Rest

instance Parse.Element Token where
    parse config =
        T.TBarline <$> Parse.get_pos <*> Parse.parse config
        <|> T.TRest <$> Parse.get_pos <*> Parse.parse config
        <|> T.TNote <$> Parse.get_pos <*> Parse.parse config
    unparse config (T.TBarline _ bar) = Parse.unparse config bar
    unparse config (T.TNote _ note) = Parse.unparse config note
    unparse config (T.TRest _ rest) = Parse.unparse config rest

data Rest = RestSustain | RestStop
    deriving (Eq, Show)

instance Parse.Element (T.Rest Rest) where
    parse _ = fmap T.Rest $
        P.char '.' *> pure RestSustain <|> P.char '_' *> pure RestStop
    unparse _ (T.Rest r) = case r of
        RestSustain -> "."
        RestStop -> "_"

instance Parse.Element (T.Note () (Pitch RelativeOctave) ()) where
    parse config = do
        pos <- Parse.get_pos
        pitch <- Parse.parse config
        note_zero_duration <- P.option False (P.char '/' *> pure True)
        return $ T.Note
            { note_call = ()
            , note_pitch = pitch
            , note_zero_duration
            , note_duration = ()
            , note_pos = pos
            }
    unparse config (T.Note { note_pitch, note_zero_duration }) =
        Parse.unparse config note_pitch
        <> if note_zero_duration then "/" else ""

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
resolve_tokens = map EList.toEither . resolve_duration . resolve_pitch

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

-- ** resolve_duration

-- | Split on barlines.
-- Error if notes not a power of 2.
-- Add duration based on power of 2.
-- Resolve note start and end times.
resolve_duration :: [T.Token call pitch dur Rest]
    -> Stream (T.Time, T.Note call pitch T.Time)
resolve_duration =
    EList.catMaybes . snd . EList.mapAccumL resolve_rests 0 . EList.zipNexts
    . concatMap resolve_barlines . group
    . Lists.splitWith is_barline
    where
    is_barline (T.TBarline _ bar) = Just bar
    is_barline _ = Nothing
    -- TODO warn about unsupported Barlines
    group (g0, gs) = g0 : map snd gs
    resolve_barlines [] = []
    resolve_barlines group@(t : _)
        | power_of_2 len = map EList.Elt $
            mapMaybe (add_dur (T.Time (1 / fromIntegral len))) group
        | otherwise = (:[]) $ EList.Meta $ T.Error (T.token_pos t) $
            "group not a power of 2: " <> showt len
        where len = length group
    add_dur dur = \case
        T.TBarline {} -> Nothing
        T.TRest _ rest -> Just (dur, Left rest)
        T.TNote _ note -> Just (dur, Right note)
    resolve_rests time ((dur, Left _), _) = (time + dur, Nothing)
    resolve_rests time ((dur, Right n), nexts) =
        ( time + dur
        , Just (time, n { T.note_duration = dur + sustain })
        )
        where
        sustain = Num.sum $ map fst $ takeWhile is_sustain nexts
        is_sustain (_, Left (T.Rest RestSustain)) = True
        is_sustain _ = False

power_of_2 :: Int -> Bool
power_of_2 n = snd (properFraction (logBase 2 (fromIntegral n))) == 0

-- ** check directives

-- variations: append 123567 for e.g. gantung 2, cilik, kecil, gede, besar,
-- kempyung / gembyang
standardNames :: [(Text, Text)]
standardNames =
    [ ("ayu kuning", "ak")
    , ("debyang debyung", "dby")
    , ("dualolo", "dll")
    , ("duduk", "ddk")
    , ("gantung", "gant")
    , ("gelut", "g")
    , ("jarik kawung", "jk")
    , ("kacaryan", "kcy")
    , ("kutuk kuning", "kk")
    , ("puthut semedi", "ps")
    , ("puthut", "p") -- 2 part pattern puthut gelut
    , ("tumurun", "tm")
    ]

-- | Valid directives at the score level.
score_tags :: [Text]
score_tags = ["source", "piece", "pathet", "inst", "section", "irama"]

-- | Valid directives at the block level.
block_tags :: [Text]
block_tags = ["gatra"]

-- | Valid values for %irama tag.  Along with %inst, affects expected
-- number of notes per barline.
irama :: [Text]
irama = ["lancar", "tanggung", "dadi", "wiled", "rangkep"]


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
    block_name block : map ("    " <>) (format_tracks (block_tracks block))

format_tracks :: Tracks -> [Text]
format_tracks (Tracks tracks) = map (fmt . track_tokens) tracks
    where fmt = format_tokens . resolve_pitch

format_tokens :: [T.Token call (Pitch Octave) dur rdur] -> Text
format_tokens = mconcat . map format
    where
    format = \case
        T.TBarline {} -> " | "
        -- T.TBarline {} -> " " <> vertical_line <> " "
        T.TRest {} -> "."
        T.TNote _ n -> format_pitch (T.note_pitch n)
            <> if T.note_zero_duration n then slash else ""
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
