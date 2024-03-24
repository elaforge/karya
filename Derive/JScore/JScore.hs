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
module Derive.JScore.JScore (
    -- * integrate
    convert_file
    -- * format
    , format_file
    , format_score
    , format_block
    , format_title
    -- * Transform
    , Transform
    , convert_laras
    , transform_block
) where
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified Util.Lists as Lists
import qualified Util.Logger as Logger
import qualified Util.Texts as Texts

import qualified Derive.JScore.Check as Check
import qualified Derive.JScore.Parse as Parse
import qualified Derive.JScore.T as T

import           Global


-- * transform

type Transform = T.Pitch -> T.Pitch

convert_laras :: T.Laras -> T.Laras -> Maybe Transform
convert_laras a b = case (a, b) of
    _ | a == b -> Just id
    (T.PelogLima, T.PelogBarang) -> Just one_to_seven
    (T.PelogBarang, T.PelogLima) -> Just seven_to_one
    (T.SlendroManyura, T.PelogBarang) -> Just one_to_seven
    (T.PelogBarang, T.SlendroManyura) -> Just seven_to_one

    -- slendro-manyura -> pelog-barang -> pelog-lima
    (T.SlendroManyura, T.PelogLima) -> Just id
    (T.PelogLima, T.SlendroManyura) -> Just id

    -- TODO pelog-nem to lima by transposing down one, not sure.
    -- can I go lima to num by going back up one?
    (T.PelogNem, T.PelogLima) -> Just $ T.add_pc T.PelogLima (-1)
    -- TODO maybe?
    (T.SlendroManyura, T.SlendroSanga) -> Just $ T.add_pc T.SlendroSanga (-1)
    (T.SlendroSanga, T.PelogNem) -> Just id -- TODO maybe?
    _ -> Nothing
    where
    one_to_seven p@(T.Pitch _ T.P1) = T.add_pc_abs (-1) p
    one_to_seven p = p
    seven_to_one p@(T.Pitch _ T.P7) = T.add_pc_abs 1 p
    seven_to_one p = p

-- * integrate

type Event = (T.Time, T.Note T.Pitch T.Time)
type Error = Text

-- t0 = convert_file "Example/tscore/java/pangkur.tscore"

convert_file :: FilePath -> IO ()
convert_file fname = do
    source <- Text.IO.readFile fname
    case convert_source source of
        Left errs -> error $ show errs
        Right lines -> mapM_ Text.IO.putStrLn lines

convert_source :: Text -> Either [T.Error] [Text]
convert_source source = do
    score <- first ((:[]) . T.Error T.fake_pos) $ Parse.parse_score source
    blocks <- convert_score score
    pure $ map format_block blocks
    where
    format_block b = Text.unlines $ block_name meta b
        : map (Text.unwords . map (pretty . convert_event)) tracks
        where
        (meta, tracks) = T.block_tracks b
    block_name meta b =
        Text.intercalate "-" (Parse.unparse (T.block_gatra b) : T.block_names b)
        <> " " <> showt (m_instrument meta)

convert_event :: Event -> (T.Time, T.Time, Text)
convert_event (start, T.Note pitch zero dur) =
    ( start
    , dur
    , pretty pitch <> if zero then "/" else ""
    )

convert_score :: T.ParsedScore
    -> Either [T.Error] [T.Block T.ParsedPitch (Meta, [[Event]])]
convert_score score = do
    T.Score score <- first ((:[]) . T.Error T.fake_pos) $ resolve_blocks score
    let blocks =
            [ b { T.block_tracks = (meta, tracks) }
            | b@(T.Block { T.block_tracks = (Just meta, tracks) })
                <- collect_metas (map snd score)
            ]
    mapM convert_block blocks

resolve_blocks :: T.ParsedScore
    -> Either Error (T.Score (T.Block T.ParsedPitch T.Tracks))
resolve_blocks = traverse resolve
    where
    resolve block = case T.block_tracks block of
        Just tracks -> Right $ block { T.block_tracks = tracks }
        -- TODO actually resolve it
        Nothing -> Right $ block { T.block_tracks = T.Tracks [] }

convert_block :: T.Block pitch (Meta, T.Tracks)
    -> Either [T.Error] (T.Block pitch (Meta, [[Event]]))
convert_block block
    | null warnings = Right $ block
        { T.block_tracks =
            (meta, map (map (second add_oct . stretch_event stretch)) events)
        }
    | otherwise = Left warnings
    where
    (meta, T.Tracks tracks) = T.block_tracks block
    (events, warnings) = fmap concat $ unzip $ map resolve tracks
    add_oct n = n { T.note_pitch = T.add_oct oct (T.note_pitch n) }
    -- *4 so gatra=4t
    stretch = (4*) $ recip $ fromIntegral $
        Check.irama_divisor (m_irama meta)
        * Check.instrument_multiplier (m_instrument meta)
    oct = Check.instrument_octave (m_instrument meta)
    resolve = Logger.runId . Check.resolve_tokens Check.BiasEnd
        . T.track_tokens

stretch_event :: T.Time -> Event -> Event
stretch_event stretch (start, note) =
    ( stretch * start
    , note { T.note_duration = stretch * (T.note_duration note) }
    )

collect_metas :: [T.Toplevel (T.Block pitch tracks)]
    -> [T.Block pitch (Maybe Meta, tracks)]
collect_metas = go []
    where
    go metas = \case
        [] -> []
        T.ToplevelMeta meta : toplevels -> go (meta : metas) toplevels
        T.BlockDefinition block : toplevels ->
            block { T.block_tracks = (make_meta metas, T.block_tracks block) }
            : go metas toplevels

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

-- * format

format_file :: Transform -> FilePath -> IO ()
format_file transform = print_score transform <=< Text.IO.readFile

print_score :: Transform -> Text -> IO ()
print_score transform source = case Parse.parse_score source of
    Left err -> Text.IO.putStrLn err
    Right score -> do
        mapM_ Text.IO.putStrLn lines
        mapM_ (Text.IO.putStrLn . T.show_error source) errs
        where (lines, errs) = format_score transform score
        -- TODO interleave the errs with the lines

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
        , format_title_block state block
        )

format_title_block :: FormatState -> Check.Block -> [Text]
format_title_block state block =
    format_title block : format_block (const id) irama inst
        (transform_block (state_transform state) block)
    where
    irama = Lists.head [i | T.Irama i <- metas]
    inst = Lists.head [i | T.Instrument i <- metas]
    metas = state_metas state

type Block tracks = T.Block T.Pitch tracks
type Token pos dur rest = T.Token pos (T.Note T.Pitch dur) rest

transform_block :: Transform
    -> Block [[Token pos dur rest]] -> Block [[Token pos dur rest]]
transform_block trans block = block
    { T.block_gatra = trans <$> T.block_gatra block
    , T.block_tracks = map (T.map_pitch trans) (T.block_tracks block)
    }

format_block :: (pos -> Text -> Text) -> Maybe T.Irama -> Maybe T.Instrument
    -> T.Block T.Pitch [[T.Token pos (T.Note T.Pitch dur) T.Rest]]
    -> [Text]
format_block fmt_pos irama inst block =
    map (("    "<>) . format_tokens fmt_pos bias) (T.block_tracks block)
    where
    -- This actually corresponds to parts which are written with every beat
    -- and which ones use overbars.  Or could say that the basic speed for
    -- GenderBarung < Wiled is 4 per bar, while the rest are 8.
    -- TODO maybe there's a more direct way?
    bias
        | inst == Just T.GenderBarung && irama >= Just T.Wiled = Check.BiasEnd
        | inst == Just T.GenderPanerus && irama >= Just T.Dadi = Check.BiasEnd
        | otherwise = Check.BiasStart

format_title :: T.Block T.Pitch tracks -> Text
format_title block =
    Texts.join2 " " (format_gatra block_gatra) (Text.unwords block_names)
    <> if null block_inferred then ""
        else " [" <> Text.unwords block_inferred <> "]"
    where
    T.Block { block_gatra, block_names, block_inferred } = block

format_meta :: T.Meta -> Text
format_meta = Text.drop 1 . Parse.unparse
    -- Input syntax uses a leading %.

format_gatra :: T.Gatra T.Pitch -> Text
format_gatra (T.Gatra n1 n2 n3 n4) =
    mconcatMap format_balungan [n1, n2, n3, n4]

format_balungan :: T.Balungan T.Pitch -> Text
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

format_tokens :: (pos -> Text -> Text) -> Check.Bias
    -> [T.Token pos (T.Note T.Pitch dur) T.Rest]
    -> Text
format_tokens fmt_pos bias = mconcat . go
    where
    go ts = zipWith (format_token fmt_pos) beats pre ++ case post of
        [] -> []
        bar : post -> format_token fmt_pos True bar : go post
        where
        beats
            | length pre >= 8 = cycle $ case bias of
                Check.BiasStart -> [True, False]
                Check.BiasEnd -> [False, True]
            | otherwise = repeat True
        (pre, post) = break is_barline ts
    is_barline (T.TBarline {}) = True
    is_barline _ = False

format_token :: (pos -> Text -> Text) -> Bool
    -> T.Token pos (T.Note T.Pitch dur) T.Rest
    -> Text
format_token fmt_pos on_beat = \case
    T.TBarline {} -> " | "
    -- T.TBarline {} -> " " <> vertical_line <> " "
    T.TRest pos (T.Rest { rest_sustain })
        | on_beat -> fmt_pos pos $ if rest_sustain then "." else "_"
        | otherwise -> " "
    T.TNote pos n -> fmt_pos pos $ format_pitch (T.note_pitch n)
        <> if T.note_zero_duration n then slash else ""
    where
    -- vertical_line = "\x007c" -- VERTICAL LINE
    slash = if use_slash
        then "\x0338" -- COMBINING LONG SOLIDUS OVERLAY
        else "\x0336" -- COMBINING LONG STROKE OVERLAY
    use_slash = True

format_pitch :: T.Pitch -> Text
format_pitch (T.Pitch oct pc) = Text.cons (T.pc_char pc) dots
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
