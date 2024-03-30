-- Copyright 2024 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
{-# LANGUAGE StrictData #-}
module Derive.JScore.Format (
    format_file
    , format_score
    , format_block
    , format_title
) where
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified Util.Lists as Lists
import qualified Util.Logger as Logger
import qualified Util.Texts as Texts

import qualified Derive.JScore.Check as Check
import qualified Derive.JScore.JScore as JScore
import qualified Derive.JScore.Parse as Parse
import qualified Derive.JScore.T as T

import           Global


type Block tracks = T.Block T.Pitch tracks
type Token pos dur rest = T.Token pos (T.Note T.Pitch dur) rest

format_file :: JScore.Transform -> FilePath -> IO ()
format_file transform = print_score transform <=< Text.IO.readFile

print_score :: JScore.Transform -> Text -> IO ()
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
    , state_transform :: JScore.Transform
    }

-- | Format a T.Score to print.  This is does some Checks but doesn't
-- normalize down to Time, because it's more like normalization than lowering.
format_score :: JScore.Transform -> T.ParsedScore -> ([Text], [T.Error])
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

transform_block :: JScore.Transform
    -> Block [[Token pos dur rest]] -> Block [[Token pos dur rest]]
transform_block trans block = block
    { T.block_gatra = trans <$> T.block_gatra block
    , T.block_tracks = map (T.map_pitch trans) (T.block_tracks block)
    }

format_block :: (pos -> Text -> Text) -> Maybe T.Irama -> Maybe T.Instrument
    -> Block [[Token pos dur T.Rest]] -> [Text]
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

format_title :: Block tracks -> Text
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

format_tokens :: (pos -> Text -> Text) -> Check.Bias -> [Token pos dur T.Rest]
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

format_token :: (pos -> Text -> Text) -> Bool -> Token pos dur T.Rest -> Text
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
