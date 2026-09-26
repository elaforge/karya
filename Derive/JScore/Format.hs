-- Copyright 2024 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
{-# LANGUAGE StrictData #-}
module Derive.JScore.Format (
    formatFile
    , formatScore
    , formatBlock
    , formatTitle
    , formatGatra
    , formatBalungan

    , transformBlock
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


type Block tracks = T.Block T.Pitch tracks
type Token pos dur rest = T.Token pos (T.Note T.Pitch dur) rest

formatFile :: Check.Transform -> FilePath -> IO ()
formatFile transform = printScore transform <=< Text.IO.readFile

printScore :: Check.Transform -> Text -> IO ()
printScore transform source = case Parse.parseScore source of
    Left err -> Text.IO.putStrLn err
    Right score -> do
        mapM_ Text.IO.putStrLn lines
        mapM_ (Text.IO.putStrLn . T.showError source) errs
        where (lines, errs) = formatScore transform score
        -- TODO interleave the errs with the lines

data FormatState = FormatState
    { stateMetas :: [T.Meta]
    , statePrevIsBlock :: Bool
    , stateTransform :: Check.Transform
    }

-- | Format a T.Score to print.  This is does some Checks but doesn't
-- normalize down to Time, because it's more like normalization than lowering.
formatScore :: Check.Transform -> T.ParsedScore -> ([Text], [T.Error])
formatScore transform score =
    ( concat $ snd $ List.mapAccumL formatToplevel state (map snd toplevels)
    , errs
    )
    where
    state = FormatState [] False transform
    (T.Score toplevels, errs) = Logger.runId $ Check.formatScore score

formatToplevel :: FormatState -> T.Toplevel Check.Block
    -> (FormatState, [Text])
formatToplevel state = \case
    T.ToplevelMeta m ->
        ( state
            { stateMetas = m : stateMetas state, statePrevIsBlock = False }
        , (if statePrevIsBlock state then [""] else []) ++ [formatMeta m]
        )
    T.BlockDefinition block ->
        ( state { statePrevIsBlock = True }
        , formatTitleBlock state block
        )

formatTitleBlock :: FormatState -> Check.Block -> [Text]
formatTitleBlock state block =
    formatTitle block : formatBlock (const id) irama inst
        (transformBlock (stateTransform state) block)
    where
    irama = Lists.head [i | T.Irama i <- metas]
    inst = Lists.head [i | T.Instrument i <- metas]
    metas = stateMetas state

transformBlock :: Check.Transform -> Block [[Token pos dur rest]]
    -> Block [[Token pos dur rest]]
transformBlock trans block = block
    { T.blockGatra = trans <$> T.blockGatra block
    , T.blockTracks = map (T.mapPitch trans) (T.blockTracks block)
    }

formatBlock :: (pos -> Text -> Text) -> Maybe T.Irama -> Maybe T.Instrument
    -> Block [[Token pos dur T.Rest]] -> [Text]
formatBlock fmtPos irama inst block =
    map (("    "<>) . formatTokens fmtPos bias) (T.blockTracks block)
    where
    -- This actually corresponds to parts which are written with every beat
    -- and which ones use overbars.  Or could say that the basic speed for
    -- GenderBarung < Wiled is 4 per bar, while the rest are 8.
    -- TODO maybe there's a more direct way?
    bias
        | inst == Just T.GenderBarung && irama >= Just T.Wiled = Check.BiasEnd
        | inst == Just T.GenderPanerus && irama >= Just T.Dadi = Check.BiasEnd
        | otherwise = Check.BiasStart

formatTitle :: Block tracks -> Text
formatTitle block =
    Texts.join2 " " (formatGatra blockGatra) (Text.unwords blockNames)
    <> if null blockInferred then ""
        else " [" <> Text.unwords blockInferred <> "]"
    where
    T.Block { blockGatra, blockNames, blockInferred } = block

formatMeta :: T.Meta -> Text
formatMeta = Text.drop 1 . Parse.unparse
    -- Input syntax uses a leading %.

formatGatra :: T.Gatra T.Pitch -> Text
formatGatra = mconcatMap formatBalungan . T.gatraNotes

formatBalungan :: T.Balungan T.Pitch -> Text
formatBalungan (T.Balungan (Just (T.Pitch oct pc)) (Just T.Gong))
    -- The hardcoded circled digit looks better than the combining enclosing
    -- circle.
    | oct == 0 = Text.singleton $ toEnum (circledDigitOne + fromEnum pc)
    where circledDigitOne = 0x2460
formatBalungan (T.Balungan pitch annot) = mconcat
    [ maybe "." formatPitch pitch
    , case annot of
        Nothing -> ""
        Just T.Gong -> Text.singleton '\x20dd' -- COMBINING ENCLOSING CIRCLE
        Just T.Kenong -> Text.singleton '\x0302' -- COMBINING CIRCUMFLEX ACCENT
    ]

formatTokens :: (pos -> Text -> Text) -> Check.Bias -> [Token pos dur T.Rest]
    -> Text
formatTokens fmtPos bias = mconcat . go
    where
    go ts = zipWith (formatToken fmtPos) beats pre ++ case post of
        [] -> []
        bar : post -> formatToken fmtPos True bar : go post
        where
        beats
            | length pre >= 8 = cycle $ case bias of
                Check.BiasStart -> [True, False]
                Check.BiasEnd -> [False, True]
            | otherwise = repeat True
        (pre, post) = break isBarline ts
    isBarline (T.TBarline {}) = True
    isBarline _ = False

formatToken :: (pos -> Text -> Text) -> Bool -> Token pos dur T.Rest -> Text
formatToken fmtPos onBeat = \case
    T.TBarline {} -> " | "
    -- T.TBarline {} -> " " <> vertical_line <> " "
    T.TRest pos (T.Rest { restSustain })
        | onBeat -> fmtPos pos $ if restSustain then "." else "_"
        | otherwise -> " "
    T.TNote pos n -> fmtPos pos $ formatPitch (T.notePitch n)
        <> if T.noteZeroDuration n then slash else ""
    where
    -- vertical_line = "\x007c" -- VERTICAL LINE
    slash = if useSlash
        then "\x0338" -- COMBINING LONG SOLIDUS OVERLAY
        else "\x0336" -- COMBINING LONG STROKE OVERLAY
    useSlash = True

formatPitch :: T.Pitch -> Text
formatPitch (T.Pitch oct pc) = Text.cons (T.pcChar pc) dots
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
