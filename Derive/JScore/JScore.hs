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
    -- * transform
    Transform
    , convert_laras
    -- * integrate
    , convert_file
    , Meta(..)
    , make_meta
) where
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified Util.Lists as Lists
import qualified Util.Logger as Logger
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
