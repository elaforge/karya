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
    , integrate_file
    , integrate
) where
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified Util.Logger as Logger
import qualified Cmd.Integrate.Convert as Convert
import qualified Cmd.Integrate.Manual as Manual
import qualified Cmd.Ruler.Gong as Gong

import qualified Derive.JScore.Check as Check
import qualified Derive.JScore.Parse as Parse
import qualified Derive.JScore.T as T

import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Id as Id
import qualified Ui.Meter.Meter as Meter
import qualified Ui.Ruler as Ruler
import qualified Ui.Ui as Ui

import           Global
import           Types


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

data Block = Block {
    block_name :: Text
    , block_tracks :: Convert.Tracks
    } deriving (Show, Eq)

type Error = Text

source_key :: Block.SourceKey
source_key = "jscore"

integrate_file :: (MonadIO m, Ui.M m) => FilePath -> m ([BlockId], [Text])
integrate_file = integrate <=< liftIO . Text.IO.readFile

integrate :: Ui.M m => Text -> m ([BlockId], [Text])
integrate source = do
    ns <- Ui.get_namespace
    let (blocks, errors) = convert_source source
    -- These blocks should all be 1 gatra.
    gatra_ruler <- get_ruler (Id.id ns "gatra") Gong.gatra
    (, errors) <$> mapMaybeM (integrate_block gatra_ruler) blocks

get_ruler :: Ui.M m => Id.Id -> Meter.Meter -> m RulerId
get_ruler ruler_id meter = Ui.lookup_ruler (Id.RulerId ruler_id) >>= \case
    Just _ -> pure (Id.RulerId ruler_id)
    Nothing -> Ui.create_ruler ruler_id (Ruler.meter_ruler meter)

integrate_block :: Ui.M m => RulerId -> Block -> m (Maybe BlockId)
integrate_block ruler_id (Block { block_name, block_tracks }) = do
    ns <- Ui.get_namespace
    let block_id = Id.make_unchecked $ Id.id ns block_name
    Manual.block source_key block_id ruler_id block_title block_tracks
    where
    block_title = ""

t0 = _print_integrate "short.jscore"

-- | Show results of integration from ghci.
_print_integrate :: FilePath -> IO ()
_print_integrate fname = do
    source <- Text.IO.readFile fname
    let (blocks, errors) = convert_source source
    mapM_ Text.IO.putStrLn errors
    mapM_ Text.IO.putStrLn $ List.intercalate [""] $ map pp_block blocks
    where
    pp_block (Block name tracks) = name <> ":" : concatMap pp_tracks tracks
    pp_tracks (note, controls) = pp_track note : map pp_track controls
    pp_track (Convert.Track title events) = title <> ": "
        <> Text.unwords (map pp_event events)
    pp_event e = pretty (Event.start e)
        <> (if Event.duration e == 0 then "" else "~" <> pretty (Event.end e))
        <> (if Event.text e == "" then "" else "(" <> Event.text e <> ")")

convert_source :: Text -> ([Block], [Error])
convert_source source = case Parse.parse_score source of
    Left err -> ([], [err])
    Right score -> second (map (T.show_error source)) $ convert_score score

convert_score :: T.ParsedScore -> ([Block], [T.Error])
convert_score score = (map (uncurry convert_block) meta_blocks, warnings)
    where
    (meta_blocks, warnings) = Logger.runId $
        Check.for_integrate Check.BiasEnd score

convert_block :: Check.Meta -> T.Block T.Pitch [[Check.Event]] -> Block
convert_block (Check.Meta { m_irama, m_instrument }) block = Block
    { block_name = Text.intercalate "-" $
        irama_prefix m_irama : instrument_prefix m_instrument
        : T.block_names block
    , block_tracks = map (convert_track m_instrument) $
        zip hands (reverse (T.block_tracks block))
    }
    where
    hands
        | length (T.block_tracks block) == 2 = [Just "l", Just "r"]
        | otherwise = repeat Nothing

convert_track :: T.Instrument -> (Maybe Text, [Check.Event])
    -> (Convert.Track, [Convert.Track])
convert_track inst (mb_hand, events) =
    ( Convert.Track
        { track_title = ">" <> instrument_prefix inst
            <> maybe "" (" | hand="<>)  mb_hand
        , track_events = notes
        }
    , (:[]) $ Convert.Track
        { track_title = "*" -- pitch
        , track_events = pitches
        }
    )
    where (notes, pitches) = unzip $ map to_events events

to_events :: (T.Time, T.Note T.Pitch T.Time) -> (Event.Event, Event.Event)
to_events (start, note) =
    ( Event.event (track_time start) (track_time (T.note_duration note))
        (if T.note_zero_duration note then "/" else "")
    , Event.event (track_time start) 0 (convert_pitch (T.note_pitch note))
    )

convert_pitch :: T.Pitch -> Text
convert_pitch (T.Pitch oct pc) = showt oct <> Text.singleton (T.pc_char pc)

track_time :: T.Time -> TrackTime
track_time = realToFrac

irama_prefix :: T.Irama -> Text
irama_prefix = \case
    T.Lancar -> "l"
    T.Tanggung -> "t"
    T.Dadi -> "d"
    T.Wiled -> "w"
    T.Rangkep -> "r"

instrument_prefix :: T.Instrument -> Text
instrument_prefix = \case
    T.GenderBarung -> "gb"
    T.GenderPanerus -> "gp"
    T.Siter -> "si"
