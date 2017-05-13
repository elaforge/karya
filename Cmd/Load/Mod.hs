-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Load mod dumps from hacked up xmp.
module Cmd.Load.Mod where
import qualified Control.Applicative as A (many)
import qualified Data.Attoparsec.Text as A
import qualified Data.Bits as Bits
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text.IO as Text.IO

import qualified Util.Map as Map
import qualified Util.ParseText as ParseText
import qualified Util.Seq as Seq
import qualified Util.Then as Then

import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.Ui as Ui
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree

import qualified Cmd.BlockConfig as BlockConfig
import qualified Cmd.Create as Create
import qualified Cmd.Ruler.Meter as Meter
import qualified Cmd.Ruler.Meters as Meters
import qualified Cmd.Ruler.RulerUtil as RulerUtil

import qualified Derive.ParseSkeleton as ParseSkeleton
import qualified Derive.ShowVal as ShowVal
import Global
import Types


-- not implemented:
-- - initial tempo and tempo changes, along with frames / row
-- - instruments (one instrument is hardcoded)
-- - most effects
-- - order list, so repeats are expanded

-- * create ui state

create :: Ui.M m => Id.Namespace -> [UiBlock] -> m ()
create name ui_blocks = do
    Ui.set_namespace name
    let mkid = Id.id name
    rid <- Create.ruler "meter44" $
        RulerUtil.meter_ruler Meter.default_config 16 (replicate 4 Meters.m44_4)
    block_ids <- zipWithM (create_block mkid rid "") [0..] ui_blocks
    root <- create_order_block mkid block_ids
    Ui.set_root_id root
    Create.unfitted_view root
    return ()

create_block :: Ui.M m => (Text -> Id.Id) -> RulerId
    -> String -> Int -> UiBlock -> m (BlockId, BlockRows)
create_block mkid rid inst num (ui_block, block_rows) = do
    block_id <- make_block mkid rid ("b" <> showt num)
        (concatMap mktrack ui_block)
    return (block_id, block_rows)
    where mktrack (ntrack, ctracks) = ('>' : inst, ntrack) : ctracks

create_order_block :: Ui.M m => (Text -> Id.Id)
    -> [(BlockId, BlockRows)] -> m BlockId
create_order_block mkid block_ids = do
    rid <- Create.ruler "order" $ Meters.ruler (order_meter block_rows)
    make_block mkid rid "order" [("tempo", tempo), (">pianoteq/c1", events)]
    where
    block_rows = map snd block_ids
    tempo = [Event.event 0 0 "6"]
    starts = scanl (+) 0 block_rows
    events =
        [ Event.event (fromIntegral start) (fromIntegral dur)
            (Id.ident_name bid)
        | (start, (bid, dur)) <- zip starts block_ids
        ]

order_meter :: [BlockRows] -> Ruler.Marklist
order_meter =
    Meter.meter_marklist Meter.default_config . Meter.make_meter 1 . (:[])
    . Meter.D . map mkd
    where mkd dur = Meter.D (replicate dur Meter.T)

make_block :: Ui.M m => (Text -> Id.Id) -> RulerId -> Text
    -> [(String, [Event.Event])] -> m BlockId
make_block mkid rid name tracks = do
    tids <- forM (zip [0..] tracks) $ \(i, (title, events)) ->
        Ui.create_track (mkid (name <> ".t" <> showt i)) $
            Track.track (txt title) (Events.from_list events)
    let block_tracks = Block.track (Block.RId rid) 20
            : [Block.track (Block.TId tid rid) 25 | tid <- tids]
    block_id <- Ui.create_block (mkid name) "" block_tracks
    Ui.set_skeleton block_id =<<
        ParseSkeleton.default_parser <$> TrackTree.tracks_of block_id
    BlockConfig.toggle_merge_all block_id
    return block_id

-- * convert

-- test = do
--     Right bs <- parse "test.dump"
--     -- Right bs <- parse "bloom.dump"
--     let bs2 = map (map_block (add_default_volume 1 38)) bs
--     pprint $ convert_blocks 1 bs2
--     -- pprint $ convert_track (head (to_tracks (head bs)))
--     -- pprint $ convert_notes (head (to_tracks (head bs)))
--     -- where to_tracks (Block rows) = Seq.rotate (map (\(Row ns) -> ns)  rows)

-- | An intermediate representation, between the row-oriented Block and
-- Ui.State.
type UiBlock = ([(NoteTrack, [ControlTrack])], BlockRows)
type NoteTrack = [Event.Event]
-- | (title, [event])
type ControlTrack = (String, [Event.Event])
-- | How many rows in a block.
type BlockRows = Int

-- | Convert parsed Blocks into UiBlocks.  Each row is given the ScoreTime
-- passed.
convert_blocks :: ScoreTime -> [Block] -> [UiBlock]
convert_blocks row_time = map (map_times (*row_time) . convert_block)

map_times :: (ScoreTime -> ScoreTime) -> UiBlock -> UiBlock
map_times f = first $ map $ \(ntrack, ctracks) ->
    (map modify ntrack, map (second (map (Event.start_ %= f))) ctracks)
    where modify = (Event.start_ %= f) . (Event.duration_ %= f)

convert_block :: Block -> UiBlock
convert_block (Block rows) = (map convert_track clipped, block_length)
    where
    to_tracks rows = Seq.rotate (map (\(Row ns) -> ns) rows)
    tracks = to_tracks rows
    block_length = fromMaybe 0 $ Seq.minimum (map track_length tracks)
    clipped = map (take block_length) tracks

track_length :: [Note] -> BlockRows
track_length = length . takeWhile (not . any (==cut_block) . note_effects)

convert_track :: [Note] -> (NoteTrack, [ControlTrack])
convert_track notes = (convert_notes notes, convert_controls notes)

convert_notes :: [Note] -> NoteTrack
convert_notes = Maybe.catMaybes . Then.mapAccumL go (Nothing, 0) final
    where
    go (prev, at) note = ((next, at+1), event)
        where (event, next) = convert_note prev at note
    final (prev, at) =
        [fst $ convert_note prev at (Note 0 0 [cut_note])]

convert_note :: Maybe Event.Event -> ScoreTime -> Note
    -> (Maybe Event.Event, Maybe Event.Event)
convert_note maybe_prev at (Note pitch _ effects)
    | pitch /= 0 = (note, Just (Event.event start 0 ""))
    | any (== cut_note) effects = (note, Nothing)
    | otherwise = (Nothing, maybe_prev)
    where
    start = note_start effects at
    note = case maybe_prev of
        Just event ->
            Just $ Event.duration_ #= (start - Event.start event) $ event
        Nothing -> Nothing

-- | Find the note start, taking the delay effect into account.  The delay
-- is actually in frames which are in the song config (and set by effects), but
-- I just hardcode it for the moment.
note_start :: [Effect] -> ScoreTime -> ScoreTime
note_start effects at =
    at + recip frames * ScoreTime.double (fromIntegral delay)
    where
    delay = fromMaybe 0 $ Seq.maximum $ map delay_effect effects
    frames = 8

-- TODO retrigger not implemented, it's the lower 4 bits
delay_effect :: Effect -> Int
delay_effect (fx, arg)
    | fx == fx_delay = Bits.shiftR arg 4
    | otherwise = 0

convert_controls :: [Note] -> [ControlTrack]
convert_controls notes = Map.assocs cont_vals
    where
    cont_vals = Map.map (map to_event) $
        Map.multimap (concat (zipWith mkcont (Seq.range_ 0 1) notes))
    mkcont at note = [(cont, (note_start (note_effects note) at, val))
        | (cont, val) <- note_controls note]
    to_event (pos, val) = Event.event pos 0 (txt val)

note_controls :: Note -> [(String, String)]
note_controls note = Maybe.maybeToList (convert_pitch note)
    ++ mapMaybe convert_effect (note_effects note)

convert_pitch :: Note -> Maybe (String, String)
convert_pitch (Note pitch _ _)
    | pitch == 0 = Nothing
    | otherwise =
        Just ("*", fromMaybe "?" $ Map.lookup pitch degree_to_note)

convert_effect :: Effect -> Maybe (String, String)
convert_effect (fx, arg)
    | fx == fx_volume = Just ("vel", c arg)
    | fx == fx_vibrato = Just ("mod", c arg)
    | otherwise = Nothing
    where
    c = untxt . (ShowVal.show_val :: Double -> Text) . (/127) . fromIntegral

cut_note = (0x0f, 0xff)
cut_block = (0x0f, 0)
fx_extended = 0x0e
fx_vibrato = 0x04
fx_volume = 0x0c
ex_cut = 0xc3
fx_delay = 0x1f

degree_to_note :: Map Int String
degree_to_note = Map.fromList $ zip [0..127] notes
    where notes = [show o ++ d | o <- [-1..9], d <- note_degrees]

-- | I could use `sharp` in here, but it's simpler to have plain text if
-- possible.
note_degrees :: [String]
note_degrees = ["c", "c#", "d", "d#", "e", "f", "f#", "g", "g#", "a", "a#", "b"]


-- * parse

newtype Block = Block [Row] deriving (Show)
newtype Row = Row [Note] deriving (Show)
data Note = Note {
    note_pitch :: Int
    , note_inst :: Int
    , note_effects :: [Effect]
    }
    deriving (Show)
type Effect = (Int, Int)

add_default_volume :: Int -> Int -> Note -> Note
add_default_volume inst vol note@(Note pitch note_inst effects)
    | pitch /= 0 && note_inst == inst && not has_vol =
        note { note_effects = (fx_volume, vol) : effects }
    | otherwise = note
    where has_vol = any ((==fx_volume) . fst) effects

map_block :: (Note -> Note) -> Block -> Block
map_block f (Block rows) = Block (map map_row rows)
    where map_row (Row notes) = Row (map f notes)

with_fx blocks = filter (not . null . note_effects)
    (concatMap block_notes blocks)

block_notes :: Block -> [Note]
block_notes = concatMap row_notes . block_rows
    where
    block_rows (Block rows) = rows
    row_notes (Row notes) = notes

parse :: FilePath -> IO (Either Text [Block])
parse fn = ParseText.parse p_blocks <$> Text.IO.readFile fn

p_blocks = A.many p_block
p_block = Block <$> parens (A.many p_row)
p_row = Row <$> parens (A.many p_note)

p_note :: A.Parser Note
p_note = parens $ do
    pitch <- number
    inst <- number
    _vol <- number
    fx1 <- p_effects
    fx2 <- p_effects
    return $ Note pitch inst (filter ((/=0) . fst) [fx1, fx2])

p_effects = parens ((,) <$> number <*> number)
parens = ParseText.between
    (ParseText.lexeme (A.char '(')) (ParseText.lexeme (A.char ')'))
number = ParseText.lexeme ParseText.p_nat
