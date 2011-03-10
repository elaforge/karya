-- | Load mod dumps from hacked up xmp.
module Cmd.LoadMod where
import Control.Monad
import qualified Data.Attoparsec.Char8 as A
import qualified Data.Bits as Bits
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Util.Control
import qualified Util.Map as Map
import qualified Util.ParseBs as Parse
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.Then as Then

import Ui
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Id as Id
import qualified Ui.Track as Track
import qualified Ui.Types as Types
import qualified Ui.State as State

import qualified Derive.Schema as Schema

import qualified Cmd.Create as Create
import qualified Cmd.MakeRuler as MakeRuler

import qualified App.Config as Config

import Util.Test


-- not implemented:
-- - initial tempo and tempo changes, along with frames / row
-- - all blocks are given the same length in the order block, so longer blocks
-- will play faster
-- - instruments (one instrument is hardcoded)
-- - most effects
-- - order list, so repeats are expanded

-- * create ui state

create :: (State.M m) => String -> [UiBlock] -> m ()
create name ui_blocks = do
    State.set_project name
    let mkid = Id.id name
    (rid, track_rid) <- Create.ruler "meter_44"
        (MakeRuler.ruler [MakeRuler.meter_ruler (1/16) MakeRuler.m44])
    block_ids <- mapM (uncurry (create_block mkid rid track_rid "ptq/c1"))
        (zip [0..] ui_blocks)
    root <- create_order_block mkid rid track_rid block_ids
    State.set_root_id root
    Create.view root
    return ()

create_block :: (State.M m) => (String -> Id.Id) -> RulerId -> RulerId
    -> String -> Int -> UiBlock -> m BlockId
create_block mkid rid track_rid inst num ui_block =
    make_block mkid rid track_rid ("b" ++ show num) (concatMap mktrack ui_block)
    where mktrack (ntrack, ctracks) = (">" ++ inst, ntrack) : ctracks

create_order_block :: (State.M m) => (String -> Id.Id) -> RulerId -> RulerId
    -> [BlockId] -> m BlockId
create_order_block mkid rid track_rid block_ids =
    make_block mkid rid track_rid "order" [("tempo", tempo), (">", events)]
    where
    tempo = [(0, Event.event ".1" 0)]
    events = [(n, Event.event (block_call bid) 1)
        | (n, bid) <- zip [0..] block_ids]
    block_call = snd . Id.un_id . Id.unpack_id

make_block :: (State.M m) => ([Char] -> Id.Id) -> RulerId -> RulerId -> String
    -> [(String, [Track.PosEvent])] -> m BlockId
make_block mkid rid track_rid name tracks = do
    tids <- forM (zip [0..] tracks) $ \(i, (title, events)) -> do
        State.create_track (mkid (name ++ ".t" ++ show i)) $
            Track.track title events Config.track_bg Config.render_config
    let block_tracks = Block.block_track (Block.RId rid) 20
            : [Block.block_track (Block.TId tid track_rid) 40 | tid <- tids]
    block_id <- State.create_block (mkid name) $
        Block.block Block.default_config ""  block_tracks Config.schema
    State.set_skeleton block_id =<<
        Schema.default_parser <$> State.get_track_info block_id
    return block_id

-- * convert

test = do
    Right bs <- parse "test.dump"
    -- Right bs <- parse "bloom.dump"
    let bs2 = map (map_block (add_default_volume 1 38)) bs
    pprint $ convert_blocks 0.25 (take 1 bs2)
    -- pprint $ convert_track (head (to_tracks (head bs)))
    -- pprint $ convert_notes (head (to_tracks (head bs)))
    -- where to_tracks (Block rows) = rotate (map (\(Row ns) -> ns)  rows)

-- | An intermediate representation, between the row-oriented Block and
-- State.State.
type UiBlock = [(NoteTrack, [ControlTrack])]
type NoteTrack = [Track.PosEvent]
-- | (title, [event])
type ControlTrack = (String, [Track.PosEvent])

-- | Convert parsed Blocks into UiBlocks.  Each row is given the ScoreTime
-- passed.
convert_blocks :: ScoreTime -> [Block] -> [UiBlock]
convert_blocks row_time = map (map_times (*row_time)) . map convert_block

map_times :: (ScoreTime -> ScoreTime) -> UiBlock -> UiBlock
map_times f = map $ \(ntrack, ctracks) ->
    (map modify ntrack, map (second (map (first f))) ctracks)
    where modify (pos, event) = (f pos, Event.modify_duration f event)

convert_block :: Block -> UiBlock
convert_block block = map convert_track (to_tracks block)
    where to_tracks (Block rows) = rotate (map (\(Row ns) -> ns)  rows)

convert_track :: [Note] -> (NoteTrack, [ControlTrack])
convert_track notes = (convert_notes notes, convert_controls notes)

convert_notes :: [Note] -> NoteTrack
convert_notes = Maybe.catMaybes . Then.mapAccumL go (Nothing, 0) final
    where
    go (prev, at) note = ((next, at+1), event)
        where (event, next) = convert_note prev at note
    final (prev, at) =
        [fst $ convert_note prev at (Note 0 0 [cut_effect])]

convert_note :: Maybe Track.PosEvent -> ScoreTime -> Note
    -> (Maybe Track.PosEvent, Maybe Track.PosEvent)
convert_note maybe_prev at (Note pitch _ effects)
    | pitch /= 0 = (note, Just (start, Event.event "" 0))
    | any (== cut_effect) effects = (note, Nothing)
    | otherwise = (Nothing, maybe_prev)
    where
    start = note_start effects at
    note = case maybe_prev of
        Just (prev_start, event) ->
            Just (prev_start, Event.set_duration (start-prev_start) event)
        Nothing -> Nothing

-- | Find the note start, taking the delay effect into account.  The delay
-- is actually in frames which are in the song config (and set by effects), but
-- I just hardcode it for the moment.
note_start :: [Effect] -> ScoreTime -> ScoreTime
note_start effects at = at + recip frames * Types.ScoreTime (fromIntegral delay)
    where
    delay = maybe 0 id $ Seq.maximum $ map delay_effect effects
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
        Map.multimap (concat (zipWith mkcont [0..] notes))
    mkcont at note = [(cont, (note_start (note_effects note) at, val))
        | (cont, val) <- note_controls note]
    to_event (pos, val) = (pos, Event.event val 0)

note_controls :: Note -> [(String, String)]
note_controls note = maybe [] (:[]) (convert_pitch note)
    ++ Seq.map_maybe convert_effect (note_effects note)

convert_pitch :: Note -> Maybe (String, String)
convert_pitch (Note pitch _ _)
    | pitch == 0 = Nothing
    | otherwise =
        Just ("*twelve", maybe "?" id (Map.lookup pitch degree_to_note))

convert_effect :: Effect -> Maybe (String, String)
convert_effect (fx, arg)
    | fx == fx_volume = Just ("vel", c arg)
    | fx == fx_vibrato = Just ("mod", c arg)
    | otherwise = Nothing
    where c = Pretty.show_float (Just 2) . (/127) . fromIntegral

cut_effect = (0x0f, 0xff)
fx_extended = 0x0e
fx_vibrato = 0x04
fx_volume = 0x0c
ex_cut = 0xc3
fx_delay = 0x1f

rotate :: [[a]] -> [[a]]
rotate xs = maybe [] (: rotate (map tail xs)) (sequence (map Seq.head xs))

degree_to_note :: Map.Map Int String
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

parse :: FilePath -> IO (Either String [Block])
parse fn = do
    s <- B.readFile fn
    return $ Parse.parse_all p_blocks s

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
parens = Parse.between (Parse.lexeme (A.char '(')) (Parse.lexeme (A.char ')'))
number = Parse.lexeme Parse.p_nat
