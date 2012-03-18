-- | Load mod dumps from hacked up xmp.
module Cmd.LoadMod where
import qualified Control.Applicative as A (many)
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
import Util.Test
import qualified Util.Then as Then

import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.State as State
import qualified Ui.Track as Track

import qualified Cmd.BlockConfig as BlockConfig
import qualified Cmd.Create as Create
import qualified Cmd.MakeRuler as MakeRuler

import qualified Derive.ParseSkeleton as ParseSkeleton
import qualified Derive.Score as Score
import Types


-- not implemented:
-- - initial tempo and tempo changes, along with frames / row
-- - instruments (one instrument is hardcoded)
-- - most effects
-- - order list, so repeats are expanded

-- * create ui state

create :: (State.M m) => Id.Namespace -> [UiBlock] -> m ()
create name ui_blocks = do
    State.set_namespace name
    let mkid = Id.unsafe_id name
    (rid, track_rid) <- Create.ruler "meter_44"
        (MakeRuler.ruler [MakeRuler.meter_ruler (1/16) MakeRuler.m44])
    block_ids <- mapM (uncurry (create_block mkid rid track_rid ""))
        (zip [0..] ui_blocks)
    root <- create_order_block mkid block_ids
    State.set_root_id root
    State.modify_default $ \d ->
        d { State.default_instrument = Just (Score.Instrument "ptq/c1") }
    Create.view root
    return ()

create_block :: (State.M m) => (String -> Id.Id) -> RulerId -> RulerId
    -> String -> Int -> UiBlock -> m (BlockId, BlockRows)
create_block mkid rid track_rid inst num (ui_block, block_rows) = do
    block_id <- make_block mkid rid track_rid ("b" ++ show num)
        (concatMap mktrack ui_block)
    return (block_id, block_rows)
    where mktrack (ntrack, ctracks) = (">" ++ inst, ntrack) : ctracks

create_order_block :: (State.M m) => (String -> Id.Id)
    -> [(BlockId, BlockRows)] -> m BlockId
create_order_block mkid block_ids = do
    (rid, track_rid) <- Create.ruler "order"
        (MakeRuler.ruler [order_meter block_rows])
    make_block mkid rid track_rid "order"
        [("tempo", tempo), (">ptq/c1", events)]
    where
    block_rows = map snd block_ids
    tempo = [(0, Event.event "6" 0)]
    starts = scanl (+) 0 block_rows
    events =
        [(fromIntegral start, Event.event (block_call bid) (fromIntegral dur))
            | (start, (bid, dur)) <- zip starts block_ids]
    block_call = snd . Id.un_id . Id.unpack_id

order_meter :: [BlockRows] -> (Ruler.Name, Ruler.Marklist)
order_meter = MakeRuler.meter_ruler 1 . MakeRuler.D . map mkd
    where
    mkd dur = MakeRuler.D (replicate dur (MakeRuler.T 1))

make_block :: (State.M m) => (String -> Id.Id) -> RulerId -> RulerId -> String
    -> [(String, [Events.PosEvent])] -> m BlockId
make_block mkid rid track_rid name tracks = do
    tids <- forM (zip [0..] tracks) $ \(i, (title, events)) ->
        State.create_track (mkid (name ++ ".t" ++ show i)) $
            Track.track title events
    let block_tracks = Block.track (Block.RId rid) 20
            : [Block.track (Block.TId tid track_rid) 25 | tid <- tids]
    block_id <- State.create_block (mkid name) $
        Block.block Block.default_config ""  block_tracks
    State.set_skeleton block_id =<<
        ParseSkeleton.default_parser <$> State.get_track_info block_id
    BlockConfig.toggle_merge_all block_id
    return block_id

-- * convert

test = do
    Right bs <- parse "test.dump"
    -- Right bs <- parse "bloom.dump"
    let bs2 = map (map_block (add_default_volume 1 38)) bs
    pprint $ convert_blocks 1 bs2
    -- pprint $ convert_track (head (to_tracks (head bs)))
    -- pprint $ convert_notes (head (to_tracks (head bs)))
    -- where to_tracks (Block rows) = Seq.rotate (map (\(Row ns) -> ns)  rows)

-- | An intermediate representation, between the row-oriented Block and
-- State.State.
type UiBlock = ([(NoteTrack, [ControlTrack])], BlockRows)
type NoteTrack = [Events.PosEvent]
-- | (title, [event])
type ControlTrack = (String, [Events.PosEvent])
-- | How many rows in a block.
type BlockRows = Int

-- | Convert parsed Blocks into UiBlocks.  Each row is given the ScoreTime
-- passed.
convert_blocks :: ScoreTime -> [Block] -> [UiBlock]
convert_blocks row_time = map (map_times (*row_time) . convert_block)

map_times :: (ScoreTime -> ScoreTime) -> UiBlock -> UiBlock
map_times f = first $ map $ \(ntrack, ctracks) ->
    (map modify ntrack, map (second (map (first f))) ctracks)
    where modify (pos, event) = (f pos, Event.modify_duration f event)

convert_block :: Block -> UiBlock
convert_block (Block rows) = (map convert_track clipped, block_length)
    where
    to_tracks rows = Seq.rotate (map (\(Row ns) -> ns) rows)
    tracks = to_tracks rows
    block_length = maybe 0 id (Seq.minimum (map track_length tracks))
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

convert_note :: Maybe Events.PosEvent -> ScoreTime -> Note
    -> (Maybe Events.PosEvent, Maybe Events.PosEvent)
convert_note maybe_prev at (Note pitch _ effects)
    | pitch /= 0 = (note, Just (start, Event.event "" 0))
    | any (== cut_note) effects = (note, Nothing)
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
note_start effects at =
    at + recip frames * ScoreTime.double (fromIntegral delay)
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
        Map.multimap (concat (zipWith mkcont (Seq.range_ 0 1) notes))
    mkcont at note = [(cont, (note_start (note_effects note) at, val))
        | (cont, val) <- note_controls note]
    to_event (pos, val) = (pos, Event.event val 0)

note_controls :: Note -> [(String, String)]
note_controls note = Maybe.maybeToList (convert_pitch note)
    ++ Maybe.mapMaybe convert_effect (note_effects note)

convert_pitch :: Note -> Maybe (String, String)
convert_pitch (Note pitch _ _)
    | pitch == 0 = Nothing
    | otherwise =
        Just ("*", maybe "?" id (Map.lookup pitch degree_to_note))

convert_effect :: Effect -> Maybe (String, String)
convert_effect (fx, arg)
    | fx == fx_volume = Just ("vel", c arg)
    | fx == fx_vibrato = Just ("mod", c arg)
    | otherwise = Nothing
    where c = Pretty.show_float (Just 2) . (/127) . fromIntegral

cut_note = (0x0f, 0xff)
cut_block = (0x0f, 0)
fx_extended = 0x0e
fx_vibrato = 0x04
fx_volume = 0x0c
ex_cut = 0xc3
fx_delay = 0x1f

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
