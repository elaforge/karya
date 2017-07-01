-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Load.Mod2 where
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Debug as Debug
import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil

import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Track as Track
import qualified Ui.Ui as Ui

import qualified Cmd.Create as Create
import qualified Cmd.Load.ModTypes as M
import qualified Cmd.Ruler.Meter as Meter
import qualified Cmd.Ruler.Meters as Meters

import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.ScoreTypes as ScoreTypes
import qualified Derive.ShowVal as ShowVal

import qualified Perform.Pitch as Pitch
import Global
import Types


data Log = UnknownCommand !Int !Int
    deriving (Eq, Show)

data State = State {
    _tempo :: !M.Tempo
    , _instruments :: IntMap.IntMap M.Instrument
    } deriving (Eq, Show)

make_ruler :: TrackTime -> Ruler.Ruler
make_ruler end =
    clip_ruler end $ Meter.make_measures Meter.default_config 1 Meters.m44
        sections (ceiling end)
    where
    sections = 1

clip_ruler :: TrackTime -> Ruler.Ruler -> Ruler.Ruler
clip_ruler at = Ruler.modify_marklist Ruler.meter (const clip)
    where clip = Ruler.marklist . takeWhile ((<=at) . fst) . Ruler.to_list

-- |
-- Make IntMap Instrument
-- map convert_block
-- make @score block using _block_order
convert :: Id.Namespace -> M.Module -> Either Ui.Error Ui.State
convert ns mod = Ui.exec Ui.empty $ do
    Ui.set_namespace ns
    bids <- forM blocks $ \(tracks, block_end, skel) -> do
        bid <- Create.block Ui.no_ruler
        rid <- Ui.create_ruler (Id.unpack_id bid) (make_ruler block_end)
        Create.set_block_ruler rid bid
        forM_ tracks $ \track ->
            Create.track_events bid rid 999 40 track
        Ui.set_skeleton bid skel
        return bid
    mapM_ Ui.destroy_ruler =<< Create.orphan_rulers

    let block_ends = [t | (_, t, _) <- blocks]
    let block_map = IntMap.fromList $ zip [0..] (zip bids block_ends)
    scores <- forM (M._block_order mod) $ \(name_, indices) -> do
        let name = if name_ == "" then "score" else name_
        -- TODO LRuler.extract_meters
        let ruler_id = Ui.no_ruler
        score <- Create.named_block (Id.id ns name) ruler_id
        Create.track_events score ruler_id 1 40 $ score_track block_map indices
        Create.unfitted_view score
        return score
    whenJust (Seq.head scores) Ui.set_root_id
    where
    blocks = map (convert_block state) (M._blocks mod)
    state = State
        { _tempo = M._default_tempo mod
        , _instruments = IntMap.fromList $ zip [1..] (M._instruments mod)
        }

-- | Make a score track with calls to the blocks.
score_track :: IntMap.IntMap (BlockId, TrackTime) -> [Int] -> Track.Track
score_track blocks indices = Track.track ">" $ Events.from_list
    [ Event.event start (end - start) call
    | (start, end, call) <- zip3 starts (drop 1 starts) calls
    ]
    where
    starts = scanl (+) 0 durs
    (calls, durs) = unzip $ map call_of indices
    call_of idx = case IntMap.lookup idx blocks of
        Just (bid, end) -> (Id.ident_name bid, end)
        Nothing -> ("block" <> showt idx, 1) -- order number with no block

-- | Figure out block length from min (max lines) (first cut_block)
-- map convert_track, merge Notes
convert_block :: State -> M.Block
    -> ([Track.Track], TrackTime, Skeleton.Skeleton)
convert_block state block =
    ( map (uncurry Track.track) (concat ctracks)
    , line_start block_len
    , make_skeleton ctracks
    )
    where
    ctracks = map (merge_notes . convert_track state block_len)
        (M._tracks block)
    block_len = fromMaybe (fromIntegral (M._block_length block)) $
        Seq.minimum $ mapMaybe track_len (M._tracks block)
    track_len = Seq.head . mapMaybe cut_block . zip [0..]
    cut_block (linenum, line)
        | M.CutBlock `elem` M._commands line = Just linenum
        | otherwise = Nothing

make_skeleton :: [[track]] -> Skeleton.Skeleton
make_skeleton track_groups =
    Skeleton.make $ concat $ zipWith make tracknums (drop 1 tracknums)
    where
    tracknums = scanl (+) 1 (map length track_groups)
    make start end = zip ts (drop 1 ts)
        where ts = Seq.range' start end 1

merge_notes :: [Note] -> [(Text, Events.Events)]
merge_notes notes =
    map (second Events.from_list) $ mapMaybe clean_track $
        note_track : pitch_track : control_tracks
    where
    note_track = case instruments of
        [inst] ->
            (ParseTitle.instrument_to_title inst, map (note_event False) notes)
        _ -> (">", map (note_event True) notes)
    pitch_track = ("*", concatMap note_pitches notes)
    control_tracks =
        [ (c, concatMap (Map.findWithDefault [] c . _controls) notes)
        | c <- controls
        ]
    controls = Set.toList $ Set.delete "*" $ Set.unions $
        map (Map.keysSet . _controls) notes
    instruments = Seq.unique $ map (M._instrument_name . _instrument) notes

clean_track :: (Text, [Event.Event]) -> Maybe (Text, [Event.Event])
clean_track (title, events)
    | null events = Nothing
    | ParseTitle.is_note_track title || ParseTitle.is_pitch_track title =
        Just (title, events)
    | otherwise = Just (title, Seq.drop_dups Event.text events)

note_event :: Bool -> Note -> Event.Event
note_event set_instrument n = Event.event (_start n) (_duration n) call
    where
    call = (if set_instrument then set_inst else "") <> _call n
    set_inst = "i " <> ShowVal.show_val (M._instrument_name (_instrument n))

note_pitches :: Note -> [Event.Event]
note_pitches n = Event.event (_start n) 0 (nn_to_call (_pitch n))
    : Map.findWithDefault [] "*" (_controls n)

-- | TODO surely this exists elsewhere?
nn_to_call :: Pitch.NoteNumber -> Text
nn_to_call nn = showt (oct-1) <> steps !! step
    where
    (oct, step) = floor nn `divMod` 12
    steps = ["c", "c#", "d", "d#", "e", "f", "f#", "g", "g#", "a", "a#", "b"]

-- * convert

data Note = Note {
    _start :: !TrackTime
    , _duration :: !TrackTime
    , _instrument :: !M.Instrument
    -- | Note call text.
    , _call :: !Text
    , _pitch :: !Pitch.NoteNumber
    , _controls :: !(Map Control [Event.Event])
    } deriving (Eq, Show)

type LineNum = Double

-- |
-- - Lookup instrument.
convert_track :: State -> LineNum -> [M.Line] -> [Note]
convert_track state block_len = go . zip [0..]
    where
    -- TODO extend 0s for commands that do that: 1 2 3 4 5 6 7
    -- TODO apply default volume
    go ((_, M.Line Nothing _ _) : lines) = go lines
    go ((linenum, M.Line (Just pitch) instnum cmds) : lines) =
        convert_note block_len (_tempo state) instrument linenum pitch
            cmds lines
        : go lines
        where
        instrument = fromMaybe no_instrument $
            IntMap.lookup instnum (_instruments state)
        no_instrument = M.Instrument (ScoreTypes.Instrument (showt instnum))
            Nothing
    go [] = []

-- |
-- Figure out note duration: min of time until next line with Pitch>0, or 0fff,
-- or...?  Convert Pitch. Collect cmds and convert to Command.
--
-- Convert 0d to linear, 01 02 to pitch 'u' or 'd', 03 to pitch linear.
--
-- - Interpret timing cmds like 1f.
convert_note :: LineNum -> M.Tempo -> M.Instrument -> LineNum
    -> Pitch.NoteNumber -> [M.Command] -> [(LineNum, M.Line)] -> Note
convert_note block_len tempo instrument linenum pitch cmds lines = Note
    { _start = start
    , _duration = end - start
    , _instrument = instrument
    , _call = note_call (M._frames tempo) cmds
    , _pitch = pitch
    , _controls = Map.fromList $ convert_commands instrument start cmds $
        takeWhile ((==Nothing) . cut_note) lines
    }
    where
    start = line_start linenum
    end = fromMaybe (line_start block_len) $ msum (map cut_note lines)

note_call :: Int -> [M.Command] -> Text
note_call frames cmds =
    Text.strip $ Text.intercalate " | " $ Maybe.catMaybes [d, r, Just ""]
    where
    d = if delay == 0 then Nothing else
        Just $ "d " <> showt delay <> "/" <> showt (frames * lines_per_t) <> "t"
    r = Nothing -- TODO?
    (delay, repeat) = fromMaybe (0, 0) $
        Seq.head [(delay, repeat) | M.DelayRepeat delay repeat <- cmds]

convert_commands :: M.Instrument -> TrackTime -> [M.Command]
    -> [(LineNum, M.Line)] -> [(Control, [Event.Event])]
convert_commands instrument start cmds lines = group_controls controls
    where
    -- TODO set and crescendo?
    -- TODO just leave it blank, but set per-instrument dyn
    inst_vol = fromMaybe max_volume $ M._volume instrument
    volume = if null [() | M.Volume _ <- cmds] then [M.Volume inst_vol] else []
    controls = map (start,) (mapMaybe command_to_control (volume ++ cmds))
        ++ concatMap line_controls lines
    line_controls (linenum, line) = map (line_start linenum,) $
        mapMaybe command_to_control (M._commands line)
    -- TODO for Portamento, get the pitch for the fst arg: port (4c) 2

group_controls :: [(TrackTime, (CommandType, Control, Call))]
    -> [(Control, [Event.Event])]
group_controls controls =
    map make_control $ Seq.keyed_group_sort (key . snd) controls
    where
    make_control (Left control, vals) =
        (control, [event start 0 call | (start, (_, _, call)) <- vals])
    make_control (Right (control, call), vals) =
        (control, [event start (end-start) call])
        where
        start = fst (head vals) -- Seq.keyed_group_sort returns NonNull
        end = fst (last vals) + 1 -- go to the end of the last one
    -- All Singles are in one group so they each get a 0 dur event.
    key (Single, control, _) = Left control
    key (Grouped, control, call) = Right (control, call)

type Control = Text
data Call = Call !Text !Double
    deriving (Eq, Ord, Show)

event :: TrackTime -> TrackTime -> Call -> Event.Event
event start dur (Call call val) =
    Event.event start dur (TextUtil.join2 call (ShowVal.show_hex_val val))

-- | Single cmds emit a single 0 dur event.  Grouped cmds emit an event with
-- the given duration as long as they stay the same.
data CommandType = Single | Grouped
    deriving (Eq, Ord, Show)

command_to_control :: M.Command -> Maybe (CommandType, Control, Call)
command_to_control c = case c of
    M.Volume v ->
        Just (Single, c_dyn, Call "" (int v / fromIntegral max_volume))
    M.Crescendo v -> Just (Grouped, c_dyn, Call "u" (int v))
    M.Decrescendo v -> Just (Grouped, c_dyn, Call "d" (int v))
    M.Portamento v -> Just (Grouped, c_pitch, Call "port" (int v))
    _ -> Nothing
    where
    int = fromIntegral

-- | There's a feature that lets volume go from 00 to 64 instead of 00 to 40.
-- I think it might be per-song.
max_volume :: Int
max_volume = 0x64

c_dyn, c_pitch :: Control
c_dyn = "dyn"
c_pitch = "*"

cut_note :: (LineNum, M.Line) -> Maybe TrackTime
cut_note (linenum, line)
    -- | not $ null [() | M.Portamento _ <- M._commands line] = Nothing
    | Just _ <- M._pitch line = Just $ line_start linenum
    | M.CutNote `elem` M._commands line = Just $ line_start linenum
    | M.CutBlock `elem` M._commands line = Just $ line_start (linenum+1)
    | otherwise = Nothing

line_start :: LineNum -> TrackTime
line_start = (/ fromIntegral lines_per_t) . ScoreTime.double

-- | Lines per 1 TrackTime.
lines_per_t :: Int
lines_per_t = 8
