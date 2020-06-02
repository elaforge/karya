-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Convert a 'M.Module' to 'Ui.State'.
module Cmd.Load.Mod where
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Track as Track
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import qualified Cmd.Create as Create
import qualified Cmd.Load.ModTypes as M
import qualified Cmd.Ruler.Meter as Meter
import qualified Cmd.Ruler.Meters as Meters

import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import Derive.ShowVal (show_val)

import qualified Perform.Pitch as Pitch
import Global
import Types


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
clip_ruler at ruler = Ruler.set_meter config (clip mlist) ruler
    where
    (config, mlist) = Ruler.get_meter ruler
    clip = Ruler.marklist . takeWhile ((<=at) . fst) . Ruler.to_list

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
        forM_ tracks $ \track -> do
            tid <- Create.track_events bid rid 999 40 track
            Ui.set_render_style (Track.Line Nothing) tid
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
    Ui.modify_config $ UiConfig.ky #= ky
    where
    blocks = map (convert_block state) (M._blocks mod)
    state = State
        { _tempo = M._default_tempo mod
        , _instruments = IntMap.fromList $ zip [1..] (M._instruments mod)
        }

ky :: Text
ky = Text.unlines
    [ "note generator:"
    , "i = inst = $inst |"
    ]

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
    | otherwise = Just (title, Seq.drop_with idempotent events)
    where
    idempotent e1 e2 = "`0x`" `Text.isPrefixOf` a && a == b
        where
        (a, b) = (Event.text e1, Event.text e2)

note_event :: Bool -> Note -> Event.Event
note_event set_instrument n = Event.event (_start n) (_duration n) call
    where
    call = (if set_instrument then set_inst else "") <> _call n
    set_inst = "i " <> show_val (M._instrument_name (_instrument n))

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
    go ((_, M.Line Nothing _ _) : lines) = go lines
    go ((linenum, M.Line (Just pitch) instnum cmds) : lines) =
        convert_note block_len (_tempo state) instrument linenum pitch
            cmds lines
        : go lines
        where
        instrument = fromMaybe no_instrument $
            IntMap.lookup instnum (_instruments state)
        no_instrument = M.Instrument (ScoreT.Instrument (showt instnum))
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
    , _controls = Map.fromListWith (Seq.merge_on Event.start) $
        convert_commands instrument start cmds $
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
    (delay, _repeat) = fromMaybe (0, 0) $
        Seq.head [(delay, repeat) | M.DelayRepeat delay repeat <- cmds]

convert_commands :: M.Instrument -> TrackTime -> [M.Command]
    -> [(LineNum, M.Line)] -> [(Control, [Event.Event])]
convert_commands instrument start cmds lines = group_controls controls
    where
    inst_vol = fromMaybe 1 $ M._volume instrument
    volume = if null [() | M.Volume _ <- cmds] then [M.Volume inst_vol] else []
    controls = map (start,) (commands_to_controls (volume ++ cmds))
        ++ concatMap line_controls lines
    line_controls (linenum, line) = map (line_start linenum,) $
        commands_to_controls (M._commands line)

group_controls :: [(TrackTime, (CommandType, Control, Text))]
    -> [(Control, [Event.Event])]
group_controls controls =
    map make_control $ Seq.keyed_group_sort (key . snd) controls
    where
    make_control (Left control, vals) =
        (control, [Event.event start 0 call | (start, (_, _, call)) <- vals])
    make_control (Right (control, call), vals) =
        ( control
        , [Event.event t (last run + one_line - t) call | run@(t:_) <- runs]
        )
        where
        runs = Seq.split_between (\t1 t2 -> t2 > t1 + one_line) (map fst vals)
        one_line = 1 / fromIntegral lines_per_t
    -- All Singles are in one group so they each get a 0 dur event.
    key (Single, control, _) = Left control
    key (Grouped, control, call) = Right (control, call)

type Control = Text
data Call = Call !Text !Double
    deriving (Eq, Ord, Show)

-- | Single cmds emit a single 0 dur event.  Grouped cmds emit an event with
-- the given duration as long as they stay the same.
data CommandType = Single | Grouped
    deriving (Eq, Ord, Show)

commands_to_controls :: [M.Command] -> [(CommandType, Control, Text)]
commands_to_controls = mapMaybe convert . (\xs -> map (xs,) xs)
    where
    convert (cmds, c) = case c of
        -- The slope is not accurate, and I'd need a ScoreTime slope for 'u'
        -- and 'd' to make it accurate.  Too much bother.
        M.VolumeSlide val
            | vol : _ <- [vol | M.Volume vol <- cmds] -> Just
                ( Grouped
                , c_dyn
                , "from=" <> ShowVal.show_hex_val vol
                    <> " | " <> c <> " " <> show_val (abs val)
                )
            | otherwise -> Just (Grouped, c_dyn, c <> " " <> show_val (abs val))
            where c = if val >= 0 then "u" else "d"
        M.Volume val
            | null [() | M.VolumeSlide _ <- cmds] ->
                Just (Single, c_dyn, ShowVal.show_hex_val val)
            | otherwise -> Nothing
        M.Command name val -> Just
            ( Single
            , "cmd"
            , "--|" <> name <> if val == 0 then "" else " " <> Num.hex 2 val
            )
        _ -> Nothing

c_dyn, c_pitch :: Control
c_dyn = "dyn"
c_pitch = "*"

cut_note :: (LineNum, M.Line) -> Maybe TrackTime
cut_note (linenum, line)
    | Just _ <- M._pitch line = Just $ line_start linenum
    | M.CutNote `elem` M._commands line = Just $ line_start linenum
    | M.CutBlock `elem` M._commands line = Just $ line_start (linenum+1)
    | otherwise = Nothing

line_start :: LineNum -> TrackTime
line_start = (/ fromIntegral lines_per_t) . ScoreTime.from_double

-- | Lines per 1 TrackTime.
lines_per_t :: Int
lines_per_t = 8
