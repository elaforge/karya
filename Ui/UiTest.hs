-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
module Ui.UiTest where
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text

import qualified Util.CallStack as CallStack
import qualified Util.Rect as Rect
import qualified Util.Seq as Seq
import qualified Util.Testing as Testing
import qualified Util.TextUtil as TextUtil
import qualified Util.Then as Then

import qualified Midi.Midi as Midi
import qualified Ui.Block as Block
import qualified Ui.Color as Color
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.Sel as Sel
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Zoom as Zoom

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Cmd.Ruler.Meter as Meter
import qualified Cmd.Simple as Simple
import qualified Cmd.TimeStep as TimeStep

import qualified Derive.ParseSkeleton as ParseSkeleton
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack

import qualified Perform.Midi.Patch as Patch
import qualified Instrument.Inst as Inst
import qualified Instrument.InstTypes as InstTypes
import qualified App.Config as Config
import Global
import Types


-- Test functions do things I don't want to include in non-testing code, such
-- as freely call 'error' and define convenient but incorrect orphan instances.
-- The orphans should be protected behind an ifdef in "Derive.TestInstances",
-- but I still don't want to mix test and non-test code.
--
-- UiTest is the most fundamental of the test modules, so I should only need
-- to check here.
#ifndef TESTING
#error "don't import testing modules from non-test code"
#endif


-- | (10, 50) seems to be the smallest x,y OS X will accept.  Apparently
-- fltk's sizes don't take the menu bar into account, which is about 44 pixels
-- high, so a y of 44 is the minimum.
default_rect :: Rect.Rect
default_rect = Rect.xywh 10 50 200 200

default_divider :: Block.Divider
default_divider = Block.Divider Color.blue

-- state

mkid :: Text -> Id.Id
mkid = Id.read_short test_ns

bid :: Text -> BlockId
bid = Id.BlockId . mkid

vid :: Text -> ViewId
vid = Id.ViewId . mkid

tid :: Text -> TrackId
tid = Id.TrackId . mkid

rid :: Text -> RulerId
rid = Id.RulerId . mkid

test_ns :: Id.Namespace
test_ns = Id.namespace "test"

default_zoom :: Zoom.Zoom
default_zoom = Config.zoom

-- * fmt

-- | Visualize event ranges.  This can be used with 'Testing.equal_fmt'.
fmt_events :: [EventSpec] -> Text
fmt_events [] = ""
fmt_events events = Text.unlines
    [ fmt_ruler min_start (events_end events)
    , fmt_events_only min_start events
    ]
    where min_start = min 0 (events_start events)

fmt_ui_events :: [Event.Event] -> Text
fmt_ui_events = fmt_events . map extract_event

fmt_events_only :: Int -> [EventSpec] -> Text
fmt_events_only min_start = Text.unlines . map event
    where
    event (start, dur, text) = gap <> arrow <> label
        where
        gap = Text.replicate (to_spaces (min start (start + dur))) " "
        arrow
            | dur == 0 && ScoreTime.is_negative dur = "<"
            | dur == 0 = ">"
            | dur < 0 = "<" <> middle <> "|"
            | otherwise = "|" <> middle <> ">"
        middle = Text.replicate (time_to_spaces (abs dur) - 1) "-"
        label = if Text.null text then "" else " [" <> text <> "]"
    to_spaces t = offset + time_to_spaces t
    offset = time_to_spaces $ fromIntegral (abs min_start)

fmt_ruler :: Int -> Int -> Text
fmt_ruler start end = Text.stripEnd $ mconcatMap (space . pretty) ts
    where
    space t = t <> Text.replicate (time_to_spaces step - Text.length t) " "
    ts = Then.takeWhile1 (<end) (Seq.range_ start 1)
    step = 1

fmt_start_duration :: [(ScoreTime, ScoreTime)] -> Text
fmt_start_duration = fmt_events . map (\(s, d) -> (s, d, ""))

fmt_blocks :: [BlockSpec] -> Text
fmt_blocks = Text.strip . Text.unlines
    . concatMap (\(block, tracks) -> [block <> ":", fmt_tracks tracks])

fmt_tracks :: [TrackSpec] -> Text
fmt_tracks [] = ""
fmt_tracks tracks = Text.unlines $
    (indent <> fmt_ruler 0 (maximum (map (events_end . snd) tracks)))
    : concatMap track tracks
    where
    track (title, events) = case Text.lines (fmt_events_only 0 events) of
        [] -> []
        x : xs -> fmt_title title <> x : map (indent<>) xs
    indent = Text.replicate (title_length + 2) " "
    fmt_title title = Text.justifyLeft title_length ' ' title <> ": "
    title_length = maximum $ map (Text.length . fst) tracks

time_to_spaces :: ScoreTime -> Int
time_to_spaces = floor . (*4)

events_end :: [(ScoreTime, ScoreTime, x)] -> Int
events_end = maybe 0 ceiling . Seq.maximum .  map (\(s, d, _) -> max s (s+d))

events_start :: [(ScoreTime, ScoreTime, x)] -> Int
events_start = maybe 0 floor . Seq.minimum . map (\(s, d, _) -> min s (s+d))

-- | Extract and fmt the fst . right element.  Many DeriveTest extractors
-- return Either Error (val, [log]).
right_fst :: (a -> Text) -> Either x (a, y) -> Text
right_fst fmt = either (const "") (fmt . fst)

-- * monadic mk- functions

-- | (block_id, tracks)
--
-- If the name ends with @=ruler@, then the length of the ruler is derived from
-- the events inside, rather than being hardcoded.  This is convenient for
-- tests and lets them avoid hardcoding the default_ruler end.
--
-- Also, if the name contains @--@, the text after it becomes the block title.
type BlockSpec = (Text, [TrackSpec])

-- | (track_title, events)
type TrackSpec = (Text, [EventSpec])

-- | (start, dur, text)
type EventSpec = (ScoreTime, ScoreTime, Text)

-- | Parse a block spec, which looks like @name[=ruler] [-- title]@
parse_block_spec :: Text -> (BlockId, Text, Bool)
parse_block_spec spec = (bid block_id, Text.strip title, has_ruler)
    where
    (name, title) = TextUtil.split1 "--" spec
    (block_id, has_ruler) = maybe (Text.strip name, False) (, True) $
        Text.stripSuffix "=ruler" (Text.strip name)

-- | Often tests work with a single block, or a single view.  To make them
-- less verbose, there is a default block and view so functions can omit the
-- parameter if convenient.
default_block_id :: BlockId
default_block_id = bid default_block_name

default_block_name :: Text
default_block_name = "b1"

default_view_id :: ViewId
default_view_id = mk_vid default_block_id

default_ruler_id :: RulerId
default_ruler_id = rid "r0"
    -- r1 would conflict with ruler automatically generated because of the
    -- =ruler suffix

-- | Return the val and state, throwing an IO error on an exception.  Intended
-- for tests that don't expect to fail here.
run :: CallStack.Stack => Ui.State -> Ui.StateId a -> (a, Ui.State)
run state m = case result of
    Left err -> errorStack $ "state error: " <> showt err
    Right (val, state', _) -> (val, state')
    where result = Ui.run_id state m

exec :: CallStack.Stack => Ui.State -> Ui.StateId a -> Ui.State
exec state m = case Ui.exec state m of
    Left err -> errorStack $ "state error: " <> pretty err
    Right state' -> state'

eval :: CallStack.Stack => Ui.State -> Ui.StateId a -> a
eval state m = case Ui.eval state m of
    Left err -> errorStack $ "state error: " <> showt err
    Right val -> val

run_mkblock :: [TrackSpec] -> ([TrackId], Ui.State)
run_mkblock tracks = (tids, state)
    where
    ((_, tids), state) = run Ui.empty (mkblock (default_block_name, tracks))

run_mkview :: [TrackSpec] -> ([TrackId], Ui.State)
run_mkview tracks = run Ui.empty $ mkblock_view (default_block_name, tracks)

run_mkblocks :: [BlockSpec] -> ([BlockId], Ui.State)
run_mkblocks = run Ui.empty . mkblocks

mkblocks :: Ui.M m => [BlockSpec] -> m [BlockId]
mkblocks blocks = mapM (fmap fst . mkblock) blocks

mkviews :: Ui.M m => [BlockSpec] -> m [ViewId]
mkviews blocks = mapM mkview =<< mkblocks blocks

mkblock :: Ui.M m => BlockSpec -> m (BlockId, [TrackId])
mkblock (spec, tracks) = do
    let (block_id, title, has_ruler) = parse_block_spec spec
    ruler_id <- if has_ruler
        then do
            let len = event_end tracks
                rid = Id.id test_ns ("r" <> showt len)
            ifM (Maybe.isJust <$> Ui.lookup_ruler (Id.RulerId rid))
                (return (Id.RulerId rid))
                (Ui.create_ruler rid (mkruler_44 len 1))
        else maybe
            (Ui.create_ruler (Id.unpack_id default_ruler_id) default_ruler)
            (const (return default_ruler_id))
                =<< Ui.lookup_ruler default_ruler_id
    mkblock_ruler ruler_id block_id title tracks
    where
    event_end :: [TrackSpec] -> Int
    event_end = ceiling . ScoreTime.to_double . maximum . (0:)
        . concatMap (map (\(s, d, _) -> max s (s+d)) . snd)

mkblock_marklist :: Ui.M m => Ruler.Marklist -> BlockId -> Text
    -> [TrackSpec] -> m (BlockId, [TrackId])
mkblock_marklist marklist block_id title tracks = do
    ruler_id <- Create.ruler "r" $
        Ruler.meter_ruler Ruler.default_config marklist
    mkblock_ruler ruler_id block_id title tracks

mkblocks_skel :: Ui.M m => [(BlockSpec, [Skeleton.Edge])] -> m ()
mkblocks_skel blocks = forM_ blocks $ \(block, skel) -> do
    (block_id, track_ids) <- mkblock block
    Ui.set_skeleton block_id (Skeleton.make skel)
    return (block_id, track_ids)

-- | Like 'mkblock', but uses the provided ruler instead of creating its
-- own.  Important if you are creating multiple blocks and don't want
-- a separate ruler for each.
mkblock_ruler :: Ui.M m => RulerId -> BlockId -> Text -> [TrackSpec]
    -> m (BlockId, [TrackId])
mkblock_ruler ruler_id block_id title tracks = do
    Ui.set_namespace test_ns
    -- Start at 1 because track 0 is the ruler.
    tids <- forM (zip [1..] tracks) $ \(i, track) ->
        Ui.create_track (Id.unpack_id (mk_tid_block block_id i))
            (make_track track)
    create_block (Id.unpack_id block_id) "" $ (Block.RId ruler_id)
        : [Block.TId tid ruler_id | tid <- tids]
    unless (Text.null title) $
        Ui.set_block_title block_id title
    Ui.set_skeleton block_id =<< parse_skeleton block_id
    -- This ensures that any state created via these functions will have the
    -- default midi config.  This saves some hassle since all tests can assume
    -- there are some instruments defined.
    Ui.modify set_default_allocations
    return (block_id, tids)

create_block :: Ui.M m => Id.Id -> Text -> [Block.TracklikeId] -> m BlockId
create_block block_id title tracks =
    Ui.create_block block_id title [Block.track tid 30 | tid <- tracks]

parse_skeleton :: Ui.M m => BlockId -> m Skeleton.Skeleton
parse_skeleton block_id = do
    tracks <- TrackTree.tracks_of block_id
    return $ ParseSkeleton.default_parser tracks

mkview :: Ui.M m => BlockId -> m ViewId
mkview block_id = do
    block <- Ui.get_block block_id
    Ui.create_view (Id.unpack_id (mk_vid block_id)) $
        Block.view block block_id default_rect default_zoom

mkblock_view :: Ui.M m => BlockSpec -> m [TrackId]
mkblock_view block_spec = (snd <$> mkblock block_spec) <* mkview block_id
    where (block_id, _, _) = parse_block_spec (fst block_spec)

mk_vid :: BlockId -> ViewId
mk_vid block_id = Id.ViewId $ Id.id ns ("v." <> block_name)
    where (ns, block_name) = Id.un_id (Id.unpack_id block_id)

mk_vid_name :: Text -> ViewId
mk_vid_name = mk_vid . bid

-- | Make a TrackId as mkblock does.  This is so tests can independently come
-- up with the track IDs mkblock created just by knowing their tracknum.
mk_tid :: TrackNum -> TrackId
mk_tid = mk_tid_block default_block_id

mk_tid_block :: CallStack.Stack => BlockId -> TrackNum -> TrackId
mk_tid_block block_id i
    | i < 1 = errorStack $
        "mk_tid_block: event tracknums start at 1: " <> showt i
    | otherwise = Id.TrackId $ Create.ids_for ns block_name "t" !! (i-1)
    where (ns, block_name) = Id.un_id (Id.unpack_id block_id)

mk_tid_name :: Text -> TrackNum -> TrackId
mk_tid_name = mk_tid_block . bid

-- | Get a TrackNum back out of a 'mk_tid' call.
tid_tracknum :: TrackId -> TrackNum
tid_tracknum = parse . Seq.rtake_while Char.isDigit . untxt . Id.ident_name
    where
    parse "" = -1
    parse ds = read ds

-- * actions

insert_event_in :: Ui.M m => Text -> TrackNum -> (ScoreTime, ScoreTime, Text)
    -> m ()
insert_event_in block_name tracknum (pos, dur, text) =
    Ui.insert_event (mk_tid_name block_name tracknum) (Event.event pos dur text)

insert_event :: Ui.M m => TrackNum -> (ScoreTime, ScoreTime, Text) -> m ()
insert_event = insert_event_in default_block_name

remove_event_in :: Ui.M m => Text -> TrackNum -> ScoreTime -> m ()
remove_event_in name tracknum pos =
    Ui.remove_events_range (mk_tid_name name tracknum)
        (Events.Point pos Event.Positive)

remove_event :: Ui.M m => TrackNum -> ScoreTime -> m ()
remove_event = remove_event_in default_block_name

-- ** make specs

-- | This is a simplification of 'TrackSpec' that assumes one pitch per note.
-- It hardcodes the scale to @*@ and all the control tracks are under a single
-- note track, but in exchange it's easier to write than full TrackSpecs.
--
-- @(inst, [(t, dur, pitch)], [(control, [(t, val)])])@
--
-- If the pitch looks like \"a -- b\" then \"a\" is the note track's event and
-- \"b\" is the pitch track's event.
type NoteSpec = (Text, [EventSpec], [(Text, [(ScoreTime, Text)])])

note_spec :: NoteSpec -> [TrackSpec]
note_spec (inst, pitches, controls) =
    -- Filter empty tracks.  Otherwise an empty pitch track will override #=.,
    -- which will be confusing.
    filter (not . null . snd) $
        note_track : pitch_track : map control_track controls
    where
    note_track = (">" <> inst, [(t, dur, s) | (t, dur, (s, _)) <- track])
    pitch_track =
        ("*", [(t, 0, s) | (t, _, (_, s)) <- track, not (Text.null s)])
    control_track (title, events) = (title, [(t, 0, val) | (t, val) <- events])
    track = [(t, d, split s) | (t, d, s) <- pitches]
    split s
        | "--" `Text.isInfixOf` s = let (pre, post) = TextUtil.split1 "--" s
            in (Text.strip pre, Text.strip post)
        | otherwise = ("", s)

-- | Abbreviation for 'note_spec' where the inst and controls are empty.
note_track :: [EventSpec] -> [TrackSpec]
note_track pitches = note_spec ("", pitches, [])

inst_note_track :: (Text, [EventSpec]) -> [TrackSpec]
inst_note_track (inst, pitches) = note_spec (inst, pitches, [])

-- | Like 'note_track', but all notes have a duration of 1.
note_track1 :: [Text] -> [TrackSpec]
note_track1 ps = note_track [(s, 1, p) | (s, p) <- zip (Seq.range_ 0 1) ps]

control_track :: [(ScoreTime, Text)] -> [EventSpec]
control_track ns = [(t, 0, s) | (t, s) <- ns]

regular_notes :: Int -> [TrackSpec]
regular_notes n = note_track $
    take n [(t, 1, p) | (t, p) <- zip (Seq.range_ 0 1) (cycle pitches)]
    where
    pitches =
        [Text.singleton o <> Text.singleton p | o <- "34567", p <- "cdefgab"]

-- | Parse a TrackSpec back out to a NoteSpec.
to_note_spec :: [TrackSpec] -> [NoteSpec]
to_note_spec = mapMaybe parse . Seq.split_with (ParseTitle.is_note_track . fst)
    where
    parse [] = Nothing
    parse ((inst, notes) : controls) =
        Just (Text.drop 1 inst, add_pitches pitches notes, [])
        where
        pitches = maybe [] snd $
            List.find (ParseTitle.is_pitch_track . fst) controls

-- | Like 'to_note_spec' but expect just notes and pitches, no controls.
to_pitch_spec :: [NoteSpec] -> [[EventSpec]]
to_pitch_spec = filter (not . null) . map (\(_, events, _) -> events)

add_pitches :: [EventSpec] -> [EventSpec] -> [EventSpec]
add_pitches = go ""
    where
    go p pitches ns@((nt, nd, n) : notes)
        | ((pt, _, nextp) : restp) <- pitches, nt >= pt = go nextp restp ns
        | otherwise = (nt, nd, add p n) : go p pitches notes
    go _ _ [] = []
    add p n = Text.intercalate " -- " $ filter (not . Text.null) [p, n]


-- * state to spec

dump_blocks :: Ui.State -> [(BlockId, (BlockSpec, [Skeleton.Edge]))]
dump_blocks state = zip block_ids (map (flip dump_block state) block_ids)
    where block_ids = Map.keys (Ui.state_blocks state)

dump_block :: BlockId -> Ui.State -> (BlockSpec, [Skeleton.Edge])
dump_block block_id state =
    ((name <> if Text.null title then "" else " -- " <> title,
        map dump_track (Maybe.catMaybes tracks)), skel)
    where
    (id_str, title, tracks, skel) = eval state (Simple.dump_block block_id)
    name = snd $ Id.un_id $ Id.read_id id_str
    dump_track (_, title, events) = (title, map convert events)
    convert (start, dur, text) =
        (ScoreTime.double start, ScoreTime.double dur, text)

-- | Like 'dump_block' but strip out everything but the tracks.
extract_tracks_of :: BlockId -> Ui.State -> [TrackSpec]
extract_tracks_of block_id state = tracks
    where ((_, tracks), _) = dump_block block_id state

-- | Get the names and tracks of the default block.
extract_tracks :: Ui.State -> [TrackSpec]
extract_tracks = extract_tracks_of default_block_id

extract_blocks :: Ui.State -> [BlockSpec]
extract_blocks = map (first Id.ident_name) . extract_block_ids

extract_block_ids :: Ui.State -> [(BlockId, [TrackSpec])]
extract_block_ids state =
    zip block_ids (map (flip extract_tracks_of state) block_ids)
    where block_ids = Map.keys (Ui.state_blocks state)

extract_skeleton :: Ui.State -> [(TrackNum, TrackNum)]
extract_skeleton = maybe [] (Skeleton.flatten . Block.block_skeleton)
    . Map.lookup default_block_id . Ui.state_blocks

extract_track_ids :: Ui.State -> [(BlockId, [TrackId])]
extract_track_ids state =
    [(block_id, tracks_of block) | (block_id, block)
        <- Map.toList (Ui.state_blocks state)]
    where
    tracks_of = Block.track_ids_of . map Block.tracklike_id . Block.block_tracks

-- * view

select :: Ui.M m => ViewId -> Sel.Selection -> m ()
select view_id sel =
    Ui.set_selection view_id Config.insert_selnum (Just sel)

select_point :: Ui.M m => ViewId -> TrackNum -> ScoreTime -> m ()
select_point view_id tracknum pos =
    select view_id (Sel.point tracknum pos Sel.Positive)

-- * non-monadic make_- functions

mkstack :: (TrackNum, ScoreTime, ScoreTime) -> Stack.Stack
mkstack (tracknum, s, e) = mkstack_block (default_block_name, tracknum, s, e)

mkstack_block :: (Text, TrackNum, ScoreTime, ScoreTime) -> Stack.Stack
mkstack_block (block, tracknum, s, e) = Stack.from_outermost
    [Stack.Block (bid block), Stack.Track (mk_tid_name block tracknum),
        Stack.Region s e]

-- ** track

make_track :: TrackSpec -> Track.Track
make_track (title, events) = Track.modify_events
    (Events.insert (map make_event events)) (empty_track title)

empty_track :: Text -> Track.Track
empty_track title = (Track.track title Events.empty)
    { Track.track_render = Track.no_render }

-- ** event

make_event :: EventSpec -> Event.Event
make_event (start, dur, text) = Event.event start dur text

extract_event :: Event.Event -> EventSpec
extract_event e = (Event.start e, Event.duration e, Event.text e)

-- ** ruler

default_ruler :: Ruler.Ruler
default_ruler = mkruler_44 32 1

default_block_end :: ScoreTime
default_block_end = Ruler.time_end default_ruler

no_ruler :: Ruler.Ruler
no_ruler = ruler []

-- | TimeStep to step by 1 ScoreTime on the default ruler.
step1 :: TimeStep.TimeStep
step1 = TimeStep.time_step (TimeStep.AbsoluteMark TimeStep.AllMarklists r_4)

-- | Create a ruler with a 4/4 "meter" marklist with the given number of marks
-- at the given distance.  Marks are rank [1, 2, 2, ...].
--
-- The end of the ruler should be at marks*dist.  An extra mark is created
-- since marks start at 0.
mkruler_44 :: Int -> ScoreTime -> Ruler.Ruler
mkruler_44 marks dist = ruler_ $ meter_marklist $ take (marks + 1) $
    zip3 (Seq.range_ 0 dist) (cycle [r_1, r_4, r_4, r_4]) labels
    where
    labels = concatMap measure [1..]
        where
        measure n = map (showt n <>) ["", ".2", ".3", ".4"]

meter_marklist :: [(TrackTime, Ruler.Rank, Text)] -> Ruler.Marklist
meter_marklist marks =
    Ruler.marklist [(t, mark rank label) | (t, rank, label) <- marks]
    where
    mark rank label = Ruler.null_mark
        { Ruler.mark_rank = rank, Ruler.mark_name = label }

ruler :: [(TrackTime, Ruler.Rank)] -> Ruler.Ruler
ruler marks = ruler_ (mk_marklist marks)

mk_marklist :: [(TrackTime, Ruler.Rank)] -> Ruler.Marklist
mk_marklist = Ruler.marklist . map (second mark)
    where
    mark rank = Ruler.null_mark
        { Ruler.mark_rank = rank, Ruler.mark_name = showt rank }

ruler_ :: Ruler.Marklist -> Ruler.Ruler
ruler_ meter = Ruler.set_meter Ruler.default_config meter $ Ruler.Ruler
    { Ruler.ruler_marklists = mempty
    , Ruler.ruler_bg = Config.ruler_bg
    , Ruler.ruler_show_names = False
    , Ruler.ruler_align_to_bottom = False
    }

r_1, r_4 :: Ruler.Rank
r_1 = Meter.r_1
r_4 = Meter.r_4

-- * allocations

midi_allocation :: Text -> Patch.Config -> UiConfig.Allocation
midi_allocation qualified config = UiConfig.allocation
    (InstTypes.parse_qualified qualified) (UiConfig.Midi config)

-- | Make Simple.Allocations from (inst, qualified, [chan]).
allocations :: [(Text, Text, [Midi.Channel])] -> UiConfig.Allocations
allocations allocs = either errorStack id $
    Simple.allocations (const $ Just settings)
        [ (inst, (qualified, map (wdev_name,) chans))
        | (inst, qualified, chans) <- allocs
        ]
    where settings = Patch.patch_defaults $ make_patch "test"
    -- Since the lookup is just const, it should never return Left.

set_default_allocations :: Ui.State -> Ui.State
set_default_allocations = Ui.config#Ui.allocations #= default_allocations

default_allocations :: UiConfig.Allocations
default_allocations = allocations
    [ ("i", "s/1", [0..2])
    , ("i1", "s/1", [0..2])
    , ("i2", "s/2", [3])
    , ("i3", "s/3", [4])
    ]

modify_midi_config :: CallStack.Stack => Text -> (Patch.Config -> Patch.Config)
    -> UiConfig.Allocations -> UiConfig.Allocations
modify_midi_config inst_ modify =
    Testing.expect_right . UiConfig.modify_allocation inst modify_alloc
    where
    inst = Score.Instrument inst_
    modify_alloc alloc = do
        config <- justErr ("not a midi alloc: " <> pretty inst) $
            UiConfig.midi_config (UiConfig.alloc_backend alloc)
        return $ alloc
            { UiConfig.alloc_backend = UiConfig.Midi (modify config) }

i1, i2, i3 :: Score.Instrument
i1 = Score.Instrument "i1"
i2 = Score.Instrument "i2"
i3 = Score.Instrument "i3"

i1_qualified :: InstTypes.Qualified
i1_qualified = InstTypes.Qualified "s" "1"

wdev :: Midi.WriteDevice
wdev = Midi.write_device wdev_name

wdev_name :: Text
wdev_name = "wdev"

-- * instrument db

default_db :: Cmd.InstrumentDb
default_db = make_db
    [ ("s", map make_patch ["1", "2", "3"])
    -- Lilypond.Constants.ly_synth
    , ("ly", [make_patch "global"])
    ]

make_patch :: InstTypes.Name -> Patch.Patch
make_patch name = Patch.patch (-2, 2) name

make_db :: [(Text, [Patch.Patch])] -> Cmd.InstrumentDb
make_db synth_patches = fst $ Inst.db $ map make synth_patches
    where
    make (name, patches) = make_synth name (map MidiInst.make_patch patches)

make_synth :: InstTypes.SynthName -> [MidiInst.Patch] -> MidiInst.Synth
make_synth name patches = MidiInst.synth name "Test Synth" patches

-- * misc

btrack :: TrackId -> Block.Track
btrack track_id = Block.track (Block.TId track_id default_ruler_id) 30
