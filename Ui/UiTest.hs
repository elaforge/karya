-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
module Ui.UiTest where
import qualified Control.Monad.Identity as Identity
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified Util.CallStack as CallStack
import qualified Util.Rect as Rect
import qualified Util.Seq as Seq
import qualified Util.Testing as Testing

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
import qualified Ui.State as State
import qualified Ui.StateConfig as StateConfig
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Types as Types

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Cmd.Meter as Meter
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

mkid :: String -> Id.Id
mkid = Id.read_short test_ns . txt

bid :: String -> BlockId
bid = Id.BlockId . mkid

vid :: String -> ViewId
vid = Id.ViewId . mkid

tid :: String -> TrackId
tid = Id.TrackId . mkid

rid :: String -> RulerId
rid = Id.RulerId . mkid

test_ns :: Id.Namespace
test_ns = Id.namespace "test"

default_zoom :: Types.Zoom
default_zoom = Config.zoom

-- * monadic mk- functions

-- | (block_id, tracks)
--
-- If the name ends with @=ruler@, then the length of the ruler is derived from
-- the events inside, rather than being hardcoded.  This is convenient for
-- tests and lets them avoid hardcoding the default_ruler end.
--
-- Also, if the name contains @--@, the text after it becomes the block title.
type BlockSpec = (String, [TrackSpec])

-- | (track_title, events)
type TrackSpec = (String, [EventSpec])

-- | (start, dur, text)
type EventSpec = (ScoreTime, ScoreTime, String)

-- | Parse a block spec, which looks like @name[=ruler] [-- title]@
parse_block_spec :: String -> (BlockId, String, Bool)
parse_block_spec spec = (bid block_id, Seq.strip title, has_ruler)
    where
    (name, title) = Seq.split1 "--" spec
    (block_id, has_ruler) = Seq.drop_suffix "=ruler" (Seq.strip name)

-- | Often tests work with a single block, or a single view.  To make them
-- less verbose, there is a default block and view so functions can omit the
-- parameter if convenient.
default_block_id :: BlockId
default_block_id = bid default_block_name

default_block_name :: String
default_block_name = "b1"

default_view_id :: ViewId
default_view_id = mk_vid default_block_id

default_ruler_id :: RulerId
default_ruler_id = rid "r0"
    -- r1 would conflict with ruler automatically generated because of the
    -- =ruler suffix

-- | Return the val and state, throwing an IO error on an exception.  Intended
-- for tests that don't expect to fail here.
run :: CallStack.Stack => State.State -> State.StateId a -> (a, State.State)
run state m = case result of
        Left err -> errorStack $ "state error: " <> showt err
        Right (val, state', _) -> (val, state')
    where result = Identity.runIdentity (State.run state m)

exec :: CallStack.Stack => State.State -> State.StateId a -> State.State
exec state m = case State.exec state m of
    Left err -> errorStack $ "state error: " <> showt err
    Right state' -> state'

eval :: CallStack.Stack => State.State -> State.StateId a -> a
eval state m = case State.eval state m of
    Left err -> errorStack $ "state error: " <> showt err
    Right val -> val

run_mkblock :: [TrackSpec] -> ([TrackId], State.State)
run_mkblock tracks = (tids, state)
    where
    ((_, tids), state) = run State.empty (mkblock (default_block_name, tracks))

run_mkview :: [TrackSpec] -> ([TrackId], State.State)
run_mkview tracks = run State.empty $ mkblock_view (default_block_name, tracks)

run_mkblocks :: [BlockSpec] -> ([BlockId], State.State)
run_mkblocks = run State.empty . mkblocks

mkblocks :: State.M m => [BlockSpec] -> m [BlockId]
mkblocks blocks = mapM (fmap fst . mkblock) blocks

mkviews :: State.M m => [BlockSpec] -> m [ViewId]
mkviews blocks = mapM mkview =<< mkblocks blocks

mkblock :: State.M m => BlockSpec -> m (BlockId, [TrackId])
mkblock (spec, tracks) = do
    let (block_id, title, has_ruler) = parse_block_spec spec
    ruler_id <- if has_ruler
        then do
            let len = event_end tracks
                rid = Id.id test_ns ("r" <> showt len)
            ifM (Maybe.isJust <$> State.lookup_ruler (Id.RulerId rid))
                (return (Id.RulerId rid))
                (State.create_ruler rid (mkruler_44 len 1))
        else maybe
            (State.create_ruler (Id.unpack_id default_ruler_id) default_ruler)
            (const (return default_ruler_id))
                =<< State.lookup_ruler default_ruler_id
    mkblock_ruler ruler_id block_id title tracks
    where
    event_end :: [TrackSpec] -> Int
    event_end = ceiling . ScoreTime.to_double . maximum . (0:)
        . concatMap (map (\(s, d, _) -> max s (s+d)) . snd)

mkblock_marklist :: State.M m => Ruler.Marklist -> BlockId -> String
    -> [TrackSpec] -> m (BlockId, [TrackId])
mkblock_marklist marklist block_id title tracks = do
    ruler_id <- Create.ruler "r" $
        Ruler.meter_ruler (Just Meter.mtype_meter)  marklist
    mkblock_ruler ruler_id block_id title tracks

mkblocks_skel :: State.M m => [(BlockSpec, [Skeleton.Edge])] -> m ()
mkblocks_skel blocks = forM_ blocks $ \(block, skel) -> do
    (block_id, track_ids) <- mkblock block
    State.set_skeleton block_id (Skeleton.make skel)
    return (block_id, track_ids)

-- | Like 'mkblock', but uses the provided ruler instead of creating its
-- own.  Important if you are creating multiple blocks and don't want
-- a separate ruler for each.
mkblock_ruler :: State.M m => RulerId -> BlockId -> String -> [TrackSpec]
    -> m (BlockId, [TrackId])
mkblock_ruler ruler_id block_id title tracks = do
    State.set_namespace test_ns
    -- Start at 1 because track 0 is the ruler.
    tids <- forM (zip [1..] tracks) $ \(i, track) ->
        State.create_track (Id.unpack_id (mk_tid_block block_id i))
            (make_track track)
    create_block (Id.unpack_id block_id) "" $ (Block.RId ruler_id)
        : [Block.TId tid ruler_id | tid <- tids]
    unless (null title) $
        State.set_block_title block_id (txt title)
    State.set_skeleton block_id =<< parse_skeleton block_id
    -- This ensures that any state created via these functions will have the
    -- default midi config.  This saves some hassle since all tests can assume
    -- there are some instruments defined.
    State.modify set_default_allocations
    return (block_id, tids)

create_block :: State.M m => Id.Id -> String -> [Block.TracklikeId] -> m BlockId
create_block block_id title tracks = State.create_block block_id
    (txt title) [Block.track tid 30 | tid <- tracks]

parse_skeleton :: State.M m => BlockId -> m Skeleton.Skeleton
parse_skeleton block_id = do
    tracks <- TrackTree.tracks_of block_id
    return $ ParseSkeleton.default_parser tracks

mkview :: State.M m => BlockId -> m ViewId
mkview block_id = do
    block <- State.get_block block_id
    State.create_view (Id.unpack_id (mk_vid block_id)) $
        Block.view block block_id default_rect default_zoom

mkblock_view :: State.M m => BlockSpec -> m [TrackId]
mkblock_view block_spec = (snd <$> mkblock block_spec) <* mkview block_id
    where (block_id, _, _) = parse_block_spec (fst block_spec)

mk_vid :: BlockId -> ViewId
mk_vid block_id = Id.ViewId $ Id.id ns ("v." <> block_name)
    where (ns, block_name) = Id.un_id (Id.unpack_id block_id)

mk_vid_name :: String -> ViewId
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

mk_tid_name :: String -> TrackNum -> TrackId
mk_tid_name = mk_tid_block . bid

-- | Get a TrackNum back out of a 'mk_tid' call.
tid_tracknum :: TrackId -> TrackNum
tid_tracknum = parse . Seq.rtake_while Char.isDigit . untxt . Id.ident_name
    where
    parse "" = -1
    parse ds = read ds

-- * actions

insert_event_in :: State.M m => String -> TrackNum
    -> (ScoreTime, ScoreTime, String) -> m ()
insert_event_in block_name tracknum (pos, dur, text) =
    State.insert_event (mk_tid_name block_name tracknum)
        (Event.event pos dur (txt text))

insert_event :: State.M m => TrackNum -> (ScoreTime, ScoreTime, String) -> m ()
insert_event = insert_event_in default_block_name

remove_event_in :: State.M m => String -> TrackNum -> ScoreTime -> m ()
remove_event_in name tracknum = State.remove_event (mk_tid_name name tracknum)

remove_event :: State.M m => TrackNum -> ScoreTime -> m ()
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
type NoteSpec = (String, [EventSpec], [(String, [(ScoreTime, String)])])

note_spec :: NoteSpec -> [TrackSpec]
note_spec (inst, pitches, controls) =
    note_track : pitch_track : map control_track controls
    where
    note_track = ('>' : inst, [(t, dur, s) | (t, dur, (s, _)) <- track])
    pitch_track = ("*", [(t, 0, s) | (t, _, (_, s)) <- track, not (null s)])
    control_track (title, events) = (title, [(t, 0, val) | (t, val) <- events])

    track = [(t, d, split s) | (t, d, s) <- pitches]
    split s
        | "--" `List.isInfixOf` s = let (pre, post) = Seq.split1 "--" s
            in (Seq.strip pre, Seq.strip post)
        | otherwise = ("", s)

-- | Abbreviation for 'note_spec' where the inst and controls are empty.
note_track :: [EventSpec] -> [TrackSpec]
note_track pitches = note_spec ("", pitches, [])

inst_note_track :: (String, [EventSpec]) -> [TrackSpec]
inst_note_track (inst, pitches) = note_spec (inst, pitches, [])

-- | Like 'note_track', but all notes have a duration of 1.
note_track1 :: [String] -> [TrackSpec]
note_track1 ps = note_track [(s, 1, p) | (s, p) <- zip (Seq.range_ 0 1) ps]

regular_notes :: Int -> [TrackSpec]
regular_notes n = note_track $ take n
    [(t, 1, p) | (t, p) <- zip (Seq.range_ 0 1) (cycle pitches)]
    where pitches = [o:p:"" | o <- "34567", p <- "cdefgab"]

-- | Parse a TrackSpec back out to a NoteSpec.
to_note_spec :: [TrackSpec] -> [NoteSpec]
to_note_spec =
    mapMaybe parse . Seq.split_with (ParseTitle.is_note_track . txt . fst)
    where
    parse [] = Nothing
    parse ((inst, notes) : controls) =
        Just (drop 1 inst, add_pitches pitches notes, [])
        where
        pitches = maybe [] snd $
            List.find (ParseTitle.is_pitch_track . txt . fst) controls

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
    add p n = List.intercalate " -- " $ filter (not . null) [p, n]


-- * state to spec

dump_block :: BlockId -> State.State -> (BlockSpec, [Skeleton.Edge])
dump_block block_id state =
    ((name ++ if Text.null title then "" else " -- " ++ untxt title,
        map dump_track (Maybe.catMaybes tracks)), skel)
    where
    (id_str, title, tracks, skel) = eval state (Simple.dump_block block_id)
    name = untxt $ snd $ Id.un_id $ Id.read_id id_str
    dump_track (_, title, events) = (untxt title, map convert events)
    convert (start, dur, text) =
        (ScoreTime.double start, ScoreTime.double dur, untxt text)

-- | Like 'dump_block' but strip out everything but the tracks.
extract_tracks_of :: BlockId -> State.State -> [TrackSpec]
extract_tracks_of block_id state = tracks
    where ((_, tracks), _) = dump_block block_id state

-- | Get the names and tracks of the default block.
extract_tracks :: State.State -> [TrackSpec]
extract_tracks = extract_tracks_of default_block_id

extract_all_tracks :: State.State -> [(BlockId, [TrackSpec])]
extract_all_tracks state =
    zip block_ids (map (flip extract_tracks_of state) block_ids)
    where block_ids = Map.keys (State.state_blocks state)

extract_skeleton :: State.State -> [(TrackNum, TrackNum)]
extract_skeleton = maybe [] (Skeleton.flatten . Block.block_skeleton)
    . Map.lookup default_block_id . State.state_blocks

extract_track_ids :: State.State -> [(BlockId, [TrackId])]
extract_track_ids state =
    [(block_id, tracks_of block) | (block_id, block)
        <- Map.toList (State.state_blocks state)]
    where
    tracks_of = Block.track_ids_of . map Block.tracklike_id . Block.block_tracks

-- * view

select :: State.M m => ViewId -> Sel.Selection -> m ()
select view_id sel =
    State.set_selection view_id Config.insert_selnum (Just sel)

select_point :: State.M m => ViewId -> TrackNum -> ScoreTime -> m ()
select_point view_id tracknum pos = select view_id (Sel.point tracknum pos)

-- * non-monadic make_- functions

mkstack :: (TrackNum, ScoreTime, ScoreTime) -> Stack.Stack
mkstack (tracknum, s, e) = mkstack_block (default_block_name, tracknum, s, e)

mkstack_block :: (String, TrackNum, ScoreTime, ScoreTime) -> Stack.Stack
mkstack_block (block, tracknum, s, e) = Stack.from_outermost
    [Stack.Block (bid block), Stack.Track (mk_tid_name block tracknum),
        Stack.Region s e]

-- ** track

make_track :: TrackSpec -> Track.Track
make_track (title, events) = Track.modify_events
    (Events.insert (map make_event events)) (empty_track title)

empty_track :: String -> Track.Track
empty_track title = (Track.track (txt title) Events.empty)
    { Track.track_render = Track.no_render }

-- ** event

make_event :: EventSpec -> Event.Event
make_event (start, dur, text) = Event.event start dur (txt text)

extract_event :: Event.Event -> EventSpec
extract_event e = (Event.start e, Event.duration e, untxt $ Event.text e)

-- ** ruler

default_ruler :: Ruler.Ruler
default_ruler = mkruler_44 32 1

default_block_end :: ScoreTime
default_block_end = Ruler.time_end default_ruler

type Marks = [(ScoreTime, Ruler.Rank)]

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
mkruler_44 marks dist = ruler $ take (marks + 1) $
    zip (Seq.range_ 0 dist) (cycle [r_1, r_4, r_4, r_4])

ruler :: Marks -> Ruler.Ruler
ruler marks = ruler_ [(Ruler.meter, mkmarklist marks)]

mkmarklist :: Marks -> Ruler.Marklist
mkmarklist = Ruler.marklist . map (second mark)
    where
    mark rank = Ruler.null_mark
        { Ruler.mark_rank = rank, Ruler.mark_name = showt rank }

ruler_ :: [(Ruler.Name, Ruler.Marklist)] -> Ruler.Ruler
ruler_ marklists = Ruler.Ruler
    { Ruler.ruler_marklists =
        Map.fromList $ map (second ((,) (Just Meter.mtype_meter))) marklists
    , Ruler.ruler_bg = Config.ruler_bg
    , Ruler.ruler_show_names = False
    , Ruler.ruler_align_to_bottom = False
    }

r_1, r_4 :: Ruler.Rank
r_1 = Meter.r_1
r_4 = Meter.r_4

-- * allocations

midi_allocation :: Text -> Patch.Config -> StateConfig.Allocation
midi_allocation qualified config = StateConfig.allocation
    (InstTypes.parse_qualified qualified) (StateConfig.Midi config)

-- | Make Simple.Allocations from (inst, qualified, [chan]).
allocations :: [(Text, Text, [Midi.Channel])] -> StateConfig.Allocations
allocations allocs = either errorStack id $
    Simple.allocations (const $ Just settings)
        [ (inst, (qualified, map (wdev_name,) chans))
        | (inst, qualified, chans) <- allocs
        ]
    where settings = Patch.patch_defaults $ make_patch "test"
    -- Since the lookup is just const, it should never return Left.

set_default_allocations :: State.State -> State.State
set_default_allocations = State.config#State.allocations #= default_allocations

default_allocations :: StateConfig.Allocations
default_allocations = allocations
    [ ("i", "s/1", [0..2])
    , ("i1", "s/1", [0..2])
    , ("i2", "s/2", [3])
    , ("i3", "s/3", [4])
    ]

modify_midi_config :: CallStack.Stack =>
    Text -> (Patch.Config -> Patch.Config)
    -> StateConfig.Allocations -> StateConfig.Allocations
modify_midi_config inst_ modify =
    Testing.expect_right . StateConfig.modify_allocation inst modify_alloc
    where
    inst = Score.Instrument inst_
    modify_alloc alloc = do
        config <- justErr ("not a midi alloc: " <> pretty inst) $
            StateConfig.midi_config (StateConfig.alloc_backend alloc)
        return $ alloc
            { StateConfig.alloc_backend = StateConfig.Midi (modify config) }

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
default_db = make_db [("s", map make_patch ["1", "2", "3"])]

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

-- | Visualize event ranges.
draw_events :: [EventSpec] -> IO ()
draw_events events =
    Text.IO.putStrLn ruler >> mapM_ (Text.IO.putStrLn . draw) events
    where
    draw (start, dur, t) = gap <> arrow <> " [" <> txt t <> "]"
        where
        gap = Text.replicate (to_steps (min start (start + dur))) " "
        arrow
            | dur < 0 = "<" <> middle <> "|"
            | otherwise = "|" <> middle <> ">"
        middle = Text.replicate (to_steps (abs dur) - 1) "-"
    to_steps t = floor (t / 0.25)
    ruler = Text.intercalate (Text.replicate (to_steps 1 - 1) " ")
        (take 12 (map showt [0..]))
