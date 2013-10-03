-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ui.UiTest where
import qualified Control.Monad.Identity as Identity
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text

import qualified System.IO as IO

import Util.Control
import qualified Util.PPrint as PPrint
import qualified Util.Rect as Rect
import qualified Util.Seq as Seq

import qualified Midi.Midi as Midi
import qualified Ui.Block as Block
import qualified Ui.Color as Color
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.Skeleton as Skeleton
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Types as Types

import qualified Cmd.Create as Create
import qualified Cmd.Meter as Meter
import qualified Cmd.Save as Save
import qualified Cmd.Simple as Simple
import qualified Cmd.TimeStep as TimeStep

import qualified Derive.ParseSkeleton as ParseSkeleton
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack

import qualified Perform.Midi.Instrument as Instrument
import qualified App.Config as Config
import Types


-- | (10, 50) seems to be the smallest x,y OS X will accept.  Apparently
-- fltk's sizes don't take the menu bar into account, which is about 44 pixels
-- high, so a y of 44 is the minimum.
default_rect :: Rect.Rect
default_rect = Rect.xywh 10 50 200 200
default_divider :: Block.Divider
default_divider = Block.Divider Color.blue

-- state

test_ns = Id.unsafe_namespace "test"
mkid name =
    fromMaybe (error $ "UiTest.mkid: invalid characters in " ++ show name) $
        Id.read_short test_ns name
bid = Types.BlockId . mkid
vid = Types.ViewId . mkid
tid = Types.TrackId . mkid
rid = Types.RulerId . mkid

default_zoom :: Types.Zoom
default_zoom = Config.zoom

-- | Save the state to disk, so I can load it into the app and see it.
save :: FilePath -> State.State -> IO ()
save = Save.write_state

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

spec_block_id :: String -> (BlockId, Bool)
spec_block_id = first bid . Seq.drop_suffix "=ruler"

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
run :: State.State -> State.StateId a -> (a, State.State)
run state m = case result of
        Left err -> error $ "state error: " ++ show err
        Right (val, state', _) -> (val, state')
    where result = Identity.runIdentity (State.run state m)

exec :: State.State -> State.StateId a -> State.State
exec state m = case State.exec state m of
    Left err -> error $ "state error: " ++ show err
    Right state' -> state'

eval :: State.State -> State.StateId a -> a
eval state m = case State.eval state m of
    Left err -> error $ "state error: " ++ show err
    Right val -> val

run_mkblock :: [TrackSpec] -> ([TrackId], State.State)
run_mkblock track_specs = (tids, state)
    where
    ((_, tids), state) =
        run State.empty (mkblock (default_block_name, track_specs))

run_mkview :: [TrackSpec] -> ([TrackId], State.State)
run_mkview track_specs =
    run State.empty (mkblock_view (default_block_name, track_specs))

mkblocks :: (State.M m) => [BlockSpec] -> m [BlockId]
mkblocks blocks = mapM (fmap fst . mkblock) blocks

mkviews :: (State.M m) => [BlockSpec] -> m [ViewId]
mkviews blocks = mapM mkview =<< mkblocks blocks

mkblock :: (State.M m) => BlockSpec -> m (BlockId, [TrackId])
mkblock (block_name, tracks) = do
    let (name, title) = (Seq.strip *** Seq.strip) $ Seq.split1 "--" block_name
    let (block_id, has_ruler) = spec_block_id name
    ruler_id <- if has_ruler
        then do
            let len = event_end tracks
                rid = Id.unsafe_id test_ns ("r" ++ show len)
            ifM (Maybe.isJust <$> State.lookup_ruler (Types.RulerId rid))
                (return (Types.RulerId rid))
                (State.create_ruler rid (mkruler len 1))
        else maybe
            (State.create_ruler (Id.unpack_id default_ruler_id) default_ruler)
            (const (return default_ruler_id))
                =<< State.lookup_ruler default_ruler_id
    mkblock_ruler ruler_id (Id.ident_name block_id ++ "--" ++ title, tracks)
    where
    event_end :: [TrackSpec] -> Int
    event_end = ceiling . ScoreTime.to_double . maximum . (0:)
        . concatMap (map (\(s, d, _) -> max s (s+d)) . snd)

mkblocks_skel :: (State.M m) => [(BlockSpec, [Skeleton.Edge])] -> m ()
mkblocks_skel blocks = forM_ blocks $ \(block, skel) -> do
    (block_id, track_ids) <- mkblock block
    State.set_skeleton block_id (Skeleton.make skel)
    return (block_id, track_ids)

-- | Like 'mkblock', but uses the provided ruler instead of creating its
-- own.  Important if you are creating multiple blocks and don't want
-- a separate ruler for each.
mkblock_ruler :: (State.M m) => RulerId -> BlockSpec -> m (BlockId, [TrackId])
mkblock_ruler ruler_id (block_name, tracks) = do
    let (name, title) = (Seq.strip *** Seq.strip) $ Seq.split1 "--" block_name
    let block_id = bid name
    State.set_namespace test_ns
    -- Start at 1 because track 0 is the ruler.
    tids <- forM (zip [1..] tracks) $ \(i, track) ->
        State.create_track (Id.unpack_id (mk_tid_block block_id i))
            (make_track track)
    create_block name "" $ (Block.RId ruler_id, 20)
        : [(Block.TId tid ruler_id, 40) | tid <- tids]
    unless (null title) $
        State.set_block_title block_id (txt title)
    State.set_skeleton block_id =<< parse_skeleton block_id
    return (block_id, tids)

parse_skeleton :: (State.M m) => BlockId -> m Skeleton.Skeleton
parse_skeleton block_id = do
    tracks <- TrackTree.tracks_of block_id
    return $ ParseSkeleton.default_parser tracks

mkview :: (State.M m) => BlockId -> m ViewId
mkview block_id = do
    block <- State.get_block block_id
    State.create_view (Id.unpack_id (mk_vid block_id)) $
        Block.view block block_id default_rect default_zoom

mkblock_view :: (State.M m) => BlockSpec -> m [TrackId]
mkblock_view block_spec =
    (snd <$> mkblock block_spec) <* mkview (bid (fst block_spec))

mk_vid :: BlockId -> ViewId
mk_vid block_id = Types.ViewId $ Id.unsafe_id ns ("v." ++ block_name)
    where (ns, block_name) = Id.un_id (Id.unpack_id block_id)

mk_vid_name :: String -> ViewId
mk_vid_name = mk_vid . bid

-- | Make a TrackId as mkblock does.  This is so tests can independently come
-- up with the track IDs mkblock created just by knowing their tracknum.
mk_tid :: TrackNum -> TrackId
mk_tid = mk_tid_block default_block_id

mk_tid_block :: BlockId -> TrackNum -> TrackId
mk_tid_block block_id i
    | i < 1 = error $ "mk_tid_block: event tracknums start at 1: " ++ show i
    | otherwise = Types.TrackId $ Create.ids_for ns block_name "t" !! (i-1)
    where (ns, block_name) = Id.un_id (Id.unpack_id block_id)

mk_tid_name :: String -> TrackNum -> TrackId
mk_tid_name = mk_tid_block . bid

-- * actions

insert_event_in :: (State.M m) => String -> TrackNum
    -> (ScoreTime, ScoreTime, String) -> m ()
insert_event_in block_name tracknum (pos, dur, text) =
    State.insert_event (mk_tid_name block_name tracknum)
        (Event.event pos dur text)

insert_event :: (State.M m) => TrackNum -> (ScoreTime, ScoreTime, String)
    -> m ()
insert_event = insert_event_in default_block_name

remove_event_in :: (State.M m) => String -> TrackNum -> ScoreTime -> m ()
remove_event_in name tracknum = State.remove_event (mk_tid_name name tracknum)

remove_event :: (State.M m) => TrackNum -> ScoreTime -> m ()
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

regular_notes :: Int -> [TrackSpec]
regular_notes n = note_track $ take n
    [(t, 1, p) | (t, p) <- zip (Seq.range_ 0 1) (cycle pitches)]
    where pitches = [o:p:"" | o <- "34567", p <- "cdefgab"]

-- * state to spec

-- | Dump a score, or part of a score, to paste into a test.
-- (global_transform, midi_config, aliases, blocks)
type Dump = (Text, MidiConfig, Aliases, [(BlockSpec, [Skeleton.Edge])])
type Aliases = [(Text, Text)]

-- | These can be used from 'Cmd.Repl.LDebug.dump_blocks' to dump state in
-- a form that can be pasted into a test, trimmed down by hand, and passed to
-- 'read_dump'.  This way problems that show up in the app can be pasted
-- into a test.
write_dump :: FilePath -> State.State -> IO ()
write_dump fname = IO.writeFile fname . PPrint.pshow . dump_blocks

read_dump :: Dump -> State.State
read_dump (global_transform, midi, aliases, blocks) = exec State.empty $ do
    mkblocks_skel blocks
    State.modify $
        (State.config#State.global_transform #= global_transform)
        . (State.config#State.midi #= midi_config midi)
        . (State.config#State.aliases #=
            Map.fromList (map (Score.Instrument *** Score.Instrument) aliases))

dump_blocks :: State.State -> Dump
dump_blocks state =
    ( State.config#State.global_transform #$ state
    , dump_midi_config $ State.config#State.midi #$ state
    , map (Score.inst_name *** Score.inst_name) . Map.toList $
        State.config#State.aliases #$ state
    , map (flip dump_block state) (Map.keys (State.state_blocks state))
    )
    where

dump_block :: BlockId -> State.State -> (BlockSpec, [Skeleton.Edge])
dump_block block_id state =
    ((name ++ if Text.null title then "" else " -- " ++ untxt title,
        map dump_track tracks), skel)
    where
    (id_str, title, tracks, skel) = eval state (Simple.dump_block block_id)
    name = snd $ Id.un_id $ Id.read_id id_str
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

select :: (State.M m) => ViewId -> Types.Selection -> m ()
select view_id sel =
    State.set_selection view_id Config.insert_selnum (Just sel)

select_point :: (State.M m) => ViewId -> TrackNum -> ScoreTime -> m ()
select_point view_id tracknum pos =
    select view_id (Types.point_selection tracknum pos)

-- * pure make_- functions

create_block :: (State.M m) => String -> String
    -> [(Block.TracklikeId, Types.Width)] -> m BlockId
create_block block_name title tracks = State.create_block (mkid block_name)
    (txt title) (map (uncurry Block.track) tracks)

mkstack :: (TrackNum, ScoreTime, ScoreTime) -> Stack.Stack
mkstack (tracknum, s, e) = mkstack_block (default_block_name, tracknum, s, e)

mkstack_block :: (String, TrackNum, ScoreTime, ScoreTime) -> Stack.Stack
mkstack_block (block, tracknum, s, e) = Stack.from_outermost
    [Stack.Block (bid block), Stack.Track (mk_tid_name block tracknum),
        Stack.Region s e]

-- ** track

make_track :: TrackSpec -> Track.Track
make_track (title, triplets) = Track.modify_events
    (Events.insert (map make_event triplets)) (empty_track title)

empty_track :: String -> Track.Track
empty_track title = Track.track (txt title) Events.empty

-- ** event

make_event :: EventSpec -> Event.Event
make_event (start, dur, text) = Event.event start dur text

extract_event :: Event.Event -> EventSpec
extract_event event =
    (Event.start event, Event.duration event, Event.event_string event)

-- ** ruler

default_ruler :: Ruler.Ruler
default_ruler = mkruler 32 1

default_block_end :: ScoreTime
default_block_end = Ruler.time_end default_ruler

no_ruler :: Ruler.Ruler
no_ruler = mkruler 0 0

ruler_until :: ScoreTime -> Ruler.Ruler
ruler_until pos = ruler [("until", Ruler.marklist [(pos, Ruler.null_mark)])]

-- | TimeStep to step by 1 ScoreTime on the default ruler.
step1 :: TimeStep.TimeStep
step1 = TimeStep.time_step (TimeStep.AbsoluteMark TimeStep.AllMarklists r_4)

-- | Create a ruler with a 4/4 "meter" marklist with the given number of marks
-- at the given distance.  Marks are rank [1, 2, 2, ...].
--
-- The end of the ruler should be at marks*dist.  An extra mark is created
-- since marks start at 0.
mkruler :: Int -> ScoreTime -> Ruler.Ruler
mkruler marks dist = ruler [(Meter.meter, marklist (marks + 1) dist)]

ruler :: [(Ruler.Name, Ruler.Marklist)] -> Ruler.Ruler
ruler marklists = Ruler.Ruler
    { Ruler.ruler_marklists = Map.fromList marklists
    , Ruler.ruler_bg = Config.ruler_bg
    , Ruler.ruler_show_names = False
    , Ruler.ruler_align_to_bottom = False
    }

marklist :: Int -> ScoreTime -> Ruler.Marklist
marklist n dist = Ruler.marklist $ take n $ zip (Seq.range_ 0 dist) m44

m44 :: [Ruler.Mark]
m44 = concatMap (\n -> [major n, minor, minor, minor]) [0..]
    where
    major n = Ruler.Mark r_1 3 (Color.rgba 0.45 0.27 0 0.35) (showt n) 0 0
    minor = Ruler.Mark r_4 2 (Color.rgba 1 0.39 0.2 0.35) "" 0 0

r_1, r_4 :: Ruler.Rank
r_1 = Meter.r_1
r_4 = Meter.r_4

mark :: Text -> Ruler.Mark
mark name = Ruler.Mark 0 3 (Color.rgba 0.4 0 0.4 0.4) name 0 0


-- * config

set_midi_config :: Instrument.Configs -> State.State -> State.State
set_midi_config = flip exec . State.set_midi_config

type MidiConfig = [(Text, [Midi.Channel])]

dump_midi_config :: Instrument.Configs -> MidiConfig
dump_midi_config configs =
    [(Score.inst_name inst, chans_of config)
        | (inst, config) <- Map.toList configs]
    where chans_of = map (snd . fst) . Instrument.config_addrs

midi_config :: MidiConfig -> Instrument.Configs
midi_config config = Instrument.configs
    [(Score.Instrument inst, map mkaddr chans) | (inst, chans) <- config]
    where mkaddr chan = (Midi.write_device "s", chan)
