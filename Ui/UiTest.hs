module Ui.UiTest where
import qualified Control.Monad.Identity as Identity
import qualified Data.Map as Map

import Util.Control
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
import qualified Cmd.MakeRuler as MakeRuler
import qualified Cmd.Serialize as Serialize
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
mkid name = fromMaybe (error $ "invalid characters in " ++ show name) $
    Id.make test_ns name
bid = Types.BlockId . mkid
vid = Types.ViewId . mkid
tid = Types.TrackId . mkid
rid = Types.RulerId . mkid

default_zoom = Config.zoom

default_view :: Block.View
default_view = Block.view default_block_id default_rect default_zoom

-- | Save the state to disk, so I can load it into the app and see it.
save :: State.State -> FilePath -> IO ()
save ui_state fname = do
    save_state <- Serialize.save_state ui_state
    Serialize.serialize fname save_state

-- * monadic mk- functions

-- | (block_id, tracks)
-- If the name ends with @=ruler@, then the length of the ruler is derived from
-- the events inside, rather than being hardcoded.  This is convenient for
-- tests and lets them avoid hardcoding the default_ruler end.
type BlockSpec = (String, [TrackSpec])
-- | (track_title, events)
type TrackSpec = (String, [EventSpec])
-- | (start, dur, text)
type EventSpec = (ScoreTime, ScoreTime, String)

-- | Often tests work with a single block, or a single view.  To make them
-- less verbose, there is a default block and view so functions can omit the
-- parameter if convenient.
default_block_id :: BlockId
default_block_id = bid default_block_name

default_block_name :: String
default_block_name = "b01"

default_view_id :: ViewId
default_view_id = mk_vid default_block_id

default_ruler_id :: RulerId
default_ruler_id = rid "r01"

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
    maybe_rid <- State.lookup_ruler default_ruler_id
    (ruler_id, block_name) <- case Seq.drop_suffix "=ruler" block_name of
        (block_name, True) -> do
            let len = event_end tracks
            ruler_id <- State.create_ruler
                (Id.unsafe_id test_ns ("r" ++ show len)) (mkruler len 1)
            return (ruler_id, block_name)
        (block_name, False) -> do
            ruler_id <- maybe
                (State.create_ruler (Id.unpack_id default_ruler_id)
                    default_ruler)
                (const (return default_ruler_id)) maybe_rid
            return (ruler_id, block_name)
    mkblock_ruler ruler_id (block_name, tracks)
    where
    event_end :: [TrackSpec] -> Int
    event_end = ceiling . ScoreTime.to_double . maximum . (0:)
        . concatMap (map (\(s, d, _) -> max s (s+d)) . snd)

mkblocks_skel :: (State.M m) => [(BlockSpec, [Skeleton.Edge])] -> m ()
mkblocks_skel blocks = forM_ blocks $ \(block, skel) ->
    mkblock block <* State.set_skeleton (bid (fst block)) (Skeleton.make skel)

-- | Like 'mkblock', but uses the provided ruler instead of creating its
-- own.  Important if you are creating multiple blocks and don't want
-- a separate ruler for each.
mkblock_ruler :: (State.M m) => RulerId -> BlockSpec -> m (BlockId, [TrackId])
mkblock_ruler ruler_id (block_name, tracks) = do
    let block_id = bid block_name
    State.set_namespace test_ns
    -- Start at 1 because track 0 is the ruler.
    tids <- forM (zip [1..] tracks) $ \(i, track) ->
        State.create_track (Id.unpack_id (mk_tid_block block_id i))
            (make_track track)
    create_block block_name "" $ (Block.RId ruler_id, 20)
        : [(Block.TId tid ruler_id, 40) | tid <- tids]
    State.set_skeleton block_id =<< parse_skeleton block_id
    return (block_id, tids)

parse_skeleton :: (State.M m) => BlockId -> m Skeleton.Skeleton
parse_skeleton block_id = do
    tracks <- TrackTree.tracks_of block_id
    return $ ParseSkeleton.default_parser tracks

mkview :: (State.M m) => BlockId -> m ViewId
mkview block_id = State.create_view (Id.unpack_id (mk_vid block_id)) $
    Block.view block_id default_rect default_zoom

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
-- note track, but in excchange it's easier to write than full TrackSpecs.
--
-- @(inst, [(t, dur, pitch)], [(control, [(t, val)])])@
type NoteSpec = (String, [EventSpec], [(String, [(ScoreTime, String)])])

note_spec :: NoteSpec -> [TrackSpec]
note_spec (inst, pitches, controls) =
    [note_track, pitch_track] ++ map control_track controls
    where
    note_track = ('>' : inst, [(t, dur, "") | (t, dur, _) <- pitches])
    pitch_track = ("*", [(t, 0, pitch) | (t, _, pitch) <- pitches])
    control_track (title, events) = (title, [(t, 0, val) | (t, val) <- events])

-- * state to spec

-- | These can be used from 'Cmd.Lang.LState.save_test' to dump state in
-- a form that can be pasted into a test, trimmed down by hand, and passed to
-- 'mkblocks_skel'.  This way problems that show up in the app can be pasted
-- into a test.
to_spec :: State.State -> [(BlockSpec, [Skeleton.Edge])]
to_spec state = map (flip block_to_spec state)
    (Map.keys (State.state_blocks state))

block_to_spec :: BlockId -> State.State -> (BlockSpec, [Skeleton.Edge])
block_to_spec block_id state = ((block_name, map dump_track tracks), skel)
    where
    (id_str, _, tracks, skel) = eval state (Simple.dump_block block_id)
    block_name = snd (Id.un_id (Id.read_id id_str))
    dump_track (_, title, events) = (title, map convert events)
    convert (start, dur, text) =
        (ScoreTime.double start, ScoreTime.double dur, text)

-- | Like 'block_to_spec' but strip out everything but the tracks.
extract_tracks_of :: BlockId -> State.State -> [TrackSpec]
extract_tracks_of block_id state = tracks
    where ((_, tracks), _) = block_to_spec block_id state

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
    title (map (uncurry Block.track) tracks)

mkstack :: (TrackNum, ScoreTime, ScoreTime) -> Stack.Stack
mkstack (tracknum, s, e) = mkstack_block (default_block_name, tracknum, s, e)

mkstack_block :: (String, TrackNum, ScoreTime, ScoreTime) -> Stack.Stack
mkstack_block (block, tracknum, s, e) = Stack.from_outermost
    [Stack.Block (bid block), Stack.Track (mk_tid_name block tracknum),
        Stack.Region s e]

-- ** track

event_track_1 = make_track ("1", [(0, 16, "hi"), (30, 32, "there")])
event_track_2 = make_track ("2", [(16, 10, "ho"), (30, 32, "eyo")])

make_track :: TrackSpec -> Track.Track
make_track (title, triplets) = Track.modify_events
    (Events.insert_events (map make_event triplets)) (empty_track title)
empty_track title = Track.track title Events.empty

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
step1 = steps 0

steps :: Int -> TimeStep.TimeStep
steps n = TimeStep.time_step n (TimeStep.AbsoluteMark TimeStep.AllMarklists 3)

-- | Create a ruler with a 4/4 "meter" marklist with the given number of marks
-- at the given distance.  Marks are rank [1, 2, 2, ...].
--
-- The end of the ruler should be at marks*dist.  An extra mark is created
-- since marks start at 0.
mkruler :: Int -> ScoreTime -> Ruler.Ruler
mkruler marks dist = ruler [marklist (marks + 1) dist]

ruler mlists =
    Ruler.Ruler (Map.fromList mlists) ruler_bg True False False False
ruler_bg = Color.rgb 1 0.85 0.5
marklist n dist = (MakeRuler.meter_marklist,
    Ruler.marklist (take n $ zip (Seq.range_ 0 dist) m44))
m44 = concatMap (\n -> [major n, minor, minor, minor]) [0..]
major n = Ruler.Mark 1 3 (Color.rgba 0.45 0.27 0 0.35) (show n) 0 0
minor = Ruler.Mark 2 2 (Color.rgba 1 0.39 0.2 0.35) "" 0 0

mark name = Ruler.Mark 0 3 (Color.rgba 0.4 0 0.4 0.4) name 0 0

-- Convert a ruler config for an overlay ruler.
overlay_ruler ruler = ruler
    { Ruler.ruler_show_names = False
    , Ruler.ruler_use_alpha = True
    , Ruler.ruler_full_width = True
    }


-- * config

set_midi_config :: Instrument.Config -> State.State -> State.State
set_midi_config = flip exec . State.set_midi_config

midi_config :: [(String, [Midi.Channel])] -> Instrument.Config
midi_config config = Instrument.config
    [(Score.Instrument inst, map mkaddr chans) | (inst, chans) <- config]
    where mkaddr chan = (Midi.write_device "s", chan)
