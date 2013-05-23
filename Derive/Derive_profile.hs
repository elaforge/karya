module Derive.Derive_profile where
import qualified Data.Time as Time
import qualified System.CPUTime as CPUTime
import qualified System.IO as IO
import qualified System.Mem as Mem

import qualified Text.Printf as Printf

import Util.Control
import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.State as State
import qualified Ui.TrackTree as TrackTree
import qualified Ui.UiTest as UiTest

import qualified Cmd.Create as Create
import qualified Cmd.Serialize as Serialize
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.ParseSkeleton as ParseSkeleton

import qualified Perform.Midi.Convert as Convert
import Types


-- q=90 means 1.5 quarters / sec
-- So 3 8ths, 6 16ths, 12 32nds
-- 32 * 60 * 60 * 2 = 230400 for 2 hours
-- two_hours = 230400
-- time_minutes events = events / 32 / 60

-- | Make a giant block with simple non-overlapping controls and tempo changes.
profile_big_block = derive_profile $ make_simple 2000

make_simple :: (State.M m) => Int -> m ()
make_simple size =
    mkblock $ make_tempo size
        : note_tracks inst1 0 ++ note_tracks inst1 2 ++ note_tracks inst2 4
    where
    note_tracks name offset = map (track_take size . track_drop offset)
        [simple_pitch_track, vel_track, note_track name]


-- | Block with a control controlling multiple note tracks.  Intended to
-- profile channelize control sharing.
profile_shared = derive_profile $ make_shared_control 2000

make_shared_control size = mkblock $
        make_tempo size
        : track_take size mod_track
        : track_set 0 ++ track_set 2 -- ++ track_set 4
    where
    track_set offset = map (track_take size . track_drop offset)
        [ simple_pitch_track
        , vel_track
        , note_track inst1
        ]

-- | Block with non-tempered scale so pitches can't share.  Intended to profile
-- channelize pitch sharing.
profile_nontempered = derive_profile $ make_nontempered 1000
    where
    make_nontempered size = mkblock $ map (track_until size)
            [ make_tempo (floor size)
            , nontempered_pitch_track
            , vel_track
            , note_track inst1
            ]

-- | Giant control track with lots of samples.  Intended to profile control
-- track derivation.
profile_control = derive_profile $ make_big_control 15000

make_big_control size = mkblock $ map (track_until size)
    [ mod_track
    , (inst1, [(0, size, "")])
    ]

profile_nested_simple = derive_profile $ make_nested_notes 10 3 60
profile_nested_controls = derive_profile $ make_nested_controls 10 3 60
-- Just plain notes, no controls, so laziness works.
profile_nested_nocontrol = derive_profile $
    make_nested [note_track inst1] 10 3 60

profile_size = derive_size $ make_nested_controls 10 3 60

-- TODO the save file should be checked in, but let's wait until they're
-- compressed
profile_bloom_derive =
    profile_saved False "prof/bloom" (UiTest.bid "bloom/order")
profile_bloom_perform =
    profile_saved True "prof/bloom" (UiTest.bid "bloom/order")

profile_pnovla_derive =
    profile_saved False "prof/pnovla.state" (UiTest.bid "pnovla2/b1")
profile_pnovla_perform =
    profile_saved True "prof/pnovla.state" (UiTest.bid "pnovla2/b1")

-- * make states

make_nested_notes :: (State.M m) => Int -> Int -> Int -> m ()
make_nested_notes = make_nested [simple_pitch_track, note_track inst1]

make_nested_controls :: (State.M m) => Int -> Int -> Int -> m ()
make_nested_controls = make_nested
    [ simple_pitch_track
    , mod_track
    , note_track inst1
    ]

-- | Try to generate a "normal" score.  This means a lightly nested structure,
-- with the terminal blocks having a more events than the intermediate ones.
--
-- This doesn't produce intermediate tempo or control curves.
make_nested :: (State.M m) => [UiTest.TrackSpec] -> Int -> Int -> Int -> m ()
make_nested bottom_tracks size depth bottom_size = do
    ruler_id <- Create.ruler "ruler" UiTest.default_ruler
    go ruler_id UiTest.default_block_name depth
    State.modify set_midi_config
    where
    go ruler_id block_name 1 = do
        UiTest.mkblock_ruler ruler_id
            (block_name, map (track_take bottom_size) bottom_tracks)
        return ()
    go ruler_id block_name depth = do
        let sub_bids = [block_name ++ "." ++ show sub | sub <- [0 .. size-1]]
            step = bottom_size * size^(depth-2)
        UiTest.mkblock_ruler ruler_id (block_name,
            map (track_take size) [ctrack (fromIntegral step) inst1 sub_bids])
        forM_ sub_bids $ \sub -> go ruler_id sub (depth-1)

mkblock :: (State.M m) => [UiTest.TrackSpec] -> m ()
mkblock tracks = do
    UiTest.mkblock (UiTest.default_block_name, tracks)
    tinfo <- TrackTree.tracks_of UiTest.default_block_id
    -- Track slicing makes things much slower.  I should profile that too, but
    -- let's profile without it first.
    State.set_skeleton UiTest.default_block_id $
        ParseSkeleton.note_bottom_parser tinfo
    State.modify set_midi_config

-- * implementation

profile_saved :: Bool -> FilePath -> BlockId -> IO ()
profile_saved with_perform fname block_id = do
    result <- print_timer ("unserialize " ++ show fname) (const "") $
        Serialize.unserialize fname
    state <- case result of
        Left err -> error $ "loading " ++ show fname ++ ": " ++ err
        Right Nothing -> error $ "loading " ++ show fname ++ ": doesn't exist"
        Right (Just (Serialize.SaveState state _)) -> return state
    let lookup = DeriveTest.lookup_from_state state
    if with_perform
        then replicateM_ 6 $ run_profile (Just lookup) block_id state
        else replicateM_ 6 $ run_profile Nothing block_id state

derive_size :: State.StateId a -> IO ()
derive_size create = do
    print_timer "force mmsgs" (const "done") (force mmsgs)
    print_timer "gc" (const "") Mem.performGC
    print_timer "busy" id $ return $ show (busy_wait 100000000)
    print_timer "length" id $ return $ show (length mmsgs)
    return ()
    where
    ui_state = UiTest.exec State.empty create
    result = DeriveTest.derive_block ui_state UiTest.default_block_id
    mmsgs = snd $ DeriveTest.perform_stream DeriveTest.default_convert_lookup
        (State.config_midi (State.state_config ui_state))
        (Derive.r_events result)

busy_wait :: Int -> Double
busy_wait ops = go ops 2
    where
    go 0 accum = accum
    go n accum
        | even n = go (n-1) (accum**2)
        | otherwise = go (n-1) (sqrt accum)

-- | This runs the derivation several times to minimize the creation cost.
derive_profile :: State.StateId a -> IO ()
derive_profile create = replicateM_ 6 $
    run_profile Nothing UiTest.default_block_id (UiTest.exec State.empty create)

run_profile :: Maybe Convert.Lookup -> BlockId -> State.State -> IO ()
run_profile maybe_lookup block_id ui_state = do
    start_cpu <- CPUTime.getCPUTime
    start <- now
    let section :: (Show a) => String -> IO ((), [a]) -> IO ()
        section = time_section (start_cpu, start)

    let result = DeriveTest.derive_block ui_state block_id
    let events = Derive.r_events result
    section "derive" $ do
        force events
        return ((), events)
    when_just maybe_lookup $ \lookup -> section "midi" $ do
        let mmsgs = snd $ DeriveTest.perform_stream lookup
                (State.config_midi (State.state_config ui_state)) events
        force mmsgs
        return ((), mmsgs)

time_section :: (Show a) => (Integer, Time.DiffTime) -> String
    -> IO (result, [a]) -> IO result
time_section (start_cpu, start_time) title op = do
    putStr $ "--> " ++ title ++ ": "
    IO.hFlush IO.stdout
    ((result, vals), cpu_secs, secs) <- cpu_time op
    cur_cpu <- CPUTime.getCPUTime
    cur_time <- now
    Printf.printf "%d vals -> %.2fcpu / %.2fs (running: %.2fcpu / %.2fs)\n"
        (length vals) cpu_secs (double secs)
        (cpu_to_sec (cur_cpu - start_cpu)) (double (cur_time - start_time))
    return result
    where
    double :: Time.DiffTime -> Double
    double = realToFrac

cpu_time :: IO a -> IO (a, Double, Time.DiffTime)
cpu_time op = do
    start_cpu <- CPUTime.getCPUTime
    start <- now
    v <- op
    end_cpu <- CPUTime.getCPUTime
    end <- now
    return (v, cpu_to_sec (end_cpu - start_cpu), end - start)

now = fmap Time.utctDayTime Time.getCurrentTime
cpu_to_sec :: Integer -> Double
cpu_to_sec s = fromIntegral s / fromIntegral (10^12)


-- * state building

inst1 = ">s/1"
inst2 = ">s/2"

note_track inst = ctrack 1 inst [""]
make_tempo n = track_take (ceiling (fromIntegral n / 10)) $
    ctrack0 10 "tempo" ["1", "2", "3", "i 1"]
mod_track = ctrack0 1 "cc1 | srate = .02" ["1", "i 0", "e 1", "i .5", "i 0"]
simple_pitch_track =
    ctrack0 1 "*twelve" ["4a", "4b", "4c", "4d", "4e", "4f", "4g", "5c"]
nontempered_pitch_track =
    ctrack0 1 "*legong" ["1", "2", "3", "5", "6", "`1^`", "`6.`"]
vel_track = ctrack0 1 "vel" ["1", ".2", ".4", ".6"]

ctrack0 :: ScoreTime -> String -> [String] -> UiTest.TrackSpec
ctrack0 step title ts =
    (title, [(p, 0, t) | (p, t) <- zip (Seq.range_ 0 step) (cycle ts)])

ctrack :: ScoreTime -> String -> [String] -> UiTest.TrackSpec
ctrack step title ts =
    (title, [(p, step, t) | (p, t) <- zip (Seq.range_ 0 step) (cycle ts)])

track_until :: ScoreTime -> UiTest.TrackSpec -> UiTest.TrackSpec
track_until until = second (takeWhile (\(p, _, _) -> p < until))

track_drop n = second (shift . drop n)
    where
    shift [] = []
    shift ((p, d, t) : rest) =
        (0, d, t) : map (\(p', d', t') -> (p' - p, d', t')) rest

track_take :: Int -> UiTest.TrackSpec -> UiTest.TrackSpec
track_take = second . take

set_midi_config :: State.State -> State.State
set_midi_config = State.config#State.midi #=
     UiTest.midi_config [(txt $ drop 1 inst1, [0, 1]), (txt $ drop 1 inst2, [2])]
