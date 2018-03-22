-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Derive_profile where
import qualified System.IO as IO
import qualified System.Mem as Mem
import qualified Text.Printf as Printf

import qualified Util.Log as Log
import qualified Util.Seq as Seq
import qualified Util.Testing as Testing
import qualified Util.Thread as Thread

import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Derive.Derive as Derive
import qualified Derive.DeriveSaved as DeriveSaved
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.ParseSkeleton as ParseSkeleton
import qualified Derive.Stream as Stream

import Global
import Types


-- q=90 means 1.5 quarters / sec
-- So 3 8ths, 6 16ths, 12 32nds
-- 32 * 60 * 60 * 2 = 230400 for 2 hours
-- two_hours = 230400
-- time_minutes events = events / 32 / 60

-- | Make a giant block with simple non-overlapping controls and tempo changes.
profile_big_block = derive_profile "big block" $ make_simple 2000

make_simple :: Ui.M m => Int -> m ()
make_simple size =
    mkblock $ make_tempo size
        : note_tracks inst1 0 ++ note_tracks inst1 2 ++ note_tracks inst2 4
    where
    note_tracks name offset = map (track_take size . track_drop offset)
        [simple_pitch_track, vel_track, note_track name]


-- | Block with a control controlling multiple note tracks.  Intended to
-- profile channelize control sharing.
profile_shared = derive_profile "shared" $ make_shared_control 2000

make_shared_control size =
    mkblock $ make_tempo size
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
profile_nontempered = derive_profile "nontempered" $ make_nontempered 1000
    where
    make_nontempered size = mkblock $ map (track_until size)
        [ make_tempo (floor (ScoreTime.to_double size))
        , nontempered_pitch_track
        , vel_track
        , note_track inst1
        ]

-- | Giant control track with lots of samples.  Intended to profile control
-- track derivation.
profile_control = derive_profile "control" $ make_big_control 15000

make_big_control size = mkblock $ map (track_until size)
    [ mod_track
    , (inst1, [(0, size, "")])
    ]

profile_nested_simple =
    derive_profile "nested simple" $ make_nested_notes 10 3 60
profile_nested_controls =
    derive_profile "nested controls" $ make_nested_controls 10 3 60
-- Just plain notes, no controls, so laziness works.
profile_nested_nocontrol = derive_profile "nested nocontrol" $
    make_nested [note_track inst1] 10 3 60

-- Profile with some bogus busy waits in there to make things clearer on the
-- heap profile.
profile_size = derive_size $ make_nested_controls 10 3 60

-- profile_bloom_derive = derive_saved False "data/bloom"
-- profile_bloom_perform = derive_saved True "data/bloom"
-- profile_pnovla_derive = derive_saved False "data/pnovla"
-- profile_pnovla_perform = derive_saved True "data/pnovla"
-- profile_viola_sonata_perform = derive_saved True "data/viola-sonata"

profile_bloom = profile_file "data/bloom"
profile_pnovla = profile_file "data/pnovla"
profile_viola_sonata = profile_file "data/viola-sonata"

profile_file :: FilePath -> IO ()
profile_file fname = do
    cmd_config <- DeriveSaved.load_cmd_config
    DeriveSaved.perform_file cmd_config fname
    return ()


-- * make states

make_nested_notes :: Ui.M m => Int -> Int -> Int -> m ()
make_nested_notes = make_nested [simple_pitch_track, note_track inst1]

make_nested_controls :: Ui.M m => Int -> Int -> Int -> m ()
make_nested_controls = make_nested
    [ simple_pitch_track
    , mod_track
    , note_track inst1
    ]

-- | Try to generate a "normal" score.  This means a lightly nested structure,
-- with the terminal blocks having a more events than the intermediate ones.
--
-- This doesn't produce intermediate tempo or control curves.
make_nested :: Ui.M m => [UiTest.TrackSpec] -> Int -> Int -> Int -> m ()
make_nested bottom_tracks size depth bottom_size = do
    ruler_id <- Create.ruler "ruler" UiTest.default_ruler
    go ruler_id UiTest.default_block_name depth
    Ui.modify set_allocations
    where
    go ruler_id block_name 1 = do
        UiTest.mkblock_ruler ruler_id (UiTest.bid block_name) ""
            (map (track_take bottom_size) bottom_tracks)
        return ()
    go ruler_id block_name depth = do
        let sub_bids = [block_name <> "." <> showt sub | sub <- [0 .. size-1]]
            step = bottom_size * size^(depth-2)
        UiTest.mkblock_ruler ruler_id (UiTest.bid block_name) ""
            (map (track_take size) [ctrack (fromIntegral step) inst1 sub_bids])
        forM_ sub_bids $ \sub -> go ruler_id sub (depth-1)

mkblock :: Ui.M m => [UiTest.TrackSpec] -> m ()
mkblock tracks = do
    UiTest.mkblock (UiTest.default_block_name, tracks)
    tinfo <- TrackTree.tracks_of UiTest.default_block_id
    -- Track slicing makes things much slower.  I should profile that too, but
    -- let's profile without it first.
    Ui.set_skeleton UiTest.default_block_id $
        ParseSkeleton.note_bottom_parser tinfo
    Ui.modify set_allocations

-- * implementation

derive_saved :: Bool -> FilePath -> IO ()
derive_saved with_perform fname = do
    cmd_config <- DeriveSaved.load_cmd_config
    (ui_state, library, aliases) <- either errorIO return
        =<< DeriveSaved.load_score (Cmd.config_instrument_db cmd_config) fname
    block_id <- maybe (errorIO $ txt fname <> ": no root block") return $
        Ui.config#Ui.root #$ ui_state
    let cmd_state = DeriveSaved.add_library library aliases
            (Cmd.initial_state cmd_config)
    ((events, logs), _cpu) <-
        DeriveSaved.timed_derive fname ui_state cmd_state block_id
    mapM_ Log.write logs
    when with_perform $ do
        ((_msgs, logs), _cpu) <- DeriveSaved.timed_perform cmd_state
            ("perform " ++ fname) ui_state events
        mapM_ Log.write logs

derive_size :: Ui.StateId a -> IO ()
derive_size create = do
    Thread.printTimer "force mmsgs" (const "done") (Thread.force mmsgs)
    Thread.printTimer "gc" (const "") Mem.performGC
    -- This puts a gap in the heap graph so I can tell which each phase begins.
    -- Surely there is a less ridiculous way to do this.
    -- TODO yes, since 8.2 ghc puts heap profile in the eventlog, but then
    -- of course h2ps doesn't work.
    Thread.printTimer "busy" id $ return $ show
        (busy_wait 100000000)
    Thread.printTimer "length" id $ return $ show (length mmsgs)
    return ()
    where
    ui_state = UiTest.exec Ui.empty create
    result = DeriveTest.derive_block ui_state UiTest.default_block_id
    mmsgs = snd $ DeriveTest.perform_stream DeriveTest.default_convert_lookup
        (Ui.config#Ui.allocations #$ ui_state)
        (Derive.r_events result)

busy_wait :: Int -> Double
busy_wait ops = go ops 2
    where
    go 0 accum = accum
    go n accum
        | even n = go (n-1) (accum**2)
        | otherwise = go (n-1) (sqrt accum)

-- | This runs the derivation several times to minimize the creation cost.
derive_profile :: String -> Ui.StateId a -> IO ()
derive_profile name create = replicateM_ 6 $
    run_profile name Nothing (UiTest.exec Ui.empty create)

run_profile :: String
    -> Maybe DeriveTest.Lookup -- ^ If given, also run a perform.
    -> Ui.State -> IO ()
run_profile fname maybe_lookup ui_state = do
    block_id <- maybe (errorIO $ txt fname <> ": no root block") return $
        Ui.config#Ui.root #$ ui_state
    start_cpu <- Thread.currentCPU
    let section = time_section start_cpu
    let result = DeriveTest.derive_block ui_state block_id
    let events = Stream.to_list $ Derive.r_events result
    section "derive" $ do
        Thread.force events
        Testing.prettyp (take 20 events)
        return events
    whenJust maybe_lookup $ \lookup -> section "midi" $ do
        let mmsgs = snd $ DeriveTest.perform_stream lookup
                (Ui.config#Ui.allocations #$ ui_state)
                (Stream.from_sorted_list events)
        Thread.force mmsgs
        return mmsgs

time_section :: Thread.Seconds -> String -> IO [a] -> IO ()
time_section start_cpu title op = do
    putStr $ "--> " ++ title ++ ": "
    IO.hFlush IO.stdout
    (vals, cpu_secs, _secs) <- Thread.timeAction op
    cur_cpu <- Thread.currentCPU
    let len = length vals
    Printf.printf "%d vals -> %.2f (%.2f / sec) (running: %.2f)\n"
        len (secs cpu_secs) (fromIntegral len / secs cpu_secs)
        (secs (cur_cpu - start_cpu))
    where
    secs :: Thread.Seconds -> Double
    secs = realToFrac


-- * state building

inst1 = ">i1"
inst2 = ">i2"

set_allocations :: Ui.State -> Ui.State
set_allocations = Ui.config#Ui.allocations #= allocs
    where
    allocs = DeriveTest.allocs_from_db UiTest.default_db
        [ ("i1", ("s/1", dev [0, 1]))
        , ("i2", ("s/2", dev [2]))
        ]
        where dev = map ("wdev",)

note_track inst = ctrack 1 inst [""]
make_tempo n = track_take (ceiling (fromIntegral n / 10)) $
    ctrack0 10 "tempo" ["1", "2", "3", "i 1"]
mod_track = ctrack0 1 "cc1 | srate = 50" ["1", "i 0", "e 1", "i .5", "i 0"]
simple_pitch_track =
    ctrack0 1 "*twelve" ["4a", "4b", "4c", "4d", "4e", "4f", "4g", "5c"]
nontempered_pitch_track =
    ctrack0 1 "*legong" ["4i", "4o", "4e", "4u", "4a", "5i", "5o"]
vel_track = ctrack0 1 "vel" ["1", ".2", ".4", ".6"]

ctrack0 :: ScoreTime -> Text -> [Text] -> UiTest.TrackSpec
ctrack0 step title ts =
    (title, [(p, 0, t) | (p, t) <- zip (Seq.range_ 0 step) (cycle ts)])

ctrack :: ScoreTime -> Text -> [Text] -> UiTest.TrackSpec
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
