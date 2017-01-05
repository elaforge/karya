-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Derive_criterion where
import qualified Criterion.Main as Criterion
import qualified Data.Map as Map

import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.Performance as Performance

import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Derive.Stream as Stream

import qualified Perform.Midi.Perform as Perform
import Global
import Types


print_results :: Bool
print_results = False

main :: IO ()
main
    | print_results = do
        let state = invert 1
        Pretty.pprint $ derive state
        Pretty.pprint $ perform state (derive state)
        Pretty.pprint $ map (second (map show_bench)) benches
    | otherwise = Criterion.defaultMain
        [ Criterion.bgroup group (map action bs)
        | (group, bs) <- benches
        ]
    where
    benches = Seq.keyed_group_stable (joined [bench_state, bench_code])
        benchmarks
    action (Bench action code state size) =
        action (Seq.join "-" [code, state, size])

joined :: [a -> String] -> a -> String
joined fs x = Seq.join "-" $ map ($x) fs

data Bench = Bench {
    benchmark_action :: String -> Criterion.Benchmark
    , bench_code :: String
    , bench_state :: String
    , bench_size :: String
    }

show_bench :: Bench -> String
show_bench (Bench _ code state size) = Seq.join "-" [code, state, size]

benchmarks :: [Bench]
benchmarks = do
    (state_name, make_state) <-
        [("simple", simple), ("no_invert", no_invert), ("invert", invert)]
    (size, state) <- [(size, make_state (size `div` per_score)) | size <- sizes]
    (code_name, action) <-
        [ ("cmd_derive", cmd_derive_bench state)
        , ("derive", derive_bench state)
        , ("perform", perform_bench state)
        ]
    return $ Bench action code_name state_name (show size)
    where
    sizes = [128, 1024, 4096]
    per_score = 32 -- sub events multiplied by this for total events

    -- Derive with track signals for all tracks, convert to Cmd.Performance,
    -- and force it as the background derivation threads do.
    cmd_derive_bench state name =
        Criterion.env (return state) $ \state ->
            Criterion.bench name $ Criterion.nf Msg.force_performance $
            cmd_derive state
    -- Just derivation, where only the events are forced.
    derive_bench state name =
        Criterion.env (return state) $ \state ->
            Criterion.bench name $ Criterion.nf id (derive state)
    -- Just the performance part, where derivation has already completed.
    perform_bench state name =
        Criterion.env (return (state, derive state)) $ \ ~(state, events) ->
            Criterion.bench name $ Criterion.nf id (perform state events)

-- *2 instruments, *16 calls to 'sub'
make_score :: [UiTest.TrackSpec] -> Ui.State
make_score sub = snd $ UiTest.run_mkblocks
    [ ("top", [(">i1", score), (">i2", score)])
    , ("sub=ruler", sub)
    ]
    where score = take 16 [(t, 4, "sub") | t <- Seq.range_ 0 4]

no_invert :: Int -> Ui.State
no_invert events_per_sub = make_score
    [ ("*", [(t, 0, p) | (t, p) <- zip ts ["3c", "3d", "3e", "3f"]])
    , ("dyn", [(t, 0, p) | (t, p) <- zip ts ["1", ".75", ".5", ".25"]])
    , (">", [(t, 1, "") | t <- ts])
    ]
    where ts = Seq.range' 0 (realToFrac events_per_sub) 1

invert :: Int -> Ui.State
invert events_per_sub = make_score
    [ (">", [(t, 1, "") | t <- ts])
    , ("*", [(t, 0, p) | (t, p) <- zip ts ["3c", "3d", "3e", "3f"]])
    , ("dyn", [(t, 0, p) | (t, p) <- zip ts ["1", ".75", ".5", ".25"]])
    ]
    where ts = Seq.range' 0 (realToFrac events_per_sub) 1

simple :: Int -> Ui.State
simple events = make_score
    [(">", [(t, 1, "") | t <- Seq.range' 0 (realToFrac events) 1])]

cmd_derive :: Ui.State -> Cmd.Performance
cmd_derive state =
    Performance.performance $
        DeriveTest.derive_block_setup setup state (get_root_id state)
    where setup = DeriveTest.with_tsigs (Map.keys (Ui.state_tracks state))

derive :: Ui.State -> Stream.Stream Score.Event
derive state =
    Derive.r_events $ DeriveTest.derive_block state (get_root_id state)

get_root_id :: Ui.State -> BlockId
get_root_id = fromMaybe (error "no root block") . (Ui.config#Ui.root #$)

perform :: Ui.State -> Stream.Stream Score.Event -> Perform.MidiEvents
perform state events =
    snd $ DeriveTest.perform_stream DeriveTest.default_convert_lookup
        (Ui.config#Ui.allocations #$ state) events
