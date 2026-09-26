-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Derive_criterion where
import qualified Criterion.Main as Criterion
import qualified Data.Map as Map

import qualified Util.Pretty as Pretty
import qualified Util.Lists as Lists
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.Performance as Performance

import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Derive.Stream as Stream

import qualified Perform.Midi.Perform as Perform
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig
import qualified Ui.UiTest as UiTest

import           Global
import           Types


printResults :: Bool
printResults = False

main :: IO ()
main
    | printResults = do
        let state = invert 1
        Pretty.pprint $ derive state
        Pretty.pprint $ perform state (derive state)
        Pretty.pprint $ map (second (map showBench)) benches
    | otherwise = Criterion.defaultMain
        [ Criterion.bgroup group (map action bs)
        | (group, bs) <- benches
        ]
    where
    benches = Lists.keyedGroupStable (joined [benchState, benchCode])
        benchmarks
    action (Bench action code state size) =
        action (Lists.join "-" [code, state, size])

joined :: [a -> String] -> a -> String
joined fs x = Lists.join "-" $ map ($x) fs

data Bench = Bench
    { benchmarkAction :: String -> Criterion.Benchmark
    , benchCode :: String
    , benchState :: String
    , benchSize :: String
    }

showBench :: Bench -> String
showBench (Bench _ code state size) = Lists.join "-" [code, state, size]

benchmarks :: [Bench]
benchmarks = do
    (stateName, makeState) <-
        [("simple", simple), ("no_invert", noInvert), ("invert", invert)]
    (size, state) <- [(size, makeState (size `div` perScore)) | size <- sizes]
    (codeName, action) <-
        [ ("cmd_derive", cmdDeriveBench state)
        , ("derive", deriveBench state)
        , ("perform", performBench state)
        ]
    pure $ Bench action codeName stateName (show size)
    where
    sizes = [128, 1024, 4096]
    perScore = 32 -- sub events multiplied by this for total events

    -- Derive with track signals for all tracks, convert to Cmd.Performance,
    -- and force it as the background derivation threads do.
    cmdDeriveBench state name =
        Criterion.env (pure state) $ \state ->
            Criterion.bench name $ Criterion.nf Msg.forcePerformance $
            cmdDerive state
    -- Just derivation, where only the events are forced.
    deriveBench state name =
        Criterion.env (pure state) $ \state ->
            Criterion.bench name $ Criterion.nf id (derive state)
    -- Just the performance part, where derivation has already completed.
    performBench state name =
        Criterion.env (pure (state, derive state)) $ \ ~(state, events) ->
            Criterion.bench name $ Criterion.nf id (perform state events)

-- *2 instruments, *16 calls to 'sub'
makeScore :: [UiTest.TrackSpec] -> Ui.State
makeScore sub = snd $ UiTest.runMkblocks
    [ ("top", [(">i1", score), (">i2", score)])
    , ("sub=ruler", sub)
    ]
    where score = take 16 [(t, 4, "sub") | t <- Lists.range_ 0 4]

noInvert :: Int -> Ui.State
noInvert eventsPerSub = makeScore
    [ ("*", [(t, 0, p) | (t, p) <- zip ts ["3c", "3d", "3e", "3f"]])
    , ("dyn", [(t, 0, p) | (t, p) <- zip ts ["1", ".75", ".5", ".25"]])
    , (">", [(t, 1, "") | t <- ts])
    ]
    where ts = Lists.range' 0 (realToFrac eventsPerSub) 1

invert :: Int -> Ui.State
invert eventsPerSub = makeScore
    [ (">", [(t, 1, "") | t <- ts])
    , ("*", [(t, 0, p) | (t, p) <- zip ts ["3c", "3d", "3e", "3f"]])
    , ("dyn", [(t, 0, p) | (t, p) <- zip ts ["1", ".75", ".5", ".25"]])
    ]
    where ts = Lists.range' 0 (realToFrac eventsPerSub) 1

simple :: Int -> Ui.State
simple events = makeScore
    [(">", [(t, 1, "") | t <- Lists.range' 0 (realToFrac events) 1])]

cmdDerive :: Ui.State -> Cmd.Performance
cmdDerive state =
    Performance.performance state $
        DeriveTest.deriveBlockSetup setup state (getRootId state)
    where setup = DeriveTest.withTsigs (Map.keys (Ui.stateTracks state))

derive :: Ui.State -> Stream.Stream Score.Event
derive state =
    Derive.rEvents $ DeriveTest.deriveBlock state (getRootId state)

getRootId :: Ui.State -> BlockId
getRootId = fromMaybe (error "no root block") . (Ui.config#UiConfig.root #$)

perform :: Ui.State -> Stream.Stream Score.Event -> Perform.MidiEvents
perform state events =
    snd $ DeriveTest.performStream DeriveTest.defaultConvertLookup
        (Ui.config#UiConfig.allocations #$ state) events
