module Derive.Derive_criterion where
import qualified Criterion.Main as Criterion

import qualified Util.Seq as Seq
import Util.Test
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Perform.Midi.Perform as Perform
import Global


main :: IO ()
main = do
    -- prettyp (take 20 $ derive invert)
    -- prettyp (take 20 $ perform invert (derive invert))
    Criterion.defaultMain $
        [ Criterion.bgroup name [derive_bench state, perform_bench state]
        | (name, state) <- states
        ]
    where
    derive_bench state =
        Criterion.env (return state) $ \state ->
            Criterion.bench "derive" $ Criterion.nf id (derive state)
    perform_bench state =
        Criterion.env (return (state, derive state)) $ \ ~(state, events) ->
            Criterion.bench "perform" $ Criterion.nf id (perform state events)

states :: [(String, State.State)]
states = [("simple", simple), ("no_invert", no_invert), ("invert", invert)]

make_score :: [UiTest.TrackSpec] -> State.State
make_score sub = snd $ DeriveTest.mkblocks
    [ ("top", [(">s/1", score), (">s/2", score)])
    , ("sub=ruler", sub)
    ]
    where score = take 16 [(t, 4, "sub") | t <- Seq.range_ 0 4]

no_invert :: State.State
no_invert = make_score
    [ ("*", [(t, 0, p) | (t, p) <- zip ts ["3c", "3d", "3e", "3f"]])
    , ("dyn", [(t, 0, p) | (t, p) <- zip ts ["1", ".75", ".5", ".25"]])
    , (">", [(t, 1, "") | t <- ts])
    ]
    where ts = Seq.range' 0 4 1

invert :: State.State
invert = make_score
    [ (">", [(t, 1, "") | t <- ts])
    , ("*", [(t, 0, p) | (t, p) <- zip ts ["3c", "3d", "3e", "3f"]])
    , ("dyn", [(t, 0, p) | (t, p) <- zip ts ["1", ".75", ".5", ".25"]])
    ]
    where ts = Seq.range' 0 4 1

simple :: State.State
simple = make_score [(">", [(t, 1, "") | t <- Seq.range' 0 4 1])]

derive :: State.State -> Derive.Events
derive state = Derive.r_events $ DeriveTest.derive_block state block_id
    where
    block_id = fromMaybe (error "no root block") $
        State.config#State.root #$ state

perform :: State.State -> Derive.Events -> Perform.MidiEvents
perform state events =
    snd $ DeriveTest.perform_stream (DeriveTest.lookup_from_state state)
        (State.config_midi (State.state_config state)) events
