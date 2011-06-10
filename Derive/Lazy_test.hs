module Derive.Lazy_test where
import qualified Control.Concurrent.MVar as MVar
import qualified Data.List as List
import qualified System.IO.Unsafe as Unsafe

import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import Util.Test
import qualified Util.Thread as Thread

import Ui
import qualified Ui.Event as Event
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.UiTest as UiTest

import qualified Cmd.Cmd as Cmd
import qualified Derive.Call.Block as Call.Block
import qualified Derive.Call.Block as Block
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Call.Note as Call.Note
import qualified Derive.CallSig as CallSig
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.LEvent as LEvent
import qualified Derive.Note as Note
import qualified Derive.Schema as Schema
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
import qualified Derive.TrackLang as TrackLang

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal


test_one = do
    -- Test laziness of a single track with error logging.
    let mkblock n = snd $ UiTest.run_mkstate
            [(">i", [(fromIntegral n, 1, if n `mod` 4 == 0 then "bad" else "")
                | n <- [0..n]])]
    (log, result) <- derive_block (mkblock 10)
    print $ take 3 $ extract_start result
    evaluated <- get_log log
    equal (length evaluated) 2 -- 1 was an error

test_two = do
    -- Two tracks should interleave evaluation.
    let mkblock n = snd $ UiTest.run_mkstate
            [ (">i1", [(fromIntegral n, 1, "") | n <- [0..n]])
            , (">i2", [(fromIntegral n, 1, "") | n <- [0,2..n]])
            ]
    (log, result) <- derive_block (mkblock 10)
    print $ take 4 $ extract_start result
    evaluated <- get_log log
    -- 4+1 extra because it has to evaluate one in advance to know how to merge
    equal evaluated
        [ "b1 b1.t0 0-1 note at: 0s"
        , "b1 b1.t1 0-1 note at: 0s"
        , "b1 b1.t0 1-2 note at: 1s"
        , "b1 b1.t1 2-3 note at: 2s"
        , "b1 b1.t0 2-3 note at: 2s"
        ]

test_control = do
    -- A control track won't derive in parallel with its note track.  Of
    -- course it would be nice to evaluate the control track incrementally as
    -- well, but not possible as long as signals are strict.  If they were
    -- lazy I'd have to modify a bunch of other places like the bsearches so
    -- I'm not going to bother for now.  It's not even clear it would be more
    -- efficient that way given that signals may be displayed.
    let mkblock n = snd $ UiTest.run_mkstate
            [ ("c", [(fromIntegral n, 0, show n) | n <- [0..n]])
            , (">i", [(fromIntegral n, 1, "") | n <- [0..n]])
            ]
    (log, result) <- derive_block (mkblock 20)
    let extract e = (Score.event_start e, DeriveTest.e_control "c" e)
    print $ take 3 $ DeriveTest.extract_stream extract result
    -- Fails as expected... but can I at least make it go forwards?  Or does
    -- it matter?
    evaluated <- get_log log
    equal (length (filter ("note at" `List.isInfixOf`) evaluated)) 3
    equal (length (filter ("control at" `List.isInfixOf`) evaluated)) 21

test_inverted_control = do
    -- On the other hand, inverted control tracks naturally derive
    -- incrementally.
    let mkblock n = snd $ UiTest.run_mkstate
            [ (">i", [(fromIntegral n, 1, "") | n <- [0..n]])
            , ("c", [(fromIntegral n, 0, show n) | n <- [0..n]])
            ]
    (log, result) <- derive_block (mkblock 20)
    let extract e = (Score.event_start e, DeriveTest.e_control "c" e)
    print $ take 3 $ DeriveTest.extract_stream extract result
    evaluated <- get_log log
    equal (length (filter ("note at" `List.isInfixOf`) evaluated)) 3
    equal (length (filter ("control at" `List.isInfixOf`) evaluated)) 6
    pprint evaluated

test_cache = do
    -- Make sure having some bits cached doesn't mess up lazy derivation.
    let mkblock n = UiTest.exec State.empty $ UiTest.mkblocks
            [ ("top", [(">i", [(fromIntegral n, 1, "sub") | n <- [0..n]])])
            , ("sub", [(">", [(0, 1, "")])])
            ]
    let ustate = mkblock 10
        derive cache damage = do
            log <- MVar.newMVar []
            let deriver = with_calls log $
                    Call.Block.eval_root_block (UiTest.bid "top")
            return (log, DeriveTest.derive_cache cache damage ustate deriver)
    (log, res1) <- derive mempty mempty
    equal (extract_start res1) (map (Left . RealTime.seconds) [0..10])
    evaluated <- get_log log
    equal (length evaluated) 11

    (log, res2) <- derive (Derive.r_cache res1) mempty
    equal (extract_start res2) (extract_start res1)
    evaluated <- get_log log
    -- Everything was cached.
    equal evaluated []

    (log, res2) <- derive (Derive.r_cache res1)
        (DeriveTest.make_damage "top" 0 1 3)
    print $ take 2 $ extract_start res2
    -- Only the first damaged event was rederived.
    evaluated <- get_log log
    equal (length evaluated) 1
    print $ extract_start res2
    evaluated <- get_log log
    -- Both were rederived, and no more.
    equal (length evaluated) 2

test_everything = do
    -- Run a complete derive through performance.
    let ustate = UiTest.exec State.empty $ UiTest.mkblocks
            [ (default_block,
                [ ("*twelve", [(0, 0, "4c")])
                , (">s/1", [(0, 1, ""), (1, 1, "bad"), (2, 1, "sub"),
                    (3, 1, "sub")])
                ])
            , ("sub", [(">", [(0, 1, "")])])
            ]
    (log, res) <- derive_block ustate
    let midi = perform res
    print (take 5 midi)
    -- Make sure errors make it all the way through.
    equal (take 1 [msg | Right msg <- take 5 midi])
        ["Error: note call not found: bad"]
    evaluated <- get_log log
    equal evaluated
        ["b1 b1.t1 0-1 note at: 0s", "sub sub.t0 0-1 note at: 2s"]

perform :: Derive.Result -> [Either DeriveTest.Midi String]
perform result = map (LEvent.either Left (Right . DeriveTest.show_log)) $
    snd $ DeriveTest.perform_stream DeriveTest.default_lookup_inst
        DeriveTest.default_midi_config (Derive.r_events result)


-- * lazy checks

-- This set of test_#_* test the laziness at various points of the derivation.
-- Since you can only find the too-strict bit of code through manual
-- searching, this should split up the evaluation stack enough to isolate
-- the too-strict section.

test_0_derive_notes = do
    -- if I can take results from an infinite score, the derivation is lazy
    let inf = [UiTest.mkevent (n, 1, "") | n <- [0..]]
    (log, deriver) <- with_logging $ Note.derive_notes 10 [] inf
    result <- Thread.timeout 0.5 $ (\v -> force v >> return v) $
        extract_run 5 $ DeriveTest.run State.empty deriver
    equal result (Just (Right [0, 1, 2, 3, 4]))
    evaluated <- get_log log
    equal (length evaluated) 5

test_1_schema = do
    let ustate = flat_block 20
    -- Since I'm not calling Block.eval_root_block I need to set the stack
    -- manually.
    (log, deriver) <- with_logging $
        Internal.with_stack_block default_block_id $ UiTest.eval ustate $
            Cmd.schema_deriver Schema.default_schema default_block_id
    let result = DeriveTest.run_ ustate deriver
    print $ extract_run 5 result
    evaluated <- get_log log
    equal (length evaluated) 5

test_2_root = do
    let ustate = flat_block 20
    (log, deriver) <- with_logging $ Block.eval_root_block default_block_id
    let result = DeriveTest.run_ ustate deriver
    print $ (extract_run 5) result
    evaluated <- get_log log
    equal (length evaluated) 5

flat_block :: Int -> State.State
flat_block n = snd $ UiTest.run_mkstate
    [(">i", [(fromIntegral n, 1, "") | n <- [0..n]])]

with_logging :: Derive.Deriver a -> IO (Log, Derive.Deriver a)
with_logging deriver = do
    log <- MVar.newMVar []
    return (log, with_calls log deriver)

extract_run :: Int
    -> Either String ([LEvent.LEvent Score.Event], Derive.State, [Log.Msg])
    -> Either String [RealTime]
extract_run n = fmap (take n) . DeriveTest.extract_run
    (map Score.event_start . LEvent.events_of)


-- * implementation

derive_block :: State.State -> IO (Log, Derive.Result)
derive_block ustate = do
    log <- MVar.newMVar []
    return (log,
        DeriveTest.derive_block_with (with_calls log) ustate default_block_id)

default_block = UiTest.default_block_name
default_block_id = UiTest.default_block_id

extract_start :: Derive.Result -> [Either RealTime String]
extract_start = DeriveTest.extract_stream Score.event_start

type Log = MVar.MVar [String]

put_log :: Log -> String -> IO ()
put_log log msg = MVar.modifyMVar_ log (\msgs -> return (msg:msgs))

get_log :: Log -> IO [String]
get_log = fmap reverse . MVar.readMVar

with_calls :: Log -> Derive.Deriver a -> Derive.Deriver a
with_calls mvar = CallTest.with_note_call "" (mk_logging_call mvar)
    . CallTest.with_control_call "" (c_set mvar)

mk_logging_call :: Log -> Derive.NoteCall
mk_logging_call log_var  = Derive.stream_generator "logging-note" $
    Call.Note.inverting_call $ \args ->
        c_note log_var (Derive.passed_event args) (Derive.passed_next args)

c_note :: Log -> Track.PosEvent -> ScoreTime -> Derive.EventDeriver
c_note log_mvar (pos, event) next_start = do
    start <- Derive.score_to_real pos
    end <- Derive.score_to_real (pos + Event.event_duration event)
    inst <- Derive.lookup_val TrackLang.v_instrument
    st <- Derive.get
    real_next <- Derive.score_to_real next_start
    let controls = Call.Note.trimmed_controls start real_next
            (Derive.state_controls st)
        pitch_sig = Derive.state_pitch st
        score_event = Score.Event start (end-start) (Event.event_bs event)
            controls pitch_sig (Derive.state_stack st) inst Score.no_attrs
    let write_log = Unsafe.unsafePerformIO $
            put_log log_mvar $ stack ++ " note at: " ++ Pretty.pretty start
        stack = Stack.unparse_ui_frame_ $ last $
            Stack.to_ui (Derive.state_stack st)
    return $! LEvent.one $! LEvent.Event $! write_log `seq` score_event

c_set :: Log -> Derive.ControlCall
c_set log_mvar = Derive.generator1 "set" $ \args -> CallSig.call1 args
    (CallSig.required "val") $ \val -> do
        pos <- Derive.passed_real args
        st <- Derive.get
        let write_log = Unsafe.unsafePerformIO $ put_log log_mvar $
                stack ++ " control at: " ++ Pretty.pretty pos
            stack = Stack.unparse_ui_frame_ $ last $
                Stack.to_ui (Derive.state_stack st)
        return $! write_log `seq` Signal.signal [(pos, val)]
