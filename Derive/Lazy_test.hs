module Derive.Lazy_test where
import Util.Control
import Util.Test

import Ui
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest

import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.LEvent as LEvent
import qualified Derive.Note as Note
import qualified Derive.Score as Score


bid = UiTest.default_block_id

-- Most of these tests can't actually test the evaluation order.  I could stick
-- in unsafePerformIOs modifying IORefs, but for now I put a trace on the track
-- derive.

test_everything = do
    -- Run a complete derive through performance, with and without cache.  Make
    -- sure that the score is only derived as much as necessary.
    let mkblock n = [("top",
            [(">s/1", [(fromIntegral n, 1, if n `mod` 4 == 0 then "bad" else "")
                | n <- [0..n]])]
            )]
    let (result, perf_events, mmsgs) = perform (mkblock 20)
    let extract_mmsgs = map $
            LEvent.either Left (Right . DeriveTest.show_log)

    -- print $ map extract_perf (take 10 mmsgs)

    let (result, perf_events, mmsgs) = perform
            [ ("top", [(">s/1", [(0, 1, ""), (1, 1, "sub"), (2, 1, "sub")])])
            , ("sub", [(">", [(0, 1, ""), (1, 1, "")])])
            ]
    print $ extract_mmsgs mmsgs
    -- pprint $ take 3 $ extract_mmsgs mmsgs
    -- print $ take 3 $ extract result
    -- pprint $ take 3 $ extract result

    -- Test with cache.
    -- Test with sub-blocks.

perform blocks = (result, pevts, mmsgs)
    where
    result = DeriveTest.derive_blocks blocks
    (pevts, mmsgs) = DeriveTest.perform_stream DeriveTest.default_lookup_inst
        DeriveTest.default_midi_config (Derive.r_events result)

test_one = do
    -- I can't make an infinitely long track because the track is a strict
    -- Data.Map.  But I can make a long one and a short one and make sure
    -- that they take about the same time.
    --
    -- Or just stick traces in derive_track.
    let mkblock n = snd $ UiTest.run_mkstate
            [(">i", [(fromIntegral n, 1, if n `mod` 4 == 0 then "bad" else "")
                | n <- [0..n]])]
    let ustate = mkblock 10
    print $ extract $ DeriveTest.derive_block ustate bid

test_lazy = print $
    -- if I can take results from an infinite score, the derivation is lazy
    fmap (take 5) $ DeriveTest.extract_run ex $ DeriveTest.run State.empty $
        Note.derive_notes 10 [UiTest.mkevent (n, 1, "") | n <- [0..]]
    where ex = map Score.event_start . LEvent.events_of

test_two = do
    let mkblock n = snd $ UiTest.run_mkstate
            [ (">i1", [(fromIntegral n, 1, "") | n <- [0..n]])
            , (">i2", [(fromIntegral n, 1, "") | n <- [0,2..n]])
            ]
    let ustate = mkblock 10
    print $ extract $ DeriveTest.derive_block ustate bid

test_control = do
    -- see if a control track will derive in parallel with its note track
    let mkblock n = snd $ UiTest.run_mkstate
            [ (">i", [(fromIntegral n, 1, "") | n <- [0..n]])
            , ("c1", [(fromIntegral n, 0,
                if n `mod` 4 == 0 then "bad" else show n) | n <- [0,1..n]])
            ]
    let ustate = mkblock 10
    -- fails as expected... but can I at least make it go forwards?  or does it
    -- matter?
    print $ extract $ DeriveTest.derive_block ustate bid

test_cache = do
    -- make sure having some bits cached doesn't mess up lazy derivation
    let mkblock n = snd $ UiTest.run_mkstate
            [(">i", [(fromIntegral n, 1, "") | n <- [0..n]])]
    let ustate = mkblock 5
    let res = DeriveTest.derive_block ustate bid
    let res2 = DeriveTest.derive_block_cache (Derive.r_cache res) mempty
            ustate bid
    print $ extract res
    print $ extract res2

extract :: Derive.Result -> [Either RealTime String]
extract = DeriveTest.extract_stream Score.event_start
