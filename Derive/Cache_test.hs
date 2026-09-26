-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Cache_test where
import qualified Control.Monad.Identity as Identity
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Util.Log as Log
import qualified Util.Ranges as Ranges
import qualified Util.Lists as Lists

import qualified Derive.Cache as Cache
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Stack as Stack
import qualified Derive.Stream as Stream
import qualified Derive.TrackWarp as TrackWarp

import qualified Instrument.Common as Common
import qualified Ui.Block as Block
import qualified Ui.Diff as Diff
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Track as Track
import qualified Ui.Types as Types
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig
import qualified Ui.UiTest as UiTest
import qualified Ui.Update as Update

import           Global
import           Types
import           Util.Test


-- * other functions

test_invalidate_damaged :: Test
test_invalidate_damaged = do
    let mkdamage tracks blocks = Derive.ScoreDamage
            (Map.fromList tracks) Set.empty
            (Set.fromList (map UiTest.bid blocks))
        empty = Derive.CachedEvents (Derive.CallType mempty Stream.empty)
        mkcache stack = Derive.Cache $
            Map.singleton (Derive.CacheKey stack) (Derive.Cached empty)
    let extract (key, cached) = (Derive.keyStack key,) $ case cached of
            Derive.Invalid -> False
            _ -> True
    let f damage stack = map extract $ Map.toList $ uncache $
            Derive.invalidateDamaged damage (mkcache stack)

    let blockStack = Stack.fromOutermost [block "top"]
        trackStack = Stack.fromOutermost [block "top", track "t"]
    equal (f (mkdamage [] []) trackStack) [(trackStack, True)]
    equal (f (mkdamage [] ["top"]) blockStack)
        [(blockStack, False)]
    equal (f (mkdamage [(UiTest.tid "t", Ranges.range 1 3)] []) trackStack)
        [(trackStack, False)]
    -- Block damage should also invalidate track caches on that block.
    equal (f (mkdamage [] ["top"]) trackStack)
        [(trackStack, False)]

    -- Damage on a track should invalidate all cache keys that have that track
    -- and range.  Also tested by 'test_stack_damage'.
    let mkstack trackName s e = Stack.fromOutermost
            [ block "b", track trackName, call "note-track", region s e
            , call "c1"
            ]
    let damage14 = mkdamage [(UiTest.tid "t", Ranges.range 1 4)] []
    -- Doesn't invalidate if the track is different, or if the range doesn't
    -- overlap.
    equal (f damage14 (mkstack "u" 2 3)) [(mkstack "u" 2 3, True)]
    equal (f damage14 (mkstack "t" 4 5)) [(mkstack "t" 4 5, True)]
    -- Match.
    equal (f damage14 (mkstack "t" 2 3)) [(mkstack "t" 2 3, False)]


-- * cached generators

-- Test interaction with the rest of the evaluation system.

test_no_damage :: Test
test_no_damage = do
    let create = mkblocks
            [ ("top", [(">i", [(0, 1, ""), (1, 2, "sub")])])
            , ("sub", [(">i", [(0, 1, ""), (1, 1, "subsub")])])
            , ("subsub", [(">i", [(0, 1, "")])])
            ]
    let (_, cached, uncached) = compareCached create (pure ())
    equal (diffEvents cached uncached) []

    -- should not have rederived sub
    stringsLike (rCacheLogs cached) ["using cache"]
    -- make sure there's stuff in the cache now
    notEqual (rCacheStacks cached) []

test_cached_track :: Test
test_cached_track = do
    -- If one track is damaged, it rederives and the other is cached.
    let create = mkblock
            [ (">i1", [(0, 1, "")])
            , (">i2", [(1, 1, "")])
            ]
    let (_, cached, uncached) = compareCached create $
            insertEvent "top.t2" 4 1 ""
    equal (diffEvents cached uncached) []
    stringsLike (rCacheLogs cached)
        [ "top \\* \\*: rederived"
        , "top top.t1 \\*: using cache"
        , "top top.t2 \\*: rederived"
        ]

test_add_remove :: Test
test_add_remove = do
    -- Make sure I get event damage from adding and removing event.
    let create = mkblocks
            [ ("top",
                [ ("tempo", [(0, 0, ".5")])
                , (">i", [(0, 1, ""), (1, 1, "")])
                ])
            ]
    let (_, cached, uncached) = compareCached create $
            Ui.removeEventsRange (UiTest.tid "top.t1")
                (Events.Point 1 Types.Positive)
    equal (diffEvents cached uncached) []

    let (_, cached, uncached) = compareCached create $
            insertEvent "top.t1" 4 1 ""
    equal (diffEvents cached uncached) []

test_double_eval :: Test
test_double_eval = do
    -- Cache entries from a block called multiple times from the same event
    -- won't collide.  This is what 'Derive.key_serial' is for.
    let create call = mkblocks
            [ ("top", [(">", [(0, 4, call <> " | sub")])])
            , ("sub=ruler", UiTest.noteTrack [(0, 1, "4c"), (1, 1, "4d")])
            ]
    let extract e = (DeriveTest.eNote e, Score.initialDynamic e)

    let (_, cached, uncached) = compareCached (create "echo 2") $
            insertEvent "top.t1" 4 1 ""
    equal (diffEvents cached uncached) []
    -- This is just 'diff_events', but shows the dyn.
    equal (DeriveTest.extract extract uncached)
        (DeriveTest.extract extract cached)

    -- It works even if both blocks are called at the same time.
    let (_, cached, uncached) = compareCached (create "echo 0") $
            insertEvent "top.t1" 4 1 ""
    equal (diffEvents cached uncached) []
    equal (DeriveTest.extract extract uncached)
        (DeriveTest.extract extract cached)

    -- This was the original case.
    let (_, cached, uncached) = compareCached (create "tile") $
            insertEvent "top.t1" 4 1 ""
    equal (diffEvents cached uncached) []

test_block_damage :: Test
test_block_damage = do
    let create = mkblocks
            [ ("top",
                [ ("tempo", [(0, 0, ".5")])
                , (">i", [(0, 1, "sub"), (1, 1, ""), (2, 1, "sub")])
                ])
            , ("sub", [(">", [(0, 1, "")])])
            ]
    -- A track disable should emit event damage for that block's range.
    let (_, cached, uncached) = compareCached create $
            Ui.addTrackFlag (UiTest.bid "sub") 1 Block.Disable
    equal (diffEvents cached uncached) []

    -- Plain old event modification works too.
    let (_, cached, uncached) = compareCached create $
            insertEvent "sub.t1" 0 0.5 ""
    equal (diffEvents cached uncached) []

test_subblock_damage :: Test
test_subblock_damage = do
    let create = mkblocks
            [ ("top", [(">i", [(0, 2, "sub")])])
            , ("alt", [(">i", [(0, 4, "sub")])])
            , ("sub=ruler", [(">", [(1, 1, "")])])
            ]
    let (_, cached, uncached) = compareCached create $
            insertEvent "sub.t1" 0 1 ""
    equal (diffEvents cached uncached) []
    equal (DeriveTest.extract Score.eventStart cached) ([0, 1], [])

    let (_, cached, uncached) = compareCachedBlock (Just (UiTest.bid "alt"))
            create $ insertEvent "sub.t1" 0 1 ""
    equal (diffEvents cached uncached) []
    equal (DeriveTest.extract Score.eventStart cached) ([0, 2], [])

test_config_damage :: Test
test_config_damage = do
    let create = mkblocks
            [ ("top", [(">i1", [(0, 1, "sub")])])
            , ("sub=ruler", [(">", [(0, 1, "")]), ("*", [(0, 0, "4c")])])
            ]
    -- I can't change UiConfig.config_global_transform because that will
    -- invalidate the cache via changed stack.
    let (_, cached, uncached) = compareCached create $
            modifyAllocConfig UiTest.i1 $
                Common.controls #= Map.fromList [(Controls.octave, 1)]
    equal (diffEvents cached uncached) []
    equal (DeriveTest.extract DeriveTest.ePitch cached) (["5c"], [])

modifyAllocConfig :: Ui.M m => ScoreT.Instrument
    -> (Common.Config -> Common.Config) -> m ()
modifyAllocConfig inst modify =
    Ui.modifyConfig $ UiConfig.allocationsMap %= Map.alter mod inst
    where
    mod Nothing = error $ "modify_alloc_config: no inst " <> prettys inst
    mod (Just alloc) = Just $ alloc
        { UiConfig.allocConfig = modify (UiConfig.allocConfig alloc) }

test_logs :: Test
test_logs = do
    let create = mkblocks
            [ ("top",
                [ (">i", [(0, 1, "sub1"), (1, 1, ""), (2, 1, "sub2")])
                ])
            , ("sub1", [(">", [(0, 1, "fail")])])
            , ("sub2", [(">", [(0, 1, "")])])
            ]
    let (_, cached, uncached) = compareCached create $
            insertEvent "sub2.t1" 1 1 ""
    -- Make sure errors are still present in the cached output.
    stringsLike (rPrettyLogs uncached)
        [ "top \\* \\*: rederived"
        , "top top.t1 \\*: rederived"
        , "top top.t1 0-1: sub1 \\* \\*: rederived"
        , "top top.t1 0-1: sub1 sub1.t1 \\*: rederived"
        , "top top.t1 0-1: sub1 sub1.t1 0-1: note generator not found"
        , "top top.t1 2-3: sub2 \\* \\*: rederived"
        , "top top.t1 2-3: sub2 sub2.t1 \\*: rederived"
        ]
    stringsLike (rPrettyLogs cached)
        [ "top \\* \\*: rederived"
        , "top top.t1 \\*: rederived"
        , "top top.t1 0-1: sub1 \\* \\*: using cache"
        , "top top.t1 0-1: sub1 sub1.t1 0-1: note generator not found"
        , "top top.t1 2-3: sub2 \\* \\*: rederived"
        , "top top.t1 2-3: sub2 sub2.t1 \\*: rederived"
        ]

test_stats :: Test
test_stats = do
    let create = mkblocks
            [ ("top",
                [ (">i", [(0, 2, "sub1"), (2, 3, "sub2")])
                ])
            , ("sub1=ruler", [(">", [(0, 1, "")]), (">", [(0, 1, "")])])
            , ("sub2=ruler", [(">", [(0, 1, ""), (1, 1, "")])])
            ]
    let eStats = Derive.collectCacheStats . Derive.stateCollect
            . Derive.rState

    let (_, cached, uncached) = compareCached create $
            insertEvent "sub2.t1" 1 1 ""
    equal (eStats uncached) mempty
    equal (eStats cached) $ mempty
        { Derive.cstatsHits = [(Left (UiTest.bid "sub1"), (0, 2))] }

    let (_, cached, _) = compareCached create $ do
            insertEvent "sub1.t1" 1 1 ""
            insertEvent "sub2.t1" 1 1 ""
    equal (eStats cached) $ mempty
        { Derive.cstatsHits = [(Right (UiTest.tid "sub1.t2"), (0, 4))] }

test_extend_control_damage :: Test
test_extend_control_damage = do
    let f = Cache._extendControlDamage
    equal (f 3 (Events.fromList [Event.event 0 0 "4c"])
            (Ranges.ranges [(2, 5)]))
        (Ranges.ranges [(0, 3)])

test_arrival_notes :: Test
test_arrival_notes = do
    -- Arrival block calls look at the control at the the bottom, not the top,
    -- so ensure that control damage works for them too.
    let create = mkblocks
            [ ("top",
                [ (">", [(2, -2, "sub")])
                , ("*", [(2, 0, "4c")])
                ])
            , ("sub=ruler", [(">", [(1, -1, "")])])
            ]
    let (_, cached, uncached) = compareCached create $
            insertEvent "top.t2" 2 0 "4d"
    equal (diffEvents cached uncached) []

test_stack_damage :: Test
test_stack_damage = do
    -- The stack is the same, since I have an equal, but the actual call
    -- has changed.
    let create = mkblocks
            [ ("top",
                [ (">", [(0, 1, "x = 1 | sub")])
                , ("*", [(0, 0, "4c")])
                ])
            , ("sub=ruler", [(">", [(0, 1, "")])])
            ]
    let (_, cached, uncached) = compareCached create $
            insertEvent "top.t1" 0 1 "t-dia = 1 | sub"
    equal (diffEvents cached uncached) []

test_failed_sub_track :: Test
test_failed_sub_track = do
    let create title = mkblocks
            [ ("top", [(">i", [(0, 1, ""), (1, 1, "sub")])])
            , ("sub=ruler", [(title, [(0, 1, "")])])
            ]
        tid = (UiTest.mkTidBlock (UiTest.bid "sub") 1)
    -- From error to non-error.
    let (orig, cached, uncached) = compareCached (create "> |") $
            Ui.setTrackTitle tid ">"
    equal (diffEvents cached uncached) []
    -- top has a dep on sub even though sub crashed.
    equal (rCacheDeps orig) $ map (second (Just . map UiTest.bid))
        [ ("top * *", ["sub", "top"])
        , ("top top.t1 *", ["sub"])
        , ("top top.t1 1-2: sub * *", ["sub"])
        ]

    -- From non-error to error.
    let (orig, cached, uncached) = compareCached (create ">") $
            Ui.setTrackTitle tid "> |"
    equal (diffEvents cached uncached) []
    equal (rCacheDeps orig) $ map (second (Just . map UiTest.bid))
        [ ("top * *", ["sub", "top"])
        , ("top top.t1 *", ["sub"])
        , ("top top.t1 1-2: sub * *", ["sub"])
        , ("top top.t1 1-2: sub sub.t1 *", [])
        ]
    stringsLike (rPrettyLogs cached)
        [ "top * sub-block damage"
        , "top.t1 * sub-block damage"
        , "top top.t1 1-2: sub * track block damage"
        , "top top.t1 1-2: sub * note transformer not found"
        ]

test_has_score_damage :: Test
test_has_score_damage = do
    let create = mkblocks
            [ ("top", [(">i", [(0, 1, "sub"), (1, 1, "sub"), (2, 1, "sub")])])
            , ("sub", [(">i", [(0, 1, "")])])
            , ("sub2", [(">i", [(0, 1, ""), (1, 1, "")])])
            ]

    -- Swap it out for an event of the same size, so the stack is the same,
    -- but it should rederive because of score damage.
    let (_, cached, uncached) = compareCached create $
            insertEvent "top.t1" 1 1 "sub2"
    equal (diffEvents cached uncached) []
    stringsLike (rBlockLogs cached)
        [ toplevelRederived True
        , "top.t1 0-1: * using cache"
        , "top.t1 1-2: * rederived * not in cache"
        , "top.t1 2-3: * using cache"
        ]

test_callee_damage :: Test
test_callee_damage = do
    -- test callee damage: sub-block is damaged, it should be rederived
    let parentSub = mkblocks
            [ ("top", [(">i", [(0, 1, ""), (1, 2, "sub")])])
            , ("sub", [(">i", [(0, 1, ""), (1, 1, "subsub")])])
            , ("subsub", [(">i", [(0, 1, "")])])
            ]
    let (_, cached, uncached) = compareCached parentSub $
            insertEvent "sub.t1" 0 0.5 ""
    equal (diffEvents cached uncached) []
    stringsLike (rBlockLogs cached)
        [ toplevelRederived False
        , "top.t1 1-3: * rederived * because of track block"
        , "sub.t1 1-2: * using cache"
        ]

    -- The cached call to "sub" depends on "sub" and "subsub" transitively.
    equal (rCacheDeps cached) $ map (second (Just . map UiTest.bid))
        [ ("top * *", ["sub", "subsub", "top"])
        , ("top top.t1 *", ["sub", "subsub"])
        , ("top top.t1 1-3: sub * *", ["sub", "subsub"])

        , ("top top.t1 1-3: sub sub.t1 *", ["subsub"])
        , ("top top.t1 1-3: sub sub.t1 1-2: subsub * *", ["subsub"])
        , ("top top.t1 1-3: sub sub.t1 1-2: subsub subsub.t1 *", [])
        ]
    -- sub is 1-3, its first elt should be 1-2, except I replaced it with .5

    -- A change to subsub means the top's "sub" call rederives as well.
    let (_, cached, uncached) = compareCached parentSub $
            insertEvent "subsub.t1" 0 0.5 ""
    equal (diffEvents cached uncached) []
    stringsLike (rBlockLogs cached)
        [ toplevelRederived False
        , "top.t1 1-3: * rederived * sub-block damage"
        , "sub.t1 1-2: * rederived * block damage"
        ]

test_collect :: Test
test_collect = do
    let blocks = mkblocks
            [ ("top=ruler", [(">i", [(0, 1, "sub"), (1, 1, "sub")])])
            , ("sub=ruler",
                [ (">i", [(0, 1, "")])
                , ("cont", [(0, 0, "1")])
                ])
            ]
    let create = blocks
            <* Ui.setRenderStyle (Track.Line Nothing) (UiTest.tid "sub.t2")
    let (_, cached, _) = compareCached create $ insertEvent "top.t1" 1 1 ""
    let (rootKey, maybeCollect) : _ = rCacheCollect cached
        Just collect = maybeCollect
    equal rootKey "top * *"

    let eWarpMaps = Lists.sortOn fst . map (bimap Stack.prettyUi_ eTrack)
            . Map.toAscList . Derive.collectWarpMap
        eTrack (TrackWarp.Track start end _warp blockId trackId) =
            (start, end, blockId, trackId)

    equal (eWarpMaps collect)
        [ ("top * *", (0, 2, UiTest.bid "top", Nothing))
        , ("top top.t1 0-1 / sub * *", (0, 1, UiTest.bid "sub", Nothing))
        ]

    -- TrackSignals are only collected for the topmost block.
    equal (Derive.collectTrackSignals collect) mempty
    equal (Derive.collectBlockDeps collect)
        (mkBlockDeps ["top", "sub"])

test_block_deps :: Test
test_block_deps = do
    let run root blocks =
            extract $ Derive.collectBlockDeps $ Derive.stateCollect $
            Derive.rState $
            DeriveTest.deriveBlockSetup mempty
                (snd (UiTest.runMkblocks blocks)) (UiTest.bid root)
        extract (Derive.BlockDeps deps) = deps
    let blocks =
            [ ("top", [(">", [(0, 1, "b1")])])
            , ("b1", [(">", [(0, 1, "b2")])])
            , ("b2", UiTest.regularNotes 2)
            , ("b3", [(">", [(0, 1, "b2")])])
            ]
    equal (run "top" blocks)
        (Set.fromList $ map UiTest.bid ["top", "b1", "b2"])
    equal (run "b3" blocks)
        (Set.fromList $ map UiTest.bid ["b3", "b2"])

test_collect_indirect_call :: Test
test_collect_indirect_call = do
    let run note = DeriveTest.deriveBlocks
            [ ("top -- ^t = \"(sub)", [(">", [(0, 1, note)])])
            , ("sub=ruler", UiTest.noteTrack1 ["4c"])
            ]
    equal (DeriveTest.extract DeriveTest.eNote (run "sub"))
        ([(0, 1, "4c")], [])
    equal (DeriveTest.extract DeriveTest.eNote (run "t"))
        ([(0, 1, "4c")], [])

    let top = UiTest.bid "top"
        sub = UiTest.bid "sub"
    let eCollection = map TrackWarp.twBlock . Derive.rTrackWarps
    equal (eCollection (run "sub")) [sub, top]
    equal (eCollection (run "t")) [sub, top]

    let eBlockDeps =
            map (fmap (fmap (eBlockDep . Derive.collectBlockDeps)))
            . rCacheCollect
        eBlockDep (Derive.BlockDeps blocks) = Set.toList blocks
    let deps =
            [ ("top * *", Just [sub, top])
            , ("top top.t1 *", Just [sub])
            , ("top top.t1 0-1: sub * *", Just [sub])
            , ("top top.t1 0-1: sub sub.t1 *", Just [])
            ]
    equal (eBlockDeps (run "sub")) deps
    equal (eBlockDeps (run "t")) deps

test_sliced_score_damage :: Test
test_sliced_score_damage = do
    -- Ensure that a cached call underneath a slice still works correctly.
    -- This is tricky because the cache relies on Stack.Region entries
    -- accurately reflecting the position of the call on the track for
    -- ScoreDamage to invalidate it.
    let create = do
            UiTest.mkblocksSkel blocks
            pure $ UiTest.bid "b9"
    let (_prev, cached, uncached) = compareCached create $
            insertEvent "b9.t3" 4 0 "7c"
    equal (diffEvents cached uncached) []
    where
    blocks =
        [ (("b9", b9), [(1, 2), (2, 3)])
        , (("b28", b28), [])
        ]
    b9 =
        [ (">", [(4, 0, "`arp-up`")])
        , (">", [(4, 3, "b28")])
        , ("*", [(4, 0, "5c")])
        ]
    b28 = [(">", [(0, 0.5, ""), (0.5, 0.5, ""), (1, 0.5, "")])]

test_sliced_control_damage :: Test
test_sliced_control_damage = do
    -- Ensure that control damage properly invalidates a call that has been
    -- sliced and shifted.
    let create = do
            UiTest.mkblocksSkel blocks
            pure (UiTest.bid "top")
    let (_prev, cached, uncached) = compareCached create $
            insertEvent "top.t1" 6 0 "0"
    equal (diffEvents cached uncached) []
    stringsLike (rBlockLogs cached)
        ["top * block damage", "sub * control damage"]
    where
    blocks =
        [ (("top", top), [(1, 2), (2, 3)])
        , (("sub", sub), [])
        ]
    top =
        [ ("vel", [(6, 0, "1")])
        , (">", [(10, 0, "`arp-up`")])
        , (">", [(10, 2, "sub")])
        ]
    sub = [(">", [(0, 1, ""), (1, 1, "")])]

test_control_damage :: Test
test_control_damage = do
    -- If I modify a control in a certain place, say a pitch track, I don't
    -- want it to derive the whole control.  Then it will damage the whole
    -- control and force everything below it to rederive.  I want it to emit
    -- control damage for only the bit of control signal that actually
    -- changed.
    let create = mkblocks
            [ ("top",
                [ ("c1", [(0, 0, "1")])
                , (">i", [(0, 1, "sub"), (1, 1, "sub")])
                ])
            , ("sub", [(">i", [(0, 1, "")])])
            ]

    -- the modification is out of range, so the caches are reused
    let (_, cached, uncached) = compareCached create $
            insertEvent "top.t1" 2 0 "0"
    equal (diffEvents cached uncached) []
    stringsLike (rCacheLogs cached)
        [ toplevelRederived True
        , "top.t1 \\*: rederived"
        , "top.t2 \\*: rederived"
        , "top.t2 0-1: * using cache"
        , "top.t2 1-2: * using cache"
        ]

    -- only the  affected event is rederived
    let (_, cached, uncached) = compareCached create $
            insertEvent "top.t1" 1 0 ".5"
    equal (diffEvents cached uncached) []
    stringsLike (rCacheLogs cached)
        [ toplevelRederived True
        , "top.t1 \\*: rederived"
        , "top.t2 \\*: * control damage"
        , "top.t2 0-1: sub \\* \\*: using cache"
        , "top.t2 1-2: sub \\* \\*: * control damage"
        , "top.t2 1-2: sub sub.t1 \\*: * control damage"
        ]

    -- if the change damages a greater control area, it should affect the
    -- event
    let (_, cached, uncached) = compareCached create $
            insertEvent "top.t1" 0 0 ".5"
    equal (diffEvents cached uncached) []
    stringsLike (rCacheLogs cached)
        [ toplevelRederived True
        , "top.t1 \\*: rederived"
        , "top.t2 \\*: * control damage"
        , "top.t2 0-1: sub \\* \\*: * control damage"
        , "top.t2 0-1: sub sub.t1 \\*: * control damage"
        , "top.t2 1-2: sub \\* \\*: * control damage"
        , "top.t2 1-2: * control damage"
        ]

test_control_damage2 :: Test
test_control_damage2 = do
    -- Damage extends to the next event.
    let create = mkblocks
            [ ("top",
                [ (">", [(0, 1, "sub"), (1, 1, "sub")])
                , ("dyn", [(0, 0, "1")])
                ])
            , ("sub=ruler", [(">", [(0, 1, "")])])
            ]
    let (_, cached, uncached) = compareCached create $
            insertEvent "top.t2" 0 0 "0.5"
    equal (diffEvents cached uncached) []

test_get_control_damage :: Test
test_get_control_damage = do
    let f events cdmg sdmg = run events $
            getControlDamage (0, 10) (mkcdamage cdmg) (mksdamage sdmg)
    -- There are no events to be damaged so the damage was likely removing
    -- them, in which case it should be propagated.
    equal (f [] [] [(0, 4)]) (Right (Just [(0, 4)]))
    equal (f [(0, 0, "0")] [] [(0, 0)])
        (Right (Just [(0, 10)]))
    equal (f [(0, 0, "0"), (4, 0, "0")] [] [(0, 0)])
        (Right (Just [(0, 4)]))
    equal (f [(0, 0, "0"), (4, 0, "0")] [] [(1, 1)])
        (Right (Just [(0, 4)]))
    equal (f [(0, 0, "0"), (4, 0, "0"), (8, 0, "0")] [] [(3, 5)])
        (Right (Just [(0, 8)]))
    equal (f [(0, 0, "0"), (4, 0, "0"), (8, 0, "0")] [] [(4, 4)])
        (Right (Just [(4, 8)]))
    -- Existing control damage isn't expanded.
    equal (f [(0, 0, "0"), (4, 0, "0")] [(2, 3)] [])
        (Right (Just [(2, 3)]))
    where
    getControlDamage range cdmg sdmg = do
        Derive.modify $ \st -> st
            { Derive.stateConstant = (Derive.stateConstant st)
                { Derive.stateScoreDamage = sdmg }
            , Derive.stateDynamic = (Derive.stateDynamic st)
                { Derive.stateControlDamage = cdmg }
            }
        Cache.getControlDamage UiTest.defaultBlockId (UiTest.mkTid 1) range
    extract = DeriveTest.extractRun $
        \(Derive.ControlDamage r) -> Ranges.extract r
    run events d = extract $ DeriveTest.run
        (snd (UiTest.runMkblock [("cont", events)])) d
    mkcdamage = Derive.ControlDamage . Ranges.ranges
    mksdamage ranges = Derive.ScoreDamage
        (Map.singleton (UiTest.mkTid 1) (Ranges.ranges ranges))
        mempty mempty

test_inverted_control_damage :: Test
test_inverted_control_damage = do
    let create = mkblocks
            [ ("top",
                [ (">i", [(0, 1, "sub"), (1, 1, "sub")])
                , ("c1", [(0, 0, "1")])
                ])
            , ("sub", [(">i", [(0, 1, "")])])
            ]

    -- only the  affected event is rederived
    let (_, cached, uncached) = compareCached create $
            insertEvent "top.t2" 1 0 ".5"
    equal (diffEvents cached uncached) []
    stringsLike (rBlockLogs cached)
        [ toplevelRederived True
        , "top.t1 0-1: * using cache"
        , "top top.t1 1-2: sub * control damage"
        ]

test_control_damage_subblock :: Test
test_control_damage_subblock = do
    -- Ensure that control damage that touches a block call gets expanded to
    -- cover the entire block.
    let create = mkblocks
            [ ("top=ruler",
                [ ("tempo", [(0, 0, "1"), (2, 0, "1")])
                , (">", [(0, 2, "b1"), (2, 2, "b1")])
                ])
            , ("b1=ruler", [(">", [(0, 2, "b2"), (2, 2, "b3")])])
            , ("b2=ruler", UiTest.noteTrack [(0, 1, "4a"), (1, 1, "4b")])
            , ("b3=ruler", UiTest.noteTrack [(0, 1, "4c"), (1, 1, "4d")])
            ]
    let (_, cached, uncached) = compareCached create $
            insertEvent "top.t1" 0 0 "2"
    equal (diffEvents cached uncached) []
    -- Everything has control damage.
    stringsLike (rBlockLogs cached)
        [ "top"
        , "0-2: b1 * control", "0-2: b2 * control", "2-4: b3 * control"
        , "2-4: b1 * control", "0-2: b2 * control", "2-4: b3 * control"
        ]

    -- Only the second b1 is rederived.
    let (_, cached, uncached) = compareCached create $
            insertEvent "top.t1" 3 0 "2"
    equal (diffEvents cached uncached) []

    stringsLike (rBlockLogs cached)
        [ "top"
        , "0-2: b1 * using cache"
        , "2-4: b1 * control", "0-2: b2 * control", "2-4: b3 * control"
        ]

test_tempo_damage :: Test
test_tempo_damage = do
    let create = mkblocks
            [ ("top",
                [ ("tempo", [(0, 0, "1"), (1, 0, "1")])
                , (">i", [(0, 1, "sub"), (1, 1, "sub"), (2, 1, "sub")])
                ])
            , ("sub", [(">i", [(0, 1, "")])])
            ]
    let (_, cached, uncached) = compareCached create $
            insertEvent "top.t1" 2 0 "2"
    equal (diffEvents cached uncached) []
    -- Damage extends to the previous event, so the first is cached, second and
    -- third are not.
    stringsLike (rBlockLogs cached)
        [ toplevelRederived True
        , "top.t2 0-1: * using cache"
        , "top.t2 1-2: * control damage"
        , "top.t2 2-3: * control damage"
        ]

    -- Change tempo call in a way that damages to the previous tempo call.
    let create = mkblocks
            [ ("top",
                [ ("tempo", [(0, 0, "1"), (2, 0, "2")])
                , (">", [(n, 1, "sub") | n <- Lists.range 0 4 1])
                ])
            , ("sub=ruler", [(">", [(0, 1, "")])])
            ]
    let (_, cached, uncached) = compareCached create $
            insertEvent "top.t1" 2 0 "i 2"
    equal (diffEvents cached uncached) []
    -- prettyp (DeriveTest.extract DeriveTest.e_note cached)

test_extend_tempo_damage :: Test
test_extend_tempo_damage = do
    -- Make sure control damage emitted by 'get_tempo_damage' is reasonable.
    let create = mkblock
            [ ("tempo", [(0, 0, "1")])
            , (">i", [(0, 1, ""), (1, 1, "")])
            , ("modulation", [(0, 1, "0")])
            ]
    let (_, cached, uncached) = compareCached create $
            Ui.insertEvent (topTid 1) (Event.event 1 0 "2")
    equal (diffEvents cached uncached) []

test_block_title_damage :: Test
test_block_title_damage = do
    -- Changing tempo in the block title invalidates the caches.
    let create =
            mkblocks
                [ ("top", [(">", [(0, 2, "sub")])])
                , ("sub=ruler", UiTest.regularNotes 2)
                ]
            <* Ui.setBlockTitle (UiTest.bid "top") "tempo = 2"
    let (_, cached, uncached) = compareCached create $
            Ui.setBlockTitle (UiTest.bid "top") "tempo = 1"
    equal (diffEvents cached uncached) []

test_track_cache :: Test
test_track_cache = do
    let create = mkblock
            [ ("dyn", [(0, 0, ".5"), (1, 0, "1")])
            , (">", [(0, 2, "")])
            ]
    -- Ensure that a control track above a note track is cached.
    let (_, cached, uncached) = compareCached create $
            Ui.insertEvent (topTid 2) $ Event.event 1 1 ""
    equal (diffEvents cached uncached) []
    stringsLike (rCacheLogs cached)
        [ "top \\* \\*: rederived"
        , "top top.t1 \\*: using cache"
        , "top top.t2 \\*: rederived"
        ]

    -- And invalidated on damage.
    let (_, cached, uncached) = compareCached create $
            Ui.insertEvent (topTid 1) $ Event.event 0 0 ".75"
    equal (diffEvents cached uncached) []
    stringsLike (rCacheLogs cached)
        [ "top \\* \\*: rederived"
        , "top top.t1 \\*: rederived * cache invalidated"
        , "top top.t2 \\*: rederived"
        ]

    let title = Ui.setBlockTitle (UiTest.bid "top")

    -- Also invalidated on block damage.
    let (_, cached, _) = compareCached (create <* title "foo = a") $
            title "foo = b"
    equal (DeriveTest.extract (DeriveTest.eEnviron "foo") cached)
        ([Just "b"], [])
    stringsLike (rCacheLogs cached)
        [ "top \\* \\*: rederived"
        , "top top.t1 \\*: rederived"
        , "top top.t2 \\*: rederived"
        ]

    -- Make sure I don't get extra cache entries when the stack changes.
    let (_, cached, _) = compareCached create $ title "foo = a"
    equal (DeriveTest.extract (DeriveTest.eEnviron "foo") cached)
        ([Just "a"], [])
    equal (rCacheStacks cached)
        ["top * *", "top top.t1 *", "top top.t2 *"]

test_track_cache2 :: Test
test_track_cache2 = do
    let create = mkblock
            [ ("tempo", [(0, 0, "1")])
            , ("dyn", [(0, 0, ".5")])
            , (">", [(0, 1, "")])
            ]
    -- Two levels of track caching.
    let (_, cached, uncached) = compareCached create $
            Ui.insertEvent (topTid 2) $ Event.event 0 0 "1"
    equal (diffEvents cached uncached) []


-- * make

block :: Text -> Stack.Frame
block = Stack.Block . UiTest.bid

track :: Text -> Stack.Frame
track = Stack.Track . UiTest.tid

region :: TrackTime -> TrackTime -> Stack.Frame
region = Stack.Region

call :: Text -> Stack.Frame
call = Stack.Call

topTid :: TrackNum -> TrackId
topTid = UiTest.mkTidName "top"

-- | The toplevel block is just about always damaged.
toplevelRederived :: Bool -> Text
toplevelRederived True = "top *: rederived * block damage"
toplevelRederived False = "top *: rederived * sub-block damage"

mkBlockDeps :: [Text] -> Derive.BlockDeps
mkBlockDeps = Derive.BlockDeps . Set.fromList . map UiTest.bid

mkblocks :: Ui.M m => [UiTest.BlockSpec] -> m BlockId
mkblocks blocks = head <$> UiTest.mkblocks blocks

mkblock :: Ui.M m => [UiTest.TrackSpec] -> m BlockId
mkblock tracks = mkblocks [("top", tracks)]

insertEvent :: Ui.M m => Text -> ScoreTime -> ScoreTime -> Text -> m ()
insertEvent tid pos dur text =
    Ui.insertEvent (UiTest.tid tid) (Event.event pos dur text)

-- * extract

-- | Extract cache logs so I can tell who rederived and who used the cache.
-- I use strings instead of parsing it into structured data because strings
-- make more informative errors when they don't match.
rCacheLogs :: Derive.Result -> [Text]
rCacheLogs = map showLog . filter Cache.isCacheLog . rLogs

-- | Sometimes the cache msgs for the track cache are just clutter.
rBlockLogs :: Derive.Result -> [Text]
rBlockLogs =
    map showLog . filter (not . trackStack)
        . filter Cache.isCacheLog . rLogs
    where
    trackStack msg = case Log.msgStack msg of
        Just stack -> case Stack.innermost stack of
            Stack.Track _ : _ -> True
            _ -> False
        _ -> False

rPrettyLogs :: Derive.Result -> [Text]
rPrettyLogs = map showLog . rLogs

-- | The logs are sorted for tests, since log order isn't really defined.
rLogs :: Derive.Result -> [Log.Msg]
rLogs = Lists.sortOn showLog . Stream.logsOf . Derive.rEvents

showLog :: Log.Msg -> Text
showLog msg =
    DeriveTest.showStack_ False (Log.msgStack msg) <> ": " <> Log.msgText msg

-- | Pull the collects out of the cache, pairing them up with the cache keys.
rCacheCollect :: Derive.Result -> [(Text, Maybe Derive.Collect)]
rCacheCollect result = Lists.sortOn fst
    [ (DeriveTest.showStack_ False (Just (Derive.keyStack key)),
        collect ctype)
    | (key, ctype) <- Map.toList cmap
    ]
    where
    cmap = uncache (Derive.rCache result)
    collect (Derive.Cached (Derive.CachedEvents (Derive.CallType collect _))) =
        Just collect
    collect _ = Nothing

rCacheDeps :: Derive.Result -> [(Text, Maybe [BlockId])]
rCacheDeps result =
    [(stack, fmap deps collect) | (stack, collect) <- rCacheCollect result]
    where
    deps collect = case Derive.collectBlockDeps collect of
        Derive.BlockDeps blocks -> Set.toAscList blocks

rCacheStacks :: Derive.Result -> [Text]
rCacheStacks = map (Stack.prettyUi_ . Derive.keyStack) . Map.keys
    . Map.filter valid . uncache . Derive.rCache
    where
    valid Derive.Invalid = False
    valid _ = True

uncache :: Derive.Cache -> Map Derive.CacheKey Derive.Cached
uncache (Derive.Cache cache) = cache

-- * run

-- UiTest.run discards the Updates, which I need.
run :: Ui.State -> Ui.StateId a -> (a, Ui.State, Update.UiDamage)
run state m = case result of
    Left err -> error $ "state error: " <> show err
    Right (val, state', updates) -> (val, state', updates)
    where result = Identity.runIdentity (Ui.run state m)

-- | Derive with and without the cache, and make sure the cache fired and the
-- results are the same.  Returns (result before modification, cached,
-- uncached).  The pre-modification result is occasionally useful to check
-- logs.
compareCached :: Ui.StateId BlockId -> Ui.StateId a
    -> (Derive.Result, Derive.Result, Derive.Result)
compareCached = compareCachedBlock Nothing

compareCachedBlock :: Maybe BlockId -> Ui.StateId BlockId
    -> Ui.StateId a -> (Derive.Result, Derive.Result, Derive.Result)
compareCachedBlock maybeRootId create modify = (result, cached, uncached)
    where
    state1 = UiTest.exec Ui.empty create
    rootId = fromMaybe (getRootId state1) maybeRootId
    result = deriveBlockCache mempty mempty state1 rootId
    cached = runCached rootId result state1 modify
    -- 'run_cached' already did this once, but it doesn't return it.
    (_, state2, _) = run state1 modify
    uncached = deriveBlockCache mempty mempty state2 rootId

-- | Run a derive after some modifications with the cache from a previous
-- derive.
runCached :: BlockId -> Derive.Result -> Ui.State -> Ui.StateId a
    -> Derive.Result
runCached rootId result state1 modify =
    deriveBlockCache (Derive.rCache result) damage state2 rootId
    where
    (_, state2, uiDamage) = run state1 modify
    (updates, _) = Diff.diff uiDamage state1 state2
    damage = Diff.deriveDiff state1 state2 uiDamage updates

deriveBlockCache :: Derive.Cache -> Derive.ScoreDamage -> Ui.State
    -> BlockId -> Derive.Result
deriveBlockCache =
    DeriveTest.deriveBlockStandard mempty DeriveTest.defaultCmdState

getRootId :: Ui.State -> BlockId
getRootId state = UiTest.eval state Ui.getRootId

scoreDamage :: Ui.StateId a -> Ui.StateId b -> Derive.ScoreDamage
scoreDamage create modify = Diff.deriveDiff state1 state2 uiDamage updates
    where
    (_, state1) = UiTest.run Ui.empty create
    (_, state2, uiDamage) = run state1 modify
    (updates, _) = Diff.diff uiDamage state1 state2

-- | 'diff_events' returns this when there were no events to diff.
expectNoEvents :: [Either DiffEvent DiffEvent]
expectNoEvents = [Left (0, 0, "NO EVENTS TO DIFF", 0)]

diffEvents :: Derive.Result -> Derive.Result -> [Either DiffEvent DiffEvent]
diffEvents r1 r2
    -- Without this check it's easy for a test to pass if the derive just
    -- crashes and produces no events.
    -- This is a hack, but otherwise I have to remember to stick a "check not
    -- null" line on every test.  In practice, I don't expect any test to
    -- produce zero events.
    | null x1 && null x2 = expectNoEvents
    | otherwise = Lists.diffEither (==) x1 x2
    where
    x1 = extract r1
    x2 = extract r2
    extract = fst . DeriveTest.extract simpleEvent

-- | (start, dur, pitch, initial_dyn)
type DiffEvent = (RealTime, RealTime, Text, Double)

simpleEvent :: Score.Event -> DiffEvent
simpleEvent e = (Score.eventStart e, Score.eventDuration e,
    DeriveTest.ePitch e, Score.initialDynamic e)
