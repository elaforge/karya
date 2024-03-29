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
    let extract (key, cached) = (Derive.key_stack key,) $ case cached of
            Derive.Invalid -> False
            _ -> True
    let f damage stack = map extract $ Map.toList $ uncache $
            Derive.invalidate_damaged damage (mkcache stack)

    let block_stack = Stack.from_outermost [block "top"]
        track_stack = Stack.from_outermost [block "top", track "t"]
    equal (f (mkdamage [] []) track_stack) [(track_stack, True)]
    equal (f (mkdamage [] ["top"]) block_stack)
        [(block_stack, False)]
    equal (f (mkdamage [(UiTest.tid "t", Ranges.range 1 3)] []) track_stack)
        [(track_stack, False)]
    -- Block damage should also invalidate track caches on that block.
    equal (f (mkdamage [] ["top"]) track_stack)
        [(track_stack, False)]

    -- Damage on a track should invalidate all cache keys that have that track
    -- and range.  Also tested by 'test_stack_damage'.
    let mkstack track_name s e = Stack.from_outermost
            [ block "b", track track_name, call "note-track", region s e
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
    let (_, cached, uncached) = compare_cached create (return ())
    equal (diff_events cached uncached) []

    -- should not have rederived sub
    strings_like (r_cache_logs cached) ["using cache"]
    -- make sure there's stuff in the cache now
    not_equal (r_cache_stacks cached) []

test_cached_track :: Test
test_cached_track = do
    -- If one track is damaged, it rederives and the other is cached.
    let create = mkblock
            [ (">i1", [(0, 1, "")])
            , (">i2", [(1, 1, "")])
            ]
    let (_, cached, uncached) = compare_cached create $
            insert_event "top.t2" 4 1 ""
    equal (diff_events cached uncached) []
    strings_like (r_cache_logs cached)
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
    let (_, cached, uncached) = compare_cached create $
            Ui.remove_events_range (UiTest.tid "top.t1")
                (Events.Point 1 Types.Positive)
    equal (diff_events cached uncached) []

    let (_, cached, uncached) = compare_cached create $
            insert_event "top.t1" 4 1 ""
    equal (diff_events cached uncached) []

test_double_eval :: Test
test_double_eval = do
    -- Cache entries from a block called multiple times from the same event
    -- won't collide.  This is what 'Derive.key_serial' is for.
    let create call = mkblocks
            [ ("top", [(">", [(0, 4, call <> " | sub")])])
            , ("sub=ruler", UiTest.note_track [(0, 1, "4c"), (1, 1, "4d")])
            ]
    let extract e = (DeriveTest.e_note e, Score.initial_dynamic e)

    let (_, cached, uncached) = compare_cached (create "echo 2") $
            insert_event "top.t1" 4 1 ""
    equal (diff_events cached uncached) []
    -- This is just 'diff_events', but shows the dyn.
    equal (DeriveTest.extract extract uncached)
        (DeriveTest.extract extract cached)

    -- It works even if both blocks are called at the same time.
    let (_, cached, uncached) = compare_cached (create "echo 0") $
            insert_event "top.t1" 4 1 ""
    equal (diff_events cached uncached) []
    equal (DeriveTest.extract extract uncached)
        (DeriveTest.extract extract cached)

    -- This was the original case.
    let (_, cached, uncached) = compare_cached (create "tile") $
            insert_event "top.t1" 4 1 ""
    equal (diff_events cached uncached) []

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
    let (_, cached, uncached) = compare_cached create $
            Ui.add_track_flag (UiTest.bid "sub") 1 Block.Disable
    equal (diff_events cached uncached) []

    -- Plain old event modification works too.
    let (_, cached, uncached) = compare_cached create $
            insert_event "sub.t1" 0 0.5 ""
    equal (diff_events cached uncached) []

test_subblock_damage :: Test
test_subblock_damage = do
    let create = mkblocks
            [ ("top", [(">i", [(0, 2, "sub")])])
            , ("alt", [(">i", [(0, 4, "sub")])])
            , ("sub=ruler", [(">", [(1, 1, "")])])
            ]
    let (_, cached, uncached) = compare_cached create $
            insert_event "sub.t1" 0 1 ""
    equal (diff_events cached uncached) []
    equal (DeriveTest.extract Score.event_start cached) ([0, 1], [])

    let (_, cached, uncached) = compare_cached_block (Just (UiTest.bid "alt"))
            create $ insert_event "sub.t1" 0 1 ""
    equal (diff_events cached uncached) []
    equal (DeriveTest.extract Score.event_start cached) ([0, 2], [])

test_config_damage :: Test
test_config_damage = do
    let create = mkblocks
            [ ("top", [(">i1", [(0, 1, "sub")])])
            , ("sub=ruler", [(">", [(0, 1, "")]), ("*", [(0, 0, "4c")])])
            ]
    -- I can't change UiConfig.config_global_transform because that will
    -- invalidate the cache via changed stack.
    let (_, cached, uncached) = compare_cached create $
            modify_alloc_config UiTest.i1 $
                Common.controls #= Map.fromList [(Controls.octave, 1)]
    equal (diff_events cached uncached) []
    equal (DeriveTest.extract DeriveTest.e_pitch cached) (["5c"], [])

modify_alloc_config :: Ui.M m => ScoreT.Instrument
    -> (Common.Config -> Common.Config) -> m ()
modify_alloc_config inst modify =
    Ui.modify_config $ UiConfig.allocations_map %= Map.alter mod inst
    where
    mod Nothing = error $ "modify_alloc_config: no inst " <> prettys inst
    mod (Just alloc) = Just $ alloc
        { UiConfig.alloc_config = modify (UiConfig.alloc_config alloc) }

test_logs :: Test
test_logs = do
    let create = mkblocks
            [ ("top",
                [ (">i", [(0, 1, "sub1"), (1, 1, ""), (2, 1, "sub2")])
                ])
            , ("sub1", [(">", [(0, 1, "fail")])])
            , ("sub2", [(">", [(0, 1, "")])])
            ]
    let (_, cached, uncached) = compare_cached create $
            insert_event "sub2.t1" 1 1 ""
    -- Make sure errors are still present in the cached output.
    strings_like (r_pretty_logs uncached)
        [ "top \\* \\*: rederived"
        , "top top.t1 \\*: rederived"
        , "top top.t1 0-1: sub1 \\* \\*: rederived"
        , "top top.t1 0-1: sub1 sub1.t1 \\*: rederived"
        , "top top.t1 0-1: sub1 sub1.t1 0-1: note generator not found"
        , "top top.t1 2-3: sub2 \\* \\*: rederived"
        , "top top.t1 2-3: sub2 sub2.t1 \\*: rederived"
        ]
    strings_like (r_pretty_logs cached)
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
    let e_stats = Derive.collect_cache_stats . Derive.state_collect
            . Derive.r_state

    let (_, cached, uncached) = compare_cached create $
            insert_event "sub2.t1" 1 1 ""
    equal (e_stats uncached) mempty
    equal (e_stats cached) $ mempty
        { Derive.cstats_hits = [(Left (UiTest.bid "sub1"), (0, 2))] }

    let (_, cached, _) = compare_cached create $ do
            insert_event "sub1.t1" 1 1 ""
            insert_event "sub2.t1" 1 1 ""
    equal (e_stats cached) $ mempty
        { Derive.cstats_hits = [(Right (UiTest.tid "sub1.t2"), (0, 4))] }

test_extend_control_damage :: Test
test_extend_control_damage = do
    let f = Cache._extend_control_damage
    equal (f 3 (Events.from_list [Event.event 0 0 "4c"])
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
    let (_, cached, uncached) = compare_cached create $
            insert_event "top.t2" 2 0 "4d"
    equal (diff_events cached uncached) []

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
    let (_, cached, uncached) = compare_cached create $
            insert_event "top.t1" 0 1 "t-dia = 1 | sub"
    equal (diff_events cached uncached) []

test_failed_sub_track :: Test
test_failed_sub_track = do
    let create title = mkblocks
            [ ("top", [(">i", [(0, 1, ""), (1, 1, "sub")])])
            , ("sub=ruler", [(title, [(0, 1, "")])])
            ]
        tid = (UiTest.mk_tid_block (UiTest.bid "sub") 1)
    -- From error to non-error.
    let (orig, cached, uncached) = compare_cached (create "> |") $
            Ui.set_track_title tid ">"
    equal (diff_events cached uncached) []
    -- top has a dep on sub even though sub crashed.
    equal (r_cache_deps orig) $ map (second (Just . map UiTest.bid))
        [ ("top * *", ["sub", "top"])
        , ("top top.t1 *", ["sub"])
        , ("top top.t1 1-2: sub * *", ["sub"])
        ]

    -- From non-error to error.
    let (orig, cached, uncached) = compare_cached (create ">") $
            Ui.set_track_title tid "> |"
    equal (diff_events cached uncached) []
    equal (r_cache_deps orig) $ map (second (Just . map UiTest.bid))
        [ ("top * *", ["sub", "top"])
        , ("top top.t1 *", ["sub"])
        , ("top top.t1 1-2: sub * *", ["sub"])
        , ("top top.t1 1-2: sub sub.t1 *", [])
        ]
    strings_like (r_pretty_logs cached)
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
    let (_, cached, uncached) = compare_cached create $
            insert_event "top.t1" 1 1 "sub2"
    equal (diff_events cached uncached) []
    strings_like (r_block_logs cached)
        [ toplevel_rederived True
        , "top.t1 0-1: * using cache"
        , "top.t1 1-2: * rederived * not in cache"
        , "top.t1 2-3: * using cache"
        ]

test_callee_damage :: Test
test_callee_damage = do
    -- test callee damage: sub-block is damaged, it should be rederived
    let parent_sub = mkblocks
            [ ("top", [(">i", [(0, 1, ""), (1, 2, "sub")])])
            , ("sub", [(">i", [(0, 1, ""), (1, 1, "subsub")])])
            , ("subsub", [(">i", [(0, 1, "")])])
            ]
    let (_, cached, uncached) = compare_cached parent_sub $
            insert_event "sub.t1" 0 0.5 ""
    equal (diff_events cached uncached) []
    strings_like (r_block_logs cached)
        [ toplevel_rederived False
        , "top.t1 1-3: * rederived * because of track block"
        , "sub.t1 1-2: * using cache"
        ]

    -- The cached call to "sub" depends on "sub" and "subsub" transitively.
    equal (r_cache_deps cached) $ map (second (Just . map UiTest.bid))
        [ ("top * *", ["sub", "subsub", "top"])
        , ("top top.t1 *", ["sub", "subsub"])
        , ("top top.t1 1-3: sub * *", ["sub", "subsub"])

        , ("top top.t1 1-3: sub sub.t1 *", ["subsub"])
        , ("top top.t1 1-3: sub sub.t1 1-2: subsub * *", ["subsub"])
        , ("top top.t1 1-3: sub sub.t1 1-2: subsub subsub.t1 *", [])
        ]
    -- sub is 1-3, its first elt should be 1-2, except I replaced it with .5

    -- A change to subsub means the top's "sub" call rederives as well.
    let (_, cached, uncached) = compare_cached parent_sub $
            insert_event "subsub.t1" 0 0.5 ""
    equal (diff_events cached uncached) []
    strings_like (r_block_logs cached)
        [ toplevel_rederived False
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
            <* Ui.set_render_style (Track.Line Nothing) (UiTest.tid "sub.t2")
    let (_, cached, _) = compare_cached create $ insert_event "top.t1" 1 1 ""
    let (root_key, maybe_collect) : _ = r_cache_collect cached
        Just collect = maybe_collect
    equal root_key "top * *"

    let e_warp_maps = Lists.sortOn fst . map (bimap Stack.pretty_ui_ e_track)
            . Map.toAscList . Derive.collect_warp_map
        e_track (TrackWarp.Track start end _warp block_id track_id) =
            (start, end, block_id, track_id)

    equal (e_warp_maps collect)
        [ ("top * *", (0, 2, UiTest.bid "top", Nothing))
        , ("top top.t1 0-1 / sub * *", (0, 1, UiTest.bid "sub", Nothing))
        ]

    -- TrackSignals are only collected for the topmost block.
    equal (Derive.collect_track_signals collect) mempty
    equal (Derive.collect_block_deps collect)
        (mk_block_deps ["top", "sub"])

test_block_deps :: Test
test_block_deps = do
    let run root blocks =
            extract $ Derive.collect_block_deps $ Derive.state_collect $
            Derive.r_state $
            DeriveTest.derive_block_setup mempty
                (snd (UiTest.run_mkblocks blocks)) (UiTest.bid root)
        extract (Derive.BlockDeps deps) = deps
    let blocks =
            [ ("top", [(">", [(0, 1, "b1")])])
            , ("b1", [(">", [(0, 1, "b2")])])
            , ("b2", UiTest.regular_notes 2)
            , ("b3", [(">", [(0, 1, "b2")])])
            ]
    equal (run "top" blocks)
        (Set.fromList $ map UiTest.bid ["top", "b1", "b2"])
    equal (run "b3" blocks)
        (Set.fromList $ map UiTest.bid ["b3", "b2"])

test_collect_indirect_call :: Test
test_collect_indirect_call = do
    let run note = DeriveTest.derive_blocks
            [ ("top -- ^t = \"(sub)", [(">", [(0, 1, note)])])
            , ("sub=ruler", UiTest.note_track1 ["4c"])
            ]
    equal (DeriveTest.extract DeriveTest.e_note (run "sub"))
        ([(0, 1, "4c")], [])
    equal (DeriveTest.extract DeriveTest.e_note (run "t"))
        ([(0, 1, "4c")], [])

    let top = UiTest.bid "top"
        sub = UiTest.bid "sub"
    let e_collection = map TrackWarp.tw_block . Derive.r_track_warps
    equal (e_collection (run "sub")) [sub, top]
    equal (e_collection (run "t")) [sub, top]

    let e_block_deps =
            map (fmap (fmap (e_block_dep . Derive.collect_block_deps)))
            . r_cache_collect
        e_block_dep (Derive.BlockDeps blocks) = Set.toList blocks
    let deps =
            [ ("top * *", Just [sub, top])
            , ("top top.t1 *", Just [sub])
            , ("top top.t1 0-1: sub * *", Just [sub])
            , ("top top.t1 0-1: sub sub.t1 *", Just [])
            ]
    equal (e_block_deps (run "sub")) deps
    equal (e_block_deps (run "t")) deps

test_sliced_score_damage :: Test
test_sliced_score_damage = do
    -- Ensure that a cached call underneath a slice still works correctly.
    -- This is tricky because the cache relies on Stack.Region entries
    -- accurately reflecting the position of the call on the track for
    -- ScoreDamage to invalidate it.
    let create = do
            UiTest.mkblocks_skel blocks
            return $ UiTest.bid "b9"
    let (_prev, cached, uncached) = compare_cached create $
            insert_event "b9.t3" 4 0 "7c"
    equal (diff_events cached uncached) []
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
            UiTest.mkblocks_skel blocks
            return (UiTest.bid "top")
    let (_prev, cached, uncached) = compare_cached create $
            insert_event "top.t1" 6 0 "0"
    equal (diff_events cached uncached) []
    strings_like (r_block_logs cached)
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
    let (_, cached, uncached) = compare_cached create $
            insert_event "top.t1" 2 0 "0"
    equal (diff_events cached uncached) []
    strings_like (r_cache_logs cached)
        [ toplevel_rederived True
        , "top.t1 \\*: rederived"
        , "top.t2 \\*: rederived"
        , "top.t2 0-1: * using cache"
        , "top.t2 1-2: * using cache"
        ]

    -- only the  affected event is rederived
    let (_, cached, uncached) = compare_cached create $
            insert_event "top.t1" 1 0 ".5"
    equal (diff_events cached uncached) []
    strings_like (r_cache_logs cached)
        [ toplevel_rederived True
        , "top.t1 \\*: rederived"
        , "top.t2 \\*: * control damage"
        , "top.t2 0-1: sub \\* \\*: using cache"
        , "top.t2 1-2: sub \\* \\*: * control damage"
        , "top.t2 1-2: sub sub.t1 \\*: * control damage"
        ]

    -- if the change damages a greater control area, it should affect the
    -- event
    let (_, cached, uncached) = compare_cached create $
            insert_event "top.t1" 0 0 ".5"
    equal (diff_events cached uncached) []
    strings_like (r_cache_logs cached)
        [ toplevel_rederived True
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
    let (_, cached, uncached) = compare_cached create $
            insert_event "top.t2" 0 0 "0.5"
    equal (diff_events cached uncached) []

test_get_control_damage :: Test
test_get_control_damage = do
    let f events cdmg sdmg = run events $
            get_control_damage (0, 10) (mkcdamage cdmg) (mksdamage sdmg)
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
    get_control_damage range cdmg sdmg = do
        Derive.modify $ \st -> st
            { Derive.state_constant = (Derive.state_constant st)
                { Derive.state_score_damage = sdmg }
            , Derive.state_dynamic = (Derive.state_dynamic st)
                { Derive.state_control_damage = cdmg }
            }
        Cache.get_control_damage UiTest.default_block_id (UiTest.mk_tid 1) range
    extract = DeriveTest.extract_run $
        \(Derive.ControlDamage r) -> Ranges.extract r
    run events d = extract $ DeriveTest.run
        (snd (UiTest.run_mkblock [("cont", events)])) d
    mkcdamage = Derive.ControlDamage . Ranges.ranges
    mksdamage ranges = Derive.ScoreDamage
        (Map.singleton (UiTest.mk_tid 1) (Ranges.ranges ranges))
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
    let (_, cached, uncached) = compare_cached create $
            insert_event "top.t2" 1 0 ".5"
    equal (diff_events cached uncached) []
    strings_like (r_block_logs cached)
        [ toplevel_rederived True
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
            , ("b2=ruler", UiTest.note_track [(0, 1, "4a"), (1, 1, "4b")])
            , ("b3=ruler", UiTest.note_track [(0, 1, "4c"), (1, 1, "4d")])
            ]
    let (_, cached, uncached) = compare_cached create $
            insert_event "top.t1" 0 0 "2"
    equal (diff_events cached uncached) []
    -- Everything has control damage.
    strings_like (r_block_logs cached)
        [ "top"
        , "0-2: b1 * control", "0-2: b2 * control", "2-4: b3 * control"
        , "2-4: b1 * control", "0-2: b2 * control", "2-4: b3 * control"
        ]

    -- Only the second b1 is rederived.
    let (_, cached, uncached) = compare_cached create $
            insert_event "top.t1" 3 0 "2"
    equal (diff_events cached uncached) []

    strings_like (r_block_logs cached)
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
    let (_, cached, uncached) = compare_cached create $
            insert_event "top.t1" 2 0 "2"
    equal (diff_events cached uncached) []
    -- Damage extends to the previous event, so the first is cached, second and
    -- third are not.
    strings_like (r_block_logs cached)
        [ toplevel_rederived True
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
    let (_, cached, uncached) = compare_cached create $
            insert_event "top.t1" 2 0 "i 2"
    equal (diff_events cached uncached) []
    -- prettyp (DeriveTest.extract DeriveTest.e_note cached)

test_extend_tempo_damage :: Test
test_extend_tempo_damage = do
    -- Make sure control damage emitted by 'get_tempo_damage' is reasonable.
    let create = mkblock
            [ ("tempo", [(0, 0, "1")])
            , (">i", [(0, 1, ""), (1, 1, "")])
            , ("modulation", [(0, 1, "0")])
            ]
    let (_, cached, uncached) = compare_cached create $
            Ui.insert_event (top_tid 1) (Event.event 1 0 "2")
    equal (diff_events cached uncached) []

test_block_title_damage :: Test
test_block_title_damage = do
    -- Changing tempo in the block title invalidates the caches.
    let create =
            mkblocks
                [ ("top", [(">", [(0, 2, "sub")])])
                , ("sub=ruler", UiTest.regular_notes 2)
                ]
            <* Ui.set_block_title (UiTest.bid "top") "tempo = 2"
    let (_, cached, uncached) = compare_cached create $
            Ui.set_block_title (UiTest.bid "top") "tempo = 1"
    equal (diff_events cached uncached) []

test_track_cache :: Test
test_track_cache = do
    let create = mkblock
            [ ("dyn", [(0, 0, ".5"), (1, 0, "1")])
            , (">", [(0, 2, "")])
            ]
    -- Ensure that a control track above a note track is cached.
    let (_, cached, uncached) = compare_cached create $
            Ui.insert_event (top_tid 2) $ Event.event 1 1 ""
    equal (diff_events cached uncached) []
    strings_like (r_cache_logs cached)
        [ "top \\* \\*: rederived"
        , "top top.t1 \\*: using cache"
        , "top top.t2 \\*: rederived"
        ]

    -- And invalidated on damage.
    let (_, cached, uncached) = compare_cached create $
            Ui.insert_event (top_tid 1) $ Event.event 0 0 ".75"
    equal (diff_events cached uncached) []
    strings_like (r_cache_logs cached)
        [ "top \\* \\*: rederived"
        , "top top.t1 \\*: rederived * cache invalidated"
        , "top top.t2 \\*: rederived"
        ]

    let title = Ui.set_block_title (UiTest.bid "top")

    -- Also invalidated on block damage.
    let (_, cached, _) = compare_cached (create <* title "foo = a") $
            title "foo = b"
    equal (DeriveTest.extract (DeriveTest.e_environ "foo") cached)
        ([Just "b"], [])
    strings_like (r_cache_logs cached)
        [ "top \\* \\*: rederived"
        , "top top.t1 \\*: rederived"
        , "top top.t2 \\*: rederived"
        ]

    -- Make sure I don't get extra cache entries when the stack changes.
    let (_, cached, _) = compare_cached create $ title "foo = a"
    equal (DeriveTest.extract (DeriveTest.e_environ "foo") cached)
        ([Just "a"], [])
    equal (r_cache_stacks cached)
        ["top * *", "top top.t1 *", "top top.t2 *"]

test_track_cache2 :: Test
test_track_cache2 = do
    let create = mkblock
            [ ("tempo", [(0, 0, "1")])
            , ("dyn", [(0, 0, ".5")])
            , (">", [(0, 1, "")])
            ]
    -- Two levels of track caching.
    let (_, cached, uncached) = compare_cached create $
            Ui.insert_event (top_tid 2) $ Event.event 0 0 "1"
    equal (diff_events cached uncached) []


-- * make

block :: Text -> Stack.Frame
block = Stack.Block . UiTest.bid

track :: Text -> Stack.Frame
track = Stack.Track . UiTest.tid

region :: TrackTime -> TrackTime -> Stack.Frame
region = Stack.Region

call :: Text -> Stack.Frame
call = Stack.Call

top_tid :: TrackNum -> TrackId
top_tid = UiTest.mk_tid_name "top"

-- | The toplevel block is just about always damaged.
toplevel_rederived :: Bool -> Text
toplevel_rederived True = "top *: rederived * block damage"
toplevel_rederived False = "top *: rederived * sub-block damage"

mk_block_deps :: [Text] -> Derive.BlockDeps
mk_block_deps = Derive.BlockDeps . Set.fromList . map UiTest.bid

mkblocks :: Ui.M m => [UiTest.BlockSpec] -> m BlockId
mkblocks blocks = head <$> UiTest.mkblocks blocks

mkblock :: Ui.M m => [UiTest.TrackSpec] -> m BlockId
mkblock tracks = mkblocks [("top", tracks)]

insert_event :: Ui.M m => Text -> ScoreTime -> ScoreTime -> Text -> m ()
insert_event tid pos dur text =
    Ui.insert_event (UiTest.tid tid) (Event.event pos dur text)

-- * extract

-- | Extract cache logs so I can tell who rederived and who used the cache.
-- I use strings instead of parsing it into structured data because strings
-- make more informative errors when they don't match.
r_cache_logs :: Derive.Result -> [Text]
r_cache_logs = map show_log . filter Cache.is_cache_log . r_logs

-- | Sometimes the cache msgs for the track cache are just clutter.
r_block_logs :: Derive.Result -> [Text]
r_block_logs =
    map show_log . filter (not . track_stack)
        . filter Cache.is_cache_log . r_logs
    where
    track_stack msg = case Log.msg_stack msg of
        Just stack -> case Stack.innermost stack of
            Stack.Track _ : _ -> True
            _ -> False
        _ -> False

r_pretty_logs :: Derive.Result -> [Text]
r_pretty_logs = map show_log . r_logs

-- | The logs are sorted for tests, since log order isn't really defined.
r_logs :: Derive.Result -> [Log.Msg]
r_logs = Lists.sortOn show_log . Stream.logs_of . Derive.r_events

show_log :: Log.Msg -> Text
show_log msg =
    DeriveTest.show_stack_ False (Log.msg_stack msg) <> ": " <> Log.msg_text msg

-- | Pull the collects out of the cache, pairing them up with the cache keys.
r_cache_collect :: Derive.Result -> [(Text, Maybe Derive.Collect)]
r_cache_collect result = Lists.sortOn fst
    [ (DeriveTest.show_stack_ False (Just (Derive.key_stack key)),
        collect ctype)
    | (key, ctype) <- Map.toList cmap
    ]
    where
    cmap = uncache (Derive.r_cache result)
    collect (Derive.Cached (Derive.CachedEvents (Derive.CallType collect _))) =
        Just collect
    collect _ = Nothing

r_cache_deps :: Derive.Result -> [(Text, Maybe [BlockId])]
r_cache_deps result =
    [(stack, fmap deps collect) | (stack, collect) <- r_cache_collect result]
    where
    deps collect = case Derive.collect_block_deps collect of
        Derive.BlockDeps blocks -> Set.toAscList blocks

r_cache_stacks :: Derive.Result -> [Text]
r_cache_stacks = map (Stack.pretty_ui_ . Derive.key_stack) . Map.keys
    . Map.filter valid . uncache . Derive.r_cache
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
compare_cached :: Ui.StateId BlockId -> Ui.StateId a
    -> (Derive.Result, Derive.Result, Derive.Result)
compare_cached = compare_cached_block Nothing

compare_cached_block :: Maybe BlockId -> Ui.StateId BlockId
    -> Ui.StateId a -> (Derive.Result, Derive.Result, Derive.Result)
compare_cached_block maybe_root_id create modify = (result, cached, uncached)
    where
    state1 = UiTest.exec Ui.empty create
    root_id = fromMaybe (get_root_id state1) maybe_root_id
    result = derive_block_cache mempty mempty state1 root_id
    cached = run_cached root_id result state1 modify
    -- 'run_cached' already did this once, but it doesn't return it.
    (_, state2, _) = run state1 modify
    uncached = derive_block_cache mempty mempty state2 root_id

-- | Run a derive after some modifications with the cache from a previous
-- derive.
run_cached :: BlockId -> Derive.Result -> Ui.State -> Ui.StateId a
    -> Derive.Result
run_cached root_id result state1 modify =
    derive_block_cache (Derive.r_cache result) damage state2 root_id
    where
    (_, state2, ui_damage) = run state1 modify
    (updates, _) = Diff.diff ui_damage state1 state2
    damage = Diff.derive_diff state1 state2 ui_damage updates

derive_block_cache :: Derive.Cache -> Derive.ScoreDamage -> Ui.State
    -> BlockId -> Derive.Result
derive_block_cache =
    DeriveTest.derive_block_standard mempty DeriveTest.default_cmd_state

get_root_id :: Ui.State -> BlockId
get_root_id state = UiTest.eval state Ui.get_root_id

score_damage :: Ui.StateId a -> Ui.StateId b -> Derive.ScoreDamage
score_damage create modify = Diff.derive_diff state1 state2 ui_damage updates
    where
    (_, state1) = UiTest.run Ui.empty create
    (_, state2, ui_damage) = run state1 modify
    (updates, _) = Diff.diff ui_damage state1 state2

-- | 'diff_events' returns this when there were no events to diff.
expect_no_events :: [Either DiffEvent DiffEvent]
expect_no_events = [Left (0, 0, "NO EVENTS TO DIFF", 0)]

diff_events :: Derive.Result -> Derive.Result -> [Either DiffEvent DiffEvent]
diff_events r1 r2
    -- Without this check it's easy for a test to pass if the derive just
    -- crashes and produces no events.
    -- This is a hack, but otherwise I have to remember to stick a "check not
    -- null" line on every test.  In practice, I don't expect any test to
    -- produce zero events.
    | null x1 && null x2 = expect_no_events
    | otherwise = Lists.diffEither (==) x1 x2
    where
    x1 = extract r1
    x2 = extract r2
    extract = fst . DeriveTest.extract simple_event

-- | (start, dur, pitch, initial_dyn)
type DiffEvent = (RealTime, RealTime, Text, Double)

simple_event :: Score.Event -> DiffEvent
simple_event e = (Score.event_start e, Score.event_duration e,
    DeriveTest.e_pitch e, Score.initial_dynamic e)
