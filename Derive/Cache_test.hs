module Derive.Cache_test where
import Control.Monad
import qualified Control.Monad.Identity as Identity
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set

import qualified Util.Log as Log
import qualified Util.Ranges as Ranges
import qualified Util.Seq as Seq
import Util.Test

import Ui
import qualified Ui.Block as Block
import qualified Ui.Diff as Diff
import qualified Ui.Event as Event
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.UiTest as UiTest
import qualified Ui.Update as Update

import qualified Derive.Cache as Cache
import qualified Derive.Call as Call
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack

import qualified Perform.Signal as Signal


-- * other functions

test_score_damage = do
    let bid = UiTest.default_block_id
        ([tid], ustate) = UiTest.run_mkstate [("t1", [])]
        mk tracks track_blocks blocks =
            Derive.ScoreDamage (Map.fromList tracks) (Set.fromList track_blocks)
                (Set.fromList blocks)
    let f = Cache.score_damage

    equal (f ustate ustate []) (mk [] [] [])
    equal (f ustate (snd $ UiTest.run_mkstate [("t2", [])]) [])
        (mk [(UiTest.tid "b1.t0", Ranges.everything)] [UiTest.bid "b1"] [])
    equal (f ustate ustate [Update.BlockUpdate bid (Update.BlockTitle "ho")])
        (mk [] [] [UiTest.bid "b1"])

    equal (f ustate ustate [Update.TrackUpdate tid Update.TrackAllEvents])
        (mk [(UiTest.tid "b1.t0", Ranges.everything)] [UiTest.bid "b1"] [])
    equal (f ustate ustate [Update.TrackUpdate tid Update.TrackBg])
        (mk [] [] [])

test_clear_damage = do
    let mkdamage tracks blocks = Derive.ScoreDamage
            (Map.fromList tracks) Set.empty (Set.fromList blocks)
        empty = Derive.CachedEvents (Derive.CachedGenerator Monoid.mempty [])
        mkcache stack = Derive.Cache $ Map.singleton stack empty
    let f damage stack = Map.keys $ uncache $
            Derive.clear_damage damage (mkcache stack)

    let stack = Stack.make [block "top", track "t", call "c", region 1 2]
    equal (f (mkdamage [] []) stack) [stack]
    equal (f (mkdamage [] [UiTest.bid "top"]) stack) []
    equal (f (mkdamage [(UiTest.tid "t", Ranges.range 2 3)] []) stack) [stack]
    equal (f (mkdamage [(UiTest.tid "t", Ranges.range 1 3)] []) stack) []
    equal (f (mkdamage [(UiTest.tid "top", Ranges.range 1 3)] []) stack) [stack]

-- * cached generators

-- Test interaction with the rest of the evaluation system.

test_no_damage = do
    let (_, cached, uncached) = compare_cached parent_sub (return ())
    equal (diff_events cached uncached) (Right [])
    strings_like (r_cache_logs cached) ["using cache"]
    -- make sure there's stuff in the cache now
    check (not (null (r_cache_stacks cached)))
    equal (r_event_damage cached) (Just [])

test_add_remove = do
    -- Make sure I get event damage from adding and removing event.
    let create = mkblocks
            [ ("top",
                [ ("tempo", [(0, 0, ".5")])
                , (">i", [(0, 1, ""), (1, 1, "")])
                ])
            ]
    let (_, cached, uncached) = compare_cached create $
            State.remove_event (UiTest.tid "top.t1") 1
    equal (diff_events cached uncached) (Right [])
    equal (r_event_damage cached) (Just [(2, 4)])

    let (_, cached, uncached) = compare_cached create $
            State.insert_event (UiTest.tid "top.t1") 4 (Event.event "" 1)
    equal (diff_events cached uncached) (Right [])
    equal (r_event_damage cached) (Just [(8, 10)])

test_block_damage = do
    let create = mkblocks
            [ ("top",
                [ ("tempo", [(0, 0, ".5")])
                , (">i", [(0, 1, "sub"), (1, 1, ""), (2, 1, "sub")])
                ])
            , ("sub", [(">", [(0, 1, "")])])
            ]
    -- A track mute should emit event damage for that block's range.
    let (_, cached, uncached) = compare_cached create $
            State.add_track_flag (UiTest.bid "sub") 1 Block.Mute
    equal (diff_events cached uncached) (Right [])
    equal (r_event_damage cached) (Just [(0, 2), (4, 6)])

    -- Plain old event modification works too.
    let (_, cached, uncached) = compare_cached create $
            State.insert_event (UiTest.tid "sub.t0") 0 (Event.event "" 0.5)
    equal (diff_events cached uncached) (Right [])
    equal (r_event_damage cached) (Just [(0, 2), (4, 6)])

-- These two tests were originally testing blocks that threw... but since
-- track title errors are now caught, they just test that changing the track
-- title emits the proper damage, which is mostly the same mechanism that
-- 'test_block_damage' tests.
test_failed_track = do
    let create = mkblocks
            [ ("top",
                [ ("tempo", [(0, 0, ".5")])
                , (">i", [(0, 2, "")])
                , ("*twelve", [])
                ])
            ]
    let (_, cached, uncached) = compare_cached create $
            State.set_track_title (UiTest.tid "top.t2") "*broken"
    equal (diff_events cached uncached) (Right [])
    equal (r_event_damage cached)
        (Just [(0, 4)])

test_failed_sub_track = do
    let create = mkblocks
            [ ("top", [(">i", [(0, 2, "sub"), (5, 1, "sub")])])
            , ("sub", [(">i", [(0, 2, "")]), ("*twelve", [])])
            ]
    let (_, cached, uncached) = compare_cached create $
            State.set_track_title (UiTest.tid "sub.t1") "*broken"
    equal (diff_events cached uncached) (Right [])
    equal (r_event_damage cached)
        (Just [(0, 2), (5, 6)])


test_has_score_damage = do
    let create = mkblocks
            [ ("top", [(">i", [(0, 1, "sub"), (1, 1, "sub"), (2, 1, "sub")])])
            , ("sub", [(">i", [(0, 1, "")])])
            , ("sub2", [(">i", [(0, 1, ""), (1, 1, "")])])
            ]

    -- Swap it out for an event of the same size, so the stack is the same,
    -- but it should rederive because of score damage.
    let (_, cached, uncached) = compare_cached create $
            State.insert_event (UiTest.tid "top.t0") 1 (Event.event "sub2" 1)
    equal (diff_events cached uncached) (Right [])
    strings_like (r_cache_logs cached)
        [ "top.t0 0-1: * using cache"
        , "top.t0 1-2: * rederived * not in cache"
        , "top.t0 2-3: * using cache"
        , toplevel_rederived
        ]
    equal (r_event_damage cached) (Just [(1, 2)])

test_callee_damage = do
    -- test callee damage: sub-block is damaged, it should be rederived
    let (_, cached, uncached) = compare_cached parent_sub $
            State.insert_event (UiTest.tid "sub.t0") 0 (Event.event "" 0.5)
    equal (diff_events cached uncached) (Right [])
    strings_like (r_cache_logs cached)
        [ "sub.t0 1-2: * using cache"
        , "top.t0 1-3: * rederived * because of sub-block"
        , toplevel_rederived
        ]
    -- The cached call to "sub" depends on "sub" and "subsub" transitively.
    equal (r_cache_deps cached)
        [ ("test/top * *",
            Just [UiTest.bid "sub", UiTest.bid "subsub", UiTest.bid "top"])
        , ("test/top test/top.t0 1-3: test/sub * *",
            Just [UiTest.bid "sub", UiTest.bid "subsub"])
        , ("test/top test/top.t0 1-3: test/sub test/sub.t0 1-2: "
                ++ "test/subsub * *",
            Just [UiTest.bid "subsub"])
        ]
    -- sub is 1-3, its first elt should be 1-2, except I replaced it with .5
    equal (r_event_damage cached) (Just [(1, 1.5)])

    -- A change to subsub means the top's "sub" call rederives as well.
    let (_, cached, uncached) = compare_cached parent_sub $
            State.insert_event (UiTest.tid "subsub.t0") 0 (Event.event "" 0.5)
    equal (diff_events cached uncached) (Right [])
    strings_like (r_cache_logs cached)
        [ "sub.t0 1-2: * rederived * sub-block damage"
        , "top.t0 1-3: * rederived * sub-block damage"
        , toplevel_rederived
        ]
    -- subsub is at realtime position 2-3
    equal (r_event_damage cached) (Just [(2, 3)])

parent_sub = mkblocks
    [ ("top", [(">i", [(0, 1, ""), (1, 2, "sub")])])
    , ("sub", [(">i", [(0, 1, ""), (1, 1, "subsub")])])
    , ("subsub", [(">i", [(0, 1, "")])])
    ]

test_collect = do
    let blocks = mkblocks
            [ ("top", [(">i", [(0, 1, "sub"), (1, 1, "sub")])])
            , ("sub",
                [ (">i", [(0, 1, "")])
                , ("cont", [(0, 0, "1")])
                ])
            ]
    let create = do
            bid <- blocks
            State.set_render_style Track.Line (UiTest.tid "sub.t1")
            return bid
    let (_, cached, _) = compare_cached create $
            State.insert_event (UiTest.tid "top.t0") 1 (Event.event "" 1)
    -- pprint (r_cache_collect cached)
    let [root, _parent] = r_cache_collect cached
    let tsig = Track.TrackSignal (Track.Control (Signal.signal [(0, 1)])) 0 1
    -- Wow, this is a hassle, but it's hard to figure out how to verify this
    -- otherwise.
    equal root ("test/top * *", Just $
        Derive.Collect
            [ mk_twarp 0 1 "sub" ["sub.t0", "sub.t1"]
            , mk_twarp 0 2 "top" ["top.t0"]
            ]
            (Map.fromList [(UiTest.tid "sub.t1", tsig)])
            (mk_gdep ["top", "sub"]))

-- If I modify a control in a certain place, say a pitch track, I don't want it
-- to derive the whole control.  Then it will damage the whole control and
-- force everything below it to rederive.  I want it to emit control damage
-- for only the bit of control signal that actually changed.
test_control_damage = do
    let create = mkblocks
            [ ("top",
                [ (">i", [(0, 1, "sub"), (1, 1, "sub")])
                , ("c1", [(0, 0, "1")])
                ])
            , ("sub", [(">i", [(0, 1, "")])])
            ]

    -- the modification is out of range, so the caches are reused
    let (_, cached, uncached) = compare_cached create $
            State.insert_event (UiTest.tid "top.t1") 2 (Event.event "0" 0)
    equal (diff_events cached uncached) (Right [])
    strings_like (r_cache_logs cached)
        [ "top.t0 0-1: * using cache"
        , "top.t0 1-2: * using cache"
        , toplevel_rederived
        ]
    equal (r_event_damage cached) (Just [(2, 2)])

    -- only the  affceted event is rederived
    let (_, cached, uncached) = compare_cached create $
            State.insert_event (UiTest.tid "top.t1") 1 (Event.event ".5" 0)
    equal (diff_events cached uncached) (Right [])
    strings_like (r_cache_logs cached)
        [ "top.t0 0-1: * using cache"
        , "top.t0 1-2: * rederived * control damage"
        , toplevel_rederived
        ]
    equal (r_event_damage cached) (Just [(1, 2)])

    -- if the change damages a greater control area, it should affect the event
    let (_, cached, uncached) = compare_cached create $
            State.insert_event (UiTest.tid "top.t1") 0 (Event.event ".5" 0)
    equal (diff_events cached uncached) (Right [])
    strings_like (r_cache_logs cached)
        [ "top.t0 0-1: * rederived * control damage"
        , "top.t0 1-2: * rederived * control damage"
        , toplevel_rederived
        ]
    equal (r_event_damage cached) (Just [(0, 2)])

-- | Extract cache logs so I can tell who rederived and who used the cache.
-- I use strings instead of parsing it into structured data because strings
-- make more informative errors when they don't match.
r_cache_logs = cache_logs . Derive.r_logs
cache_logs msgs = [show_msg_stack m | m <- msgs,
        any (`List.isInfixOf` Log.msg_string m) cache_msgs]
    where cache_msgs = ["rederived generator", "using cache"]

show_msg_stack m = simple_stack (maybe (Stack.make []) id (Log.msg_stack m))
        ++ ": " ++ Log.msg_string m
simple_stack stack
    | null ui = "<no stack>"
    | otherwise = Seq.join ": " (map Stack.unparse_ui_frame ui)
    where ui = Stack.to_ui stack

test_tempo_damage = do
    let create = mkblocks
            [ ("top",
                [ ("tempo", [(0, 0, "1")])
                , (">i", [(0, 1, "sub"), (1, 1, "sub"), (2, 1, "sub")])
                ])
            , ("sub",
                [ (">i", [(0, 1, "")]) ])
            ]
    let (_, cached, uncached) = compare_cached create $
            State.insert_event (UiTest.tid "top.t0") 1 (Event.event "2" 0)
    equal (diff_events cached uncached) (Right [])
    -- first is cached, second and third are not
    strings_like (r_cache_logs cached)
        [ "top.t1 0-1: * using cache"
        , "top.t1 1-2: * rederived"
        , "top.t1 2-3: * rederived"
        , toplevel_rederived
        ]

-- ** support

-- | The toplevel block is just about always damaged.
toplevel_rederived :: String
toplevel_rederived = "test/top *: rederived * sub-block damage"

-- UiTest.run discards the Updates, which I need.
run :: State.State -> State.StateId a -> (a, State.State, [Update.Update])
run state m = case result of
        Left err -> error $ "state error: " ++ show err
        Right (val, state', updates) -> (val, state', updates)
    where result = Identity.runIdentity (State.run state m)

type Result = Derive.Result [Score.Event]

r_event_damage r = Ranges.extract ranges
    where Derive.EventDamage ranges = Derive.r_event_damage r
r_logs = map log_with_stack . Derive.r_logs

log_with_stack :: Log.Msg -> String
log_with_stack msg = Log.msg_string msg
    -- Pretty.pretty (Log.msg_stack msg) ++ ": " ++ Log.msg_string msg

r_events = fmap (map simple_event) . Derive.r_result

r_cache_collect :: Derive.Result a -> [(String, Maybe Derive.Collect)]
r_cache_collect result =
    [(simple_stack stack, collect ctype) | (stack, ctype) <- Map.assocs cmap]
    where
    cmap = uncache (Derive.r_cache result)
    collect (Derive.CachedEvents (Derive.CachedGenerator collect _)) =
        Just collect
    collect _ = Nothing

r_cache_deps :: Derive.Result a -> [(String, Maybe [BlockId])]
r_cache_deps result =
    [(stack, fmap deps collect) | (stack, collect) <- r_cache_collect result]
    where
    deps collect = case Derive.collect_local_dep collect of
        Derive.GeneratorDep blocks -> Set.toAscList blocks

mk_gdep :: [String] -> Derive.GeneratorDep
mk_gdep = Derive.GeneratorDep . Set.fromList . map UiTest.bid

mk_twarp :: RealTime -> RealTime -> String -> [String] -> Derive.TrackWarp
mk_twarp start end block tracks = Derive.TrackWarp start end
    (UiTest.bid block) (map UiTest.tid tracks) Score.id_warp

r_cache_stacks = Map.keys . uncache . Derive.r_cache
uncache (Derive.Cache cache) = cache

type SimpleEvent = (RealTime, RealTime, String)
simple_event :: Score.Event -> SimpleEvent
simple_event e =
    (Score.event_start e, Score.event_duration e, Score.event_string e)

mkblocks block_tracks = do
    forM_ block_tracks $ \(bid, tracks) ->
        UiTest.mkstate bid tracks
    return $ UiTest.bid (fst (head block_tracks))

-- Derive with and without the cache, and make sure the cache fired and the
-- results are the same.  Returns (result before modification, cached,
-- uncached).  The pre-modification result is occaisionally useful to check
-- logs.
compare_cached :: State.StateId BlockId -> State.StateId a
    -> (Result, Result, Result)
compare_cached create modify = (result1, cached, uncached)
    where
    (bid, state1) = UiTest.run State.empty create
    result1 = derive_block Derive.empty_cache Monoid.mempty state1 bid
    (_, state2, ui_updates) = run state1 modify
    updates = ui_updates ++ diff_updates
    damage = Cache.score_damage state1 state2 updates
    cached = derive_block (Derive.r_cache result1) damage state2 bid
    uncached = derive_block Derive.empty_cache Monoid.mempty state2 bid
    diff_updates = case Diff.diff state1 state2 of
        Left err -> error ("diff error: " ++ err)
        Right diff_updates -> diff_updates

diff_events :: Result -> Result
    -> Either Derive.DeriveError [(Either SimpleEvent SimpleEvent)]
diff_events r1 r2 = do
    e1 <- Derive.r_result r1
    e2 <- Derive.r_result r2
    return $ Seq.diff (==) (map simple_event e1) (map simple_event e2)

-- * derive

derive_block :: Derive.Cache -> Derive.ScoreDamage -> State.State
    -> BlockId -> Derive.Result [Score.Event]
derive_block cache damage ui_state block_id =
    derive cache damage ui_state deriver
    where deriver = Call.eval_root_block block_id

derive :: Derive.Cache -> Derive.ScoreDamage -> State.State
    -> Derive.Deriver a -> Derive.Result a
derive cache damage ui_state deriver =
    Derive.derive constant cache damage DeriveTest.default_environ deriver
    where
    constant = DeriveTest.default_constant DeriveTest.default_call_map ui_state

-- *

block = Stack.Block . UiTest.bid
track = Stack.Track . UiTest.tid
region = Stack.Region
call = Stack.Call

test_lookup_prefix = do
    let no_cache = Derive.CachedEvents
            (Derive.CachedGenerator Monoid.mempty Monoid.mempty)
    let cache = Derive.Cache $ Map.fromList $
            map (\k -> (Stack.make k, no_cache)) $
                [ [block "A"]
                , [block "A", track "T"]
                , [block "B"]
                ]
    let f stack = map fst $ Cache.lookup_prefix stack cache
    equal (f (Stack.make [block "A"]))
        [[block "A"], [block "A", track "T"]]
    equal (f (Stack.make [block "nothing"])) []
