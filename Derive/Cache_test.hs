module Derive.Cache_test where
import Control.Monad
import qualified Control.Monad.Identity as Identity
import qualified Data.Map as Map
import qualified Data.Set as Set

import Util.Control
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
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
import qualified Derive.TrackWarp as TrackWarp

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

    let create = UiTest.mkstate "b1" [(">", [(0, 1, ""), (1, 1, "")])]
        modify = State.remove_event (UiTest.mk_tid_name "b1" 0) 0
    equal (score_damage create (return ())) (mk [] [] [])
    equal (score_damage create modify)
        (mk [(UiTest.mk_tid_name "b1" 0, Ranges.ranges [(0, 1)])]
            [UiTest.bid "b1"] [])

test_clear_damage = do
    let mkdamage tracks blocks = Derive.ScoreDamage
            (Map.fromList tracks) Set.empty (Set.fromList blocks)
        empty = Derive.CachedEvents (Derive.CachedGenerator mempty [])
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
    equal (diff_events cached uncached) []

    -- should not have rederived sub
    strings_like (r_cache_logs cached) ["using cache"]
    -- make sure there's stuff in the cache now
    check (not (null (r_cache_stacks cached)))

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
    equal (diff_events cached uncached) []

    let (_, cached, uncached) = compare_cached create $
            State.insert_event (UiTest.tid "top.t1") 4 (Event.event "" 1)
    equal (diff_events cached uncached) []

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
    equal (diff_events cached uncached) []

    -- Plain old event modification works too.
    let (_, cached, uncached) = compare_cached create $
            State.insert_event (UiTest.tid "sub.t0") 0 (Event.event "" 0.5)
    equal (diff_events cached uncached) []

r_logs :: Derive.Result -> [String]
r_logs = map DeriveTest.show_log_stack . snd . LEvent.partition
    . Derive.r_events

-- TODO test fail track parse, should damage track
-- TODO test localdeps... how do they get reset anyway?

test_failed_sub_track = do
    let create = mkblocks
            [ ("top", [(">i", [(0, 2, "sub"), (5, 1, "sub")])])
            , ("sub", [(">i", [(0, 2, "")]), ("*twelve", [])])
            ]
    let (_, cached, uncached) = compare_cached create $
            State.set_track_title (UiTest.tid "sub.t1") "*broken"
    -- TODO shouldn't I be checking this?
    pprint (r_logs cached)
    equal (diff_events cached uncached) []

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
    equal (diff_events cached uncached) []
    strings_like (r_cache_logs cached)
        [ "top.t0 0-1: * using cache"
        , "top.t0 1-2: * rederived * not in cache"
        , "top.t0 2-3: * using cache"
        , toplevel_rederived
        ]

test_callee_damage = do
    -- test callee damage: sub-block is damaged, it should be rederived
    let (_, cached, uncached) = compare_cached parent_sub $
            State.insert_event (UiTest.tid "sub.t0") 0 (Event.event "" 0.5)
    equal (diff_events cached uncached) []
    strings_like (r_cache_logs cached)
        [ "top.t0 1-3: * rederived * because of sub-block"
        , "sub.t0 1-2: * using cache"
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

    -- A change to subsub means the top's "sub" call rederives as well.
    let (_, cached, uncached) = compare_cached parent_sub $
            State.insert_event (UiTest.tid "subsub.t0") 0 (Event.event "" 0.5)
    equal (diff_events cached uncached) []
    strings_like (r_cache_logs cached)
        [ "top.t0 1-3: * rederived * sub-block damage"
        , "sub.t0 1-2: * rederived * sub-block damage"
        , toplevel_rederived
        ]

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
    let extract = second (fmap extract_collect)
        extract_collect (Derive.Collect wmap tsigs _env ldep) =
            (Seq.sort_on fst (map (first Stack.show_ui) (Map.toAscList wmap)),
                tsigs, ldep)

    -- Wow, this is a hassle, but it's hard to figure out how to verify this
    -- otherwise.
    let tw start end bid = Left $ TrackWarp.TrackWarp
            (start, end, Score.id_warp, UiTest.bid bid, Nothing)
        track tid = Right (UiTest.tid tid)
    equal (extract root) ("test/top * *", Just $
        ( [ ("test/top * *", tw 0 2 "top")
          , ("test/top test/top.t0 *", track "top.t0")
          , ("test/top test/top.t0 0-1: test/sub * *", tw 0 1 "sub")
          , ("test/top test/top.t0 0-1: test/sub test/sub.t0 *", track "sub.t0")
          , ("test/top test/top.t0 0-1: test/sub test/sub.t1 *", track "sub.t1")
          ]
        , Map.fromList [(UiTest.tid "sub.t1", tsig)]
        , mk_gdep ["top", "sub"]
        ))

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
    equal (diff_events cached uncached) []
    strings_like (r_cache_logs cached)
        [ "top.t0 0-1: * using cache"
        , "top.t0 1-2: * using cache"
        , toplevel_rederived
        ]

    -- only the  affected event is rederived
    let (_, cached, uncached) = compare_cached create $
            State.insert_event (UiTest.tid "top.t1") 1 (Event.event ".5" 0)
    equal (diff_events cached uncached) []
    strings_like (r_cache_logs cached)
        [ "top.t0 0-1: * using cache"
        , "top.t0 1-2: * rederived * control damage"
        , toplevel_rederived
        ]

    -- if the change damages a greater control area, it should affect the event
    let (_, cached, uncached) = compare_cached create $
            State.insert_event (UiTest.tid "top.t1") 0 (Event.event ".5" 0)
    equal (diff_events cached uncached) []
    strings_like (r_cache_logs cached)
        [ "top.t0 0-1: * rederived * control damage"
        , "top.t0 1-2: * rederived * control damage"
        , toplevel_rederived
        ]

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
    equal (diff_events cached uncached) []
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

-- | Extract cache logs so I can tell who rederived and who used the cache.
-- I use strings instead of parsing it into structured data because strings
-- make more informative errors when they don't match.
r_cache_logs :: Derive.Result -> [String]
r_cache_logs = map DeriveTest.show_log_stack . filter DeriveTest.cache_msg
    . snd . LEvent.partition . Derive.r_events

log_with_stack :: Log.Msg -> String
log_with_stack msg = Log.msg_string msg
    -- Pretty.pretty (Log.msg_stack msg) ++ ": " ++ Log.msg_string msg

r_cache_collect :: Derive.Result -> [(String, Maybe Derive.Collect)]
r_cache_collect result = Seq.sort_on fst
    [(DeriveTest.show_stack (Just stack), collect ctype)
        | (stack, ctype) <- Map.assocs cmap]
    where
    cmap = uncache (Derive.r_cache result)
    collect (Derive.CachedEvents (Derive.CachedGenerator collect _)) =
        Just collect
    collect _ = Nothing

r_cache_deps :: Derive.Result -> [(String, Maybe [BlockId])]
r_cache_deps result =
    [(stack, fmap deps collect) | (stack, collect) <- r_cache_collect result]
    where
    deps collect = case Derive.collect_local_dep collect of
        Derive.GeneratorDep blocks -> Set.toAscList blocks

mk_gdep :: [String] -> Derive.GeneratorDep
mk_gdep = Derive.GeneratorDep . Set.fromList . map UiTest.bid

r_cache_stacks = Map.keys . uncache . Derive.r_cache
uncache (Derive.Cache cache) = cache

mkblocks block_tracks = do
    forM_ block_tracks $ \(bid, tracks) ->
        UiTest.mkstate bid tracks
    return $ UiTest.bid (fst (head block_tracks))

-- | Derive with and without the cache, and make sure the cache fired and the
-- results are the same.  Returns (result before modification, cached,
-- uncached).  The pre-modification result is occaisionally useful to check
-- logs.
compare_cached :: State.StateId BlockId -> State.StateId a
    -> (Derive.Result, Derive.Result, Derive.Result)
compare_cached create modify = (result1, cached, uncached)
    where
    (bid, state1) = UiTest.run State.empty create
    result1 = DeriveTest.derive_block_cache mempty mempty state1 bid
    (_, state2, cmd_updates) = run state1 modify
    damage = Cache.score_damage state1 state2 updates
    cached = DeriveTest.derive_block_cache (Derive.r_cache result1)
        damage state2 bid
    uncached = DeriveTest.derive_block_cache mempty mempty state2 bid
    updates = case Diff.diff cmd_updates state1 state2 of
        Left err -> error ("diff error: " ++ err)
        Right diff_updates -> diff_updates

score_damage :: State.StateId a -> State.StateId b -> Derive.ScoreDamage
score_damage create modify = Cache.score_damage state1 state2 updates
    where
    (_, state1) = UiTest.run State.empty create
    (_, state2, cmd_updates) = run state1 modify
    updates = case Diff.diff cmd_updates state1 state2 of
        Left err -> error ("diff error: " ++ err)
        Right diff_updates -> diff_updates

diff_events :: Derive.Result -> Derive.Result
    -> [Either SimpleEvent SimpleEvent]
diff_events r1 r2 = Seq.diff (==) (extract r1) (extract r2)
    where extract = fst . DeriveTest.extract simple_event

type SimpleEvent = (RealTime, RealTime, String)
simple_event :: Score.Event -> SimpleEvent
simple_event e =
    (Score.event_start e, Score.event_duration e, Score.event_string e)

-- *

block = Stack.Block . UiTest.bid
track = Stack.Track . UiTest.tid
region = Stack.Region
call = Stack.Call
