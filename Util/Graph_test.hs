module Util.Graph_test where
import qualified Data.Array.IArray as IArray
import qualified Data.List as List
import qualified Util.Graph
import Util.Graph (build)
import Util.Test


test_toggle_edge = do
    let f = Util.Graph.toggle_edge
        eq mg1 mg2 = case (mg1, mg2) of
            (Just g1, Just g2) -> graph_equal g1 g2
            (_, _) -> equal mg1 mg2
    eq (f (0, 1) (build [])) (Just (build [(0, 1)]))
    eq (f (0, 1) (build [(0, 1)])) (Just (build []))
    eq (f (0, 1) (build [(1, 0)])) Nothing
    eq (f (1, 1) (build [])) Nothing

test_splice_above = do
    let f = Util.Graph.splice_above

    graph_equal (f 0 1 (build [(1, 2), (1, 3)]))
        (build [(0, 1), (1, 2), (1, 3)])
    graph_equal (f 2 1 (build [(0, 1)])) (build [(0, 2), (2, 1)])
    graph_equal (f 3 2 (build [(0, 2), (2, 1)]))
        (build [(0, 3), (3, 2), (2, 1)])
    graph_equal (f 5 2 xgraph)
        (build [(0, 5), (4, 5), (5, 2), (2, 3), (2, 1)])

    -- finally a place I could use quickcheck...
    let idempotent new p graph =
            graph_equal (f new p (f new p graph)) (f new p graph)
    idempotent 5 2 xgraph
    idempotent 1 2 (build [(0, 1), (1, 3), (0, 2), (2, 4)])
    idempotent 3 2 (build [(0, 1), (1, 3), (0, 2), (2, 4)])

test_splice_below = do
    let f = Util.Graph.splice_below
    graph_equal (f 1 0 (build [])) (build [(0, 1)])
    graph_equal (f 2 1 (build [(0, 1), (1, 3), (1, 4)]))
        (build [(0, 1), (1, 2), (2, 3), (2, 4)])
    graph_equal (f 1 2 (build [(2, 0), (2, 3)]))
        (build [(2, 1), (1, 0), (1, 3)])
    graph_equal (Util.Graph.splice_above 1 2 (build [(2, 0), (2, 3)]))
        (build [(1, 2), (2, 0), (2, 3)])

test_add_edges = do
    let f = Util.Graph.add_edges
    graph_equal (f [(0, 1)] (build [])) (build [(0, 1)])
    graph_equal (f [(1, 0)] (build [(0, 1)])) (build [(0, 1), (1, 0)])
    graph_equal (f [(0, 1), (1, 0)] (build [])) (build [(0, 1), (1, 0)])

test_remove_edges = do
    let f = Util.Graph.remove_edges
    graph_equal (f [(0, 1)] (build [])) (build [])
    graph_equal (f [(0, 1)] (build [(0, 2)])) (build [(0, 2)])
    graph_equal (f [(0, 1)] (build [(0, 1)])) (build [])
    graph_equal (f [(0, 1)] (build [(0, 1), (0, 2)])) (build [(0, 2)])
    graph_equal (f [(0, 1), (0, 2)] (build [(0, 1), (0, 2)])) (build [])

test_map_vertices = do
    let f = Util.Graph.map_vertices
    equal (f (\v -> max 0 (v-1)) (build [(0, 1), (1, 2), (2, 3)]))
        (build [(0, 1), (1, 2)])
    equal (f (+1) (build [(0, 1), (1, 2), (2, 3)]))
        (build [(1, 2), (2, 3), (3, 4)])

    equal (f (\v -> max 0 (v-1)) (build [(0, 2), (4, 2), (2, 1), (2, 3)]))
        (build [(3, 1), (1, 0), (1, 2)])
    let decr v = if v > 2 then v-1 else v
    let xg = build [(0, 1), (0, 3), (4, 1), (4, 3)]
    equal (f decr xg)
        (build [(0, 1), (0, 2), (3, 1), (3, 2)])

test_insert_vertex = do
    let f = Util.Graph.insert_vertex
        g = build [(0, 1), (1, 2)]
    graph_equal (f 0 g) (build [(1, 2), (2, 3)])
    graph_equal (f 1 g) (build [(0, 2), (2, 3)])
    graph_equal (f 3 g) (build [(0, 1), (1, 2)])

-- 0 4
--  2
-- 1 3
xgraph = build [(0, 2), (4, 2), (2, 1), (2, 3)]

test_unlink_vertex = do
    let f = Util.Graph.unlink_vertex
    graph_equal (f 0 xgraph) (build [(4, 2), (2, 1), (2, 3)])
    graph_equal (f 1 xgraph) (build [(0, 2), (4, 2), (2, 3)])
    graph_equal (f 2 xgraph) (build [(0, 1), (0, 3), (4, 1), (4, 3)])
    throws (f (-1) xgraph) "out of range"

test_remove_vertex = do
    let f = Util.Graph.remove_vertex
    -- x 3
    --  1
    -- 0 2
    graph_equal (f 0 xgraph) (build [(3, 1), (1, 0), (1, 2)])
    -- 0 4  0 3
    --  x    x
    -- 1 3  1 2
    graph_equal (f 2 xgraph) (build [(0, 1), (0, 2), (3, 1), (3, 2)])

test_move = do
    let f = Util.Graph.move
        g = build [(0, 1), (1, 2)]

    equal (f 5 0 g) Nothing
    equal (f 0 5 g) Nothing
    -- array [(0,[1]),(1,[2]),(2,[])]       0 -> 1 -> 2
    --      2 0
    -- array [(0,[]), (1,[0]),(2,[1])       2 -> 1 -> 0
    equal (f 2 0 g) (Just (build [(2, 1), (1, 0)]))
    --      2 1
    -- array [(0,[2]),(1,[]), (2,[1])       0 -> 2 -> 1
    equal (f 2 1 g) (Just (build [(0, 2), (2, 1)]))

graph_equal graph1 graph2
    | norm graph1 == norm graph2 = success $ "graph == " ++ show graph1
    | otherwise = failure $ "graph " ++ show graph1 ++ ":\n"
        ++ Util.Graph.draw graph1
        ++ "*** /= " ++ show graph2 ++ " ***\n" ++ Util.Graph.draw graph2
    where
    norm = map (\(p, cs) -> (p, List.sort cs)) . filter (not . null . snd)
        . IArray.assocs
