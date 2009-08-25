module Util.Graph_test where
import qualified Data.Array.IArray as IArray
import qualified Data.Graph as Graph
import qualified Util.Graph
import Util.Test


build edges =
    Graph.buildG (0, maximum (-1 : (map (\(x, y) -> max x y) edges))) edges

test_toggle_edge = do
    let f = Util.Graph.toggle_edge
        eq mg1 mg2 = case (mg1, mg2) of
            (Just g1, Just g2) -> graph_equal g1 g2
            (_, _) -> equal mg1 mg2
    eq (f (0, 1) (build [])) (Just (build [(0, 1)]))
    eq (f (0, 1) (build [(0, 1)])) (Just (build []))
    eq (f (0, 1) (build [(1, 0)])) Nothing

test_add_edge = do
    let f = Util.Graph.add_edge
    graph_equal (f (0, 1) (build [])) (build [(0, 1)])
    graph_equal (f (1, 0) (build [(0, 1)])) (build [(0, 1), (1, 0)])

test_remove_edge = do
    let f = Util.Graph.remove_edge
    graph_equal (f (0, 1) (build [])) (build [])
    graph_equal (f (0, 1) (build [(0, 2)])) (build [(0, 2)])
    graph_equal (f (0, 1) (build [(0, 1)])) (build [])
    graph_equal (f (0, 1) (build [(0, 1), (0, 2)])) (build [(0, 2)])

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
    throws "out of range" $ f (-1) xgraph

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

graph_equal graph1 graph2
    | e graph1 == e graph2 = success Nothing $ "graph == " ++ show graph1
    | otherwise = failure Nothing $ "graph " ++ show graph1 ++ ":\n"
        ++ Util.Graph.draw graph1
        ++ "*** /= " ++ show graph2 ++ " ***\n" ++ Util.Graph.draw graph2
    where
    e = filter (not . null . snd) . IArray.assocs
