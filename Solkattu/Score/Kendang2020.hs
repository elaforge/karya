module Solkattu.Score.Kendang2020 where
import           Prelude hiding ((.), repeat)

import           Solkattu.Dsl.Kendang


kendang_farans :: Korvai
kendang_farans = faran $ korvaiS adi $ map su $ concat
    [ map (make (k.t.k.p) (k.t.k.p . k.p)) -- (p.n.p.k) (p.n.p.k . t.k)
        [ k.p.k.t.p.k.a.k -- k.t.k.n.p.k.t.k
        , a.a.k.t.p.k.a.k -- o.o.k.n.p.k.t.k
        , a.a.t.t.p.k.a.k -- o.o.n.n.p.k.t.k
        , a.p.k.t.p.k.a.k -- o.t.k.n.p.k.t.k
        , o.__.o.t.p.k.a.k -- od.__.od.n.p.k.t.k
--         , o.d.o.n.p.k.t.k
--         , o.k.o.n.p.k.t.k
--         , o&t.k.o.n.p.k.t.k
--         , p.u.__.n.p.k.t.k
--         , o.u.__.n.p.k.t.k
--         ]
--     , map (make (o.u.__.k) (o.u.__.k . t.k))
--         [ o.u.__.k.k.o.o.k -- 11
--         , o.u.p.k.k.o.o.k
--         , o.k.o.u.__.k.t.k
--         , o.k.o.u.p.k.t.k -- 14
--         ]
--     , map (make (o.__.k.__) (o.k.p.k . t.k))
--         [ o.k.o.o.k.o.o.k
--         , o.__.k.o.k.o.o&t.k
--         , o.o.k.o.k.o.o&t.k
--         , o.__.k.t.k.o.o&t.k
--         , o.o.k.t.k.o.o&t.k
--         , k.__.k.t.k.o.o&t.k
--         , k.p.k.t.k.o.o&t.k
--         , n.k.p.p.k.o.o.k
--         ]
--     , [ make (o.o.k.t) (p.k.p.k . t.k) (p.k.o.o.k.t.p.k)
--       , make (n.o.o&k.__) (o&k.__.u.__ . p.k) (n.o.o&k.__.u.__.p.k)
--       ]
--     ]
        ]
    ]
    where
    make fill1 fill2 pattern =
        long . long
        . group pattern . group pattern . long
        . r2 short . fill1 . long
        . r3 short . fill2 . nakatiku
        where
        long = group pattern . nakatiku
        short = takeM 6 pattern
    -- k2 = makeKendang2
    --     [ (taka.naka, k.p.t.l)
    --     , (taka.naka.tiku, k.p.t.l.k.p)
    --
    --     , (ktknpktk, k.p.t.l.k.p.a.o)
    --     , (ooknpktk, a.a.p.l.k.p.a.o)
    --     , (otknpktk, a.k.p.l.k.p.a.o)
    --     , (od__odnpktk, o.__.o.l.k.p.a.o)
    --     ] where KendangPasang.Strokes {..} = KendangPasang.notes
