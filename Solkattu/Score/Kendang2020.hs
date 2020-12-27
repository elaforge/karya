module Solkattu.Score.Kendang2020 where
import           Prelude hiding ((.), repeat)

import           Solkattu.Dsl.Kendang


farans :: Korvai
farans = faran $ korvaiS adi $ map su $ concat
    [ map (make (t.k.p.k) (t.k.p.k . p.k)) -- (p.n.p.k) (p.n.p.k . t.k)
        [ k.p.k.t.p.k.a.k -- k.t.k.n.p.k.t.k
        , a.a.k.t.p.k.a.k -- o.o.k.n.p.k.t.k
        , a.a.t.t.p.k.a.k -- o.o.n.n.p.k.t.k
        , a.p.k.t.p.k.a.k -- o.t.k.n.p.k.t.k
        , o.__.o.t.p.k.a.k -- od.__.od.n.p.k.t.k
        , o.t.o.t.p.k.a.k -- o.d.o.n.p.k.t.k
        , o.p.o.t.p.k.a.k -- o.k.o.n.p.k.t.k
        , pk.p.a.t.p.k.a.k -- o&t.k.o.n.p.k.t.k
        , p.u.__.t.p.k.a.k -- p.u.__.n.p.k.t.k
        , o.u.__.t.p.k.a.k -- o.u.__.n.p.k.t.k
        ]
    , map (make (o.u.__.k) (o.u.__.k.p.k)) -- (o.u.__.k) (o.u.__.k . t.k)
        [ o.u.__.p.p.a.o.k
        , o.u.k.p.p.a.o.k
        , a.p.o.u.__.k.a.k
        , a.p.o.u.p.k.a.k
        ]
    , map (make (a.__.p.__) (a.t.p.k.a.k)) --  (o.__.k.__) (o.k.p.k . t.k)
        [ a.p.a.a.p.a.a.p -- o.k.o.o.k.o.o.k
        , a.__.p.a.p.a.t.p -- o.__.k.o.k.o.o&t.k
        , a.a.p.a.p.a.t.p -- o.o.k.o.k.o.o&t.k
        , a.__.p.k.p.a.t.p -- o.__.k.t.k.o.o&t.k
        , a.a.p.k.p.a.t.p -- o.o.k.t.k.o.o&t.k
        , p.__.p.t.p.a.t.p -- k.__.k.t.k.o.o&t.k
        , p.k.p.t.p.a.t.p -- k.p.k.t.k.o.o&t.k
        , t.p.k.k.p.a.o.p -- n.k.p.p.k.o.o.k
        ]
    -- , [ make (o.o.k.t) (p.k.p.k . t.k) (p.k.o.o.k.t.p.k)
    , [ make (a.a.p.k) (p.k.p.k.t.k) (p.k.a.a.p.k.t.k)
      -- , make (n.o.o&k.__) (o&k.__.u.__ . p.k) (n.o.o&k.__.u.__.p.k)
      , make (t.o.pk.__) (pk.__.u.__.p.k) (t.o.pk.__.u.__.p.k)
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
