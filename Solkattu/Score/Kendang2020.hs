-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Score.Kendang2020 where
import           Prelude hiding ((.), repeat)

import           Solkattu.Dsl.Kendang


farans :: Korvai
farans = faran $ korvaiS adi $ map su $ concat
    [ map (make (t.k.p.k) (t.k.p.k . p.k)) -- (p.n.p.k) (p.n.p.k . t.k)
        [ k.p.k.t.p.k.o.k -- k.t.k.n.p.k.t.k
        , o.o.k.t.p.k.o.k -- o.o.k.n.p.k.t.k
        , o.o.t.t.p.k.o.k -- o.o.n.n.p.k.t.k
        , o.p.k.t.p.k.o.k -- o.t.k.n.p.k.t.k
        , i.__.i.t.p.k.o.k -- od.__.od.n.p.k.t.k
        , i.t.i.t.p.k.o.k -- o.d.o.n.p.k.t.k
        , i.p.i.t.p.k.o.k -- o.k.o.n.p.k.t.k
        , pk.p.o.t.p.k.o.k -- o&t.k.o.n.p.k.t.k
        , p.u.__.t.p.k.o.k -- p.u.__.n.p.k.t.k
        , i.u.__.t.p.k.o.k -- o.u.__.n.p.k.t.k
        ]
    , map (make (i.u.__.k) (i.u.__.k.p.k)) -- (o.u.__.k) (o.u.__.k . t.k)
        [ i.u.__.p.p.o.i.k
        , i.u.k.p.p.o.i.k
        , o.p.i.u.__.k.o.k
        , o.p.i.u.p.k.o.k
        ]
    , map (make (o.__.p.__) (o.t.p.k.o.k)) --  (o.__.k.__) (o.k.p.k . t.k)
        [ o.p.o.o.p.o.o.p -- o.k.o.o.k.o.o.k
        , o.__.p.o.p.o.t.p -- o.__.k.o.k.o.o&t.k
        , o.o.p.o.p.o.t.p -- o.o.k.o.k.o.o&t.k
        , o.__.p.k.p.o.t.p -- o.__.k.t.k.o.o&t.k
        , o.o.p.k.p.o.t.p -- o.o.k.t.k.o.o&t.k
        , p.__.p.t.p.o.t.p -- k.__.k.t.k.o.o&t.k
        , p.k.p.t.p.o.t.p -- k.p.k.t.k.o.o&t.k
        , t.p.k.k.p.o.i.p -- n.k.p.p.k.o.o.k
        ]
    -- , [ make (i.i.k.t) (p.k.p.k . t.k) (p.k.i.i.k.t.p.k)
    , [ make (o.o.p.k) (p.k.p.k.t.k) (p.k.o.o.p.k.t.k)
      -- , make (n.i.i&k.__) (i&k.__.u.__ . p.k) (n.i.i&k.__.u.__.p.k)
      , make (t.i.pk.__) (pk.__.u.__.p.k) (t.i.pk.__.u.__.p.k)
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
