-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Library of mridangam patterns for realizing solkattu.
module Derive.Solkattu.Patterns where
import qualified Derive.Solkattu.Solkattu as Solkattu
import Derive.Solkattu.Dsl
import Global


defaults :: Solkattu.Patterns
defaults = check $ Solkattu.patterns
    [ (5, [k, t, k, n, o])
    , (6, [k, t, __, k, n, o])
    , (7, [k, __, t, __, k, n, o])
    , (8, [k, t, __, k, __, n, __, o])
    , (9, [k, __, t, __, k, __, n, __, o])
    ]

patterns2 :: Solkattu.Patterns
patterns2 = check $ Solkattu.patterns
    [ (5, [k, t, k, n, o])
    , (7, [k, t, __, k, n, __, o])
    , (9, [k, t, __, __, k, n, __, __, o])
    ]

families567 :: [[(Int, (Solkattu.Speed, [MNote]))]]
families567 = map (\xs -> zip [5..] (map (Solkattu.S2,) xs))
    [ [ [k, __, t, __, k, __, k, t, o, __]
      , [k, __, t, __, __, __, k, __, k, t, o, __]
      , [k, __, __, __, t, __, __, __, k, __, k, t, o, __]
      ]
    , [ [k, __, t, __, k, __, k, n, o, __]
      , [k, __, t, __, __, __, k, __, k, n, o, __]
      , [k, __, __, __, t, __, __, __, k, __, k, n, o, __]
      ]
    , [ [k, t, p, k, p, k, t, k, n, o]
      , kp <> [k, t, p, k, p, k, t, k, n, o]
      , kpnp <> [k, t, p, k, p, k, t, k, n, o]
      ]
    , [ [k, t, k, t, p, k, p, t, o, __]
      , [p, __, k, t, k, t, p, k, p, t, o, __]
      , [k, __, p, __, k, t, k, t, p, k, p, t, o, __]
      ]
    , [ [n, __, k, t, p, k, p, t, o, __]
      , [p, __, n, __, k, t, p, k, p, t, o, __]
      , [k, __, p, __, n, __, k, t, p, k, p, t, o, __]
      ]
    , [ [u, __, k, t, p, k, p, t, o, __]
      , [p, __, u, __, k, t, p, k, p, t, o, __]
      , [k, __, p, __, u, __, k, t, p, k, p, t, o, __]
      ]
    , [ [k, __, t, __, k, t, __, k, n, o]
      , kp <> [k, __, t, __, k, t, __, k, n, o]
      , kpnp <> [k, __, t, __, k, t, __, k, n, o]
      ]
    , [ [k, p, k, od, __, k, t, k, n, o]
      , [k, p, __, k, od, __, k, t, __, k, n, o]
      , [k, p, __, __, k, od, __, k, __, t, __, k, n, o]
      ]
    ]
    where
    kp = [k, p]
    kpnp = [k, p, n, p]

