-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
module Derive.Solkattu.MridangamScore where
import Prelude hiding ((.), repeat)

import qualified Util.CallStack as CallStack
import Derive.Solkattu.Dsl hiding ((&))
import qualified Derive.Solkattu.Korvai as Korvai
import Derive.Solkattu.MridangamDsl
import qualified Derive.Solkattu.Solkattu as Solkattu


-- * exercises

sriram :: [Korvai]
sriram = korvais (adi 4)
    -- TODO not right
    [ o&n.__.p.k.t.k . tri (n.k.t.p.k.t.k)
        . repeat 4 (o.t.k.n . s2 (k.t.p.k))
        . repeat 4 (n . s2 (k.t.p.k)  . o.t.k)
        . o&n.__.p.k . tari . tri (p.k.p.t.p.k . tari)
    ]

c_exercises :: [Sequence]
c_exercises =
    [ o&d.__4 . k.t.p.k.n.__.o.__.k.t.p.k -- janahan
    ]

-- ** tisram

ganesh_16_11_14 :: Korvai
ganesh_16_11_14 = korvai (adi 6) $
    tri (reduce3 1 mempty (o&n.p.k.o&d.__)) . o&n.p.k . o&n.p.k.__ . o&n.p.k.__3

-- * sarvalagu, fills

namita_dimita_dimi :: [Sequence]
namita_dimita_dimi =
    [ o&n.__.k.t.p.k.p.k.t.k.n.o.o.k.k.__
    , k.t.k.t . k.t.k.n.kt.p.k . o.t.k.n.kt.p.k . o.n.kt.p.k
    -- goes past sam: previous . o.t.k.o&n.kt.p.k
    ]
    where kt = s2 (k.t)

janahan :: [Korvai]
janahan = korvais (adi 4)
    [ o&d.__4 . repeat 7 (n.p.k.t.p.k.t.p) . k.t.p.k
    ]

nakanadin :: [Korvai]
nakanadin = korvais (beats 2 8)
    [ d.__3.y.n.y.d.__3.y.d.y.n.y.n.y
    ]

-- * korvais

p16_12_06_sriram1 :: Korvai
p16_12_06_sriram1 = korvai (adi 8) $
    p&k.__4.p&t.__4.kitakina . tri (okto . n.o.o.k)
        . od.__.od.__4 . p&u.__.p&u.__4 . od.__4
    . p&t.__4.kitakina . tri okto . od.__4 . p&k.__4 . od.__4
    . kitakina . tri (n.o.o.k) . od.__.k.__.od.__4
    . utarangam_var2
    where
    _utarangam = tri_ (u.__8) (k_kto . k.__.k_kto . k.__.t.__.k_kto)
    _utarangam_var1 =
        k_kto . k.__.k_kto . k.__.t.__.k_kto . u.__6
        . k.p . k_kto . k.__.k_kto . k.__.t.__.k_kto . i.__4
        . k.p.n.p . repeat 4 k_kto
    utarangam_var2 =
        k_kto_s . u.__6 . k.p.k_kto_s . i.__4 . k.p.n.p . repeat 4 k_kto
    k_kto_s = mconcat $ expand 3 2 (k.__.t.__.k_kto)
    k_kto = k.__.k.t.o.__
    okto = o.k.t.o. k .t.p.k
    kitakina = k.t.k.n.o.k.o&t.k.tari

p16_12_06_sriram2 :: Korvai
p16_12_06_sriram2 = korvai (adi 7) $
      repeat 2 kook . od.__.k.d.__.k.__                                .od.__7
    . repeat 2 kook . od.__.k.d.__.k.__.n.p.k.d.__.k.__                .od.__7
    . repeat 2 kook . od.__.k.d.__.k.__.n.p.k.d.__.k.__.o.o.k.d.__.k.__.p&u.__7
    . tri (p5.u.__ . p5.u.__.u.__ . p5)
    where kook = k.o.o.k.n.p.k

p16_12_06_janahan1 :: Korvai
p16_12_06_janahan1 = korvai (adi 8) $
    tri (op_od_ . on.p.op_od_ . on.k.o&t.k.op_od_)
        . trin __ (tri p5) (tri p6) (tri p7)
    where
    op_od_ = on.p.k.od.__.o

p16_12_06_janahan2 :: Korvai
p16_12_06_janahan2 = korvai (adi 8) $
    tri (k.__.t.__.kook) . tri (t.__.kook) . tri kook
        . tdgnt . p6
        . tdgnt . p6 . k.p.p6
        . tdgnt . p6 . k.p.p6 . k.p.n.p.p6
    where
    kook = k.o.o.k
    tdgnt = spread 2 $ k.t.k.n.o

-- * farans

farans :: [Korvai]
farans = korvais (adi 8) $ concat
    [ map (make (p.n.p.k) (p.n.p.k . t.k))
        [ k.t.k.n.p.k.t.k
        , o.o.k.n.p.k.t.k
        , o.o.n.n.p.k.t.k
        , o.t.k.n.p.k.t.k
        , od.__.od.n.p.k.t.k
        , o.d.o.n.p.k.t.k
        , p.k.p.n.p.k.t.k
        , o&t.k.o.n.p.k.t.k
        , p.u.__.n.p.k.t.k
        , o.u.__.n.p.k.t.k
        ]
    , map (make (o.u.__.k) (o.u.__.k . t.k))
        [ o.u.__.k.k.o.o.k -- 11
        , o.u.p.k.k.o.o.k
        , o.k.o.u.__.k.t.k
        , o.k.o.u.p.k.t.k -- 14
        ]
    , map (make (o.__.k.__) (o.k.p.k . t.k))
        [ o.k.o.o.k.o.o.k
        , o.__.k.o.k.o.o&t.k
        , o.o.k.o.k.o.o&t.k
        , o.__.k.t.k.o.o&t.k
        , o.o.k.t.k.o.o&t.k
        , k.__.k.t.k.o.o&t.k
        , k.p.k.t.k.o.o&t.k
        , n.k.p.p.k.o.o.k
        ]
    , [ make (o.o.k.t) (p.k.p.k . t.k) (p.k.o.o.k.t.p.k)
      , make (n.o.o&k.__) (o&k.__.u.__ . p.k) (n.o.o&k.__.u.__.p.k)
      ]
    ]
    where
    make fill1 fill2 pattern =
        long . long
        . pattern . pattern . long
        . repeat 2 short . fill1 . long
        . repeat 3 short . fill2 . tari
        where
        long = pattern . tari
        short = takeM 6 pattern

-- * fragments

eddupu6 :: [Korvai]
eddupu6 = korvais (beats 3 4)
    [ repeat 2 (k.__.p.__.k.__)
    , repeat 2 (od.__.p.k.n.o)
    , repeat 3 (k.o.o.k)
    , repeat 2 (o.o.t.__.k.__)
    , k.p.k.__.t.__.k.t.__.k.n.o
    , __.__.u.__3.k.o.o&t.k.n.o.k
    , s2 $ repeat 2 nang_kita
    ]

eddupu10 :: [Korvai]
eddupu10 = korvais (beats 5 4)
    [ repeat 2 $ u.__3.k.o.o&t.k.n.o.k
    , __.__ . repeat 3 p6
    ]

tari :: Sequence
tari = n.p.u.p.k.t.p.k

nang_kita :: Sequence
nang_kita = o&n . __ . p.k.tari

on :: Sequence
on = o&n

-- * realize

adi :: Matras -> Solkattu.Tala
adi = Solkattu.adi_tala

beats :: Aksharas -> Matras -> Solkattu.Tala
beats aksharas nadai = Solkattu.Tala aksharas 0 nadai

korvais :: CallStack.Stack => Solkattu.Tala -> [Sequence] -> [Korvai]
korvais tala = map (korvai tala)

realize, realizep :: Korvai.Korvai -> IO ()
realize = realize_ True
realizep = realize_ False

realize_ :: Bool -> Korvai.Korvai -> IO ()
realize_ = realize_instrument Korvai.mridangam
