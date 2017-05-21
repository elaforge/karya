-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
-- | This is analogous to "Derive.Solkattu.Score", except for mridangam specific
-- scores.
module Derive.Solkattu.Score.MridangamScore where
import Prelude hiding ((.), repeat)

import qualified Derive.Solkattu.Korvai as Korvai
import Derive.Solkattu.MridangamDsl
import qualified Derive.Solkattu.Tala as Tala


-- * exercises

c_exercises :: [Sequence]
c_exercises =
    [ o&d.__4 . k.t.p.k.n.__.o.__.k.t.p.k -- janahan
    ]

-- ** tisram

ganesh_16_11_14 :: Korvai
ganesh_16_11_14 = date 2016 11 14 $ exercise $ ganesh $ korvai1 adi $ nadai 6 $
    tri (reduce3 1 mempty (o&n.p.k.o&d.__)) . o&n.p.k . o&n.p.k.__ . o&n.p.k.__3

ganesh_17_02_13 :: Korvai
ganesh_17_02_13 = date 2017 2 13 $ ganesh $ sarvalaghu $ korvai adi $
    map (nadai 6)
    [ takitadin.kadin . takitadin.kadin . takitatin.katin
        . takitatin.k.takitatin.k
    , takitadin.kadin . __ . dropM 1 takitadin . kadin . takitatin.katin
        . __ . p.k.od.__ . p.k.od.__ . p.k.od
    ]
    where
    takitadin   = k.p.k.od.__
    takitatin   = k.p.k. d.__
    kadin = k.od.__.o&n.__.k.__
    katin = k. d.__.  n.__.k.__

-- * sarvalaghu

din_nadin :: Korvai
din_nadin = sarvalaghu $ ganesh $ korvai (beats 4)
    [ od.__.on.od.l.od.on.l.od.l.on.od.l.od.on.l
    , su $ repeat 2 $ d.__.p.k.n.l.d.__.p.l.d.__.n.__.l.__
    , su $ repeat 2 $ d.__.p.k.n.l.d.l.p.l.d.l.n.l.p.l
    ]

nadin_ka :: Korvai
nadin_ka = sarvalaghu $ ganesh $ date 2017 5 15 $ korvai1 (beats 4) $
    on.od.__.k.(n.d.__.k).(n.d.__.k).o.od.__.k
    -- 4 nd to switch to kandam

-- ** sarvalaghu fills

namita_dimita_dimi :: [Sequence]
namita_dimita_dimi =
    [ o&n.__.k.t.p.k.p.k.t.k.n.o.o.k.k.__
    , k.t.k.t . k.t.k.n.kt.p.k . o.t.k.n.kt.p.k . o.n.kt.p.k
    -- goes past sam: previous . o.t.k.o&n.kt.p.k
    ]
    where kt = su (k.t)

janahan_exercise :: Korvai
janahan_exercise = exercise $ janahan $ korvai1 adi $
    o&d.__4 . repeat 7 (n.p.k.t.p.k.t.p) . k.t.p.k

nakanadin :: Korvai
nakanadin = sarvalaghu $ korvai1 (beats 2) $ su $
    d.__3.y.n.y.d.__3.y.d.y.n.y.n.y

-- * korvais

p16_12_06_sriram2 :: Korvai
p16_12_06_sriram2 = date 2016 12 6 $ sriram $ korvai1 adi $ nadai 7 $
      repeat 2 kook . od.__.k.d.__.k.__                                .od.__7
    . repeat 2 kook . od.__.k.d.__.k.__.n.p.k.d.__.k.__                .od.__7
    . repeat 2 kook . od.__.k.d.__.k.__.n.p.k.d.__.k.__.o.o.k.d.__.k.__.p&u.__7
    . tri (p5.u.__ . p5.u.__.u.__ . p5)
    where kook = k.o.o.k.n.p.k

p16_12_06_janahan1 :: Korvai
p16_12_06_janahan1 = date 2016 12 6 $ janahan $ korvai1 adi $ su $
    tri (op_od_ . on.k.op_od_ . on.k.o&t.k.op_od_)
        . trin __ (tri p5) (tri p6) (tri p7)
    where
    op_od_ = on.p.k.od.__.o

p16_12_06_janahan2 :: Korvai
p16_12_06_janahan2 = date 2016 12 6 $ janahan $ korvai1 adi $ su $
    tri (k.__.t.__.kook) . tri (t.__.kook) . tri kook
        . tdgnt . p6
        . tdgnt . p6 . k.p.p6
        . tdgnt . p6 . k.p.p6 . k.p.n.p.p6
    where
    kook = k.o.o.k
    tdgnt = spread 2 $ k.t.k.n.o

all_korvais :: [Korvai]
all_korvais =
    [ p16_12_06_sriram2, p16_12_06_janahan1, p16_12_06_janahan2
    ]

-- * korvai sequences

-- TODO use this as a template to fill in various themes
ksequence :: Korvai
ksequence = date 2013 10 24 $ ganesh $ sequence_t $ korvai1 adi $
    mconcat $ map (sam.)
    -- TODO put the whole thing in su?
    [ sarva `replaceEnd` theme
    , takeD 4 sarva `replaceEnd` theme
        . rtakeD 4 sarva `replaceEnd` theme
    , theme . ptheme . theme . ptheme
    , tri_ (od.__4) theme
    , theme . od.__4 . theme . od.__2 . eme . od.__2 . eme
    , theme . od.__4 . theme . od.__2 . eme . od . me . od . me
    , tri_ od (theme.me)
    , theme . od . theme.me . od . theme.me.me
    , repeat 2 (theme.od.__2 . eme.od.__2 . eme . od.__2)
        . theme.od.__2 . eme.od . me.od.me

    -- 1st variation
    , tri_ (o.__2) (ptheme . me . pme . ome . pme . su (k.o))
    -- 2nd variation
    , tri_ (o.__2) (ptheme . pme . ome . pme . ome)
    ]
    where
    sarva = sd $
          on.od.od.on. su (on.on) .od.od.on
        . p&n.d.d.n  . n          .od.od.on
    -- sarva =
    --       on.__.od.__.od.__.on.__.(on.on) . od.__.od.__.on.__
    --     . p&n.__.d.__.d.__.n.__ . n.__    . od.__.od.__.on.__
    theme = su $ o.__.k.__.o.k.t.k.o.k.o.k.o.u.__.k
    -- TODO not quite right, this has to be ptheme when not on an som or arudi
    ptheme = su t `replaceStart` theme
    eme = rtakeM 4 theme
    me = rtakeM 2 theme

    -- TODO these seem to vary irregularly
    ome = su $ k.o.o.u.__.k
    pme = su $ k.o.p.u.__.k

-- * farans

farans :: Korvai
farans = faran $ korvai adi $ map su $ concat
    [ map (make (p.n.p.k) (p.n.p.k . t.k))
        [ k.t.k.n.p.k.t.k
        , o.o.k.n.p.k.t.k
        , o.o.n.n.p.k.t.k
        , o.t.k.n.p.k.t.k
        , od.__.od.n.p.k.t.k
        , o.d.o.n.p.k.t.k
        , o.k.o.n.p.k.t.k
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
        . repeat 3 short . fill2 . nakatiku
        where
        long = pattern . nakatiku
        short = takeM 6 pattern

-- * fragments

eddupu6 :: Korvai
eddupu6 = korvai (beats 3)
    [ repeat 2 (k.__.p.__.k.__)
    , repeat 2 (od.__.p.k.n.o)
    , repeat 3 (k.o.o.k)
    , repeat 2 (o.o.t.__.k.__)
    , k.p.k.__.t.__.k.t.__.k.n.o
    , __.__.u.__3.k.o.o&t.k.n.o.k
    , su $ repeat 2 nang_kita
    ]

eddupu10 :: Korvai
eddupu10 = korvai (beats 5)
    [ repeat 2 $ u.__3.k.o.o&t.k.n.o.k
    , __.__ . repeat 3 p6
    ]

nang_kita :: Sequence
nang_kita = o&n . __ . p.k.nakatiku

taka, takanaka :: Sequence
taka = k.p
takanaka = k.p.n.p

-- -- * realize

adi :: Tala.Tala
adi = Tala.adi_tala

-- | For a fragment which fits a certain number of beats.
beats :: Akshara -> Tala.Tala
beats aksharas = Tala.Tala "beats" [Tala.I] aksharas

realize, realizep :: Korvai.Korvai -> IO ()
realize = realize_ True
realizep = realize_ False

realize_ :: Bool -> Korvai.Korvai -> IO ()
realize_ = realize_instrument Korvai.mridangam

ganesh, janahan, sriram :: Korvai -> Korvai
ganesh = source "ganesh"
janahan = source "janahan"
sriram = source "sriram"
