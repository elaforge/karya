-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
-- | This is analogous to the solkattu scores, except for mridangam specific
-- scores.
module Solkattu.Score.Mridangam2013 where
import           Prelude hiding ((.), repeat)

import qualified Solkattu.Dsl.Misc as Misc

import           Solkattu.Dsl.Mridangam


-- 2013-06-05 - ta di thom nam, nadindin sarvalaghu

e_ktkt :: Korvai
e_ktkt = date 2013 6 5 $ exercise $ ganesh $ korvaiS adi
    [ r2 (n.p.ktkt.pk) . n.p.kt.kp.kt.kp.ktkt.pk
    , n.p . r6 (kt.kp) . ktkt.pk
    , n.p . r3 (kt.kp . cmap hv [k, t, k].p) . ktkt.pk
    , n.p . r6 (cmap hv [k, t, k].p) . ktkt.pk
    , n.o . r3 (o&k.t.kp . kt.kp) . ktkt.pk
    , n.o . r6 (o&k.t.kp) . ktkt.pk
    , n . r3 (o.k.o&t.k.p .kt.k) . p.ktkt.pk
    , n . r6 (o.k.o&t.k) . p.ktkt.pk
    ]

e_3s_5s :: Korvai
e_3s_5s = date 2013 6 12 $ exercise $ ganesh $ korvaiS1 adi $
    r8 takita . r8 tdgnt . r4 takita . r4 tdgnt . r2 (r2 takita . r2 tdgnt)
    . r4 (takita.tdgnt)
    where
    takita = "kD_"
    tdgnt = "ktkno"

-- 2013-06-19 - sarvalaghu, nakatarikitataka, start of farans, p5 variants
-- 2013-07-17 - farans, 555, 666, 777

dinnagina_sequences :: Korvai
dinnagina_sequences = date 2013 9 11 $
    comment "apply pattern to any combination of 3x oktk okok ou_k ookn etc." $
    korvai adi
    [ s $ mconcat $ make_dinna
        "o_k_oktk oktk oktk" t o
        ("oktkok", "pktkpk") ("okoktk", "pkpktk")
    , s $ mconcat $ make_dinna
        "o_k_oktk oktk okok" t o
        ("tkokok", "tkpkpk") -- maybe? TODO verify
        ("", "")
        -- ("okokok", "pkpkpk") -- TODO made up, not as interesting
    , dateS 2013 9 18 $ s $ mconcat $ make_dinna
        "o_k_oktkokokou_k" t o
        -- ("ou_kko", "pu_kko") ("koou_k", "kopu_k")
        ("ou_kko", "pu_kko") ("okou_k", "pkpu_k")
    -- okk makes the following beat into a rest
    , dateS 2013 10 9 $ s $ mconcat $ make_dinna
        "__k_oktkoktkokk_" t __
        ("okk___", "pkk___") ("", "")
    , dateS 2013 10 24 $ s $ mconcat $ make_dinna
        "o_k_oktkokokokko" t o
        ("okokko", "pkpkko") ("", "")
    , dateS 2013 10 29 $ s $ mconcat $ make_dinna
        "o_k_ooknooknookn" p o
        ("ooknok", "ppknpk") ("", "")
    , dateS 2013 10 29 $ s $ mconcat $ make_dinna
        "o_knookno_knookn" p o
        ("ooknok", "ppknpk") ("", "")
    , dateS 2019 4 8 $ s $ mconcat $
        let ktok = su "ktok"
            ktpk = su "ktpk"
        in make_dinna
            (r2 (o.k.o.n.ktok) . o.n.ktok) o o
            ("okon".ktpk, "pkpn".ktok) ("", "")
    , dateS 2019 4 29 $ s $ mconcat $
        -- The end of theme is "knpk", but when repeated it becomes "knpkknpk".
        -- It's more graceful to replace with "onpk".
        -- TODO this is an awkward way to do exceptions, but I can't think of
        -- anything more clever.  I'd want to override 'make_dinna's 'me'
        -- assignment, but I'd want some kind of generic override by name.
        let theme = "otknpk" . r2 "Tknpk"
            me = g "onpk"
        in
        Misc.replaceAt 6 (su $ tri_ (od.__) (theme.me)) $
        Misc.replaceAt 7 (su $ trin (od.__) theme (theme.me) (theme.me.me)) $
        make_dinna theme o od
            ("_Tknpk", "_Xknpk") ("", "")
    , dateS 2024 7 14 $ commentS "replace npk with n_ktpk" $ s $ mconcat $
        let theme = "otknpk" . "Tknpk" . "Tk" . n_ktpk
            me = g $ o.n_ktpk
            n_ktpk = su "n_ktpk"
        in
        Misc.replaceAt 6 (su $ tri_ (od.__) (theme.me)) $
        Misc.replaceAt 7 (su $ trin (od.__) theme (theme.me) (theme.me.me)) $
        make_dinna theme o od ("_Tk".n_ktpk, "_Xk".n_ktpk) ("", "")
    , dateS 2024 9 8 $ s $ mconcat $ make_dinna
        "okookn okookn ookn" o o ("okookn", "ppkpkn") ("", "")
    , dateS 2024 9 8 $ s $ mconcat $ make_dinna -- like oktkoktkokk_
        "__k_oktkokokouk_" __ od ("__puk_", "__ouk_") ("", "")
    ]

more_dinnaginna :: Korvai
more_dinnaginna = date 2024 9 2 $ elaforge $ korvai adi
    [ s $ mconcat $ make_dinna
        "o_k_Nnpk noIn onpk" t od -- alternate: onpk -> o n ktok
        ("In onpk", p&i."npnpk") ("", "")
    , s $ mconcat $ make_dinna
        (dha_ge_terekita.dhatidhage.dhinnagene) n od
        ("no".dhinnagene, "np".p&i.",pk") ("", "")
    ]
    where
    dha_ge_terekita = "N_o_ktpk"
    dhatidhage = "Nlno"
    dhinnagene = o&i.y.o.k

make_dinna :: Sequence -> Sequence -> Sequence -> (Sequence, Sequence)
    -> (Sequence, Sequence) -> [Sequence]
make_dinna theme_ repl sep (theme1, ptheme1) (theme2, ptheme2) = map su $
    [ sarvaA_ 16 ptheme
    , sarvaA_ 8 ptheme . sarvaA_ 8 ptheme
    -- 1   2   3   4   X   O   X   O   |
    -- o o o o o p p p p p p o o o o o |
    -- +-----+-----+---+-----+-----+---
    , r2 $ split 12 id closed theme . split 4 closed id theme
    , trip (sep.__8) id
    , theme.sep.__8 . ptheme.sep.__4 . eme.sep.__4 . eme
    , theme.sep.__8 . ptheme.sep.__4 . eme.sep.__2 . me.sep.__2 . me
    , trip (sep.__) $ \theme -> theme.me
    , trin (sep.__) theme (ptheme.me) (ptheme.me.me)
    ,      theme.sep.__4 . eme.sep.__4 . eme.sep.__4
        . ptheme.sep.__4 . eme.sep.__4 . eme.sep.__4
        . ptheme.sep.__4 . eme.sep.__ . me.sep.__.me
    -- variation 1
    , trip (sep.__4) $ \th -> th . r2 (g theme1 . g ptheme1)
    -- variation 2
    ] ++ guard (not (null theme2))
    [ trip (sep.__4) $ \th -> th . r2 (g theme2 . g ptheme2)
    ]
    where
    trip sep make = trin sep (make theme) (make ptheme) (make ptheme)
    theme = group theme_
    ptheme = group (repl `replaceStart` theme_)
    eme = rtakeM 8 theme -- dropM 8 theme
    me = rtakeM 4 theme -- dropM 12 theme
    split m a b seq = a pre . b post
        where (pre, post) = splitM_ m seq

guard :: Bool -> [a] -> [a]
guard b xs = if b then xs else []

namita_dimita_seq :: Korvai
namita_dimita_seq = korvaiS adi $
    [ sequence t1 t1_end
    , sequence t2 t2_end
    , sequence t3 t3_end
    ]
    where
    sequence t end =
          (lh&rh) . (lh&rh) . o&rh . t
        . (lh&rh) . t . o&rh . t
        . r4 t
        . tri_ (od.__.k.p.k) end
    t1 =     group $ n.l.d.od.n.o.od.__
    t1_end = group $ n.l.d.od.n .__.od.l.od.n.__ .od.l.od.n.__ .od.l
    t2 =     group $ su $ on.__.ktpk.p&t.__.k.__.on.__.ktpk
    t2_end = group $ su $ tri (on.__.ktpk.p&t.__.k.__) . on.__.ktpk
    t3 =     group $ su $ on.__.ktpk.p.u.__.k.on.__.ktpk
    t3_end = group $ su $ tri (on.__.ktpk.p.u.__.k) . on.__.ktpk

    rh = n.l.d.d.l.d.d.l
    lh = o.__3.o.__3.o.__

-- ** sarvalaghu fills

namita_dimita_dimi :: [Sequence]
namita_dimita_dimi =
    [ o&n.__.k.t.p.k.p.k.t.k.n.o.o.k.k.__
    , k.t.k.t . k.t.k.n.kt.p.k . o.t.k.n.kt.p.k . o.n.kt.p.k
    -- goes past sam: previous . o.t.k.o&n.kt.p.k
    ]
    where kt = su (k.t)

janahan_exercise :: Korvai
janahan_exercise = exercise $ janahan $ korvaiS1 adi $
    o&d.__4 . r7 (n.p.k.t.p.k.t.p) . k.t.p.k

nakanadin :: Korvai
nakanadin = sarvalaghu $ korvaiS1 (beats 2) $ su $
    d.__3.y.n.y.d.__3.y.d.y.n.y.n.y

farans :: Korvai
farans = exercise $ faran $ korvaiS adi $ map su $ concat
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
    , [make "pkoo" "oupktk" "pkoonook"]
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
        . group pattern . group pattern . long
        . r2 short . fill1 . long
        . r3 short . fill2 . nakatiku
        where
        long = group pattern . nakatiku
        short = takeM 6 pattern

-- * fragments

eddupu6 :: Korvai
eddupu6 = korvaiS (beats 3)
    [ r2 (k.__.p.__.k.__)
    , r2 (od.__.p.k.n.o)
    , r3 (k.o.o.k)
    , r2 (o.o.t.__.k.__)
    , k.p.k.__.t.__.k.t.__.k.n.o
    , __.__.u.__3.k.o.o&t.k.n.o.k
    , su $ r2 $ o&n . __ . p.k.nakatiku
    ]

eddupu10 :: Korvai
eddupu10 = korvaiS (beats 5)
    [ r2 $ u.__3.k.o.o&t.k.n.o.k
    , __.__ . r3 p6
    ]
