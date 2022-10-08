-- Copyright 2022 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Score.Mridangam2022 where
import           Prelude hiding ((.), repeat)

import           Solkattu.Dsl.Mridangam


e_n_dd_dd :: Korvai
e_n_dd_dd = date 2022 2 6 $ exercise $ ganesh $ korvaiS adi
    [ r2 "N.dD.dD."   . "N.dd.dd. n.dD.dD."
    , "N.dD.dD. dN.D.dD. N.dd.dd. n.dD.dD."
    , "N.dD.dD. dN.D.dD. N.dd.dd. dn.D.dD."
    , r2 "dN.D.dD."   . "dn.d.dd. dn.D.dD."
    , "on.D.dD. dN.D.dD." . r3 "dN.D.".d
      . "on.d.dd. dn.d.dd." . r3 "dn.d.".d
    , "on.D." . r2 "dN.D.".d . "on.d." . r2 "dn.d.".d
    , r2 $ g "on.D.dN." . g "pn.d.dn."
    , r4 (g "on.D.d" . g "pn.d.d") . r2 (g "on.d" . g "pn.d")
    ]

e_n_dd_dd3 :: Korvai
e_n_dd_dd3 = date 2022 2 6 $ exercise $ ganesh $ korvaiS adi $ map (nadai 6)
    [ r3 (g "N.dD.dD.")   . o & (r2 (g "n.dd.dd.")) . g "n.dD.dD."
    , o & (r2 (g "n.dD.dD.")) . g "dN.D.dD." . r2 (g "n.dd.dd.") . g "n.dD.dD."
    , r3 $ g "N.dD.dD." . g "dN.D.dD."
    , r3 $ g "on.D.dD." . g "dn.d.dd."
    -- TODO middle one without thom
    , r3 $ g "on.D.dD." . g "dN.D.dD." . r3 (g "dN.D.").d
    , r3 $ g "on.D." . r2 (g "dN.D.").d . (g "on.d.") . r2 (g "dn.d.").d
    , r3 $ g "on.d.dN." . g "pn.d.dn."
    , r2 (g "on.d.d" . g "pn.d.d") . r3 (g "on.d" . g "pn.d")
    ]

c_22_02_20 :: Korvai
c_22_02_20 = date 2022 2 20 $ ganesh $ korvaiS adi
    [ sarvaD_ 4 . __. nd3."N__k__D"
    , sarvaD_ 4 . u.__3 . nd3."N_k_D"
    , sarvaD_ 5 . __.nd3."NkD"
    , sarvaD_ 5 . __.nd3."N__k__D_" . nd3."N_k_D_" . nd3."NkD"
    , nd3."NkD__".su p6 . nd3."N_k_D__".su (p6.p6) . nd3."N__k__D__"
        . su (r3 p6)."N_k_D__" . su (r3 p6)."NkD__" . su (r3 p6)
    ]
    where
    nd3 = r3 nd
    nd = g $ on.su (su ktok).od

c_22_03_02 :: Korvai
c_22_03_02 = date 2022 3 2 $ ganesh $ korvaiS adi
    [ "NNkD__NkNNkD__nknnkd__nknnkd" . p.u.su ktok
    , "NNkD__NkNNkD__nknnkd_knd_knd__k_"
    , make "kktkkooko"
    , make (su ("ktknpktk".nakatiku) . o)
    , make (su (r2 "Nkpknook") . o)
    ]
    where
    make p9 = tri_ (su p6) (g p9 . "NkD__")
        . su p6.u.__.su (kp.p6).u.__ . su (kpnp.p6)

x_22_07_09 :: Korvai
x_22_07_09 = elaforge $ comment "solo from dream" $ korvaiS1 adi $
    -- From dream where I was singing with a large group.  Later switched
    -- to mridangam but couldn't get a comfortable seat.  Famous player was
    -- next to me.  After that a dream where I was doing something like this
    -- solo.
    -- all closed strokes, in this style but stretched out longer
    -- K is hv, or rather preceding strokes are lt
    "kktkkpkpkk" . su "pktkpkpktk" . "p_"."tkkpkkpkp" . su "ktkpktkp" ."k_"
    . su "pktkpktkpktkpkpktkpkpktk".p.__.su "pkK_pkK_pktkpkpktk"."p_"
    . su "pkK_pkpkK__kpkpkK_" . "_".su"_k"
    -- TODO fix this up so it lines up better
    . "Kkkkkktkkpkpkkpp".su "pktpkpktk"."p_kk".su "pktpkpktk"."p__kkk"
    . su "pkpktpktkpktkp"."___".plak.__ . su "pktkpktk"."p___".plak.plak.__
    . su "pktkpkpktkpkpktk"."p__"
    . lt (su "pktkpktkpktkpktk".su (r6 "pkt").p.__).plak . su (r5 "pkt".__).__
    . plak.__.tri_ plak (su "pktpkt__") . "__________"
    -- after a long while do nam
    . hv k.su pk."pkpkkppkpkkp"."NN". __.su pk."pkpkkppkkpk".su pk."pk"
    . hv k.su pk."pkpkkppkpkkp"."NN". __.su pk."pkkp"."NN".__."kkp_kkp"
    -- etc.
    where
    -- but tha with fingers in the middle
    plak = hv (p&k)

elaforge = source "elaforge"

s_22_09_25 :: Korvai
s_22_09_25 = sollu $ korvaiS (beats 2) $ map su
    [ "ktokou_kokokou_k"
    , "okou_kokou_kou_k"
    , "npk_u_pk".nakatiku
    , "____u_pk".nakatiku
    , "kookD_kookD_kook"
    , r2 "o_knookn"
    , "___kpktknkoN_N_k"
    , "oo_".tri_ "D_" "N_k"
    , "ktokn_o_ktoknook"
    , "ktokotokotokotok"
    ]
