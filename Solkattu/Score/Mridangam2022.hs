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
