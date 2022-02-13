-- Copyright 2022 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Score.Mridangam2022 where
import           Prelude hiding ((.), repeat)

import qualified Solkattu.Tala as Tala

import           Solkattu.Dsl.Mridangam


-- 2022-02-06
--   noN,d but at 90 and with kandam
--   n,dd,dd: each 2x
--     normal
--     ~~ dN,D,dD
--     ~~ dN,D,dD ~ dn,D,dD
-- 4   all dn,d,dd,
--     on,D,dD, dN,D,dD, ~ ~ dN,D,dN,D,dN,D,d|on,d
--     no thom version
-- 7   second half, with and without thom
-- 8   reduce 4s to 2s

e_n_dd_dd :: Korvai
e_n_dd_dd = date 2022 2 6 $ exercise $ ganesh $ korvaiS adi
    [ r2 "N.dD.dD."   . "N.dd.dd. n.dD.dD."
    , "N.dD.dD. dN.D.dD. N.dd.dd. n.dD.dD."
    , "N.dD.dD. dN.D.dD. N.dd.dd. dn.D.dD."
    , r2 "dN.D.dD."   . "dn.d.dd. dn.D.dD."
    , "on.D.dD. dN.D.dD." . r3 "dN.D.".d
      . "on.d.dd. dn.d.dd." . r3 "dn.d.".d
    , "on.D." . r2 "dN.D.".d . "on.d." . r2 "dn.d.".d
    , r2 "on.D. pn.d. dN.D.d"
    ]
