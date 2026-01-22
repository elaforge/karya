-- Copyright 2026 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Solkattu.Score.MridangamBali where
import           Prelude hiding ((.))

import           Solkattu.Dsl.Mridangam


legong_mridangam :: Korvai
legong_mridangam = elaforge $ korvai adi
    -- TODO mostly beat at end, but inconsistent around repeats
    [ s $ __D 2 . sd "_o_n.N" . "_k_k.N_kknkok"
    . "pkpkdndni_N_i_iio_k.N_.N" . "pkokok."
    , x2 $ s $ awak
    , s $ "kpki.N___i_N_i_oii_ko_kok.N_kpko"
        . "kkok_okok.N__pkokkok_okok.I_I_iok"
        . "pkdndnok__okokk.D_D_N_N_.Niipkon"
        . "npkn_d_n_n_d_n_"."_pkokokoN__dnpkpk"
        . "dnokdndnD_k.N___dnokpkok.N_____k"
        . "ok.N___kok.N___" . "_pkokok.N__i_i_ik"
        . "dnokpkpkN_k.U_dndiiipk__kkokok.N"
        . "_k.N___k_D_k__k_D_D_k_k.N_kkdnok"
        . "pkpkdndni.o".su "ktpi"."iiio_k.U_.Ukkokok.N"
    ]
    where
    -- . is light thom
    -- TODO alternately, play like normal thom, and heavy thom is N or D
    awak =
          "N___k_k_N__dndnokdnokok.N__dndnok"
        . "pkdndnokdnokokk.D_D_N_N_.Diipkon"
        . "npkn_d_n_n_d_n_"."_pkdnpkd".su "ktpi"."iiiokok"
        . ".N__pkokdnokokk.D_D_N_N_.Diipkon"
        . "npkn_d_n_n_d_n_"."_pkdnpkdndnpkdnpk"
        . "piiiP.N_kkokok.N__pkdndnok.N___k"
        . "kkN_kkokkkN_kkokdnokok.N_k.N___k"
        . "pkokok.Ni_.N_i_iio_k.N___kkokok."
        -- TODO 2nd repeat skips the last line, how to write that?
    -- Sometimes k is tut
