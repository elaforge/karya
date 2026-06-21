-- Copyright 2024 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE NoMonomorphismRestriction #-}
module Solkattu.Score.MridangamMohra where
import           Prelude hiding ((.), repeat)

import qualified Solkattu.Score.Mohra as Mohra

import           Solkattu.Dsl.Mridangam


rohan_mohra1 :: Korvai
rohan_mohra1 = rohan $ date 2025 1 25 $
    similarTo "SolkattuMohra" "c_mohra_youtube" $
    korvaiS1 adi $ Mohra.make su Mohra.A1 (a1, a2, a3) (b1, b2, b3)
    where
    a1 = "oUkpk_u___ktktpk"
    a2 = "pi_ipktk".nakatiku
    a3 = "oUkpk_u___ktkpkn"
    b1 = r2 ("oKtk".p'."ktk") -- alternate: r4 "oKtko"
    b2 = "oU__ _oUk"
    b3 = n . r2 "oK__oK_u_pkn" . "oK_oK_oU"

karaikuda_mohra :: Korvai
karaikuda_mohra = mohra $ date 2026 4 28 $ source "Karaikudi Mani" $
    korvaiS1 adi $
    Mohra.makeA su Mohra.A2 Mohra.A2 (a1, a2, a3) (b1, b2, b3)
    where
    a1 = o.k.nakatiku.o&u.__4
    a2 = o.k.nakatiku.o&u.__.o&u.__4
    a3 = o.k.nakatiku.o&u.__.o&u.__.o&u.__4
    b1 = "ou_ko_k_ou_ko___"
    b2 = "ou_ko___"
    b3 = tri "D___" "ou_ko_k_"
