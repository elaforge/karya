-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Score.Mridangam2023 where
import           Prelude hiding ((.), repeat)

import           Solkattu.Dsl.Mridangam


-- https://www.youtube.com/channel/UC09zwvvoVr7pbyNqC7ejALg

ex_sai_shiv_11 :: Korvai
ex_sai_shiv_11 = date 2023 3 20 $
    exercise $ source "https://www.youtube.com/watch?v=9l_RlC54pNU" $
    korvaiS adi
    [ seqs 4 . seqs 2 . seqs 1 . tri123 (od.__4) ending
    ]
    where
    seqs times = mconcatMap (seq times) [p, k, o, n]
    seq times st = repeat times (st.__."ktpk") . ending
    ending = g $ on.__. su "ktpk oknpknpk"

ex_sai_shiv_12 :: Korvai
ex_sai_shiv_12 = date 2023 3 20 $
    exercise $ source "https://www.youtube.com/watch?v=cVnNm1pa64U" $
    korvaiS adi
    [ seq1 "kook" -- takadinna, can do kooknook
    , seq1 "ktpk" -- kitataka
    , seq1 "onpk" -- dinnadaka
    ]
    where
    seq1 pat = r4 (pat.od.__4) . r4 (su (pat.pat).od.__4)
        . r4 (nadai 6 (su (pat.pat.pat)).od.__4)
        . r4 (su (su (r4 pat)).od.__4)

-- similar to what I played, from
-- "08-Thani Avarthanam", "Cleveland Aradhana 2004"
mohra_bs :: Korvai
mohra_bs = date 2023 3 30 $ korvaiS adi
    [ su $ r2 (a1.a2.a3.b1)
        . a1.a2.a3."o_k_D___"
        . "o_k_u_pk"."npupktok" . "o_k_D___"
        . "o_k_u_pk".nakatiku . r2 ("N_pk".nakatiku).nakatiku
    , "k_t_kD_k_D_" . "ktk".su "kn" . o.__
    . "i_kD_k_D_"  . "ktk".su "kn" . o.__
    . nadai 6 (tri_ p5 "kD_k_D_" . tri_ __ p5)
    ]
    where
    a1 = "K___u_pk".nakatiku
    a2 = "npk_u_pk"."npupkto_"
    a3 = "K___u_pk"."npupktok"
    b1 = "o_k_D__ko_k_D___"
