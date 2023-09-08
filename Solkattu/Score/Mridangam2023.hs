-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Score.Mridangam2023 where
import           Prelude hiding ((.), repeat)

import qualified Solkattu.Tala as Tala

import           Solkattu.Dsl.Mridangam


-- https://www.youtube.com/playlist?list=PLfELs4Vrnswpsj7lMwKUS7vvaO6Cc-OJJ

e_sai_shiv_1_2 :: Korvai
e_sai_shiv_1_2 = date 2023 4 3 $
    exercise $ source "https://www.youtube.com/watch?v=3Nd-z4JcXM8" $
    comment "kita clarity" $ korvaiS adi
    [ mconcatMap single tadithomnam
    , mconcatMap double [(p, k), (o, n)]
    , mconcatMap p1 tadithomnam . mconcatMap (su • p1) tadithomnam
    , mconcatMap single2 tadithomnam
    , mconcatMap double2 [(p, k), (o, n)]
    , mconcatMap p2 tadithomnam . mconcatMap (su • p2) tadithomnam
    ]
    where
    p1 s = s.__.s.__.r4 (g "ktk")
    single s = p1 s . su (p1 s)
    double (s1, s2) = p1 s1 . p1 s2 . su (p1 s1) . su (p1 s2)
    p2 s = s.__.s.__.r3 (g "ktk") . g (su "n_ktpk")
    single2 s = p2 s . su (p2 s)
    double2 (s1, s2) = p2 s1 . p2 s2 . su (p2 s1) . su (p2 s2)

e_sai_shiv_3 :: Korvai
e_sai_shiv_3 = date 2023 4 3 $
    exercise $ source "https://www.youtube.com/watch?v=VkfSoWLe7NE" $
    comment "kita clarity" $ korvaiS Tala.misra_chapu
    [sd kto1234, kto1234, su (r2 kto1234)]
    where
    kto num = g $ repeat num kt . o.__
    kto1234 = mconcatMap kto [1..4]

e_sai_shiv_4 :: Korvai
e_sai_shiv_4 = date 2023 4 3 $
    exercise $ source "https://www.youtube.com/watch?v=_geIheJ_eIg" $
    comment "kita clarity" $ korvaiS adi
    [ mconcatMap (p1 3) tadithomnam
    , mconcatMap (p1 2) tadithomnam
    , mconcatMap (p1 1) tadithomnam
    , mconcatMap p2 tadithomnam
    , mconcatMap (\s -> g (s.__.kt.o.__)) tadithomnam
        . mconcatMap (\s -> g (s.__.kt)) tadithomnam
        . sd (mconcatMap r2 tadithomnam)
        . sd (mconcat tadithomnam)
    ]
    where
    p1 num s = repeat num (g (s.__.r3 kt)) . r3 kt.o.__
    p2 s = g (s.__.kt.kt.o.__)

e_sai_shiv_5 :: Korvai
e_sai_shiv_5 = date 2023 4 3 $
    exercise $ source "https://www.youtube.com/watch?v=yQlfLMi97lY" $
    comment "thom" $ korvai adi
    [ x2 $ s $ r2 p1
    , x2 $ s $ r3 (g "o_ktookt") . g "oktoktok"
    , x2 $ s $ r2 (g "o_ktooktookt") . g "oktoktok"
    , x2 $ s $ r4 (g "ookt") . "o_ktookt" . g "oktoktok"
    ]
    where p1 = g "o_ktooktoktoktok"

e_sai_shiv_7 :: Korvai
e_sai_shiv_7 = date 2023 4 4 $
    exercise $ source "https://www.youtube.com/watch?v=pmTW9hgsSB0" $
    comment "gumiki" $ korvaiS adi
    [ let p1 s = s.__4 . r3 "kook" in mconcatMap p1 tadithomnam
    , let p1 s = s.__4 . r3 "oktk" in mconcatMap p1 tadithomnam
    , let p1 s = s.__4 . r3 "okTk" in mconcatMap p1 tadithomnam
    , r2 $ r3 "o_kt"."ktpk"
    , r2 $ sd $ "nddn" & su "o/o/o/o/" . "nddn"
    ]

e_sai_shiv_9 :: Korvai
e_sai_shiv_9 = date 2023 4 16 $
    exercise $ source "https://www.youtube.com/watch?v=yKv1GveMB94" $
    comment "double strokes" $ korvaiS1 adi $
    tdtn (__8 . r4 takadinnataka)
    . tdtn (__6 . r3 takadinnataka)
    . tdtn (__4 . r2 takadinnataka)
    . tdtn (__ . takadinnataka)
    . tdtn (__ . "kook")
    . tdtn (__ . "ko")
    . sd (g "ppkkoonn") . sd (g "pkon")
    where
    tdtn = g • prefixes tadithomnam
    takadinnataka = "kooknk"

e_sai_shiv_11 :: Korvai
e_sai_shiv_11 = date 2023 3 20 $
    exercise $ source "https://www.youtube.com/watch?v=9l_RlC54pNU" $
    comment "meetu clarity" $ korvaiS adi
    [ seqs 4 . seqs 2 . seqs 1 . tri123 (od.__4) ending
    ]
    where
    seqs times = mconcatMap (seq times) tadithomnam
    seq times st = repeat times (st.__."ktpk") . ending
    ending = g $ on.__. su "ktpk oknpknpk"

e_sai_shiv_12 :: Korvai
e_sai_shiv_12 = date 2023 3 20 $
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

-- *

e_shankar_prasad_1 :: Korvai
e_shankar_prasad_1 = date 2023 4 4 $
    exercise $ source "https://www.youtube.com/watch?v=uyLXzYGsNp8" $
    korvaiS adi
    [ mconcatMap p1 tadithomnam
    , mconcatMap p2 tadithomnam
    , p3 k
    ]
    where
    p1 s = s.__."ktktpk"."Tknpk_v_"
    p2 s = p1 s . g "oTknpk_v_" . g "otknpk_"
    p3 s = s.__."ktktpk" . r2 ("Tknpk_"."ok") . "Tknpk_v_"


tadithomnam :: [Sequence]
tadithomnam = [p, k, o, n]

-- *

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

-- Tabla exercise from Swapan Chaudhuri.
e_swapan :: Korvai
e_swapan = exercise $ date 2023 8 28 $ korvaiS adi
    [ su $ r2 pattern
    , su $ su $ r4 pattern
    , su $ r2 (replace n d pattern)
    , su $ su $ r4 (replace n d pattern)
    ]
    where
    pattern = "n_pknp tpktpkn___" . "pknptpktpkn_ktpk"

-- *

{-
thom        . / o   ge
tha         - / +   kat
ki          l / k   tet
ta          t       te
nam         n       na
din         d       tin
chapu       u       tA
dheem       i       tun
din + thom  D       dha

intro
   .   1   .   2    .   3   .   4    .   5   .   6    .   7   .   8
                |   o           .|   o           .| o       n k o +
                |           o    |         o   o  | l   o o   n k o

   .   1   .   2    .   3   .   4    .   5   .   6    .   7   .   8
 k n k   n k n k|   . o   u   k n| o     . o   . o| k   o l o l . o
 n k n k   n k n| i   n   i   i i|     o   n     n|   k   o l o l -

[[ repeat 2x
   .   1   .   2    .   3   .   4    .   5   .   6    .   7   .   8
       -   -   o|   k n k n k o +| n k o l o l . o|   k n k n k o -
       o   o   n|     k n k n k o| + n k o l o l n|     k n k n k o

   .   1   .   2    .   3   .   4    .   5   .   6    .   7   .   8
 k + n k n k o -| n k o l o   k .| D   D   +   +  | . D   - k   o -
 - k   n k n k o| - n k o l o o  | n   n   D   D  |     i i   k   o

   .   1   .   2    .   3   .   4    .   5   .   6    .   7   .   8
 k + n k   n   k|   k   n   k   -| k - n - k - n -| k - k t o - o -
 +   k d   k   n|   n   k   n    | - k - n - k - k|t+ii i i   o - o

   .   1   .   2    .   3   .   4    .   5   .   6    .   7   .   8
 . D     k n o -| n k o - o - - .| D   D   +   +  | . o   - k   o
 - n       k n o|   n   o   o o  | n   n   D   D  |   n i i   k   o

   .   1   .   2    .   3   .   4    .   5   .   6    .   7   .   8
 + k n k   n   k|   k   n   k   -| k - n - k - n -| n - k - n - k -
 +   k d   k   d|   d   k   d    | - k - n - k - n| - n - k - n - k

   .   1   .   2    .   3   .   4    .   5   .   6    .   7   .   8
 k     u k . D  | - - o - o - . D|   k - k n k n k| o l . D       -
   i i i k      | o o n o n o   n|   - k - k n k n| k o   n       o

   .   1   .   2    .   3   .   4    .   5   .   6    .   7   .   8
   - o   -   o -|   - o   -   o  | n k o - o   . D|   - . D       -
 o o n   o o n o| o o n   o o n o| k n   o   o   n|   o   n       o
                                    ==> skip to ]] on 2nd repeat

   .   1   .   2    .   3   .   4    .   5   .   6    .   7   .   8
 k - o - o   . D|   . D   u   u  | o   - . o      | - - o - o - . o
 - k - o   o   -| i       i   i i|     o - n      | o o n o n o   n
]]

   .   1   .   2    .   3   .   4    .   5   .   6    .   7   .   8
   k   . o      | +   o   +   o  | o   k o   k o  | . o   + k n o
 k   i   n      | i   k   i   k i| i   o     o   o|       +   k n o

   .   1   .   2    .   3   .   4    .   5   .   6    .   7   .   8
   o     o   o  | . o   k   k o  |   o     o   o  | . o   o     o
 o   o     o   o|   n     k   k o| o   o     o   o|   i   i   i   o

   .   1   .   2    .   3   .   4    .   5   .   6    .   7   .   8
 k   n   n   o  |     o   o     .| o   o          | . o     k   o
   k   n   n   o|       o   o o  | n   n   o   o  |     i i   k   o

   .   1   .   2    .   3   .   4    .   5   .   6    .   7   .   8
 + k n k   n   k|   k   n   k   +| k n o   o   . o|   + n k + n k +
     k n   k   n|   n   k   n   k| + k n o   o   n|     + n k + n k

   .   1   .   2    .   3   .   4    .   5   .   6    .   7   .   8
 n k o + n k n k| o     . o     k| n k o   k   o  | . o
 + n k o + n k n| k   o   n      | k n k o   k   o|   n           o

   .   1   .   2    .   3   .   4    .   5   .   6    .   7   .   8
 o   . o        | o   . o       +| k n o   o   . o|               +
   o   n       o|   o   n        | + k n o   o   n|     i   i   i i

   .   1   .   2    .   3   .   4    .   5   .   6    .   7   .   8
 n k o   k n k  | o     . o   n k| n k     +      |     o   o   . o
 k n   o   k n k|     o   u     n| k i i i   +    | o o   o   o

   .   1   .   2    .   3   .   4    .   5   .   6    .   7   .   8
     . o        |   o            | o   o         .| o       n k o
   o           o|       o     o  |         o   o  |     o o   n k o

   .   1   .   2    .   3   .   4    .   5   .   6    .   7   .   8
 k n k   n k n k| u . o          | o     . o   . o|     o   o   . o
   k n k   n k n| i     kt+ii i i|     o   u     u| o o   o   o   n
-}
