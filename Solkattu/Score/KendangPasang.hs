-- Copyright 2026 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE NoMonomorphismRestriction #-}
module Solkattu.Score.KendangPasang where
import           Prelude hiding ((.), repeat)

import           Solkattu.Dsl.KendangPasang


{-
    __ | ~~ | ~~.o | ~~'o |
    agem kanan
    __ | ~~x | ~~'oo.o | ~~'oo.o | ~4.o 4.o 4.o | ~~'o 4.o | ~~'o |
    agem kiri
    __ | ~~x | ~~'oo.o | ~~'o 4.o | ~~'o 4.o | ~~'o |
    nyregseg
    ~~ | ~~  | ~~ | ~~ | ~~.o | ~~'oo.o | tap
    ngumbang
    __ | ~~ | ~o 'o.o.o | __ ...~ | ~~ | ~~.o | ~~'oo.o |
    duduk
    ~~'oo.o | ~~'oo.o | ~~'o |            stand
    __ .o  | ~~'o | __ o | _~ | ~~ | ~~ | ~~ | ~~ | ~~.o | ~~'oo.o | motorcycle
    swimming
    __ | ~~ | ~o 'o.o.o | __ ...~ | ~~4 | _ .o 4.o 4.o | ~~'o | _o ~~4 | pop up
                 egel2 (wiggle)
    __ | ~~ | ~~ | ~~x | ~~.o 4.o | ~~'o | ~~.o 4.o | ~~'oo.o | tap
    ngumbang circles
    __ | ~~ | ~o 'o.o.o | __ ...~ | ~~ | ~~.o | ~~'o |

    slow

    ~~'oo.o | ~~'oo.o | ~~'oo.o | ~~4.o 4.o | ~~'oo.o | motorcycle
    swimming
    __ | ~~ | ~o 'o.o.o | __ ...~ | ~~4 | _ .o 4.o 4.o | ~~'o | _o ~~4 | pop up
                 egel2 (wiggle)
    __ | ~~ | ~~ | ~~x | ~~.o 4.o | ~~'o | ~~.o 4.o | ~~'oo.o | tap
    ngumbang circles
    __ | ~~ | ~o 'o.o.o | __ ...~ | ~~ | ~~.o | ~~'oo.o |
    __ | ~~ | ~o 'o.o.o | __ ...~ | ~~4 | _ .o 4.o 4.o | ~~ | ~~.o | ~~'o |

    _ ‗     = 1 rest 2 rest
    t l     = kam pang
    k p     = ka pak
    i . o   = tut de dag
    Y       = pung
-}
-- High speed is about 90bpm
bapang_saba_angsels :: Korvai
bapang_saba_angsels = korvaiV adi
    -- ~~.o
    [ sarvaD_ 2 . "ypd_ypd_"    -- ~~.a
        . ydyd.__6              -- ~~'o
    , i_ii                      -- __
        . sarvaD_ 2 . su "kpkYY_Y_Ykp_" . __n 3     -- ~~x
    , sarvaD_ 2 . su "p_kpd_p_o_d_" . __.__         -- ~~'oo.o
        . su "pkpod_kpd_kd_d_kp_kpd_p_o_d_" . "__"  -- ~~'oo.o
    , sarvaD_ 1 . r3 "_yd_"     -- ~5.o 5.o 5.o
        . ydyd . "__yd_"        -- ~~'o 5.o
    ]
    where
    i_ii = __D 1 . "_i_iio".su "io_o"."iio_"
    ydyd = "_ydyd" . su "yd_d_kpkpo".d

bapang_saba_middle :: Korvai
bapang_saba_middle = korvaiV adi
    [ __D 4 . "_tloitloioiioioi" -- transition
    , t1a       . t2' . t3 . t4 `replaceEnd` angsel
    , r2 t1b    . t2  . t3 . t4 `replaceEnd` angsel -- beginning, angsel
    , r2 t1b    . t2  . t3 . t4 -- beginning
    , t1c       . t2  . t3 . t4 -- when slow, melody up
    , d_i_d.t1b . t2  . t3 . t4 -- when fast
    ]
    where
    t1a = "_Yod_YYYYkpoioio" . d_i_d
    t1b = "itloioiod_ptltlo"
    t2 =  "itlkptloioiod_d_kpYYYkpd_i_oioio"
    t2' = "i_tli_ioioiod_d_kpYYYkpd_iioioio"
    t3 = d_i_d . "i_od_i_iod_YYkpo"
    t4 = "itlkptltlYod_" . su "kpkY" . "YYd_iod_d_iioioio"
    angsel = g "Yooidkptlooid___"
    t1c = d_i_d . "itldidio" . su "okpY" . "YYYkpo"
    d_i_d = "d_iod___ioiod___"

bapang_saba_accel :: Korvai
bapang_saba_accel = korvaiV adi
    [ r3 "dkptl_t_l_d_idio" . "dkpYY_Y_Y_d_p_oi"
    , sd $ r6 "dtli" . d.__4 . "pYd_"
    ]

-- * legong

legong1 :: Korvai
legong1 = korvaiV adi
    [ __D 2 . "__d___i_o_d__i_iod_iitloi"
    . "tlkptltliod_Y_YYd_iod_odkpoioiod"

    , "d___i_i_d __tltloitloioiod__tltloi"
    . "pktltloitloioii od_d_i_i_odYYkpoi"
    . "kptl_t_l_l_t_l_" . "_kptlkpt" . su "pkpY" . "YYYoioi"
    . "od__tloitloioiiod_d_i_i_odYYkpdi"
    . "kptl_t_l_l_t_l_" . "_kptlkptltlkptlkp"
    . "_YYYpod_iididiod" . "__kptltldiod___i"
    . "iid_iidiiid_iidi" . "tldidiod_iod___i"
    . "kpdidiodYod_Y_YY" . "d_iod___iididiod"

    , "_kpY_d___Y_d_Y_dYY_id_ioiod__kpoi"
    .  "ioi_oioiod_kpkpiioi_oioioY_Y_Yoi"
    .  "kptltloi__oioiiod_d_i_i_odYYkpoi"
    ]

{-
intro
1  .   1   .   2    .   3   .   4    .   5   .   6    .   7   .   8
                |   o           .|   o           .| o       n k o +
                |           o    |         o   o  | l   o o   n k o

2  .   1   .   2    .   3   .   4    .   5   .   6    .   7   .   8
 k n k   n k n k|   . o   u   k n| o     . o   . o| k   o l o l . o
 n k n k   n k n| i   n   i   i i|     o   n     n|   k   o l o l -

[[ repeat 2x
3  .   1   .   2    .   3   .   4    .   5   .   6    .   7   .   8
       -   -   o|   k n k n k o +| n k o l o l . o|   k n k n k o -
       o   o   n|     k n k n k o| + n k o l o l n|     k n k n k o

4  .   1   .   2    .   3   .   4    .   5   .   6    .   7   .   8
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
