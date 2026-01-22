-- Copyright 2026 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Solkattu.Score.KendangPasang where
import           Prelude hiding ((.), repeat)

import           Solkattu.Dsl.KendangPasang


{-
    intro
    ~~ | ~<~ | ~~.o | ~~'o |
    agem kanan
    __ | ~~x | ~~'oo.o | ~~'oo.o | ~5.o 5.o 5.o | ~~'o 5.o | ~~'o |
    agem kiri
    __ | ~~x | ~~'oo.o | ~~'o 5.o | ~~'o 5.o | ~~'o |
    nyregseg
    ~~ | ~~  | ~~ | ~<~ | ~~.o | ~~ oo.o |
    ngumbang
    __ | ~~x | ~o 'o.o.o | __ ...~ | ~<~  | ~~.o | ~~'oo.o |
    [ TODO fill in ]
    __ | ~~x | ~o 'o.o.o | __ ...~ | ~~5 | _ .o 5.o 5.o |
       ~~'oo.o | ~~'oo.o | ~~'oo.o | ~~'o |
    __ .o  | ~~'o | __ o | __ | ~~ | ~<~ | ~~ | ~~ | ~~.o | ~~'oo.o |
    __ | ~~x | ~o 'o.o.o | __ ...~ | ~~5 | _ .o 5.o 5.o | ~~'o | _o ~~5 |
    __ | ~~  | ~o 'o.o.o | __ ...~ | ~<~ | ~~.o | ~~'oo.o |
    __ | ~~x | ~o 'o.o.o | __ ...~ | ~~5 | _ .o 5.o 5.o | ~~'o |
    slow
-}

{-
    ending
    ~~'oo.o | ~~'oo.o | ~~'oo.o | ~~5.o 5.o | ~~'oo.o |
    __ | ~~ | ~o 'o.o.o | __ ...~ | ~~5 | _ .o 5.o 5.o |
       ~~'oo.o | ~~'oo.o | ~~'oo.o | ~~'o 5.o | ~~ oo.o |
    __ | ~~ | ~o 'o.o.o | __ ...~ | ~~5 | _ .o 5.o 5.o | ~~'o | _o ~~5 |
    __ | ~~ | ~~ | ~~x | ~~.o 5.o | ~~'o | ~~.o | ~~'oo.o |
    __ | ~~  | ~o 'o.o.o | __ ...~ | ~~ | ~~.o 5.o | ~~'oo.o |
    __ | ~~x | ~o 'o.o.o | __ ...~ | ~~5 | _ .o 5.o 5.o | ~~'o |
    slow
-}
-- High speed is about 90bpm
bapang_saba_beginning :: Korvai
bapang_saba_beginning = korvaiV adi
    -- ~~.o
    [ sarvaD_ 2 . "ypo_ypo_" . yoyo.__6
    , i_ii . sarvaD_ 2 . su "kpkYY_Y_Ykp_" . __n 3
    , r2 $ su "pkp.o_kpo_ko_o_kp_kpo_o_i_o_" . "__"
    , sarvaD_ 1 . r3 "_yo_" . yoyo . "__yo_"
        . yoyo.__6 . i_ii
    ]
    where
    i_ii = __D 1 . "_i_iio".su "io_o"."iio_"
    yoyo = "_yoyo" . su "yo_o_kpkp.".o

bapang_saba_middle :: Korvai
bapang_saba_middle = korvaiV adi
    [ __D 4 . "_tl.itl.i.ii.i.i" -- transition
    , t1a . t2' . t3 . t4 `replaceEnd` angsel
    , r2 t1b . t2 . t3 . t4 `replaceEnd` angsel -- beginning, angsel
    , r2 t1b . t2 . t3 . t4 -- beginning
    , t1c . t2 . t3 . t4 -- when slow
    , o_i_o.t1b . t2 . t3 . t4 -- when fast
    ]
    where
    t1a = "_Y.o_YYYYkp.i.i." . o_i_o
    t1b = "itl.i.i.o_ptltl."
    t2 =  "itlkptl.i.i.o_o_kpYYYkpo_i_.i.i."
    t2' = "i_tli_i.i.i.o_o_kpYYYkpo_ii.i.i."
    t3 = o_i_o . "i_.o_i_i.o_YYkp."
    t4 = "itlkptltlY.o_" . su "kpkY" . "YYo_i.o_o_ii.i.i."
    angsel = g "Y..iokptl..io___"
    t1c = o_i_o . "itloioi." . su ".kpY" . "YYYkp."
    o_i_o = "o_i.o___i.i.o___"

bapang_saba_accel :: Korvai
bapang_saba_accel = korvaiV adi
    [ r3 "okptl_t_l_o_ioi." . "okpYY_Y_Y_o_p_.i"
    , sd $ r6 "otli" . o.__4 . "pYo_"
    ]

-- * legong

legong1 :: Korvai
legong1 = korvaiV adi
    [ __D 2 . "__o___i_._o__i_i.o_iitloi"
    . "tlkptltli.o_Y_YYo_i.o_.okpoioi.o"

    , "o___i_i_o __tltloitloioi.o__tltloi"
    . "pktltloitloioii .o_o_i_i_.oYYkpoi"
    . "kptl_t_l_l_t_l_" . "_kptlkpt" . su "pkpY" . "YYYoioi"
    . ".o__tloitloioii.o_o_i_i_.oYYkpoi"
    . "kptl_t_l_l_t_l_" . "_kptlkptltlkptlkp"
    . "_YYYp.o_iioioi.o" . "__kptltloi.o___i"
    . "iio_iioiiio_iioi" . "tloioi.o_i.o___i"
    . "kpoioi.oY.o_Y_YY" . "o_i.o___iioioi.o"

    , "_kpY_o___Y_o_Y_oYY_io_i.i.o__kp.i"
    .  "i.i_.i.i.o_kpkpii.i_.i.i.Y_Y_Y.i"
    .  "kptltl.i__.i.ii.o_o_i_i_.oYYkp.i"
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
