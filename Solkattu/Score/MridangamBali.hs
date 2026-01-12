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

-- TODO or would it be better to notate the composite and figure out
-- a tunggal realization naturally?  In that case it should be like
-- "konnakol" except single letter so it fits.
-- It's too annoying to add new syllables, the whole syllable -> stroke
-- thing still seems clunky and complicated.
legong_pasang :: Korvai
legong_pasang = korvaiV adi $ interleave pasang
    where
    pasang =
        -- TODO a o just doesn't look good as dag tut.
        -- +o maybe? or oO?
        -- +o looks better, but idents have to be lowercase letters.
        -- o i i o ? a i i a?
        -- a u u a is closer to syllables but what about kumpung?
        -- aoeiuy
        -- kp - kapak
        -- tl - kampang
        -- uy - kumpung
        -- oi - de tut?  How about a for strong dag = de + tong?
        -- This would dispense with capitals for consistency.
        [ "________ _a___o_. _a__o_o. a_ootTao"
        , "kPkPtTtT Y.a_Y_YY a_o.a_.a kPaoao.a"
        ]
        -- use o O
        -- [ "________ _o___O_. _o__O_O. o_OOtToO"
        -- , "kPkPtTtT Y.o_Y_YY o_O.o_.o kPoOoO.o"
        -- ]
        -- use + o
        -- [ "________ _+___o_. _+__o_o. +_ootT+o"
        -- , "kPkPtTtT Y.+_Y_YY +_o.+_.+ kP+o+o.+"
        -- ]
        -- use o i, all ids
        -- [ "________ _o___i_. _o__i_i. o_iitloi"
        -- , "kpkptltl Y.o_Y_YY o_i.o_.o kpoioi.o"
        -- ]

interleave :: [SequenceK2] -> [Sequence]
interleave xs = mconcat [[realizeWadon x, realizeLanang x] | x <- xs]

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
