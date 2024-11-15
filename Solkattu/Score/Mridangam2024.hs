-- Copyright 2024 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Score.Mridangam2024 where
import           Prelude hiding ((.), repeat)

import qualified Solkattu.Tala as Tala

import           Global
import           Solkattu.Dsl.Mridangam


misc_sarva :: Korvai
misc_sarva = date 2024 7 5 $ elaforge $ sarvalaghu $ korvaiS adi
    [ "D_" . r4 "kd_" . r2 (g "k_kd_") . r2 "kd_" . "k_"
    , "D_" . r4 "kd_" . r2 (g "k_kd_") . "kd_" . su "N_ktoko_k_"
    , "D_" . r4 "kd_" . r3 (g "k_kd_") . "kd_" -- 2 12 15 3
    , "D_" . r5 "kd_" . r3 (g "k_kd_") -- 2 15 15
    , r5 "kd_" . r3 (g "k_kd_") . "k_" -- 15 15 2
    , "D_" . r4 "kd_" . r2 (g "k_kd_") . "knpnd_k_"
    , "D_" . r4 "kd_" . g "k_kd_" . r2 "knpnd_" . "k"
    , "D_" . r4 "kd_" . "k_" . "knpnd_" . "knpnpnpnd_"
    ]
    -- for decrescendo, switch from "D_kD_" to "d_nd_nd"...

e_24_06_30 :: Korvai
e_24_06_30 = date 2024 6 30 $ ganesh $ exercise $
    comment "use different sarvalaghu, add speed w/ simpler sarva" $ korvaiS adi
    [ tkon $ su ("ktktpk".nakatiku) . sarvaD_ 2 ]
    where
    tkon = prefixes [p, k, o, n]
    -- r2 NNNdnNNd
    -- r2 NDNdnDNd
    -- NDDNNDDN

e_24_06_30_koraippu :: Korvai
e_24_06_30_koraippu = date 2024 6 30 $ ganesh $ koraippu $ korvaiS adi
    [ kor "pk" 5
    , kor (su "n_ktpk") 4
    , kor "kook" 3
    , kor (su "kpk_oD_N_k") 2
    , kor (su "kpnpk_oD_N_k") 1
    , kor p7 0
    , __D 2 . (p7b.p7).(p7b'.p7').p7b . tri p7
    ]
    where
    kor pat gap = __D 2 . tri_ pat (p7a.__M gap) . tri_ (karv gap) pat
    karv n = if n <= 0 then mempty else od.__n n
        -- TODO this is one of those karvai filled in with D, which should
        -- disappear when the karvai becomes 0
    p7a = g "N_kD_kN"
    p7b = g "N,D,ND,"
    p7b' = g "n,d,nd,"
    p7 = su "onpknok_oD_N_k"
    p7'= su "pnpknpk_oD_N_k"

c_24_08_18 :: Korvai
c_24_08_18 = date 2024 8 18 $ ganesh $
    similarTo "Solkattu2016" "c_16_12_06_sriram1" $ korvai adi $ variations $
    -- development:
    -- === dropD 1, dropD 1, dropD 0
    -- 1/2 avartanam: dropD 2, dropD 1, dropD 2, dropD 0
    -- TODO suppress technique tk -> kk, I really want t_k_
    [ sarvaD_ 5.5 . "P_" . su (ktkn.nakatiku)
    . sarvaD_ 5 . "P_X_" . su (ktkn.nakatiku)
    . sarvaD_ 2 . su (ktkn.nakatiku) . sarvaD_ 1.5 . "P_" . su (ktkn.nakatiku)
    . sarvaD_ 2 . su (ktkn.nakatiku) . sarvaD_ 1 . "P_X_" . su (ktkn.nakatiku)
    ] ++ map su
    [ sequence ("P_X_ktknokTk".nakatiku."t_k_oktkokokou_k") (o.__6)
        [0, 0, 0, 5, 5, 7, 7]
    , sequence theme5 (o.__6) [0, 1, 2, 6, 6, 8, 8]
    , sequence theme52 (od.__8) [0, 1, 2, 6, 6, 8, 8, 10, 10]
    , sequence ("P___X___ktknokTk".nakatiku."D_kpkD_kookpkD_k") (od.__6)
        [0, 1, 2, 6, 6, 8, 8]
    ]
    where
    sequence theme karvai drops = mconcat
        [ dropD d theme . if last then p else karvai
        | (d, last) <- zipLast drops
        ]
    ktkn = "ktknokTk"
    theme5 = "P___X___".ktkn.nakatiku."t_k_oktkokokou_k"
    theme52 = theme5."o_k_"

c_24_09_28 :: Korvai
c_24_09_28 = date 2024 9 28 $ ganesh $
    comment "tisram pattern works with any 21212" $ korvai adi
    [ s $ r2 (sarvaD_ 6 . theme)
    . r3 (sarvaD_ 2 . theme) . sarvaD_ 2 .  su "/ktkoktk/ktkoktk"
    , s $ __D 1 . r4 (theme . d.__4) . nadai 3 (__.theme)
    , eddupu 1 $ s $ theme.d.__4 . theme.d.__4 . nadai 3 (__.theme).od
    ]
    where
    theme = su "/_kt/_kt/_ktktok"

zipLast [] = []
zipLast [x] = [(x, True)]
zipLast (x:xs) = (x, False) : zipLast xs


{-
-- TODO figure out why ghc gives "Ambiguous type variable 'sollu0' when an
-- unused binding goes through a function... and only for IsString?
thing :: SequenceT sollu -> SequenceT sollu
thing = id

xyz = 'a'
    where
    z = thing "abc"
-}

{-
    In k t k kto sequence in 3s final k__3t__3 k t k kto -> kk_tt_k t k kto
    in 2s, r2 ("k_t_" . su "ktpkpktkno") . "PPXX".su "ktpkpktkno"
-}
