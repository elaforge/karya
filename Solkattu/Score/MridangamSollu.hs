-- Copyright 2026 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE NoMonomorphismRestriction #-}
module Solkattu.Score.MridangamSollu where
import           Prelude hiding ((.), repeat)

import qualified Solkattu.Tala as Tala

import           Global
import           Solkattu.Dsl.Mridangam



misc_sollus :: Korvai
misc_sollus = date 2025 12 12 $ korvaiV adi
    [ __D 5 . "pn_no_" . "pknno_D"
    ]

dinna_kitataka :: Korvai
dinna_kitataka = sollu $ sudhindra $ korvaiV adi $
    map (sarvaSam adi) patterns
    where
    patterns = map su
        [ repeat 4 dinna
        , repeat 2 (od.__.dinna).dinna
        , repeat 2 (o.k.dinna) . dinna
        , repeat 2 (o.t.k.n.kttk) . dinna
        , tri (o.k) dinna
        ]
    kttk = su (k.t.o.k)
    dinna = o.n.kttk

sollus :: Korvai
sollus = sollu $ korvaiV adi
    [ sarvaD_ 5.5 . "koD".su "_k"."DkDkD_"
    , sarvaD_ 4.5 . "kktku".su (pk.r2 (g takatiku))
    , sarvaD_ 4.5 . "kktku".su (pk.g takatiku.nakatiku)
    , sarvaD_ 5.5 . "kktku".su (pk.nakatiku)
    , sarvaD_ 5 . su "u_pktpktpu_kt_k_u_pknook"
    , sarvaD_ 6.5 . "rknu".su "ktpk"
    -- from tabla, dha trakra dhet tette dhinna gena
    , sarvaD_ 6 . "NxqTktpk"
    , sarvaD_ 5 . "NxqTktpk Inon"
    , sarvaD_ 6 . "NkNoInon"
    ]
    where
    takatiku = "tpupktpk"

ganesh_sollus :: Korvai
ganesh_sollus = date 2024 12 1 $ sollu $ korvaiV adi
    [ sarvaD_ 6 . su (r2 ("otkn".su ktok) . o.n.su ktok)
    , sarvaD_ 6 . su "_tpknooknpk_pu_k"
    , sarvaD_ 6 . su "kookkookk_oD_N_k"
    , sarvaD_ 6 . su ("oo_".tri "D_" "N_k")
    , sarvaD_ 6 . su ("oo_".tri "D_" "kpk")
    , sarvaD_ 6 . su "okoTknpktkoTknpk" -- or start with t
    , sarvaD_ 6 . su ("okoTkn".su ktpk."tkoTkn".su ktpk)
    , sarvaD_ 6 . su (__.tri "D__" "ook") -- 5*3
    , sarvaD_ 6.25 . k.u.su (p.k.nakatiku)
    , sarvaD_ 6 . su ("kook".nakatiku."nook")
    , sarvaD_ 6 . su ("kookN_pk".nakatiku)
    , sarvaD_ 6 . su "kooknpkD_kD_N_k_" -- 4 + 6
    , sarvaD_ 6 . su "tkpknook npk_pu_k"
    , sarvaD_ 6 . su "npk_pu_kpu_kpu_k"
    , sarvaD_ 6 . su (reduceTo 3 1 "kookD_") -- emphasis on each group
    ]

-- Patterns for resuming sarvalaghu.
reenter :: Korvai
reenter = date 2025 9 13 $ elaforge $ korvaiV adi
    [ __D 1.5 . "kkkoD" . su"_ko_N_ktok" . sarvaD_ 4
    , __D 1.25 . g (su "ktpkpktkno").u.__3.su "n_ktpk" . sarvaD_ 4
    ]

rohan_sollus_end :: Korvai
rohan_sollus_end = rohan $ sollu $ korvaiV adi
    [ __D 3.25 . su "okooko".o . __D 3 . su "otootoo"       -- 75
    , __D 2.5 . "otkn".kttk.od . __D 2.25 . "ktkn".kttk.od  -- 75
    , __D 7 . su "_upknook"                                 -- 75
    , __D 7 . su "nkktkktk"                                 -- 60
    , __D (6 + 6/8) . su "_np nnoD_ onn"                    -- 60
    , __D 2.5 . su (__ . tri od "npk") . od.__n 10 . su (r3 "npkD") -- 75
    , __D 4 . su ("oo_o_oo_" . r2 "___o_oo_" . "o_oo_o_o").od -- 75
    , __D 5 . su ("o___" . r2 (g "ktktpktp") . "ktkt")      -- 85
    , __D 5 . su "pu_ko_k_ pu_ko_k_ n_pu_ko_"               -- 75
    , __D 3 . "ktkt" . "ktkn".kttk."Tk" . "tkoo".kttk."Tk"  -- 60
    , __D 3 . "ktkp" . "ktkn".kttk."Tk" . su "n_ktpkT_k_n_ktpk" -- 60
        . "otkn".kttk.od .__ . __D 6
    , let kttk = "kt" in __D 5.5                            -- 60
        . su ("ktkp ktk" . r2 (n.kttk."Tk") . n.kttk
        . "oktn".kttk).od.__5 . __D 6
    , __D 4 . su ("N___koD_" . "N_k_kpko" . "D_N_k_ko" . "D_N_k_kp") -- 70
    , __D 2 . su "o_k_on,nok N_D_k_" . __D 2 . su "on,npk on,npk N_k_" -- 70
    , __D 4 . su ("o_t_k_N_ktpkoktk" . "nookN_D_k_N_ktpk")  -- 85
    , __D 6 . su ("puko" . r4 "Uko")                        -- 75
    , __D 6 . su "kt_kk_ktkk_oN_k_".od                      -- 75
        -- do with 3x: Nk_, N_k_, N__k_
    , __D 6 . su ("N_kNkNNk" . "NNkNkNNk")                  -- 60
    , __D 6 . su ("ookTkpnn" . "ppkTkonn")                  -- 60
        -- dhere for kTk
    , __D 4 . su ("o_k_okTko_N_ktpk" . "n_k_okTko_N_ktpk")  -- 75
    , __D 4 . "otkNkdpn" . "ptkNkdpn"                       -- 120
    , __D 6 . "Nkdpnoto"
    ]
    where
    kttk = su "ktpk"
    -- for kt, keep thumb tucked in

embellishments :: Korvai
embellishments = sollu $ korvaiV adi
    -- [ su "onpkno" `replaceStart` sarva `replaceEnd` su "onpkno"
    [ startEnd (su "onpkno") sarva
    , startEnd (su "pnpknp") s_nd_k
    , startEnd (su (r2 "onpkno")) sarva
    , startEnd (su "oktkno") sarva
    , startEnd (su "pu_kno") sarva
    , startEnd (su "pu_knpu_kno_") sarva
    ]
    where
    startEnd sol sarva = sol `replaceStart` sarva `replaceEnd` sol
    sarva = r2 "N.dD.dD." . "N.dd.dd.n.dD.dD."
    -- sarva = r2 "n_ddn_ddnoDdn_dd"
    s_nd_k = r2 $ r3 "nd_k" . su "n_o_ktok"

sollu_variations :: Korvai
sollu_variations = sollu $ korvaiV adi
    [ su $ r4 theme
        . r2 (g "Nkdpn ptkNkdpn") . r2 (g "Nkdpn") . tri "d__" "Nk"
        . od.__8.nakatiku
    , su $ theme . r2 (g "Nkdpn ptkNkdpn") . r2 (g "Nkdpn") . tri "d__" "Nk"
        . od
    -- , __D 6 . su "kt_kk_ktkk_oN_k_".od
    ]
    where
    theme = g "otkNkdpn ptkNkdpn"
