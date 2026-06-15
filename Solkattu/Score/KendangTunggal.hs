-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Score.KendangTunggal where
import           Prelude hiding ((.), repeat)

import qualified Solkattu.Score.Mohra as Mohra
import qualified Solkattu.Tala as Tala

import           Solkattu.Dsl.Kendang


farans :: Korvai
farans = faran $ korvaiV adi $ map su $ concat
    [ map (make (t.k.p.k) (t.k.p.k . p.k)) -- (p.n.p.k) (p.n.p.k . t.k)
        [ k.p.k.t.p.k.o.k -- k.t.k.n.p.k.t.k
        , o.o.k.t.p.k.o.k -- o.o.k.n.p.k.t.k
        , o.o.t.t.p.k.o.k -- o.o.n.n.p.k.t.k
        , o.p.k.t.p.k.o.k -- o.t.k.n.p.k.t.k
        , i.__.i.t.p.k.o.k -- od.__.od.n.p.k.t.k
        , i.t.i.t.p.k.o.k -- o.d.o.n.p.k.t.k
        , i.p.i.t.p.k.o.k -- o.k.o.n.p.k.t.k
        , pk.p.o.t.p.k.o.k -- o&t.k.o.n.p.k.t.k
        , p.u.__.t.p.k.o.k -- p.u.__.n.p.k.t.k
        , i.u.__.t.p.k.o.k -- o.u.__.n.p.k.t.k
        ]
    , map (make (i.u.__.k) (i.u.__.k.p.k)) -- (o.u.__.k) (o.u.__.k . t.k)
        [ i.u.__.p.p.o.i.k
        , i.u.k.p.p.o.i.k
        , o.p.i.u.__.k.o.k
        , o.p.i.u.p.k.o.k
        ]
    , map (make (o.__.p.__) (o.t.p.k.o.k)) --  (o.__.k.__) (o.k.p.k . t.k)
        [ o.p.o.o.p.o.o.p -- o.k.o.o.k.o.o.k
        , o.__.p.o.p.o.t.p -- o.__.k.o.k.o.o&t.k
        , o.o.p.o.p.o.t.p -- o.o.k.o.k.o.o&t.k
        , o.__.p.k.p.o.t.p -- o.__.k.t.k.o.o&t.k
        , o.o.p.k.p.o.t.p -- o.o.k.t.k.o.o&t.k
        , p.__.p.t.p.o.t.p -- k.__.k.t.k.o.o&t.k
        , p.k.p.t.p.o.t.p -- k.p.k.t.k.o.o&t.k
        , t.p.k.k.p.o.i.p -- n.k.p.p.k.o.o.k
        ]
    -- , [ make (i.i.k.t) (p.k.p.k . t.k) (p.k.i.i.k.t.p.k)
    , [ make (o.o.p.k) (p.k.p.k.t.k) (p.k.o.o.p.k.t.k)
      -- , make (n.i.i&k.__) (i&k.__.u.__ . p.k) (n.i.i&k.__.u.__.p.k)
      , make (t.i.pk.__) (pk.__.u.__.p.k) (t.i.pk.__.u.__.p.k)
      ]
    ]
    where
    make fill1 fill2 pattern =
        long . long
        . group pattern . group pattern . long
        . r2 short . fill1 . long
        . r3 short . fill2 . nakatiku
        where
        long = group pattern . nakatiku
        short = takeM 6 pattern

exercise1 :: Korvai
exercise1 = exercise $ date 2026 1 25 $ korvaiV Tala.any_beats
    [ r4 "kipp" -- TODO kipp or ippk?  Last does seem to start on i.
    , r4 "kipkpp"
    , r4 "kipkppkp"
    , "ipkppkpkip.o_kpi_kp.o_kpkpkpkppk"
    ]
    -- Should this be u for kum?

korvais :: Korvai
korvais = korvaiV adi
    [ tri "o_pk" nakatiku
    , reduce3 2 p5 "p_p_yooyo__" . trin "o__" p5 ("pk".p5) ("pktk".p5)
    ,    tri "o_" (su $ reduce3 2 ø "p_p_ekpktoyeo_" . "kpkp")
    ]

karaikudi_korvai :: Korvai
karaikudi_korvai = date 2026 4 28 $ source "Karaikudi Mani" $ korvaiS1 adi $
    su $ reduce3x 4 2 theme . tri "__" (r3 "po_" . r3 p7)
    where theme = "p_p_.kpktoy.o_"
    -- where theme = "p_p_okpktoyod_"

reduce3x :: Pretty sollu => FMatra -> FMatra -> SequenceT sollu
    -> SequenceT sollu
reduce3x to by seq = mconcatMap r3 (reduceToL to by seq)

sarva :: Korvai
sarva = sarvalaghu $ korvaiV adi
    [ "i_t_t_upktt_t_i_" . "i_t_t_i_it_tt_to" -- nddn
    , r4 "o_po_itp" -- d_nd_dn_
    , r4 "o__tp_tp" -- o__nd_Nd
    ]

mohra_sequence :: Korvai
mohra_sequence = korvaiV adi $ map su
    -- tang gu, tarikitataka
    [ let trkttk = "tkttkt" in
        r2 (sd "o__" . trkttk) . sd "o_p_" . r2 (sd "i__" . trkttk) . nakatiku
    -- thom takadit thom kitataka
    , r2 "t_.kp_o_pkpk" . nakatiku . r2 ("o_pk".nakatiku) . nakatiku
    -- takita takita
    , r2 "tkppkppkppkt" . nakatiku . r2 "pkppkttkppkp" . nakatiku
    -- faran
    , r2 ("pkuopkpk" . nakatiku) . r3 "pkoupkpk" . nakatiku
    , r5 "pkuo" . "pkpk" . nakatiku . r5 "pkuo" . "P_pk".nakatiku
    , r3 (r2 ("o_pk".nakatiku) . nakatiku) . tri "o___" nakatiku
    ]

k_mohra :: Korvai
k_mohra = mohra $ korvaiS1 adi $ Mohra.make su  Mohra.A1
    ( "P___" . "y_pk".nakatiku
    , "tkp_y_pk".nakatiku
    , "P___" . "y_pk".nakatiku
    )
    ( "iy_po_i_o_i_o___"
    , "iy_po___"
    , tri "o___" "iy_po_i_"
    )

{-
    - farans
    - korvais
    - sarvalaghu
    - p5 pattern variations
    - mohra korvai sequence
-}
