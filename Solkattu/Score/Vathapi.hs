-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Score.Vathapi where
import Prelude hiding ((.), repeat)

import Solkattu.MridangamGlobal


    -- pallavi
    --
    -- vaataapi gaNapatim bajEham |         <-- many sangatis
    -- vaaraNaasyam varapradam shree ||

sarva0 = korvaiS1 adi $ repeat 4 $
    repeat 2 (n.l.d.d) & (o.__.o.o.__.o.o.__)

    -- ... varapradam shree ||

fill0 = korvaiS1 adi $ su $ repeat 2 $
    repeat 4 (k.t.k.t.o.__) . n.p.u.p.k.t.o.__
    -- repeat 4 (ta.ri.ki.ta.thom.__) . (di.ku.ta.ri.ki.ta.thom.__)

tir1 = korvaiS1 adi $ restD 2 . tri_ (od.__.o) (su (ktkt.pk).od.od.k) . od
    -- tarikitataka tat din na thom __ ga

    -- anupallavi
    --
    -- bootaadi sam sEvita caraNam |
    -- boota bowdika prapanca baraNam ||

sarva1 = korvaiS1 adi $
    repeat 2 $
    repeat 2 (n.d.__.n) & (o.o.__.o.__.o.__.o)
    . repeat 2 (n.d.__.n) & (o.__n 8)
    -- oonnpktk naka

    -- veetaraagiNam vinata yOginam (shree) |
    -- vishva kaaraNam vigna vaaraNam ||
    -- (vaathapi)

sarva2 = korvaiS1 adi $ repeat 2 $
    on.od.on. su (pk.n.o).od.on . su pk
    -- ta din ta din takadin ta din

    -- caraNam
    --
    -- puraa kumba sambhava munivara prapoojitam |
    -- trikONa madya gatam |

sarva3 = korvaiS1 adi $
      on.__.on.__.on.od.__5.on.__.on.od.__2.o
    . on.k.on.k.on.od.__5.on.k.on.od.__2.o

    -- muraari pramukaat yupaasitam |
    -- moolaadhaara kshEtraa stitam |
    -- paraadi sattvaari vaakaatmagam |

-- sarva4 = korvaiS1 adi $ nakanadin

    -- praNava svaroopa vakratunDam |

sarva5 = korvaiS1 adi $ su $
    repeat 2 $ repeat 3 (yjyj.d.__.lt p.k) . (t.k.o.o.k.t.o.k)
    where yjyj = y.j.y.j

    -- nirandaram niDala candra kaNDam |
    -- nija vaamakara vidrutEkshutanDam |
    -- karaambuja paaSa beejaapooram |
    -- kaloosha vidhooram bootaahaaram |
    -- haraadi guruguha tOshita bimbam |
    -- hamsadhwani booshita hErambam ||


tir2 = korvaiS1 adi $ restD 1 . __. tri_ (od.__.k) (su (p.kt.p.kt.pk) . od.od.k)
tir3 = korvaiS1 adi $ __n 3 . tri_ (od.__.k) (su (p.kt.p.ktkt.pk) . od.od.k)
    -- od.__.k instead of od.__.o, since it starts with p
    -- kitataka tarikitataka tat din na tang __ ga
tir4 = korvaiS1 adi $ tri_ (od.__.o) (su (ktkt.p.kt.p.ktkt.pk) . od.od.k)
