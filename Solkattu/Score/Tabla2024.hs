-- Copyright 2024 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Solkattu.Score.Tabla2024 where
import           Prelude hiding ((.))

import           Solkattu.Dsl.Bol


c_24_01_04_rela :: Korvai
c_24_01_04_rela = date 2024 1 4 $ colby $ rela $ korvaiS tintal
    [ kaliMt (-4) 0 $ r2 $ dha_tette . takga_gadi . dha_tette . takga_dhenne
    ]
    where
    dha_tette = "dha_tette tettegene"
    takga_gadi = "takga gadi terekite"
    takga_dhenne = "takga dhenne nana gene"
    -- na on kinar
    _strokes = [("gadi", "ge tun")]

c_24_01_04_rela_jhaptal :: Korvai
c_24_01_04_rela_jhaptal = date 2024 1 4 $ colby $ rela $ korvaiS jhaptal
    [ kaliM (4*4) (7*4) $ su $ r2 $
        dha_tette.tette_gene.takga_dhenne.nana_gene
        . r2 (r2 takga_dhenne.nana_gene)
    ]
    where
    dha_tette = "dha_tette"
    tette_gene = "tette gene"
    takga_dhenne = "takga dhenne"
    nana_gene = "nana gene"
    -- na on kinar

c_24_01_04_rela_tintal :: Korvai
c_24_01_04_rela_tintal = date 2024 1 4 $ colby $ rela $ korvaiS tintal
    [ kaliMt 0 0 $ su $ r2 $
        r2 (dhenne_gene.naganaga.terekite) . naganaga.terekite
        . dhenne_gene.r2 (naganaga.terekite).dhenne_gene.naganaga.terekite
    ]
    where
    dhenne_gene = "dhenne gene"
    naganaga = "naganaga"
    -- na on kinar

c_24_01_04_chakradar :: Korvai
c_24_01_04_chakradar = date 2024 1 4 $ colby $ chakradar $ korvaiS1 tintal $
    nadai 3 $ r3 $
    "kat_tettetette"."katette getette".su "dha_gerenage".trkttk.taa.trkt
    . su (r3 (dha.ge.tetekata)) . "dhadhintaa_kat_"."dha__ghen_te"
    . r3 ("dha_ne". su (dha.ge.tetekata) . "dhatet") . dha.__3

c_24_01_04_rela2 :: Korvai
c_24_01_04_rela2 = date 2024 1 4 $ colby $ benares $ rela $ korvaiS tintal
    [ dhenne_gene.takga.dhenne_gene."dhenne dhati gene"
        . __D 12 -- TODO incomplete
    ]
    where
    dhenne_gene = "dhenne gene"
    takga = "takga"
    -- na on kinar

c_24_01_11 :: Korvai
c_24_01_11 = date 2024 1 12 $ colby $ korvaiS jhaptal
    [ kaliM (9*4) (15*4) $ r2 $
        takga_dhinne . dhinna_gena . r2 takga_dhinne . dhinna_gena
        . "dha trekre dhet tette gene" . "dhinna gena dhati dhage"
        . dhinna_gena
    ]
    where
    takga_dhinne = "takga dhinne"
    dhinna_gena = "dhinna gena"
    -- trekre can also be played trkt
    -- tak is like re, cupped closed stroke

terekite :: Sequence
terekite = "terekita"
