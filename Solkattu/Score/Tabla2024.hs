{-# LANGUAGE RecordWildCards #-}
-- Copyright 2024 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Solkattu.Score.Tabla2024 where
import           Prelude hiding ((.))

import qualified Solkattu.Instrument.Tabla as Tabla

import           Solkattu.Dsl.Bol


c_24_01_04_rela :: Korvai
c_24_01_04_rela = date 2024 1 4 $ colby $ rela $ korvaiS tintal tablaKinar
    [ kaliMt (-4) 0 $ r2 $ dha_tette . takga_gadi . dha_tette . takga_dhenne
    ]
    where
    dha_tette = "dha_tette tettegene"
    takga_gadi = "takga gadi terekite"
    takga_dhenne = "takga dhenne nana gene"

c_24_01_04_rela_jhaptal :: Korvai
c_24_01_04_rela_jhaptal = date 2024 1 4 $ colby $ rela $
    korvaiS jhaptal tablaKinar
    [ kaliM (4*4) (7*4) $ su $ r2 $
        dha_tette.tette_gene.takga_dhenne.nana_gene
        . r2 (r2 takga_dhenne.nana_gene)
    ]
    where
    dha_tette = "dha_tette"
    tette_gene = "tette gene"
    takga_dhenne = "takga dhenne"
    nana_gene = "nana gene"

c_24_01_04_rela_tintal :: Korvai
c_24_01_04_rela_tintal = date 2024 1 4 $ colby $ rela $
    korvaiS tintal tablaKinar
    [ kaliMt 0 0 $ su $ r2 $
        r2 (dhenne_gene.naganaga.terekite) . naganaga.terekite
        . dhenne_gene.r2 (naganaga.terekite).dhenne_gene.naganaga.terekite
    ]
    where
    dhenne_gene = "dhenne gene"
    naganaga = "naganaga"

c_24_01_04_chakradar :: Korvai
c_24_01_04_chakradar = date 2024 1 4 $ colby $ chakradar $
    korvaiS1 tintal tabla $ nadai 3 $ r3 $ g $
    "kat_tettetette"."katette getette".su "dha_gerenage".trkttk.tA.trkt
    . su (r3 (dha.ge.tetekata)) . "dhadhintA_kat_"."dha__ghen_te"
    . r3 ("dha_ne". su (dha.ge.tetekata) . "dhatet") . dha.__3
    where
    tabla = makeTabla Sur
        [ ("dhin", ge & tun)
        , ("tin", ka & tun)
        ] where Tabla.Strokes { .. } = Tabla.notes

c_24_01_11 :: Korvai
c_24_01_11 = date 2024 1 12 $ colby $ kaida $ korvaiS jhaptal tabla
    [ kaliM (9*4) (15*4) $ r2 $ theme1 . theme2
    , r3 theme1 . theme2 -- aaab
    , pat2 . takga_dhinne . tri_ (dha.__) pat2 . theme1.theme2
    , theme1 . r2 (pat2.dha.__) . theme1.theme2
    , theme1 . r4 (dha.__5) . theme1.theme2
    , theme1 . r2 (dha.__4) . r4 (dha.__3) . theme1.theme2
    , theme1 . __.dha.__4 . r5 (dha.__3) . theme1.theme2
    , pat2.pat3 . pat3.pat2 . theme1.theme2
    , pat3.pat2.pat2 . pat3.pat2.pat2.takga_dhinne . theme2
    , pat3.pat3.pat3.dha.__4 . theme2 -- irregular, 15 beats instead of 20
        . kali (pat3.pat3.pat3.dha.__4) . theme2
    , pat2 . tri_ (dha.__6) pat3 . dha.__4 . theme2
    , pat2.r3 pat3 . r2 (dha.__6) . dha.__4 . theme2
    , pat2.pat3.pat3.r2 dhinne_dhinna_gena . r2 (dha.__6) . dha.__4 . theme2
    , pat2.pat3 . r6 dhinne_dhinna_gena.dha.__4 . theme2
    , pat2.pat3 . r3 dhinne_dhinna_gena . r2 pat2 . tak.ga.dha.__4 . theme2
    , pat2.pat3 . r2 dhinne_dhinna_gena
        . r3 (g "dhinne dhinna gege takga").dha.__4 . theme2
    , theme2.theme2b
    , r3 theme2.theme2b
    , r2 (takeM 10 theme2).theme2 . theme2.theme2b
    , takeM 10 theme2.__ . r2 "gene dhinna".__ . theme2.theme2b . __M 20
        -- It's a 1.5, which comes to 3 with kali.
    , tihai (takeM 10 theme2 . __ . tri_ (na.__) "gene dhin") (na.__4)
    , tihai (takeM 6 theme2 . r3 (g "gene dhinna gene")) (dha.__4)
    ]
    where
    pat2 = g $ takga_dhinne.dhinna_gena
    pat3 = g $ r2 takga_dhinne.dhinna_gena
    dhinne_dhinna_gena = g $ dhin.ne.dhinna_gena
    theme1 = pat2 . pat3
    theme2 = "dha trekre dhet tette gene" . dhinna_gena . "dhati dhage"
        . dhinna_gena
    theme2b = dha.__8.takga_dhinne . kali pat2
    -- This identifies this as kaida rather than rela: dhinne tak is hard to
    -- play quickly, also dhati dhage tuna kena is typical of kaida.
    takga_dhinne = "takga dhinne"
    dhinna_gena = "dhinna gena"
    tabla = makeTabla Kinar
        [ ("tak", re)
        -- dhin tin are on tun, I think this is a regular pattern like with
        -- Kinar.  TODO Is there a way to infer it?
        , ("dhin", tun)
        ] where Tabla.Strokes { .. } = Tabla.notes

c_24_01_26_chalan1 :: Korvai
c_24_01_26_chalan1 = date 2024 1 26 $ colby $ chalan $ lucknow $
    korvaiS tintal tabla
    [ kaliMt (-3) 0 $ su $ r2 $
        "dha_dha_dha_ gege" . nage_dhine.dhina_gena
        . "tette gege".nage_dhine . nage_dhine.dhina_gena
        . "kitataka terekita" . nage_dhine.dhina_gena
        . "dhara gege".nage_dhine . nage_dhine.dhina_gena
    ]
    where
    nage_dhine = "nage dhine"
    dhina_gena = "dhina gena"
    tabla = makeTabla Kinar
        [ ("ra", na)
        , ("kitataka", ka.na.na.ka)
        ]
        where Tabla.Strokes { .. } = Tabla.notes

-- try at 60bpm
c_24_01_26_chalan2 :: Korvai
c_24_01_26_chalan2 = date 2024 1 26 $ colby $ chalan $ lucknow $
    korvaiS tintal tablaKinar
    [ kaliMt (-2) 0 $ su $ r2 $
        "dhin__dha_redha_" . "tet__dha_redha_" . "dhina_dhatidha_"
        . "gena dhagena dhina gena" . "trkt dhina gena" . "dhatidha gena"
        . "dha__dha__gena" . "dhati dhagena dhati dhage dhina gena"
    ]

terekite :: Sequence
terekite = "terekita"
