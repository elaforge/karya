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
    takga_gadi = "takga gadi terekita"
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
        r2 (dhenne_gene.naganaga.terekita) . naganaga.terekita
        . dhenne_gene.r2 (naganaga.terekita).dhenne_gene.naganaga.terekita
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

{-
     0 theme
     1 aaab
     2 23 dha 2 dha 2       = (.5) 2 (.5) 2
     3 23 2 dha 2 dha       = 2 (.5) 2 (.5)
     4 23 5*4               = 5*4
     5 23 4 4 4*3           = 4 4 3333 = 5*4
     6 23 _1 4 5*3          = 5 + 15
     7 23 32
     8 322 322 1 ->b        = 7 7 1 = 15
     9 333 dha4 ->b         = 9 + 1 = 10 irregular
    10 23 d6 3 d6 3 d4      = 1.5 + 3 + 1.5 + 3 + 1 = 3 + 6 + 1
    11 23 33   d3 d3 d4     = 6 + 3 + 1
    12 23 3 (66) d3 d3 d4   = 33 -> (66)
    13 23 (66) 66 66 d4     = d3 d3 -> 66 66
    14 23 (666) 22 0.5 d4   = 4.5 + 4 + 0.5 + 1 = 9 + 1
    15 23 (66) (888) d4  = 3 + 6 + 1
    16 b a'
    17 bbb a'
    18 b10 b10 b(16)    = 5 * 2
    19 genedhina _ (genedhina)2 _
    20 tihai
    21 tihai2
-}
c_24_01_11 :: Korvai
c_24_01_11 = date 2024 1 12 $ colby $ kaida $ korvaiS jhaptal tabla
    [ kaliM (9*4) (15*4) $ r2 $ theme1 . theme2                 -- 0
    , r3 theme1 . theme2 -- aaab
    , pat2 . takga_dhinne . tihai pat2 (dha.__) . theme1.theme2
    , theme1 . r2 (pat2.dha.__) . theme1.theme2
    , theme1 . r4 (dha.__5) . theme1.theme2                     -- 4
    , theme1 . r2 (dha.__4) . r4 (dha.__3) . theme1.theme2
    , theme1 . __.dha.__4 . r5 (dha.__3) . theme1.theme2
    , pat2.pat3 . pat3.pat2 . theme1.theme2
    , pat3.pat2.pat2 . pat3.pat2.pat2.takga_dhinne . theme2     -- 8
    , pat3.pat3.pat3.dha.__4 . theme2 -- irregular, 15 beats instead of 20
        . kali (pat3.pat3.pat3.dha.__4) . theme2
    , pat2 . tihai pat3 (dha.__6) . dha.__4 . theme2
    , pat2.r3 pat3 . r2 (dha.__6) . dha.__4 . theme2
    , pat2.pat3.pat3.r2 dhinne_dhinna_gena . r2 (dha.__6).dha.__4 . theme2 -- 12
    , pat2.pat3 . r6 dhinne_dhinna_gena.dha.__4 . theme2
    , pat2.pat3 . r3 dhinne_dhinna_gena . r2 pat2 . tak.ga.dha.__4 . theme2
    , pat2.pat3 . r2 dhinne_dhinna_gena
        . r3 (g "dhinne dhinna gege takga").dha.__4 . theme2
    , theme2.theme1b                                            -- 16
    , r3 theme2.theme1b
    , r2 (takeM 10 theme2).theme2 . theme2.theme1b
    , takeM 10 theme2.__ . r2 "gene dhinna".__ . theme2.theme1b . __M 20
        -- It's a 1.5, which comes to 3 with kali.
    , tihai (takeM 10 theme2 . __ . tihai "gene dhin" "na_") (na.__4)   -- 20
    , tihai (takeM 6 theme2 . r3 (g "gene dhinna gene")) (dha.__4)
    ]
    where
    pat2 = g $ takga_dhinne.dhinna_gena
    pat3 = g $ r2 takga_dhinne.dhinna_gena
    dhinne_dhinna_gena = g $ dhin.ne.dhinna_gena
    theme1 = pat2 . pat3
    theme2 = "dha trekre dhet tette gene" . dhinna_gena . "dhati dhage"
        . dhinna_gena
    theme1b = dha.__8.takga_dhinne . kali pat2
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

to_chalan :: Korvai
to_chalan = date 2024 12 29 $ colby $ chalan $ lucknow $
    korvaiS tintal tablaKinar
    [ kal $ r2 (dhin_dha_redha.tet_dha_redha)
    , kal $ dhin_dha_redha.tet_dha_redha . dhateka_dha_redha.tet_dha_redha
    , kal $ dhin_dha_redha."tet__dha_re dhati".dhateka_dha_redha.tet_dha_redha
    , kal $ dhin_dha_redha."tet__dha_re dhatetka dhati dha_redha_".tet_dha_redha
    , kal $ "dhin__dha_redhatetka" . r2 "dhati dhatetka"
        . "dha_redha_".tet_dha_redha
    -- emphasis on tktrkt
    , kal $ dhin_dha_redha.tet_dha_redha.dhin_dha_redha.__.__."trkttk tktrkt"
    , kal $ dhin_dha_redha.__.__."trkttk tktrkt".dhateka_dha_redha.tet_dha_redha
    -- This one is 2x long, but feels awkward to go back to 1x, only to expand
    -- to 2x again later.  Omit it?
    , kal $
        dhin_dha_redha . r3 (__.__."trkttk tktrkt")
        . dhin_dha_redha.tet_dha_redha . dhateka_dha_redha.tet_dha_redha
    -- TODO nadai3 makes kali2 0 16 incorrect, would evaluation state fix this?
    , kal $ dhin_dha_redha . nadai 3 "trkttk tktrkt".dhateka_dha_redha
        . tet_dha_redha
    , kal $ dhin_dha_redha.tet_dha_redha . "__ trkt dhinagena"
        . dhateka_dha_redha
    , kali2 (-4) 16 $ dhin_dha_redha.tet_dha_redha . "trkt dhinagena"
        . "dhati dhati dhage dhinagena"
    , kali2 (-4) 16 $ dhin_dha_redha.tet_dha_redha
        . "dhina_dhatidha_ gena dhagena dhinagena"
    , kali2 (-4) 48 $ dhin_dha_redha.tet_dha_redha
        . r3 "dhina_dhatidha_ gena dhagena dhinagena"
    , kali2 (-4) 48 $ dhin_dha_redha.tet_dha_redha
        . r2 "dhina_" . r2 "dhatidha_" . r2 "gena dhagena dhina gena"
        . "__trkt dhinagena dhatidhage dhinagena"
    ]
    where
    kal = kali2 0 16
    dhin_dha_redha = "dhin__dha_redha_"
    tet_dha_redha = "tet__dha_redha_"
    dhateka_dha_redha = "dhatetka dha_redha_"

yt_tihais :: Korvai
yt_tihais = korvaiV tintal tablaKinar
    -- Abhishiek Borkar, Bhimpalas at 34:42
    -- tak is left hand finger flick
    -- actually starts at 11.25 because 2x speed (16 - 9.5/2)
    [ __D 6.5 . r2 "ge__ge__tun_tA_tak_ga_" . "ge__ge__tun_na_dha"
    ]

c_24_12_02 :: Korvai
c_24_12_02 = date 2024 12 2 $ source "skanda" $ korvaiS tintal tablaKinar
    -- rotate accent around, or accent on 2nd dha
    [ kali2 (-8) 16 $ sd $ "dhati dhagena dha".trkt."dhati dhage dhina gena"
    , kali2 (-8) 16 $
        dha.__.terekita.taka.dhere.dhere.kitataka
      . dha.__.terekita.taka.dhi.__.na.__.kitataka
    , kali2 (-8) 8 $
        dha.__.gerenaga.terekita.dha.__.gerenaga
      . dha.__.dha.__.gerenaga.dhi.__.na.__.gerenaga
    ]
    where
    -- I do dhi -> tun, but actually it's dhin -> tin, with tun instead of din.
    -- Dhi seems ok since it sounds almost the same, but tun /= tin
    -- tabla = makeTabla Kinar
    --     [ ("tin", tun )
    --     , ("dhin", ge & tun)
    --     ]
    --     where Tabla.Strokes { .. } = Tabla.notes
