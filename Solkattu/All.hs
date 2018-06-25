-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- automatically generated by extract_korvais
-- | Collect korvais into one database.
-- This is automatically generated, but checked in for convenience.
-- Don't edit it directly.  Any modifications to the the source
-- directory should cause it to be regenerated.
module Solkattu.All where
import qualified Solkattu.Korvai as Korvai
import Solkattu.Metadata
import qualified Solkattu.Score.Mridangam2013
import qualified Solkattu.Score.Mridangam2016
import qualified Solkattu.Score.Mridangam2017
import qualified Solkattu.Score.Mridangam2018
import qualified Solkattu.Score.MridangamSarva
import qualified Solkattu.Score.Solkattu2013
import qualified Solkattu.Score.Solkattu2014
import qualified Solkattu.Score.Solkattu2016
import qualified Solkattu.Score.Solkattu2017
import qualified Solkattu.Score.Solkattu2018
import qualified Solkattu.Score.SolkattuMohra


korvais :: [Korvai.Korvai]
korvais = map Korvai.inferMetadata
    [ setLocation ("Solkattu.Score.Mridangam2013",15,"dinnagina_sequence") Solkattu.Score.Mridangam2013.dinnagina_sequence
    , setLocation ("Solkattu.Score.Mridangam2013",85,"t_17_02_13") Solkattu.Score.Mridangam2013.t_17_02_13
    , setLocation ("Solkattu.Score.Mridangam2013",98,"din_nadin") Solkattu.Score.Mridangam2013.din_nadin
    , setLocation ("Solkattu.Score.Mridangam2013",105,"nadin_ka") Solkattu.Score.Mridangam2013.nadin_ka
    , setLocation ("Solkattu.Score.Mridangam2013",110,"nadindin") Solkattu.Score.Mridangam2013.nadindin
    , setLocation ("Solkattu.Score.Mridangam2013",130,"nadindin_negative") Solkattu.Score.Mridangam2013.nadindin_negative
    , setLocation ("Solkattu.Score.Mridangam2013",143,"namita_dimita") Solkattu.Score.Mridangam2013.namita_dimita
    , setLocation ("Solkattu.Score.Mridangam2013",160,"janahan_exercise") Solkattu.Score.Mridangam2013.janahan_exercise
    , setLocation ("Solkattu.Score.Mridangam2013",164,"nakanadin") Solkattu.Score.Mridangam2013.nakanadin
    , setLocation ("Solkattu.Score.Mridangam2013",171,"farans") Solkattu.Score.Mridangam2013.farans
    , setLocation ("Solkattu.Score.Mridangam2013",217,"eddupu6") Solkattu.Score.Mridangam2013.eddupu6
    , setLocation ("Solkattu.Score.Mridangam2013",228,"eddupu10") Solkattu.Score.Mridangam2013.eddupu10
    , setLocation ("Solkattu.Score.Mridangam2016",12,"t_16_11_14") Solkattu.Score.Mridangam2016.t_16_11_14
    , setLocation ("Solkattu.Score.Mridangam2017",11,"c_17_07_10") Solkattu.Score.Mridangam2017.c_17_07_10
    , setLocation ("Solkattu.Score.Mridangam2017",15,"e_1") Solkattu.Score.Mridangam2017.e_1
    , setLocation ("Solkattu.Score.Mridangam2017",24,"e_2") Solkattu.Score.Mridangam2017.e_2
    , setLocation ("Solkattu.Score.Mridangam2018",12,"e_323_1") Solkattu.Score.Mridangam2018.e_323_1
    , setLocation ("Solkattu.Score.Mridangam2018",26,"e_323_2") Solkattu.Score.Mridangam2018.e_323_2
    , setLocation ("Solkattu.Score.Mridangam2018",36,"e_18_03_19") Solkattu.Score.Mridangam2018.e_18_03_19
    , setLocation ("Solkattu.Score.Mridangam2018",44,"e_18_03_28") Solkattu.Score.Mridangam2018.e_18_03_28
    , setLocation ("Solkattu.Score.Mridangam2018",49,"e_18_05_25") Solkattu.Score.Mridangam2018.e_18_05_25
    , setLocation ("Solkattu.Score.Mridangam2018",60,"tir_18_06_15") Solkattu.Score.Mridangam2018.tir_18_06_15
    , setLocation ("Solkattu.Score.Mridangam2018",92,"e_18_06_22") Solkattu.Score.Mridangam2018.e_18_06_22
    , setLocation ("Solkattu.Score.MridangamSarva",18,"kir1") Solkattu.Score.MridangamSarva.kir1
    , setLocation ("Solkattu.Score.MridangamSarva",23,"kir2") Solkattu.Score.MridangamSarva.kir2
    , setLocation ("Solkattu.Score.MridangamSarva",43,"kir3") Solkattu.Score.MridangamSarva.kir3
    , setLocation ("Solkattu.Score.MridangamSarva",49,"kir4") Solkattu.Score.MridangamSarva.kir4
    , setLocation ("Solkattu.Score.MridangamSarva",54,"kir5") Solkattu.Score.MridangamSarva.kir5
    , setLocation ("Solkattu.Score.MridangamSarva",63,"mel1") Solkattu.Score.MridangamSarva.mel1
    , setLocation ("Solkattu.Score.MridangamSarva",68,"mel2") Solkattu.Score.MridangamSarva.mel2
    , setLocation ("Solkattu.Score.MridangamSarva",75,"dinna_kitataka") Solkattu.Score.MridangamSarva.dinna_kitataka
    , setLocation ("Solkattu.Score.MridangamSarva",89,"farans") Solkattu.Score.MridangamSarva.farans
    , setLocation ("Solkattu.Score.MridangamSarva",102,"kir6") Solkattu.Score.MridangamSarva.kir6
    , setLocation ("Solkattu.Score.MridangamSarva",124,"kir_misra_1") Solkattu.Score.MridangamSarva.kir_misra_1
    , setLocation ("Solkattu.Score.MridangamSarva",130,"kir_misra_2") Solkattu.Score.MridangamSarva.kir_misra_2
    , setLocation ("Solkattu.Score.MridangamSarva",135,"c_17_10_23a") Solkattu.Score.MridangamSarva.c_17_10_23a
    , setLocation ("Solkattu.Score.MridangamSarva",141,"c_17_10_23b") Solkattu.Score.MridangamSarva.c_17_10_23b
    , setLocation ("Solkattu.Score.MridangamSarva",147,"c_18_05_25") Solkattu.Score.MridangamSarva.c_18_05_25
    , setLocation ("Solkattu.Score.Solkattu2013",21,"c_13_07_23") Solkattu.Score.Solkattu2013.c_13_07_23
    , setLocation ("Solkattu.Score.Solkattu2013",28,"c_13_08_14") Solkattu.Score.Solkattu2013.c_13_08_14
    , setLocation ("Solkattu.Score.Solkattu2013",68,"c_yt1") Solkattu.Score.Solkattu2013.c_yt1
    , setLocation ("Solkattu.Score.Solkattu2013",80,"c_13_10_29") Solkattu.Score.Solkattu2013.c_13_10_29
    , setLocation ("Solkattu.Score.Solkattu2013",94,"c_13_11_05") Solkattu.Score.Solkattu2013.c_13_11_05
    , setLocation ("Solkattu.Score.Solkattu2013",102,"c_13_11_12") Solkattu.Score.Solkattu2013.c_13_11_12
    , setLocation ("Solkattu.Score.Solkattu2013",119,"c_13_12_11") Solkattu.Score.Solkattu2013.c_13_12_11
    , setLocation ("Solkattu.Score.Solkattu2013",157,"k1_1") Solkattu.Score.Solkattu2013.k1_1
    , setLocation ("Solkattu.Score.Solkattu2013",174,"k1_2") Solkattu.Score.Solkattu2013.k1_2
    , setLocation ("Solkattu.Score.Solkattu2013",187,"k1_3") Solkattu.Score.Solkattu2013.k1_3
    , setLocation ("Solkattu.Score.Solkattu2013",221,"k3s") Solkattu.Score.Solkattu2013.k3s
    , setLocation ("Solkattu.Score.Solkattu2013",266,"t1s") Solkattu.Score.Solkattu2013.t1s
    , setLocation ("Solkattu.Score.Solkattu2013",288,"t2s") Solkattu.Score.Solkattu2013.t2s
    , setLocation ("Solkattu.Score.Solkattu2013",321,"t3s") Solkattu.Score.Solkattu2013.t3s
    , setLocation ("Solkattu.Score.Solkattu2013",358,"t4s2") Solkattu.Score.Solkattu2013.t4s2
    , setLocation ("Solkattu.Score.Solkattu2013",383,"t4s3") Solkattu.Score.Solkattu2013.t4s3
    , setLocation ("Solkattu.Score.Solkattu2013",406,"t5s") Solkattu.Score.Solkattu2013.t5s
    , setLocation ("Solkattu.Score.Solkattu2013",462,"koraippu_misra_no_karvai") Solkattu.Score.Solkattu2013.koraippu_misra_no_karvai
    , setLocation ("Solkattu.Score.Solkattu2013",505,"koraippu_misra") Solkattu.Score.Solkattu2013.koraippu_misra
    , setLocation ("Solkattu.Score.Solkattu2013",543,"tir_18") Solkattu.Score.Solkattu2013.tir_18
    , setLocation ("Solkattu.Score.Solkattu2014",17,"c_14_01_01") Solkattu.Score.Solkattu2014.c_14_01_01
    , setLocation ("Solkattu.Score.Solkattu2014",42,"c_14_01_14") Solkattu.Score.Solkattu2014.c_14_01_14
    , setLocation ("Solkattu.Score.Solkattu2014",78,"c_14_02_05") Solkattu.Score.Solkattu2014.c_14_02_05
    , setLocation ("Solkattu.Score.Solkattu2014",118,"c_14_02_20") Solkattu.Score.Solkattu2014.c_14_02_20
    , setLocation ("Solkattu.Score.Solkattu2014",146,"c_14_02_27") Solkattu.Score.Solkattu2014.c_14_02_27
    , setLocation ("Solkattu.Score.Solkattu2014",181,"c_14_03_13") Solkattu.Score.Solkattu2014.c_14_03_13
    , setLocation ("Solkattu.Score.Solkattu2014",203,"c_14_03_26") Solkattu.Score.Solkattu2014.c_14_03_26
    , setLocation ("Solkattu.Score.Solkattu2014",230,"c_14_04_21") Solkattu.Score.Solkattu2014.c_14_04_21
    , setLocation ("Solkattu.Score.Solkattu2014",248,"c_14_04_29") Solkattu.Score.Solkattu2014.c_14_04_29
    , setLocation ("Solkattu.Score.Solkattu2014",284,"c_14_06_06") Solkattu.Score.Solkattu2014.c_14_06_06
    , setLocation ("Solkattu.Score.Solkattu2016",13,"c_16_09_28") Solkattu.Score.Solkattu2016.c_16_09_28
    , setLocation ("Solkattu.Score.Solkattu2016",39,"c_16_12_06_sriram1") Solkattu.Score.Solkattu2016.c_16_12_06_sriram1
    , setLocation ("Solkattu.Score.Solkattu2016",75,"c_16_12_06_sriram2") Solkattu.Score.Solkattu2016.c_16_12_06_sriram2
    , setLocation ("Solkattu.Score.Solkattu2016",97,"c_16_12_06_janahan1") Solkattu.Score.Solkattu2016.c_16_12_06_janahan1
    , setLocation ("Solkattu.Score.Solkattu2016",106,"c_16_12_06_janahan2") Solkattu.Score.Solkattu2016.c_16_12_06_janahan2
    , setLocation ("Solkattu.Score.Solkattu2017",20,"koraippu_janahan") Solkattu.Score.Solkattu2017.koraippu_janahan
    , setLocation ("Solkattu.Score.Solkattu2017",79,"e_spacing") Solkattu.Score.Solkattu2017.e_spacing
    , setLocation ("Solkattu.Score.Solkattu2017",94,"c_17_02_06") Solkattu.Score.Solkattu2017.c_17_02_06
    , setLocation ("Solkattu.Score.Solkattu2017",104,"c_17_03_20") Solkattu.Score.Solkattu2017.c_17_03_20
    , setLocation ("Solkattu.Score.Solkattu2017",127,"c_17_09_25") Solkattu.Score.Solkattu2017.c_17_09_25
    , setLocation ("Solkattu.Score.Solkattu2017",152,"c_17_04_04") Solkattu.Score.Solkattu2017.c_17_04_04
    , setLocation ("Solkattu.Score.Solkattu2017",178,"c_17_04_23") Solkattu.Score.Solkattu2017.c_17_04_23
    , setLocation ("Solkattu.Score.Solkattu2017",203,"c_17_05_10") Solkattu.Score.Solkattu2017.c_17_05_10
    , setLocation ("Solkattu.Score.Solkattu2017",251,"c_17_05_11") Solkattu.Score.Solkattu2017.c_17_05_11
    , setLocation ("Solkattu.Score.Solkattu2017",279,"c_17_05_19") Solkattu.Score.Solkattu2017.c_17_05_19
    , setLocation ("Solkattu.Score.Solkattu2017",285,"c_17_05_19_janahan") Solkattu.Score.Solkattu2017.c_17_05_19_janahan
    , setLocation ("Solkattu.Score.Solkattu2017",309,"c_17_06_02_janahan") Solkattu.Score.Solkattu2017.c_17_06_02_janahan
    , setLocation ("Solkattu.Score.Solkattu2017",322,"c_17_06_15") Solkattu.Score.Solkattu2017.c_17_06_15
    , setLocation ("Solkattu.Score.Solkattu2017",337,"c_17_06_19") Solkattu.Score.Solkattu2017.c_17_06_19
    , setLocation ("Solkattu.Score.Solkattu2017",363,"c_17_06_19_koraippu") Solkattu.Score.Solkattu2017.c_17_06_19_koraippu
    , setLocation ("Solkattu.Score.Solkattu2017",388,"c_17_07_13") Solkattu.Score.Solkattu2017.c_17_07_13
    , setLocation ("Solkattu.Score.Solkattu2017",482,"c_17_07_19") Solkattu.Score.Solkattu2017.c_17_07_19
    , setLocation ("Solkattu.Score.Solkattu2017",495,"c_17_08_21") Solkattu.Score.Solkattu2017.c_17_08_21
    , setLocation ("Solkattu.Score.Solkattu2017",517,"c_17_08_29") Solkattu.Score.Solkattu2017.c_17_08_29
    , setLocation ("Solkattu.Score.Solkattu2017",580,"c_17_10_23") Solkattu.Score.Solkattu2017.c_17_10_23
    , setLocation ("Solkattu.Score.Solkattu2017",640,"c_17_12_11") Solkattu.Score.Solkattu2017.c_17_12_11
    , setLocation ("Solkattu.Score.Solkattu2017",660,"speaking1") Solkattu.Score.Solkattu2017.speaking1
    , setLocation ("Solkattu.Score.Solkattu2018",14,"yt_mannargudi1") Solkattu.Score.Solkattu2018.yt_mannargudi1
    , setLocation ("Solkattu.Score.Solkattu2018",45,"e_18_02_26") Solkattu.Score.Solkattu2018.e_18_02_26
    , setLocation ("Solkattu.Score.Solkattu2018",67,"yt_mannargudi2") Solkattu.Score.Solkattu2018.yt_mannargudi2
    , setLocation ("Solkattu.Score.Solkattu2018",108,"yt_pmi1") Solkattu.Score.Solkattu2018.yt_pmi1
    , setLocation ("Solkattu.Score.Solkattu2018",149,"yt_karaikudi1") Solkattu.Score.Solkattu2018.yt_karaikudi1
    , setLocation ("Solkattu.Score.Solkattu2018",205,"c_18_03_19") Solkattu.Score.Solkattu2018.c_18_03_19
    , setLocation ("Solkattu.Score.Solkattu2018",252,"c_18_03_28") Solkattu.Score.Solkattu2018.c_18_03_28
    , setLocation ("Solkattu.Score.Solkattu2018",296,"c_18_04_25") Solkattu.Score.Solkattu2018.c_18_04_25
    , setLocation ("Solkattu.Score.Solkattu2018",325,"c_18_05_25") Solkattu.Score.Solkattu2018.c_18_05_25
    , setLocation ("Solkattu.Score.SolkattuMohra",33,"c_mohra") Solkattu.Score.SolkattuMohra.c_mohra
    , setLocation ("Solkattu.Score.SolkattuMohra",50,"c_mohra2") Solkattu.Score.SolkattuMohra.c_mohra2
    , setLocation ("Solkattu.Score.SolkattuMohra",68,"c_mohra_youtube") Solkattu.Score.SolkattuMohra.c_mohra_youtube
    ]
