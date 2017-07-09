-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
-- | Global imports for solkattu score modules.
module Derive.Solkattu.SolkattuGlobal (
    module Derive.Solkattu.SolkattuGlobal
    , module Derive.Solkattu.Dsl
    , module Derive.Solkattu.DslSollu
) where
import Prelude hiding ((.))

import qualified Util.CallStack as CallStack
import Derive.Solkattu.Dsl
import Derive.Solkattu.DslSollu
import qualified Derive.Solkattu.KendangTunggal as KendangTunggal
import qualified Derive.Solkattu.Korvai as Korvai
import qualified Derive.Solkattu.Mridangam as Mridangam
import qualified Derive.Solkattu.Reyong as Reyong
import qualified Derive.Solkattu.Sequence as Sequence
import qualified Derive.Solkattu.Solkattu as Solkattu
import qualified Derive.Solkattu.Tala as Tala

Mridangam.Strokes {..} = Mridangam.notes


type Sequence = Seq Korvai.Stroke
type Seq stroke = [Sequence.Note (Solkattu.Note stroke)]

-- * fragments

tdgnt, td_gnt, t_d_gnt :: Seq stroke
tdgnt = ta.din.gin.na.thom
td_gnt = ta.din.__.gin.na.thom
t_d_gnt = ta.__.din.__.gin.na.thom

takadinna :: Seq stroke
takadinna = ta.ka.din.na

takita :: Seq stroke
takita = ta.ki.ta

kita :: Seq stroke
kita = ki.ta

on :: Mridangam.SNote
on = o & n

-- * realize

type MStrokes = [(Seq Mridangam.Stroke, [Mridangam.SNote])]

make_mridangam :: CallStack.Stack => MStrokes -> Korvai.Instruments
make_mridangam strokes = mempty
    { Korvai.inst_mridangam = check $
        Mridangam.instrument (_standard_mridangam ++ strokes)
            Mridangam.default_patterns
    }

make_kendang1 :: CallStack.Stack =>
    [(Seq KendangTunggal.Stroke, [KendangTunggal.SNote])]
    -> Korvai.Instruments
make_kendang1 strokes = mempty
    { Korvai.inst_kendang_tunggal = check $
        KendangTunggal.instrument strokes KendangTunggal.default_patterns
    }

make_reyong :: CallStack.Stack => [(Seq Reyong.Stroke, [Reyong.SNote])]
    -> Korvai.Instruments
make_reyong strokes = mempty
    { Korvai.inst_reyong = check $
        Reyong.instrument strokes Reyong.rhythmic_patterns
    }

korvai1 :: CallStack.Stack => Tala.Tala -> Korvai.Instruments -> Sequence
    -> Korvai
korvai1 tala inst seq = Korvai.korvai tala inst [seq]

korvai :: CallStack.Stack => Tala.Tala -> Korvai.Instruments -> [Sequence]
    -> Korvai
korvai = Korvai.korvai

-- | 'make_mridangam' gives this to all mridangam stroke maps.
_standard_mridangam :: [(Seq stroke, [Mridangam.SNote])]
_standard_mridangam =
    [ (takadinna, [k, o, o, k])
    , (tdgnt, [k, t, k, n, o])
    , (dheem, [od])
    ]
    where Mridangam.Strokes {..} = Mridangam.notes

-- * metadata

ganesh :: Korvai -> Korvai
ganesh = source "ganesh"

-- * misc

adi :: Tala.Tala
adi = Tala.adi_tala

vary :: Korvai -> Korvai
vary = Korvai.vary $ Solkattu.vary (Solkattu.variations [Solkattu.standard])
