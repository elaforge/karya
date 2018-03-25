-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
-- | Global imports for solkattu score modules.
module Solkattu.SolkattuGlobal (
    module Solkattu.SolkattuGlobal
    , module Solkattu.Dsl
    , module Solkattu.DslSollu
) where
import Prelude hiding ((.), (^))

import qualified Util.CallStack as CallStack
import Solkattu.Dsl
import Solkattu.DslSollu
import qualified Solkattu.Instrument.KendangTunggal as KendangTunggal
import qualified Solkattu.Instrument.Mridangam as Mridangam
import qualified Solkattu.Instrument.Reyong as Reyong
import qualified Solkattu.Instrument.Sargam as Sargam
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Realize as Realize
import qualified Solkattu.Solkattu as Solkattu
import qualified Solkattu.Tala as Tala


Mridangam.Strokes {..} = Mridangam.notes

type Sequence = SequenceT Solkattu.Sollu
type Section = Korvai.Section Solkattu.Sollu

-- * fragments

tdgnt, td_gnt, t_d_gnt :: Sequence
tdgnt = ta.din.gin.na.thom
td_gnt = ta.din.__.gin.na.thom
t_d_gnt = ta.__.din.__.gin.na.thom

takadinna, takita, kita, taka, diku, tari, kitataka, tarikita :: Sequence
takadinna = ta.ka.din.na
takita = ta.ki.ta
kita = ki.ta
taka = ta.ka
naka = na.ka
diku = di.ku
tari = ta.ri
kitataka = kita.taka
tarikita = tari.kita

talang :: Sequence
talang = ta.lang

dinga, tanga, langa :: Sequence
dinga = din.__.ga
tanga = tang.__.ga
langa = lang.__.ga

-- ** standard fragments

tk, tktu :: Sequence
tk = group $ Solkattu.Standard ^ (ta.ka)
tktu = group $ Solkattu.Standard ^ (ta.ka.ti.ku)

-- * instruments

on :: Mridangam.SNote
on = o & n

type StrokeMap stroke = [(Sequence, [Realize.SNote stroke])]

makeMridangam :: CallStack.Stack => StrokeMap Mridangam.Stroke
    -> Korvai.StrokeMaps
makeMridangam strokes = makeMridangam0 (_mridangamStrokes ++ strokes)

-- | Make a mridangam StrokeMap, but without the default '_mridangamStrokes'.
makeMridangam0 :: CallStack.Stack => StrokeMap Mridangam.Stroke
    -> Korvai.StrokeMaps
makeMridangam0 strokes = mempty
    { Korvai.instMridangam = check $
        Realize.instrument strokes Mridangam.defaultPatterns
    }

makeKendang1 :: CallStack.Stack => StrokeMap KendangTunggal.Stroke
    -> Korvai.StrokeMaps
makeKendang1 strokes = mempty
    { Korvai.instKendangTunggal = check $
        Realize.instrument (_kendangStrokes ++ strokes)
            KendangTunggal.defaultPatterns
    }

makeReyong :: CallStack.Stack => StrokeMap Reyong.Stroke -> Korvai.StrokeMaps
makeReyong strokes = mempty
    { Korvai.instReyong = check $
        Realize.instrument (_reyongStrokes ++ strokes) Reyong.rhythmicPatterns
    }

makeSargam :: CallStack.Stack => StrokeMap Sargam.Stroke
    -> [(Solkattu.Pattern, [Realize.SNote Sargam.Stroke])]
    -> Korvai.StrokeMaps
makeSargam strokes patterns = mempty
    { Korvai.instSargam = check $
        Realize.instrument strokes (check $ Realize.patterns patterns)
    }

korvai :: Tala.Tala -> Korvai.StrokeMaps -> [Section] -> Korvai
korvai = Korvai.korvai

korvai1 :: Tala.Tala -> Korvai.StrokeMaps -> Section -> Korvai
korvai1 tala smaps section = korvai tala smaps [section]

korvaiS :: Tala.Tala -> Korvai.StrokeMaps -> [Sequence] -> Korvai
korvaiS = Korvai.korvaiInferSections

korvaiS1 :: Tala.Tala -> Korvai.StrokeMaps -> Sequence -> Korvai
korvaiS1 tala smaps seq = korvaiS tala smaps [seq]

-- | 'makeMridangam' gives this to all mridangam stroke maps.
_mridangamStrokes :: [(Sequence, [Mridangam.SNote])]
_mridangamStrokes =
    [ (thom, [o])
    , (dhom, [o])
    , (tang, [u])
    , (lang, [u])
    , (talang, [p, u])
    , (takadinna, [k, o, o, k])
    , (tdgnt, [k, t, k, n, o])
    , (tk, [k, p])
    , (tktu, [k, p, n, p])
    ]
    where Mridangam.Strokes {..} = Mridangam.notes

_kendangStrokes :: [(Sequence, [KendangTunggal.SNote])]
_kendangStrokes =
    [ (thom, [a])
    , (tang, [u])
    , (lang, [u])
    , (talang, [o, u])
    , (takadinna, [p, a, o, p])
    , (tdgnt, [o, k, p, t, a])
    , (tk, [p, k])
    , (tktu, [p, k, t, k])
    ]
    where KendangTunggal.Strokes {..} = KendangTunggal.notes

_reyongStrokes :: [(Sequence, [Reyong.SNote])]
_reyongStrokes =
    [ (thom, [o])
    , (tang, [o])
    , (lang, [o])
    , (talang, [b, o])
    ]
    where Reyong.Strokes {..} = Reyong.notes

-- TODO
-- vary :: Korvai -> Korvai
-- vary = Korvai.vary $ Solkattu.vary (Solkattu.variations [Solkattu.standard])
