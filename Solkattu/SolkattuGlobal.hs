-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
-- | Global imports for solkattu score modules.
module Solkattu.SolkattuGlobal (
    module Solkattu.SolkattuGlobal
    , module Solkattu.Dsl
    , module Solkattu.DslSollu
    , module Solkattu.Interactive
) where
import Prelude hiding ((.), (^))
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified Util.CallStack as CallStack
import qualified Solkattu.Format.Html as Html
import qualified Solkattu.Format.Terminal as Terminal
import qualified Solkattu.Instrument.KendangTunggal as KendangTunggal
import qualified Solkattu.Instrument.Mridangam as Mridangam
import qualified Solkattu.Instrument.Reyong as Reyong
import qualified Solkattu.Instrument.Sargam as Sargam
import qualified Solkattu.Interactive as Interactive
import Solkattu.Interactive (diff, diffw)
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Realize as Realize
import qualified Solkattu.Solkattu as Solkattu
import qualified Solkattu.Tala as Tala

import Global
import Solkattu.Dsl
import Solkattu.DslSollu


Mridangam.Strokes {..} = Mridangam.notes

type Sequence = SequenceT Solkattu.Sollu
type Section = Korvai.Section Solkattu.Sollu

-- * fragments

tdgnt, td_gnt, t_d_gnt :: Sequence
tdgnt = ta.din.gin.na.thom
td_gnt = ta.din.__.gin.na.thom
t_d_gnt = ta.__.din.__.gin.na.thom

takadinna, takita, kita, taka, tiku, diku, tari, dugu :: Sequence
takadinna = ta.ka.din.na
takita = ta.ki.ta
kita = ki.ta
taka = ta.ka
naka = na.ka
tiku = ti.ku
diku = di.ku
tari = ta.ri
dugu = du.gu

jonu :: Sequence
jonu = jo.nu

kitataka, tarikita :: Sequence
kitataka = kita.taka
tarikita = tari.kita

talang :: Sequence
talang = ta.lang

dinga, dingu, tanga, langa :: Sequence
dinga = din.__.ga
dingu = din.__.gu
tanga = tang.__.ga
langa = lang.__.ga

-- ** fast variants

kt, tk, tkt, tkdn, trkt, kttk :: Sequence
kt = su kita
tk = su taka
tkt = su takita
tkdn = su takadinna
trkt = su tarikita
kttk = su kitataka

-- ** standard fragments

-- It's awkward that these use mridangam fingerings instead of solkattu, but
-- the solkattu are otherwise ambiguous, and too long anyway.  I originally
-- used solkattu without vowels, but later I wanted to use the same names for
-- fast variants.

kp, kpnp :: Sequence
kp = Solkattu.Standard ^ taka
kpnp = Solkattu.Standard ^ (taka.tiku)

-- These are more problematic because different realizations are common, e.g.
-- oktp, pktp.
oknp, ktktoknp :: Sequence
oknp = Solkattu.Standard ^ tarikita
ktktoknp = Solkattu.Standard ^ (tarikita.taka.taka)

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
    { Korvai.smapMridangam = check $ Realize.strokeMap $
        Realize.patternKeys Mridangam.defaultPatterns ++ strokes
    }

-- * interactive utilities

-- ** realize

index :: Int -> Korvai -> Korvai
index i korvai = case Korvai.korvaiSections korvai of
    Korvai.Mridangam sections ->
        korvai { Korvai.korvaiSections = Korvai.Mridangam [sections !! i] }
    Korvai.Sollu sections ->
        korvai { Korvai.korvaiSections = Korvai.Sollu [sections !! i] }

realize, realizep :: Korvai.Korvai -> IO ()
realize = realizeM None
realizep = realizeM Patterns

realizeM :: Abstraction -> Korvai.Korvai -> IO ()
realizeM = _printInstrument Korvai.mridangam

realizeK1 :: Abstraction -> Korvai.Korvai -> IO ()
realizeK1 = _printInstrument Korvai.kendangTunggal

realizeR :: Abstraction -> Korvai.Korvai -> IO ()
realizeR = _printInstrument Korvai.reyong

realizeSargam :: Abstraction -> Korvai.Korvai -> IO ()
realizeSargam = _printInstrument Korvai.sargam

realizeKon :: Int -> Korvai -> IO ()
realizeKon width = Terminal.printKonnakol width Patterns

htmlWriteAll :: FilePath -> Abstraction -> Korvai -> IO ()
htmlWriteAll = Html.writeAll

_printInstrument :: Solkattu.Notation stroke => Korvai.Instrument stroke
    -> Abstraction -> Korvai -> IO ()
_printInstrument inst =
    Interactive.printInstrument inst (get (Korvai.instName inst))
    where get name = Map.findWithDefault [] name _instrumentDefaultStrokes

-- ** lint

-- | Show shadowed strokes in the stroke map.
lintM :: Korvai.Korvai -> IO ()
lintM = _printLint Korvai.mridangam _mridangamStrokes

makeKendang1 :: CallStack.Stack => StrokeMap KendangTunggal.Stroke
    -> Korvai.StrokeMaps
makeKendang1 strokes = mempty
    { Korvai.smapKendangTunggal = check $ Realize.strokeMap $
        Realize.patternKeys KendangTunggal.defaultPatterns ++ _kendangStrokes
            ++ strokes
    }

lintK1 :: Korvai -> IO ()
lintK1 = _printLint Korvai.kendangTunggal _kendangStrokes

makeReyong :: CallStack.Stack => StrokeMap Reyong.Stroke -> Korvai.StrokeMaps
makeReyong strokes = mempty
    { Korvai.smapReyong = check $ Realize.strokeMap $
        Realize.patternKeys Reyong.rhythmicPatterns ++ _reyongStrokes ++ strokes
    }

lintR :: Korvai -> IO ()
lintR = _printLint Korvai.reyong _reyongStrokes

makeSargam :: CallStack.Stack => StrokeMap Sargam.Stroke -> Korvai.StrokeMaps
makeSargam strokes = mempty
    { Korvai.smapSargam = check $ Realize.strokeMap strokes }

lintS :: Korvai -> IO ()
lintS = _printLint Korvai.sargam []

korvai :: Tala.Tala -> Korvai.StrokeMaps -> [Section] -> Korvai
korvai = Korvai.korvai

korvai1 :: Tala.Tala -> Korvai.StrokeMaps -> Section -> Korvai
korvai1 tala smaps section = korvai tala smaps [section]

korvaiS :: Tala.Tala -> Korvai.StrokeMaps -> [Sequence] -> Korvai
korvaiS = Korvai.korvaiInferSections

korvaiS1 :: Tala.Tala -> Korvai.StrokeMaps -> Sequence -> Korvai
korvaiS1 tala smaps seq = korvaiS tala smaps [seq]

lintAll :: Korvai -> IO ()
lintAll = Text.IO.putStr • allLints

allLints :: Korvai -> Text
allLints korvai =
    Text.unlines $ List.intersperse "" $
        mapMaybe lintsOf (Korvai.korvaiInstruments korvai)
    where
    lintsOf (name, Korvai.GInstrument inst)
        | Text.null warn = Nothing
        | otherwise = Just $ "    " <> name <> ":\n" <> warn
        where warn = Korvai.lint inst (get name) korvai
    get name = Map.findWithDefault [] name _instrumentDefaultStrokes

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
    , (kp, [k, p])
    , (kpnp, [k, p, n, p])
    , (oknp, [o, k, n, p]) -- fast version: pktp
    , (ktktoknp, [k, t, k, t, o, k, n, p])
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
    , (kp, [p, k])
    , (kpnp, [p, k, t, k])
    , (oknp, [a, k, t, o])
    , (ktktoknp, [k, p, k, p, a, k, t, o])
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

_printLint :: Pretty stroke => Korvai.Instrument stroke -> [(Sequence, x)]
    -> Korvai -> IO ()
_printLint inst strokes korvai =
    Text.IO.putStr $ Korvai.lint inst (map fst strokes) korvai

_instrumentDefaultStrokes :: Map Text [Sequence]
_instrumentDefaultStrokes = Map.fromList
    [ pair Korvai.mridangam _mridangamStrokes
    , pair Korvai.kendangTunggal _kendangStrokes
    , pair Korvai.reyong _reyongStrokes
    ]
    where
    pair :: Korvai.Instrument stroke -> [(Sequence, [Realize.SNote stroke])]
        -> (Text, [Sequence])
    pair inst strokes = (Korvai.instName inst, map fst strokes)

-- TODO
-- vary :: Korvai -> Korvai
-- vary = Korvai.vary $ Solkattu.vary (Solkattu.variations [Solkattu.standard])
