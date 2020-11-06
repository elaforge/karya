-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
-- | Global imports for solkattu score modules.
module Solkattu.Dsl.Solkattu (
    module Solkattu.Dsl.Solkattu
    , module Solkattu.Dsl.Generic
    , module Solkattu.Dsl.Interactive
) where
import           Prelude hiding ((.), (^))
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified Util.CallStack as CallStack
import qualified Solkattu.Dsl.Interactive as Interactive
import           Solkattu.Dsl.Interactive (diff, diffw)
import qualified Solkattu.Dsl.MridangamNotation as MridangamNotation
import qualified Solkattu.Format.Terminal as Terminal
import qualified Solkattu.Instrument.KendangPasang as KendangPasang
import qualified Solkattu.Instrument.KendangTunggal as KendangTunggal
import qualified Solkattu.Instrument.Mridangam as Mridangam
import qualified Solkattu.Instrument.Reyong as Reyong
import qualified Solkattu.Instrument.Sargam as Sargam
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Part as Part
import qualified Solkattu.Realize as Realize
import qualified Solkattu.S as S
import qualified Solkattu.Solkattu as Solkattu
import qualified Solkattu.Tala as Tala

import           Global
import           Solkattu.Dsl.Generic


type Sequence = SequenceT Solkattu.Sollu
type SequenceR stroke = SequenceT (Realize.Stroke stroke)
type SequenceM = SequenceR Mridangam.Stroke

type Section = Korvai.Section Solkattu.Sollu

-- * sollus

_sollu :: Solkattu.Sollu -> Sequence
_sollu s = [S.Note (Solkattu.Note (Solkattu.note s))]

dheem = _sollu Solkattu.Dheem
dhom = _sollu Solkattu.Dhom
di = _sollu Solkattu.Di
din = _sollu Solkattu.Din
dim = _sollu Solkattu.Dim
dit = _sollu Solkattu.Dit
du = _sollu Solkattu.Du
ga = _sollu Solkattu.Ga
gin = _sollu Solkattu.Gin
gu = _sollu Solkattu.Gu
jo = _sollu Solkattu.Jo
ka = _sollu Solkattu.Ka
ki = _sollu Solkattu.Ki
ku = _sollu Solkattu.Ku
kum = _sollu Solkattu.Kum
mi = _sollu Solkattu.Mi
na = _sollu Solkattu.Na
nam = _sollu Solkattu.Nam
nang = _sollu Solkattu.Nang
nu = _sollu Solkattu.Nu
ri = _sollu Solkattu.Ri
ta = _sollu Solkattu.Ta
tam = _sollu Solkattu.Tam
tat = _sollu Solkattu.Tat
tha = _sollu Solkattu.Tha
thom = _sollu Solkattu.Thom
ti = _sollu Solkattu.Ti

tang = _sollu Solkattu.Tang
tong = _sollu Solkattu.Tong
lang = _sollu Solkattu.Lang

-- * fragments

-- TODO these should be namedT Solkattu.GPattern "p#", but that makes
-- 'spread' not work.
tdgnt, td_gnt, t_d_gnt :: Sequence
tdgnt = ta.din.gin.na.thom
td_gnt = ta.din.__.gin.na.thom
t_d_gnt = ta.__.din.__.gin.na.thom

takadinna, takita, kita, taka, tiku, diku, tari, gugu, dugu :: Sequence
takadinna = ta.ka.din.na
takita = ta.ki.ta
kita = ki.ta
taka = ta.ka
naka = na.ka
tiku = ti.ku
diku = di.ku
tari = ta.ri
gugu = gu.gu
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
kp = namedT Solkattu.GFiller "2t" (Solkattu.Standard ^ taka)
kpnp = namedT Solkattu.GFiller "4t" (Solkattu.Standard ^ (taka.tiku))

-- These are more problematic because different realizations are common, e.g.
-- oktp, pktp.
oknp, ktktoknp :: Sequence
oknp = namedT Solkattu.GFiller "4t" $ Solkattu.Standard ^ tarikita
ktktoknp = namedT Solkattu.GFiller "8t" $
    Solkattu.Standard ^ (tarikita.taka.taka)

nakatiku :: Sequence
nakatiku = namedT Solkattu.GPattern "8n" $
    Solkattu.Standard ^ (naka.tiku.tari.kita)
    -- also diku.tari.kita.taka

takadugutarikita :: Sequence
takadugutarikita = namedT Solkattu.GPattern "8t" $
    Solkattu.Standard ^ (taka.dugu.tari.kita)

-- * instruments

Mridangam.Strokes {..} = Mridangam.notes

-- | Merge a sequence of left hand strokes with one of right hand strokes.
-- Both sequences must have the same length and structure.
(&) :: CallStack.Stack => SequenceM -> SequenceM -> SequenceM
(&) = MridangamNotation.merge

on :: SequenceM
on = o&n

-- | Parse a string to mridangam strokes.
strM :: CallStack.Stack => String -> SequenceM
strM str = mconcatMap toSeq $ Solkattu.check $ Mridangam.fromString str
    where
    toSeq Nothing = __
    toSeq (Just stroke) = Realize.strokeToSequence stroke

type StrokeMap stroke =
    [ ( Sequence
      , [S.Note Solkattu.Group (Solkattu.Note (Realize.Stroke stroke))]
      )
    ]

makeMridangam :: StrokeMap Mridangam.Stroke -> Korvai.StrokeMaps
makeMridangam strokes = makeMridangam0 (_mridangamStrokes ++ strokes)

-- | Make a mridangam StrokeMap, but without the default '_mridangamStrokes'.
makeMridangam0 :: StrokeMap Mridangam.Stroke -> Korvai.StrokeMaps
makeMridangam0 strokes = mempty
    { Korvai.smapMridangam = Realize.strokeMap Mridangam.defaultPatterns strokes
    }

-- | Show shadowed strokes in the stroke map.
lintM :: Korvai.Korvai -> IO ()
lintM = _printLint Korvai.mridangam _mridangamStrokes

makeKendang1 :: StrokeMap KendangTunggal.Stroke -> Korvai.StrokeMaps
makeKendang1 strokes = mempty
    { Korvai.smapKendangTunggal =
        Realize.strokeMap KendangTunggal.defaultPatterns
            (_kendangStrokes1 ++ strokes)
    }

lintK1 :: Korvai -> IO ()
lintK1 = _printLint Korvai.kendangTunggal _kendangStrokes1

makeKendang2 :: StrokeMap KendangPasang.Stroke -> Korvai.StrokeMaps
makeKendang2 strokes = mempty
    { Korvai.smapKendangPasang =
        Realize.strokeMap KendangPasang.defaultPatterns
            (_kendangStrokes2 ++ strokes)
    }

lintK2 :: Korvai -> IO ()
lintK2 = _printLint Korvai.kendangTunggal _kendangStrokes2

makeReyong :: StrokeMap Reyong.Stroke -> Korvai.StrokeMaps
makeReyong strokes = mempty
    { Korvai.smapReyong = Realize.strokeMap
        Reyong.rhythmicPatterns (_reyongStrokes ++ strokes)
    }

lintR :: Korvai -> IO ()
lintR = _printLint Korvai.reyong _reyongStrokes

makeSargam :: [(S.Matra, SequenceR Sargam.Stroke)]
    -> StrokeMap Sargam.Stroke -> Korvai.StrokeMaps
makeSargam patterns strokes = mempty { Korvai.smapSargam = convert }
    where
    convert = do
        patterns <- mapM (traverse Realize.solkattuToRealize) $
            map (first Solkattu.pattern) patterns
        pmap <- Realize.patternMap patterns
        Realize.strokeMap pmap strokes

lintS :: Korvai -> IO ()
lintS = _printLint Korvai.sargam []

_defaultStrokes :: Korvai.Instrument stroke -> [Sequence]
_defaultStrokes inst = Map.findWithDefault [] (Korvai.instName inst)
    _instrumentDefaultStrokes

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
_mridangamStrokes :: [(Sequence, SequenceM)]
_mridangamStrokes =
    [ (thom, o)
    , (dhom, o)
    , (tang, u)
    , (lang, u)
    , (talang, p.u)
    , (takadinna, k.o.o.k)
    , (tdgnt, k.t.k.n.o)
    , (kp, k.p)
    , (kpnp, k.p.n.p)
    , (oknp, o.k.n.p) -- fast version: pktp
    , (ktktoknp, k.t.k.t.o.k.n.p)
    , (nakatiku, n.p.u.p.k.t.p.k)
        -- alternate: t.p.u.p.k.t.p.k
        -- alternate: t.p.u.o.k.t.p.k
    , (takadugutarikita, t.k.o.o.k.t.p.k)
    ]
    where Mridangam.Strokes {..} = Mridangam.notes

_kendangStrokes1 :: [(Sequence, SequenceR KendangTunggal.Stroke)]
_kendangStrokes1 =
    [ (thom, a)
    , (tang, u)
    , (lang, u)
    , (talang, o.u)
    , (takadinna, p.a.o.p)
    , (tdgnt, o.k.p.t.a)
    , (kp, p.k)
    , (kpnp, p.k.t.k)
    , (oknp, a.k.t.o)
    , (ktktoknp, k.p.k.p.a.k.t.o)
    , (nakatiku, t.o.u.k.p.a.o.k)
    , (takadugutarikita, k.p.a.a.k.p.k.t)
    ]
    where KendangTunggal.Strokes {..} = KendangTunggal.notes

_kendangStrokes2 :: [(Sequence, SequenceR KendangPasang.Stroke)]
_kendangStrokes2 =
    [ (thom, a)
    , (tang, y)
    , (lang, y)
    , (talang, a.u)
    , (takadinna, p.a.o.k)
    , (tdgnt, o.k.p.l.a)
    , (kp, k.p)
    , (kpnp, k.p.t.l)
    , (oknp, o.t.l.a)
    , (ktktoknp, k.p.k.p.o.t.l.a)
    , (nakatiku, t.u.y.k.p.a.o.k)
    , (takadugutarikita, k.p.a.a.t.l.k.p)
    ]
    where KendangPasang.Strokes {..} = KendangPasang.notes

_reyongStrokes :: [(Sequence, SequenceR Reyong.Stroke)]
_reyongStrokes =
    [ (thom, o)
    , (tang, o)
    , (lang, o)
    , (talang, b.o)
    , (nakatiku, i.r3.i.r2.r3.i.r3.r2)
        -- TODO melodic version, there could also be a rhythmic version
    ]
    where Reyong.Strokes {..} = Reyong.notes

_printLint :: Pretty stroke => Korvai.Instrument stroke -> [(Sequence, x)]
    -> Korvai -> IO ()
_printLint inst strokes korvai =
    Text.IO.putStr $ Korvai.lint inst (map fst strokes) korvai

_instrumentDefaultStrokes :: Map Text [Sequence]
_instrumentDefaultStrokes = Map.fromList
    [ pair Korvai.mridangam _mridangamStrokes
    , pair Korvai.kendangTunggal _kendangStrokes1
    , pair Korvai.kendangPasang _kendangStrokes2
    , pair Korvai.reyong _reyongStrokes
    ]
    where
    pair :: Korvai.Instrument stroke -> [(Sequence, x)] -> (Text, [Sequence])
    pair inst strokes = (Korvai.instName inst, map fst strokes)

-- * realize

realize, realizep :: Korvai.Korvai -> IO ()
realize = realizeM concrete
realizep = realizeM id

-- The actual Config transformers are in Generic.

realizeM :: (Terminal.Config -> Terminal.Config) -> Korvai.Korvai -> IO ()
realizeM = _printInstrument Korvai.mridangam

realizek, realizekp :: Korvai.Korvai -> IO ()
realizek = _printInstrument Korvai.kendangTunggal concrete
realizekp = _printInstrument Korvai.kendangTunggal id

realizek2 :: Korvai.Korvai -> IO ()
realizek2 = _printInstrument Korvai.kendangPasang concrete

realizeR :: (Terminal.Config -> Terminal.Config) -> Korvai.Korvai -> IO ()
realizeR = _printInstrument Korvai.reyong

realizeSargam :: (Terminal.Config -> Terminal.Config) -> Korvai.Korvai -> IO ()
realizeSargam = _printInstrument Korvai.sargam

realizeKon :: Korvai -> IO ()
realizeKon = Terminal.printKonnakol (wide Terminal.defaultConfig)

realizeKon_ :: Int -> Korvai -> IO ()
realizeKon_ width = Terminal.printKonnakol
    (Terminal.defaultConfig { Terminal._terminalWidth = width })

-- | 'realizeParts' specialized to mridangam, and disbale the usual
-- 'Interactive.printInstrument' lint and write diff stuff.
realizePartsM :: (Terminal.Config -> Terminal.Config) -> [Part] -> IO ()
realizePartsM configure = Part.realizeParts realize
    where
    inst = Korvai.mridangam
    realize = Interactive.printInstrument False False inst
        (_defaultStrokes inst) (configure Terminal.defaultConfig)

_printInstrument :: Solkattu.Notation stroke => Korvai.Instrument stroke
    -> (Terminal.Config -> Terminal.Config) -> Korvai -> IO ()
_printInstrument inst setConfig =
    Interactive.printInstrument True True inst (_defaultStrokes inst)
        (setConfig Terminal.defaultConfig)

-- * korvai

korvai :: Tala.Tala -> Korvai.StrokeMaps -> [Section] -> Korvai
korvai = Korvai.korvai

korvai1 :: Tala.Tala -> Korvai.StrokeMaps -> Section -> Korvai
korvai1 tala smaps section = korvai tala smaps [section]

korvaiS :: Tala.Tala -> Korvai.StrokeMaps -> [Sequence] -> Korvai
korvaiS = Korvai.korvaiInferSections

korvaiS1 :: Tala.Tala -> Korvai.StrokeMaps -> Sequence -> Korvai
korvaiS1 tala smaps seq = korvaiS tala smaps [seq]

-- TODO
-- vary :: Korvai -> Korvai
-- vary = Korvai.vary $ Solkattu.vary (Solkattu.variations [Solkattu.standard])
