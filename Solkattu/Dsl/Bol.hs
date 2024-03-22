{-# LANGUAGE RecordWildCards #-}
-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Details in "Solkattu.Bol".
module Solkattu.Dsl.Bol (
    module Solkattu.Dsl.Bol
    , module Solkattu.Talas
    , module Solkattu.Dsl.Generic
    , module Solkattu.Dsl.Interactive
) where
import           Prelude hiding ((.), (^))
import qualified Data.String as String
import           GHC.Stack (HasCallStack)

import qualified Solkattu.Bol as Bol
import qualified Solkattu.Dsl.Interactive as Interactive
import           Solkattu.Dsl.Interactive (diff, diffw)
import qualified Solkattu.Dsl.Notation as Notation
import qualified Solkattu.Format.Terminal as Terminal
import qualified Solkattu.Instrument.Mridangam as Mridangam
import qualified Solkattu.Instrument.Tabla as Tabla
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Realize as Realize
import qualified Solkattu.S as S
import qualified Solkattu.Solkattu as Solkattu
import qualified Solkattu.Talas as Talas
import           Solkattu.Talas (jhaptal, kehrwa, rupak, tintal)

import           Global
import           Solkattu.Dsl.Generic


type Sequence = SequenceT (Realize.Stroke Bol.Bol)

-- | Realized sequence of strokes.
type SequenceM = SequenceT (Realize.Stroke Tabla.Stroke)

type Section = Korvai.Section Sequence

instance String.IsString Sequence where
    -- Even with InstanceSigs, this doesn't actually work to add a call stack.
    -- fromString :: HasCallStack => String -> Sequence
    fromString s = strS (txt s)

-- | Parse a string to bols.  Look for syllables inside words.
strS :: HasCallStack => Text -> Sequence
strS str = mconcatMap (maybe __ make) $ Solkattu.check $ Bol.parseBols str
    where
    make = \case
        Bol.S1 b1 -> _bol b1
        Bol.S2 b1 b2 -> su (_bol b1 . _bol b2)

-- * sollus

_bol :: Bol.Bol -> Sequence
_bol s = S.singleton $ S.Note (Solkattu.Note (Solkattu.note (Realize.stroke s)))

dha = _bol Bol.Dha
dhaS = Solkattu.Sur ^ dha -- dha but explicitly on sur
dhe = _bol Bol.Dhe
dhin= _bol Bol.Dhin
dhom= _bol Bol.Dhom
di  = _bol Bol.Di
ga  = _bol Bol.Ga
ge  = _bol Bol.Ge
ka  = _bol Bol.Ka
ki  = _bol Bol.Ki
na  = _bol Bol.Na
ne  = _bol Bol.Ne
ra  = _bol Bol.Ra
re  = _bol Bol.Re
tA  = _bol Bol.TA
ta  = _bol Bol.Ta
taa = _bol Bol.Taa
tak = _bol Bol.Tak
te  = _bol Bol.Te
tet = _bol Bol.Tet
ti  = _bol Bol.Ti
tin = _bol Bol.Tin
tu  = _bol Bol.Tu
tun = _bol Bol.Tun

-- * fragments

tette :: Sequence
tette = tet.te
taka :: Sequence
taka = ta.ka

tr, kt, trkt, tktrkt :: Sequence
tr = "tr" -- these are defined in Solkattu.Bol.sequences
kt = "tk"
trkt = "trkt"
tktrkt = su $ ta.ka.te.re.ki.ta
trkttk = su $ te.re.ki.ta.ta.ka

kttk :: Sequence
kttk = "kttk"

-- | Standard pakhawaj tette kata gadi gene pattern.
tetekata :: Sequence
tetekata = namedT Solkattu.GPattern "8n" $
    Solkattu.Standard ^ "tette katA gadi gene"

-- * notation

-- | Just tri_ flipped, is this easier to read for hindustani, which often
-- has nested tihais?
tihai :: Sequence -> Sequence -> Sequence
tihai seq sep = tri_ (hv sep) seq

-- | Experiment for tihais where the 3rd is a variant.
tihai2 :: Sequence -> Sequence -> Sequence
tihai2 seq sep = seq.sep.seq.sep

-- * transform

-- | Change to kali within the given time range.
kaliM :: S.FMatra -> S.FMatra -> Sequence -> Sequence
kaliM start end seq = pre <> kali within <> post
    where
    (pre, seq2) = splitM_ start seq
    (within, post) = splitM_ (end - start) seq2

-- | Like kaliM but for tintal, start and end are offsets from tintal
-- kali.  TODO this should be generalized for any tal, but I'd have to either
-- put this in the sequence to interpret later, or have an ambient State.
-- It's also wrong in different nadai, which is also what makes Duration
-- incorrect.  So State with talam and nadai would be useful.
kaliMt :: S.FMatra -> S.FMatra -> Sequence -> Sequence
kaliMt start end = kaliM (32+start) (48+end)

kali :: Sequence -> Sequence
kali = mapB $ \case
    Bol.Dha -> Just Bol.Taa -- taa spelling for ta to remember it came from dha
    Bol.Dhe -> Just Bol.The -- for dhere dhere
    Bol.Dhen -> Just Bol.Ten
    Bol.Dhet -> Just Bol.Tet
    Bol.Dhi -> Just Bol.Tun
    Bol.Dhin -> Just Bol.Tin -- behause dha dhin dhin -> taa tin tin
    Bol.Ga -> Just Bol.Ka
    Bol.Ge -> Just Bol.Ke
    Bol.Gi -> Just Bol.Ki
    Bol.Ghin -> Just Bol.Kin
    bol -> Just bol

mapB :: (Bol.Bol -> Maybe Bol.Bol) -> Sequence -> Sequence
mapB f = fmap $ \case
    Solkattu.Note n -> case traverse f (Solkattu._sollu n) of
        Nothing -> Solkattu.Space Solkattu.Rest
        Just s -> Solkattu.Note $ n { Solkattu._sollu = s }
    note -> note

-- * realize

realize :: Korvai -> IO ()
realize = Terminal.printBol (concrete Terminal.bolConfig)

realize3 :: Korvai -> IO ()
realize3 = Terminal.printBol $ concrete $
    Terminal.bolConfig { Terminal._overrideStrokeWidth = Just 3 }

realize_ :: Int -> Korvai -> IO ()
realize_ width = Terminal.printBol
    (concrete $ Terminal.bolConfig { Terminal._terminalWidth = width })

realizet :: Korvai.Korvai -> IO ()
realizet = _printInstrument Just Korvai.ITabla concrete

realizem :: Korvai.Korvai -> IO ()
realizem = _printInstrument Just Korvai.IMridangam concrete

_printInstrument
    :: (Solkattu.Notation stroke1, Solkattu.Notation stroke2, Ord stroke1)
    => (Realize.Stroke stroke1 -> Maybe (Realize.Stroke stroke2))
    -> Korvai.Instrument stroke1
    -> (Terminal.Config -> Terminal.Config)
    -> Korvai -> IO ()
_printInstrument postproc inst setConfig =
    Interactive.printInstrument True True inst []
        (setConfig Terminal.defaultConfig) postproc

-- * bol map

type StrokeMap stroke =
    [ ( Sequence
      , S.Sequence Solkattu.Group (Solkattu.Note (Realize.Stroke stroke))
      )
    ]

tablaKinar :: Korvai.StrokeMaps
tablaKinar = makeTabla Kinar []

tablaSur :: Korvai.StrokeMaps
tablaSur = makeTabla Sur []

makeTabla :: Na -> StrokeMap Tabla.Stroke -> Korvai.StrokeMaps
makeTabla naKinar strokes =
    makeTabla0 $ map toSequence (_tablaStrokesNa naKinar)
        ++ defaultStrokes ++ strokes
    where
    defaultStrokes = map toSequence _tablaStrokes
    toSequence = second (mconcatMap Realize.strokeToSequence)

makeMridangam :: Na -> StrokeMap Mridangam.Stroke -> Korvai.StrokeMaps
makeMridangam naKinar strokes =
    makeMridangam0 $ map toSequence (_mridangamStrokesNa naKinar)
        ++ defaultStrokes ++ strokes
    where
    defaultStrokes = map toSequence _mridangamStrokes
    toSequence = second (mconcatMap Realize.strokeToSequence)

-- | Make a tabla StrokeMap, but without the default '_tablaStrokes'.
-- This also makes a default map for mridangam, but it can be overridden with
-- an explicit 'makeMridangam'.
makeTabla0 :: StrokeMap Tabla.Stroke -> Korvai.StrokeMaps
makeTabla0 strokes = mempty
    { Korvai.smapTabla = Realize.strokeMap mempty $ map (first strip) strokes
    , Korvai.smapBolMridangam = Realize.strokeMap mempty $ map (first strip) $
        map (second (fmap (fmap (fmap _toMridangam)))) strokes
    }
    where strip = fmap (fmap Realize._stroke)

makeMridangam0 :: StrokeMap Mridangam.Stroke -> Korvai.StrokeMaps
makeMridangam0 strokes = mempty
    { Korvai.smapBolMridangam =
        Realize.strokeMap Mridangam.defaultPatterns (map (first strip) strokes)
    }
    where strip = fmap (fmap Realize._stroke) -- TODO as in makeTabla0

data Na = Kinar | Sur
    deriving (Show, Eq)

-- TODO all this underscore stuff is ugly, these should go in a separate module

-- TODO
-- for rela tuna tends to be tu.na, but in kaida tends to be kat&tu . na
-- e.g. dhage naga tuna, tu could have kat or not
_tablaStrokes :: [(Sequence, [Tabla.Stroke])]
_tablaStrokes =
    [ ("dheredhere", [ge & the, rhe, the, rhe]) -- implicit ge on first stroke
    , ("dhere", [the, rhe]) -- TODO not sure if a standalone dhere has ge?
    -- TODO I can do normalization to reduce permutations:
    -- tira = tiri = tari = tere
    -- kite = kita
    , ("tarikita", [tet, te, ka, tet])
    , ("terekita", [tet, te, ka, tet])
    , ("terekite", [tet, te, ka, tet])
    , ("tirakita", [tet, te, ka, tet])
    , ("tirakite", [tet, te, ka, tet])
    , ("tirikita", [tet, te, ka, tet])
    , ("kitataka", [ka, tet, te, ka])
    , ("kitetaka", [ka, tet, te, ka])
    -- , ("takaterekita", [te, ka, tet, te, ka, tet, te, ka])
    , ("taka", [te, ka]) -- TODO works for taka terekita and terekita taka
    , ("dhennegene", [ge & tun, nhe, ge, ne])
    , ("tennekene", [tun, nhe, ka, ne])
    , ("dhenne", [ge & tun, ne])
    , ("taran ne", [tun, daya Tabla.Ran, ne]) -- play on rim when followed by ne
    , ("taran", [daya Tabla.Tu3, tun]) -- otherwise play in middle
    , (dhaS, [ge & tin])
    ] ++ map (second (:[])) -- direct bol -> stroke correspondence
    [ ("di", tun)
    , ("dhen", ge & tun)
    , ("dhet", ge & tette)
    , ("dhi", ge & tun)
    , ("dhin", ge & tin)
    , ("din", tin)
    , ("ga", ge)
    , ("ge", ge)
    , ("ghen", ge)
    , ("ka", ka)
    , ("kat", ka)
    , ("ke", ka)
    , ("ki", ka)
    , ("kran", ka &+ na)
    , ("kre", ka &+ tet)
    , ("na", na)
    , ("ne", ne)
    , ("ran", daya Tabla.Ran)
    , ("re", daya Tabla.Re)
    , ("tA", tin)
    , ("ta", tet)
    , ("tak", daya Tabla.Tak) -- but sometimes Re
    , ("te", te)
    , ("ten", tun)
    , ("tet", tet)
    , ("the", the)
    , ("ti", daya Tabla.Ti)
    , ("tin", tin)
    , ("tre", daya Tabla.Tre)
    , ("tu", tun)
    , ("tun", tun)
    , ("ṭa", te)
    ]
    where
    -- Even though I do define (&) for Sequence, I use single strokes here,
    -- it should wind up the same but is simpler types.
    Tabla.Strokes { .. } = Tabla.strokes
    (&) = Tabla.both
    (&+) = Tabla.flam
    nhe = daya Tabla.Nhe
    daya = Tabla.Daya

-- | Bols that differ based on Na.
_tablaStrokesNa :: Na -> [(Sequence, [Tabla.Stroke])]
_tablaStrokesNa = \case
    Kinar -> [(bol, [stroke]) | (bol, stroke, _) <- onKinarSur]
    Sur -> [(bol, [stroke]) | (bol, _, stroke) <- onKinarSur]
    where
    onKinarSur =
        [ ("dha", ge & na,  ge & tin)
        , ("taa", na,       tin) -- kali of dha
        ]
    Tabla.Strokes { .. } = Tabla.strokes
    (&) = Tabla.both

_mridangamStrokesNa :: Na -> [(Sequence, [Mridangam.Stroke])]
_mridangamStrokesNa naKinar =
    map (second (map _toMridangam)) $ _tablaStrokesNa naKinar

_mridangamStrokes :: [(Sequence, [Mridangam.Stroke])]
_mridangamStrokes = map (second (map _toMridangam)) _tablaStrokes

-- Even though there is a direct translation from tabla to mridangam, I don't
-- want to encode it at some lower level, but leave it in StrokeMaps.  This is
-- so I can still put in exceptions.  Also there is literal translation where
-- dhi and perhaps tun is dheem, and idiomatic where it should maybe be din.
_toMridangam :: Tabla.Stroke -> Mridangam.Stroke
_toMridangam = \case
    Tabla.Both baya daya -> Mridangam.Both (thoppi baya) (valantalai daya)
    -- TODO Mridangam can also do flam, just not idiomatic
    Tabla.Flam baya daya -> Mridangam.Both (thoppi baya) (valantalai daya)
    Tabla.Baya baya -> Mridangam.Thoppi $ thoppi baya
    Tabla.Daya daya -> Mridangam.Valantalai $ valantalai daya
    where
    thoppi = \case
        Tabla.Ka -> Mridangam.Tha Mridangam.Palm
        Tabla.Ge -> Mridangam.Thom Mridangam.Low
    valantalai = \case
        Tabla.The -> Mridangam.Ki -- could play dhere on mridangam, right?
        Tabla.Na ->  Mridangam.Nam
        Tabla.Ne ->  Mridangam.Ki
        Tabla.Nhe -> Mridangam.Dheem
        Tabla.Ran -> Mridangam.Dheem -- or Tan?
        Tabla.Re ->  Mridangam.Ki
        Tabla.Rhe -> Mridangam.Ta
        Tabla.Tak -> Mridangam.Ki
        Tabla.Te ->  Mridangam.Ta
        Tabla.Tet -> Mridangam.Ki
        Tabla.Tette -> Mridangam.Ki -- TODO need to infer which one
        Tabla.Ti ->  Mridangam.Mi
        Tabla.Tin -> Mridangam.Din
        Tabla.Tre -> Mridangam.Ki -- TODO add a mridangam tre
        -- kre turns into Both above, I guess I need flam there too.
        Tabla.Tu3 -> Mridangam.Dheem
        Tabla.Tun -> Mridangam.Dheem

-- | Merge a sequence of left hand strokes with one of right hand strokes.
-- Both sequences must have the same length and structure.
(&) :: HasCallStack => SequenceM -> SequenceM -> SequenceM
a & b = S.fromList $ Notation.merge Tabla.bothR (S.toList a) (S.toList b)

(&+) :: HasCallStack => SequenceM -> SequenceM -> SequenceM
a &+ b = S.fromList $ Notation.merge Tabla.flamR (S.toList a) (S.toList b)

-- * korvai

korvai :: Talas.Tal -> Korvai.StrokeMaps -> [Section] -> Korvai
korvai = Korvai.bolKorvai

korvai1 :: Talas.Tal -> Korvai.StrokeMaps -> Section -> Korvai
korvai1 tala smaps section = korvai tala smaps [section]

korvaiS :: Talas.Tal -> Korvai.StrokeMaps -> [Sequence] -> Korvai
korvaiS tala smaps = korvai tala smaps • Korvai.inferSections

korvaiS1 :: Talas.Tal -> Korvai.StrokeMaps -> Sequence -> Korvai
korvaiS1 tala smaps seq = korvaiS tala smaps [seq]

-- * metadata

akash :: Korvai -> Korvai
akash = source "akash"

colby :: Korvai -> Korvai
colby = source "colby"

bat :: Korvai -> Korvai
bat = withType "bat"

chalan :: Korvai -> Korvai
chalan = withType "chalan"

kaida :: Korvai -> Korvai
kaida = withType "kaida"

mukra :: Korvai -> Korvai
mukra = withType "mukra"

tukra :: Korvai -> Korvai
tukra = withType "tukra"

-- | Generally faster than kaida, dayan played closer to the edge. Generally
-- even rhythm without nadai change or gaps.
rela :: Korvai -> Korvai
rela = withType "rela"

chakradar :: Korvai -> Korvai
chakradar = withType "chakradar"

theka :: Korvai -> Korvai
theka = withType "theka"

lucknow :: Korvai -> Korvai
lucknow = withGharana "lucknow"

benares :: Korvai -> Korvai
benares = withGharana "benares"
