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
import           Solkattu.Dsl.Interactive (diff, diffw)
import qualified Solkattu.Format.Terminal as Terminal
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Realize as Realize
import qualified Solkattu.S as S
import qualified Solkattu.Solkattu as Solkattu
import qualified Solkattu.Talas as Talas
import           Solkattu.Talas (jhaptal, kehrwa, rupak, tintal)

import           Global
import           Solkattu.Dsl.Generic


type Sequence = SequenceT (Realize.Stroke Bol.Bol)

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
dhe = _bol Bol.Dhe
dhom= _bol Bol.Dhom
di  = _bol Bol.Di
ga  = _bol Bol.Ga
ge  = _bol Bol.Ge
ka  = _bol Bol.Ka
ki  = _bol Bol.Ki
na  = _bol Bol.Na
ne  = _bol Bol.Ne
ra  = _bol Bol.Ra
ta  = _bol Bol.Ta
taa = _bol Bol.Taa
te  = _bol Bol.Te
tet = _bol Bol.Tet
ti  = _bol Bol.Ti
tin = _bol Bol.Tin
tu  = _bol Bol.Tu
tun  = _bol Bol.Tun

-- * fragments

tette :: Sequence
tette = tet.te
taka :: Sequence
taka = ta.ka

tr, kt, trkt, tktrkt :: Sequence
tr = "tr"
kt = "tk"
trkt = "trkt"
tktrkt = su $ ta.ka.ti.ra.ki.te
trkttk = su $ ti.ra.ki.te.ta.ka

kttk :: Sequence
kttk = "kttk"

tetekata :: Sequence
tetekata = namedT Solkattu.GPattern "8n" $
    Solkattu.Standard ^ "tette katA gadi gene"

kali :: Sequence -> Sequence
kali = mapB $ \case
    Bol.Dha -> Just Bol.Taa
    Bol.Dhe -> Just Bol.The
    Bol.Dhen -> Just Bol.Ten
    Bol.Dhet -> Just Bol.Tet
    Bol.Dhi -> Just Bol.Tun
    Bol.Dhin -> Just Bol.Tin -- behause dha dhin dhin -> ta tin tin
    Bol.Ga -> Just Bol.Ka
    Bol.Ge -> Just Bol.Ke
    bol -> Just bol

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

{-
realizet :: Korvai.Korvai -> IO ()
realizet = _printInstrument Just Korvai.ITabla concrete

_printInstrument
    :: (Solkattu.Notation stroke1, Solkattu.Notation stroke2, Ord stroke1)
    => (Realize.Stroke stroke1 -> Maybe (Realize.Stroke stroke2))
    -> Korvai.Instrument stroke1
    -> (Terminal.Config -> Terminal.Config)
    -> Korvai -> IO ()
_printInstrument postproc inst setConfig =
    Interactive.printInstrument True True inst (_defaultStrokes inst)
        (setConfig Terminal.defaultConfig) postproc
-}

-- * korvai

korvai :: Talas.Tal -> [Section] -> Korvai
korvai = Korvai.tablaKorvai

korvai1 :: Talas.Tal -> Section -> Korvai
korvai1 tala section = korvai tala [section]

korvaiS :: Talas.Tal -> [Sequence] -> Korvai
korvaiS tala = korvai tala • Korvai.inferSections

korvaiS1 :: Talas.Tal -> Sequence -> Korvai
korvaiS1 tala seq = korvaiS tala [seq]

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
